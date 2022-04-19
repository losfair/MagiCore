package miniooo.control

import spinal.core._
import spinal.lib._
import miniooo.util._
import MiniOoOExt._

case class BackendPipeline[T <: PolymorphicDataChain](inputType: HardType[T])
    extends Area {
  private val spec = Machine.get[MachineSpec]
  private val sem = Machine.get[MachineSemantics]

  val reset = Bool()

  val prf = PrfUnit(reset = reset)
  Machine.provide(prf.interface)

  val rename = RenameUnit(dataType = inputType, reset = reset)
  Machine.provide(RenameInterface(rename))

  val dispatch =
    DispatchUnit(dataType = HardType(rename.outType))
  reset := dispatch.reset
  val exception = dispatch.exception

  val oooIssue = IssueUnit(
    c = IssueConfig(portSpecs =
      sem.functionUnits
        .filter(x => !x.inOrder)
        .map(u =>
          IssueSpec(
            staticTag = u.staticTag,
            warnOnBlockedIssue = u.warnOnBlockedIssue,
            fastWakeup = u.isAlu
          )
        )
    ),
    dataType = HardType(dispatch.outType),
    reset = reset
  )

  val inOrderIssue =
    if (sem.functionUnits.exists(x => x.inOrder))
      InOrderIssueUnit(
        c = IssueConfig(portSpecs =
          sem.functionUnits
            .filter(x => x.inOrder)
            .map(u =>
              IssueSpec(
                staticTag = u.staticTag,
                warnOnBlockedIssue = u.warnOnBlockedIssue,
                fastWakeup = u.isAlu
              )
            )
        ),
        dataType = HardType(dispatch.outType),
        reset = reset
      )
    else null

  rename.io.output >> dispatch.io.input

  new ResetArea(reset = reset, cumulative = true) {
    dispatch.io.oooOutput >/-> oooIssue.io.input
    if (inOrderIssue != null)
      dispatch.io.inOrderOutput >/-> inOrderIssue.io.input
    else
      dispatch.io.inOrderOutput.setBlocked()
  }

  rename.io.physSrcRegActivationMask_ooo << oooIssue.io.issueMonitor
    .translateWith(
      oooIssue.io.issueMonitor.payload.toPhysSrcRegActivationMask()
    )

  if (inOrderIssue != null)
    rename.io.physSrcRegActivationMask_ino << inOrderIssue.io.issueMonitor
      .translateWith(
        inOrderIssue.io.issueMonitor.payload.toPhysSrcRegActivationMask()
      )
  else rename.io.physSrcRegActivationMask_ino.setIdle()

  for (
    ((fu, i), j) <- sem.functionUnits.zipWithIndex
      .filter(x => !x._1.inOrder)
      .zipWithIndex
  ) {
    val unit = fu.generate(HardType(oooIssue.issueDataType))
    oooIssue.io
      .issuePorts(j)
      .asInstanceOf[Stream[PolymorphicDataChain]] >> unit.io_input
      .setCompositeName(this, "ooo_function_unit_input_" + i)
      .asInstanceOf[Stream[PolymorphicDataChain]]
    unit.io_output.setCompositeName(
      this,
      "ooo_function_unit_output_" + i
    ) >> dispatch.io.commit(i)
    oooIssue.io.issueAvailable(j) := unit.io_available
  }

  if (inOrderIssue != null)
    for (
      ((fu, i), j) <- sem.functionUnits.zipWithIndex
        .filter(x => x._1.inOrder)
        .zipWithIndex
    ) {
      val unit = fu.generate(HardType(inOrderIssue.issueDataType))
      inOrderIssue.io
        .issuePorts(j)
        .asInstanceOf[Stream[PolymorphicDataChain]] >> unit.io_input
        .setCompositeName(this, "ino_function_unit_input_" + i)
        .asInstanceOf[Stream[PolymorphicDataChain]]
      unit.io_output.setCompositeName(
        this,
        "ino_function_unit_output_" + i
      ) >> dispatch.io.commit(i)
    }

  val io = new Bundle {
    val input = Stream(inputType)

    val writebackMonitor =
      Vec(Flow(CommitRequest(dispatch.dataType)), spec.commitWidth)
  }

  io.input >> rename.io.input
  dispatch.io.writebackMonitor
    .zip(io.writebackMonitor)
    .foreach(x => x._1 >> x._2)
}
