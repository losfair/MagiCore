package magicore.control

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import scala.collection.mutable.ArrayBuffer

case class BackendPipeline[T <: PolymorphicDataChain](inputType: HardType[T])
    extends Area {
  private val spec = Machine.get[MachineSpec]
  private val sem = Machine.get[MachineSemantics]

  val epochMgr = EpochManager()
  Machine.provide(epochMgr)

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
            fastWakeup = u.isAlu,
            inOrder_sideEffect = u.inOrder_sideEffect
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
                fastWakeup = u.isAlu,
                inOrder_sideEffect = u.inOrder_sideEffect
              )
            )
        ),
        dataType = HardType(dispatch.outType),
        reset = reset
      )
    else null

  rename.io.output >> dispatch.io.input

  Machine.get[MachineException].resetArea {
    dispatch.io.oooOutput >/-> oooIssue.io.input
    if (inOrderIssue != null)
      dispatch.io.inOrderOutput >/-> inOrderIssue.io.input
    else
      dispatch.io.inOrderOutput.setBlocked()
  }

  private val functionUnitInstances_ = (0 until sem.functionUnits.length)
    .map(_ => null.asInstanceOf[FunctionUnitInstance])
    .to[ArrayBuffer]

  for (
    ((fu, i), j) <- sem.functionUnits.zipWithIndex
      .filter(x => !x._1.inOrder)
      .zipWithIndex
  ) {
    val unit = fu.generate(HardType(oooIssue.issueDataType))
    functionUnitInstances_.update(i, unit)
    oooIssue.io
      .issuePorts(j)
      .asInstanceOf[Stream[PolymorphicDataChain]] >> unit.io_input
      .setCompositeName(this, "ooo_function_unit_input_" + i)
      .asInstanceOf[Stream[PolymorphicDataChain]]
    unit.io_output.setCompositeName(
      this,
      "ooo_function_unit_output_" + i
    ) >> dispatch.io.commitOoO(j)
    oooIssue.io.issueAvailable(j) := unit.io_available
  }

  if (inOrderIssue != null)
    for (
      ((fu, i), j) <- sem.functionUnits.zipWithIndex
        .filter(x => x._1.inOrder)
        .zipWithIndex
    ) {
      val unit = fu.generate(HardType(inOrderIssue.issueDataType))
      functionUnitInstances_.update(i, unit)
      inOrderIssue.io
        .issuePorts(j)
        .asInstanceOf[Stream[PolymorphicDataChain]] >> unit.io_input
        .setCompositeName(this, "ino_function_unit_input_" + i)
        .asInstanceOf[Stream[PolymorphicDataChain]]
      unit.io_output.setCompositeName(
        this,
        "ino_function_unit_output_" + i
      ) >> dispatch.io.commitInO(j)
    }

  val io = new Bundle {
    val input = Stream(inputType)

    val writebackMonitor =
      Vec(Flow(CommitRequest(dispatch.dataType, genRegWriteValue = false)), spec.commitWidth)
  }

  io.input >> rename.io.input
  dispatch.io.writebackMonitor
    .zip(io.writebackMonitor)
    .foreach(x => x._1 >> x._2)

  val functionUnitInstances = functionUnitInstances_.toSeq

  def lookupFunctionUnitInstancesByType[T <: FunctionUnitInstance](
      cls: Class[T]
  ): Seq[T] =
    functionUnitInstances
      .filter(x => cls.isAssignableFrom(x.getClass()))
      .map(_.asInstanceOf[T])
}
