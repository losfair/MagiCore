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

  val issue = IssueUnit(
    c = IssueConfig(portSpecs =
      sem.functionUnits.map(u =>
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

  rename.io.output >> dispatch.io.input

  new ResetArea(reset = reset, cumulative = true) {
    dispatch.io.output >/-> issue.io.input
  }

  issue.io.issueMonitor.translateWith(
    issue.io.issueMonitor.payload.toPhysSrcRegActivationMask()
  ) >> rename.io.physSrcRegActivationMask
  for ((fu, i) <- sem.functionUnits.zipWithIndex) {
    val unit = fu.generate(HardType(issue.issueDataType))
    issue.io
      .issuePorts(i)
      .asInstanceOf[Stream[PolymorphicDataChain]] >> unit.io_input
      .setCompositeName(this, "function_unit_input_" + i)
      .asInstanceOf[Stream[PolymorphicDataChain]]
    unit.io_output.setCompositeName(
      this,
      "function_unit_output_" + i
    ) >> dispatch.io.commit(i)
    issue.io.issueAvailable(i) := unit.io_available
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
