package magicore.control

import spinal.core._
import spinal.lib._
import magicore.util.PolymorphicDataChain

case class CommitEffect(dataType: HardType[_ <: Data] = null)
    extends Bundle
    with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]
  val data: Data = if (dataType == null) null else dataType()
  val robIndex = spec.robEntryIndexType()
  def parentObjects = Seq(data)

  def refine(): CommitEffect = {
    val e = CommitEffect()
    e.robIndex := robIndex
    e
  }
}

trait FunctionUnit {
  def staticTag: Data
  def warnOnBlockedIssue: Boolean = false
  def inOrder: Boolean = false
  def inOrder_sideEffect: Boolean = false
  def isAlu: Boolean =
    false // The ALU receives special treatment for low-latency issueing.
  def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance
  def generateEffect(): Option[EffectInstance] = None
}

trait FunctionUnitInstance extends Area {
  def io_available: Bool
  def io_input: Stream[_ <: PolymorphicDataChain]
  def io_output: Stream[CommitRequest]
}

trait EffectInstance extends Area {
  val io_effect: Vec[Flow[CommitEffect]] =
    Vec(
      Flow(CommitEffect(Machine.get[FullCommitRequestType].ty)),
      Machine.get[MachineSpec].commitWidth
    )
  val io_reset: Bool = Bool()
}
