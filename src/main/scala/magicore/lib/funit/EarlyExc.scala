package magicore.lib.funit

import spinal.core._
import spinal.lib._
import magicore.util.PolymorphicDataChain
import magicore.control._
import magicore.util.MultiLaneFifo

case class EarlyException() extends Bundle with PolymorphicDataChain {
  def parentObjects = Seq()
  val code = MachineExceptionCode()
}

class EarlyExcPassthrough(staticTagData: => Data) extends FunctionUnit {
  def staticTag: Data = staticTagData
  private val spec = Machine.get[MachineSpec]

  private var effInst: EffectInstance = null

  override def inOrder: Boolean = true

  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    new FunctionUnitInstance {
      val io_available: Bool = null
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))
      val out = CommitRequest(null)

      val op = io_input.payload.lookup[EarlyException]
      val token = io_input.payload.lookup[CommitToken]
      out.regWriteValue.assignDontCare()
      out.token := token
      out.exception.assignDontCare()
      out.exception.valid := True
      out.exception.code := op.code

      io_output <-/< io_input.translateWith(out)
    }
  }
}
