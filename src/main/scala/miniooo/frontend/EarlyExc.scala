package miniooo.frontend

import spinal.core._
import spinal.lib._
import miniooo.util.PolymorphicDataChain
import miniooo.control._
import miniooo.util.MultiLaneFifo

object EarlyExceptionCode extends SpinalEnum(binarySequential) {
  val DECODE_ERROR, CACHE_MISS =
    newElement()
}

case class EarlyException() extends Bundle with PolymorphicDataChain {
  def parentObjects = Seq()
  val code = EarlyExceptionCode()
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
      out.exception.valid := True

      switch(op.code) {
        is(EarlyExceptionCode.DECODE_ERROR) {
          out.exception.code := MachineExceptionCode.DECODE_ERROR
        }
        is(EarlyExceptionCode.CACHE_MISS) {
          out.exception.code := MachineExceptionCode.INSN_CACHE_MISS
        }
      }

      out.exception.assignUnassignedByName(MachineException.idle)

      io_output <-/< io_input.translateWith(out)
    }
  }
}
