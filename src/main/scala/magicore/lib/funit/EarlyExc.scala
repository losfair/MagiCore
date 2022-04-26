package magicore.lib.funit

import spinal.core._
import spinal.lib._
import magicore.util.PolymorphicDataChain
import magicore.control._
import magicore.util.MultiLaneFifo

object EarlyExceptionCode extends SpinalEnum(binarySequential) {
  val DECODE_ERROR, CACHE_MISS, SERIALIZE, INSN_CACHE_FLUSH, EXCEPTION_RETURN,
      INSN_ALIGNMENT_ERROR, EXT_INTERRUPT, ENV_CALL =
    newElement()
}

case class EarlyException() extends Bundle with PolymorphicDataChain {
  def parentObjects = Seq()
  val code = EarlyExceptionCode()
  val interruptCause = UInt(4 bits)
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

      switch(op.code) {
        is(EarlyExceptionCode.DECODE_ERROR) {
          out.exception.code := MachineExceptionCode.DECODE_ERROR
        }
        is(EarlyExceptionCode.CACHE_MISS) {
          out.exception.code := MachineExceptionCode.INSN_CACHE_MISS
        }
        is(EarlyExceptionCode.SERIALIZE) {
          out.exception.code := MachineExceptionCode.SERIALIZE
        }
        is(EarlyExceptionCode.INSN_CACHE_FLUSH) {
          out.exception.code := MachineExceptionCode.INSN_CACHE_FLUSH
        }
        is(EarlyExceptionCode.EXCEPTION_RETURN) {
          out.exception.code := MachineExceptionCode.EXCEPTION_RETURN
        }
        is(EarlyExceptionCode.INSN_ALIGNMENT_ERROR) {
          out.exception.code := MachineExceptionCode.INSN_ALIGNMENT_ERROR
        }
        is(EarlyExceptionCode.EXT_INTERRUPT) {
          out.exception.code := MachineExceptionCode.EXT_INTERRUPT
          out.exception.extInterrupt_cause := op.interruptCause.asBits
        }
        is(EarlyExceptionCode.ENV_CALL) {
          out.exception.code := MachineExceptionCode.ENV_CALL
        }
      }

      io_output <-/< io_input.translateWith(out)
    }
  }
}
