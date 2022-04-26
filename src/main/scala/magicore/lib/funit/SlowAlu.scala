package magicore.lib.funit

import spinal.core._
import spinal.lib._
import magicore.util.PolymorphicDataChain
import magicore.control._

object SlowAluOpcode extends SpinalEnum(binarySequential) {
  val CLZ, CTZ =
    newElement()
}

case class SlowAluOperation() extends Bundle {
  val opcode = SlowAluOpcode()
}

class SlowAlu(staticTagData: => Data) extends FunctionUnit {
  private val spec = Machine.get[MachineSpec]
  def staticTag: Data = staticTagData
  override def isAlu: Boolean = false
  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    // https://gitter.im/SpinalHDL/SpinalHDL?at=5bbe075e435c2a518e81dd83
    object LeadingZeros {
      def apply(input: Bits): UInt =
        calcOnes(~input).resize(log2Up(input.getWidth + 1))
      def calcOnes(input: Bits): UInt = input.getWidth match {
        case 0 => U""
        case 1 => input.asUInt
        case a => {
          val leftBits = 1 << (log2Up(a) - 1)
          val upper = calcOnes(input.resizeLeft(leftBits))
          val lower =
            calcOnes(input.resize(a - leftBits)).resize(upper.getWidth)
          (upper.msb ## lower.msb).mux(
            B"11" -> U"10" @@ upper.resize(upper.getWidth - 1),
            B"10" -> U"01" @@ lower.resize(lower.getWidth - 1),
            default -> U"00" @@ upper.resize(upper.getWidth - 1)
          )
        }
      }
    }
    case class ClzReq() extends Bundle {
      val token = CommitToken()
      val value = spec.dataType
    }
    new FunctionUnitInstance {
      val io_available = True
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))

      val clzPipeline = new Area {
        val input = Stream(ClzReq())
        input.setIdle()

        val output = Stream(CommitRequest(null))

        val out = CommitRequest(null)
        out.exception := MachineException.idle
        out.regWriteValue(0) := LeadingZeros(input.value).asBits.resized
        out.token := input.token
        output << input.translateWith(out)
      }

      clzPipeline.output >/-> io_output

      val triggerLogic = new Area {
        val bufferedInput = io_input.stage()
        bufferedInput.setBlocked()

        val op = bufferedInput.payload.lookup[SlowAluOperation]
        val token = bufferedInput.payload.lookup[CommitToken]
        val issue = bufferedInput.payload.lookup[IssuePort[_]]

        when(bufferedInput.valid) {
          switch(op.opcode) {
            is(SlowAluOpcode.CLZ) {
              val clzPayload = ClzReq()
              clzPayload.token := token
              clzPayload.value := issue.srcRegData(0)
              clzPipeline.input << bufferedInput.translateWith(clzPayload)
            }
            is(SlowAluOpcode.CTZ) {
              val ctzPayload = ClzReq()
              ctzPayload.token := token
              ctzPayload.value := issue.srcRegData(0).reversed
              clzPipeline.input << bufferedInput.translateWith(ctzPayload)
            }
          }
        }
      }
    }
  }
}
