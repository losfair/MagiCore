package miniooo.lib.funit

import spinal.core._
import spinal.lib._
import miniooo.util.PolymorphicDataChain
import miniooo.control._

case class MultiplierConfig(
)

case class MultiplierOperation() extends Bundle {
  val aSigned = Bool()
  val bSigned = Bool()
  val upperHalf = Bool()
}

class Multiplier(staticTagData: => Data, c: MultiplierConfig)
    extends FunctionUnit {
  def staticTag: Data = staticTagData
  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    new FunctionUnitInstance {
      private val spec = Machine.get[MachineSpec]

      val io_available = True
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))

      val in = io_input.payload
      val out = CommitRequest(null)

      val dispatchInfo = in.lookup[DispatchInfo]
      val issue = in.lookup[IssuePort[_]]

      case class Payload() extends Bundle {
        val a = spec.dataType
        val b = spec.dataType
        val token = CommitToken()
        val op = MultiplierOperation()
      }

      val initialPayload = Payload()
      initialPayload.a := issue.srcRegData(0)
      initialPayload.b := issue.srcRegData(1)
      initialPayload.token := dispatchInfo.lookup[CommitToken]
      initialPayload.op := in.lookup[MultiplierOperation]
      val stream = io_input.translateWith(initialPayload).stage()

      case class Intermediate() extends Bundle {
        val value = SInt(((spec.dataWidth.value + 1) * 2) bits)
        val token = CommitToken()
        val op = MultiplierOperation()
      }
      val stage1Payload = Intermediate()

      val aExt =
        ((stream.payload.a.msb && stream.payload.op.aSigned) ## stream.payload.a).asSInt
      val bExt =
        ((stream.payload.b.msb && stream.payload.op.bSigned) ## stream.payload.b).asSInt
      stage1Payload.value := aExt * bExt
      stage1Payload.token := stream.payload.token
      stage1Payload.op := stream.payload.op
      val stage1 = stream.translateWith(stage1Payload).stage()

      out.token := stage1.payload.token
      out.exception := MachineException.idle

      val outValue = stage1.payload.value.asBits
      out.regWriteValue(0) := stage1.payload.op.upperHalf ? outValue(
        spec.dataWidth.value * 2 - 1 downto spec.dataWidth.value
      ) | outValue(spec.dataWidth.value - 1 downto 0)
      io_output <-/< stage1.translateWith(out)
    }
  }

}
