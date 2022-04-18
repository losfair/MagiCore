package miniooo.lib.funit

import spinal.core._
import spinal.lib._
import miniooo.util.PolymorphicDataChain
import miniooo.control._

case class MultiplierConfig(

)

case class Multiplier(staticTag: Data, c: MultiplierConfig) extends FunctionUnit {
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
        val robIndex = spec.robEntryIndexType()
      }

      val initialPayload = Payload()
      initialPayload.a := issue.srcRegData(0)
      initialPayload.b := issue.srcRegData(1)
      initialPayload.robIndex := dispatchInfo.robIndex
      val stream = io_input.translateWith(initialPayload).stage()

      case class Intermediate() extends Bundle {
        val value = spec.dataType
        val robIndex = spec.robEntryIndexType()
      }
      val stage1Payload = Intermediate()
      stage1Payload.value := (stream.payload.a.asUInt * stream.payload.b.asUInt).asBits.resized
      stage1Payload.robIndex := stream.payload.robIndex
      val stage1 = stream.translateWith(stage1Payload).stage()

      out.robAddr := stage1.payload.robIndex
      out.regWriteValue(0) := stage1.payload.value
      io_output << stage1.translateWith(out)
    }
  }

}
