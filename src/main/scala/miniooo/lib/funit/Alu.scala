package miniooo.lib.funit

import spinal.core._
import spinal.lib._
import miniooo.util.PolymorphicDataChain
import miniooo.control._

case class AluConfig(
    alu32: Boolean = false
)

object AluOpcode extends SpinalEnum(binarySequential) {
  val ADD, SUB, AND, OR, XOR = newElement()
}

case class AluOperation() extends Bundle with PolymorphicDataChain {
  def parentObjects = Seq()

  val opcode = AluOpcode()
  val alu32 = Bool()
  val const = Machine.get[MachineSpec].dataType
  val useConst = Bool()
}

case class Alu(staticTag: Data, c: AluConfig) extends FunctionUnit {
  override def singleCycleBypassable = true

  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    new FunctionUnitInstance {
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))

      val in = io_input.payload
      val out = CommitRequest(null)

      val op = in.lookup[AluOperation]
      val dispatchInfo = in.lookup[DispatchInfo]
      val issue = in.lookup[IssuePort[_]]
      val a = issue.srcRegData(0).asUInt
      val b = op.useConst ? op.const.asUInt | issue.srcRegData(1).asUInt
      out.robAddr := dispatchInfo.robIndex

      val outValue = UInt(out.regWriteValue(0).getWidth bits)
      outValue.assignDontCare()

      switch(op.opcode) {
        is(AluOpcode.ADD) {
          outValue := a + b
        }
        is(AluOpcode.SUB) {
          outValue := a - b
        }
        is(AluOpcode.AND) {
          outValue := a & b
        }
        is(AluOpcode.OR) {
          outValue := a | b
        }
        is(AluOpcode.XOR) {
          outValue := a ^ b
        }
      }

      if (c.alu32) {
        when(op.alu32) {
          out.regWriteValue(0) := outValue(31 downto 0).asBits.resized
        } otherwise {
          out.regWriteValue(0) := outValue.asBits
        }
      } else {
        out.regWriteValue(0) := outValue.asBits
      }

      io_output << io_input.translateWith(out)
    }
  }

}
