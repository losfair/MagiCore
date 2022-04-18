package miniooo

import spinal.core._
import spinal.lib._
import miniooo.control._
import miniooo.lib.funit._
import miniooo.util._
import scala.reflect._

class MiniGen extends Component {
  object TestTag {
    def static(tag: Int): TestTag = {
      val t = TestTag()
      assert(tag >= 0 && tag <= t.tag.maxValue)
      t.tag := tag
      t
    }
  }
  case class TestTag() extends Bundle {
    val tag = UInt(4 bits)
  }

  val mspec = MachineSpec(
    numArchitecturalRegs = 16,
    numPhysicalRegs = 64,
    dataWidth = 64 bits,
    maxNumSrcRegsPerInsn = 2,
    maxNumDstRegsPerInsn = 1,
    issueQueueSize = 16,
    functionUnitTagType = HardType(TestTag()),
    robSize = 64,
    commitWidth = 2
  )

  val msem = new MachineSemantics {
    def functionUnits: Seq[FunctionUnit] = Seq(
      new Alu(TestTag.static(0), AluConfig(alu32 = true), lowLatency = true),
      new Multiplier(TestTag.static(1), MultiplierConfig()),
      new Divider(TestTag.static(2))
    )
  }

  object GenericOpcode extends SpinalEnum(binarySequential) {
    val ADD, SUB, AND, OR, XOR, MOV, DIV_S, DIV_U, REM_S, REM_U = newElement()

    def translateToAlu(
        that: SpinalEnumCraft[GenericOpcode.type]
    ): (Bool, SpinalEnumCraft[AluOpcode.type]) = {
      val out = AluOpcode()
      val ok = True
      out.assignDontCare()
      switch(that) {
        is(ADD) { out := AluOpcode.ADD }
        is(SUB) { out := AluOpcode.SUB }
        is(AND) { out := AluOpcode.AND }
        is(OR) { out := AluOpcode.OR }
        is(XOR) { out := AluOpcode.XOR }
        is(MOV) { out := AluOpcode.MOV }
        default { ok := False }
      }
      (ok, out)
    }
  }

  case class MockPayload() extends Bundle with PolymorphicDataChain {
    val decode = DecodeInfo(null)
    val const = UInt(32 bits)
    val useConst = Bool()
    val opc = GenericOpcode()
    val alu32 = Bool()
    def parentObjects: Seq[Data] = Seq(decode)

    override def decodeAs[T <: AnyRef](ctag: ClassTag[T]): Option[T] = {
      if (ctag == classTag[AluOperation]) {
        val op = AluOperation()
        op.alu32 := alu32
        op.const := const.asBits.resized
        op.opcode := GenericOpcode.translateToAlu(opc)._2
        op.useConst := useConst
        Some(op.asInstanceOf[T])
      } else if (ctag == classTag[DividerOperation]) {
        val op = DividerOperation()
        op.signed := opc === GenericOpcode.DIV_S || opc === GenericOpcode.REM_S
        op.useRemainder := opc === GenericOpcode.REM_S || opc === GenericOpcode.REM_U
        Some(op.asInstanceOf[T])
      } else {
        None
      }
    }
  }

  Machine.provide(mspec)
  Machine.provide(msem)
  val pipeline = BackendPipeline(MockPayload())
  val io = new Bundle {
    val input = slave(Stream(MockPayload()))

    val regReadAddr = in(Machine.get[MachineSpec].archRegIndexType)
    val regReadData = out(Machine.get[MachineSpec].dataType)
    val writebackMonitor = out(
      Vec(
        pipeline.io.writebackMonitor.dataType(),
        pipeline.io.writebackMonitor.size
      )
    )
  }
  io.input >> pipeline.io.input
  val prfIf = Machine.get[PrfInterface]
  io.regReadData := prfIf.readAsync(pipeline.rename.cmt(io.regReadAddr)).data
  pipeline.io.writebackMonitor
    .zip(io.writebackMonitor)
    .foreach(x => x._1 >> x._2)
}

object MiniGenVerilogSyncReset {
  object SyncResetSpinalConfig
      extends SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)
      )

  def main(args: Array[String]) {
    SyncResetSpinalConfig.generateVerilog(Machine.build { new MiniGen })
  }
}
