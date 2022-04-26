package magicore

import spinal.core._
import spinal.lib._
import magicore.control._
import magicore.lib.funit._
import magicore.util._
import scala.reflect._
import spinal.lib.bus.amba4.axi._

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
    numArchitecturalRegs = 32,
    numPhysicalRegs = 64,
    addrWidth = 32 bits,
    dataWidth = 64 bits,
    maxNumSrcRegsPerInsn = 2,
    maxNumDstRegsPerInsn = 1,
    issueQueueSize = 16,
    functionUnitTagType = HardType(TestTag()),
    robSize = 64,
    commitWidth = 2
  )

  val msem = new MachineSemantics {
    lazy val functionUnits: Seq[FunctionUnit] = Seq(
      new Alu(TestTag.static(0), AluConfig(alu32 = true)),
      new Multiplier(TestTag.static(1), MultiplierConfig()),
      new Divider(TestTag.static(2), enableException = true),
      new Lsu(TestTag.static(3), LsuConfig())
    )
  }

  object GenericOpcode extends SpinalEnum(binarySequential) {
    val ADD, SUB, AND, OR, XOR, MOV, DIV_S, DIV_U, REM_S, REM_U, LD, ST =
      newElement()

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
    val replaceOperandBwithConst = Bool()
    val opc = GenericOpcode()
    val alu32 = Bool()
    def parentObjects: Seq[Data] = Seq(decode)

    override def decodeAs[T <: AnyRef](ctag: ClassTag[T]): Option[T] = {
      if (ctag == classTag[AluOperation]) {
        val op = AluOperation()
        op.alu32 := alu32
        op.const := const.asBits.resized
        op.opcode := GenericOpcode.translateToAlu(opc)._2
        op.predicated := False
        op.replaceOperandBwithConst := replaceOperandBwithConst
        op.fillBranchFieldsForNonBranch()
        Some(op.asInstanceOf[T])
      } else if (ctag == classTag[DividerOperation]) {
        val op = DividerOperation()
        op.signed := opc === GenericOpcode.DIV_S || opc === GenericOpcode.REM_S
        op.useRemainder := opc === GenericOpcode.REM_S || opc === GenericOpcode.REM_U
        Some(op.asInstanceOf[T])
      } else if (ctag == classTag[MultiplierOperation]) {
        val op = MultiplierOperation()
        op.aSigned := False
        op.bSigned := False
        op.upperHalf := False
        Some(op.asInstanceOf[T])
      } else if (ctag == classTag[LsuOperation]) {
        val op = LsuOperation()
        op.isStore := opc === GenericOpcode.ST
        op.isFence := False
        op.size := LsuOperationSize.WORD
        op.offset := const.asSInt
        op.signExt := False
        Some(op.asInstanceOf[T])
      } else {
        None
      }
    }
  }

  Machine.provide(mspec)
  Machine.provide(msem)
  val pipeline = BackendPipeline(MockPayload())
  val lsu =
    pipeline.lookupFunctionUnitInstancesByType(classOf[LsuInstance]).head
  val io = new Bundle {
    val input = slave(Stream(MockPayload()))
    val writebackMonitor = out(
      Vec(
        pipeline.io.writebackMonitor.dataType(),
        pipeline.io.writebackMonitor.size
      )
    )
    val memBus = master(Axi4(lsu.io_axiMaster.config))
  }
  io.input >> pipeline.io.input
  val prfIf = Machine.get[PrfInterface]
  pipeline.io.writebackMonitor
    .zip(io.writebackMonitor)
    .foreach(x => x._1 >> x._2)
  lsu.io_axiMaster >> io.memBus
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
