package miniooo.isa.riscv

import spinal.core._
import spinal.lib._
import miniooo.util._
import MiniOoOExt._
import miniooo.control._
import miniooo.frontend._
import miniooo.lib.funit._
import spinal.lib.bus.amba4.axi._

case class RiscvProcessor(resetPc: BigInt) extends Component {
  object FuTag extends SpinalEnum(binarySequential) {
    val ALU, LSU, MUL, DIV, EARLY_EXC = newElement()
  }

  val mspec = MachineSpec(
    numArchitecturalRegs = 32,
    numPhysicalRegs = 64,
    addrWidth = 32 bits,
    dataWidth = 32 bits,
    maxNumSrcRegsPerInsn = 2,
    maxNumDstRegsPerInsn = 1,
    issueQueueSize = 16,
    functionUnitTagType = HardType(FuTag()),
    robSize = 64,
    commitWidth = 2
  )

  val fspec = FrontendSpec(
    icacheSize = 8192,
    icacheMemPortDataWidth = 32,
    insnWidth = 32 bits,
    addrWidth = 32 bits,
    resetPc = resetPc,
    globalHistorySize = 1024
  )

  Machine.provide(mspec)
  Machine.provide(fspec)

  val msem = new MachineSemantics {
    lazy val functionUnits: Seq[FunctionUnit] = Seq(
      new Alu(FuTag.LSU, AluConfig(alu32 = false)),
      new Multiplier(FuTag.MUL, MultiplierConfig()),
      new Divider(FuTag.DIV, enableException = true),
      new Lsu(FuTag.LSU, LsuConfig()),
      new EarlyExcPassthrough(FuTag.EARLY_EXC)
    )
  }
  Machine.provide(msem)

  val pipeline = BackendPipeline(DecodePacket())
  val lsu =
    pipeline.lookupFunctionUnitInstancesByType(classOf[LsuInstance]).head

  val fetch = FetchUnit()
  val decode = RiscvDecoder(
    aluPort = FuTag.ALU,
    earlyExceptionPort = FuTag.EARLY_EXC,
    lsuPort = FuTag.LSU,
    mulPort = FuTag.MUL,
    divPort = FuTag.DIV
  )
  fetch.io.output >> decode.io.input
  decode.io.output >/-> pipeline.io.input

  fetch.io.branchInfoFeedback := decode.io.branchInfoFeedback
  fetch.io.flush := False

  val io = new Bundle {
    val writebackMonitor = out(
      Vec(
        pipeline.io.writebackMonitor.dataType(),
        pipeline.io.writebackMonitor.size
      )
    )
    val iBus = master(Axi4ReadOnly(fetch.io.memBus.config))
    val dBus = master(Axi4(lsu.io_axiMaster.config))
  }

  pipeline.io.writebackMonitor
    .zip(io.writebackMonitor)
    .foreach(x => x._1 >> x._2)
  fetch.io.memBus >> io.iBus
  lsu.io_axiMaster >> io.dBus
}

object RiscvProcessorSyncReset {
  object SyncResetSpinalConfig
      extends SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)
      )

  def main(args: Array[String]) {
    SyncResetSpinalConfig.generateVerilog(Machine.build {
      new RiscvProcessor(resetPc = 0x0)
    })
  }
}
