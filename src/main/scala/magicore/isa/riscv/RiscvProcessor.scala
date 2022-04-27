package magicore.isa.riscv

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import magicore.control._
import magicore.frontend._
import magicore.lib.funit._
import spinal.lib.bus.amba4.axi._

case class RiscvProcessor(
    resetPc: BigInt = 0x0,
    debug: Boolean = false,
    initBranchPredictionBuffers: Boolean = false
) extends Area {
  object FuTag extends SpinalEnum(binarySequential) {
    val ALU, LSU, MUL, DIV, SLOW_ALU, EARLY_EXC, CSR = newElement()
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
    writebackWidth = 2
  )

  val fspec = FrontendSpec(
    icacheSize = 16384,
    icacheMemPortDataWidth = 32,
    insnWidth = 32 bits,
    addrWidth = 32 bits,
    resetPc = resetPc,
    globalHistorySize = 4096,
    btbSize = 128,
    initBranchPredictionBuffers = initBranchPredictionBuffers
  )

  Machine.provide(mspec)
  Machine.provide(fspec)

  if (debug) Machine.provide(MachineDebugMarker)

  val intrSvc = RvInterruptService()
  Machine.provide(intrSvc)

  val csr = RvCsrFileReg()
  csr.provide()

  val msem = new MachineSemantics {
    lazy val functionUnits: Seq[FunctionUnit] = Seq(
      new Alu(FuTag.ALU, AluConfig(alu32 = false)),
      new Multiplier(FuTag.MUL, MultiplierConfig()),
      new Divider(FuTag.DIV, enableException = false),
      new SlowAlu(FuTag.SLOW_ALU),
      new Lsu(FuTag.LSU, LsuConfig()),
      new EarlyExcPassthrough(FuTag.EARLY_EXC),
      new RvCsr(FuTag.CSR)
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
    divPort = FuTag.DIV,
    csrPort = FuTag.CSR,
    slowAluPort = FuTag.SLOW_ALU
  )
  fetch.io.output >> decode.io.input

  Machine.get[MachineException].resetArea {
    decode.io.output >/-> pipeline.io.input
  }

  fetch.io.branchInfoFeedback := decode.io.branchInfoFeedback

  val io = new Bundle {
    val writebackMonitor =
      Vec(
        pipeline.io.writebackMonitor.dataType(),
        pipeline.io.writebackMonitor.size
      ) // out

    val iBus = Axi4ReadOnly(fetch.io.memBus.config) // master
    val dBus = Axi4(lsu.io_axiMaster.config) // master
    val interrupt = RvInterruptLines() // in
  }

  intrSvc.setLines(io.interrupt)

  pipeline.io.writebackMonitor
    .zip(io.writebackMonitor)
    .foreach(x => x._1 >> x._2)
  fetch.io.memBus >> io.iBus
  lsu.io_axiMaster >> io.dBus
}
