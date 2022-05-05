package magicore.isa.riscv

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import magicore.control._
import magicore.frontend._
import magicore.lib.funit._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping

case class RiscvProcessor(
    resetPc: BigInt = 0x0,
    debug: Boolean = false,
    initBranchPredictionBuffers: Boolean = false,
    rv64: Boolean = false,
    ioMemoryRegions: Seq[SizeMapping] = Seq(),
    amo: Boolean = true,
    compressed: Boolean = false
) extends Area {
  if(!rv64 && compressed) {
    throw new Exception("RV32 with compressed is not supported")
  }
  object FuTag extends SpinalEnum(binarySequential) {
    val ALU, LSU, MUL, DIV, SLOW_ALU, EARLY_EXC, CSR = newElement()
  }

  val dataWidth = if (rv64) 64 else 32

  val mspec = MachineSpec(
    numArchitecturalRegs = 32,
    numPhysicalRegs = 64,
    addrWidth = 32 bits,
    dataWidth = dataWidth bits,
    maxNumSrcRegsPerInsn = 2,
    maxNumDstRegsPerInsn = 1,
    issueQueueSize = 16,
    functionUnitTagType = HardType(FuTag()),
    robSize = 64,
    writebackWidth = 2
  )

  val fspec = FrontendSpec(
    icacheSize = 16384,
    icacheMemPortDataWidth = dataWidth,
    insnWidth = 32 bits,
    addrWidth = 32 bits,
    resetPc = resetPc,
    globalHistorySize = 4096,
    btbSize = 128,
    initBranchPredictionBuffers = initBranchPredictionBuffers,
    compressed = compressed
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
      new Alu(FuTag.ALU, AluConfig(alu32 = rv64)),
      new Multiplier(FuTag.MUL, MultiplierConfig(mul32 = rv64)),
      new Divider(FuTag.DIV, div32 = rv64, enableException = false),
      new SlowAlu(FuTag.SLOW_ALU),
      new Lsu(FuTag.LSU, LsuConfig(ioMemoryRegions = ioMemoryRegions)),
      new EarlyExcPassthrough(FuTag.EARLY_EXC),
      new RvCsr(FuTag.CSR)
    )
  }
  Machine.provide(msem)

  val pipeline = BackendPipeline(DecodePacket())
  val lsu =
    pipeline.lookupFunctionUnitInstancesByType(classOf[LsuInstance]).head

  if (compressed) {
    val decompressor = RiscvDecompressor()
    decompressor.provide()
  }

  val fetch = FetchUnit()
  val decode = RiscvDecoder(
    amo = amo,
    compressed = compressed,
    rv64 = rv64,
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

    val iBus = Axi4ReadOnly(
      fetch.io.memBus.config.copy(dataWidth = lsu.io_axiMaster.config.dataWidth)
    ) // master
    val dBus = Axi4(lsu.io_axiMaster.config) // master
    val interrupt = RvInterruptLines() // in
  }

  intrSvc.setLines(io.interrupt)

  pipeline.io.writebackMonitor
    .zip(io.writebackMonitor)
    .foreach(x => x._1 >> x._2)
  if (fetch.io.memBus.config.dataWidth != io.iBus.config.dataWidth) {
    assert(fetch.io.memBus.config.dataWidth * 2 == io.iBus.config.dataWidth)
    val upsizer = Axi4ReadOnlyUpsizer(fetch.io.memBus.config, io.iBus.config, 4)
    fetch.io.memBus >> upsizer.io.input
    upsizer.io.output >> io.iBus
  } else {
    fetch.io.memBus >> io.iBus
  }
  lsu.io_axiMaster >> io.dBus
}
