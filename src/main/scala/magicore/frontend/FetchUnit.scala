package magicore.frontend

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import magicore.control._
import vexriscv.ip.InstructionCache
import vexriscv.ip.MemoryTranslatorBusParameter
import vexriscv.ip.InstructionCacheConfig
import spinal.lib.bus.amba4.axi._
import scala.reflect._
import magicore.lib.funit.AluBranchContext

object BranchInfoFeedback {
  def idle: BranchInfoFeedback = {
    val x = BranchInfoFeedback()
    x.isUnconditionalStaticBranch := False
    x.isConditionalBranch := False
    x.isUnconditionalDynamicBranch := False
    x.backward := False
    x.target.assignDontCare()
    x
  }
}

case class BranchInfoFeedback() extends Bundle {
  private val fspec = Machine.get[FrontendSpec]

  val isUnconditionalStaticBranch = Bool()
  val isConditionalBranch = Bool()
  val isUnconditionalDynamicBranch = Bool()
  val backward = Bool()
  val target = fspec.addrType()
}

case class FetchPacket() extends Bundle with PolymorphicDataChain {
  private val fspec = Machine.get[FrontendSpec]
  def parentObjects = Seq()

  val cacheMiss = Bool()
  val pc = fspec.addrType()
  val insn = fspec.insnType()
  val pcTag = Bool()
  val globalHistory = fspec.globalHistoryType()
  val predictedBranchValid = Bool()
  val predictedBranchTarget = fspec.addrType()

  override def decodeAs[T <: AnyRef](ctag: ClassTag[T]): Option[T] = {
    if (ctag == classTag[AluBranchContext]) {
      val ctx = AluBranchContext(
        globalHistoryWidth = fspec.globalHistoryWidth
      )
      ctx.pc := pc.asBits
      ctx.predictedBranchValid := predictedBranchValid
      ctx.predictedBranchTarget := predictedBranchTarget.asBits
      Some(ctx.asInstanceOf[T])
    } else {
      None
    }
  }
}

case class FetchRestartSignal(valid: Bool, pc: UInt)

case class FetchUnit() extends Area {
  private val fspec = Machine.get[FrontendSpec]

  def pcWordAddr(pc: UInt): UInt = pc >> log2Up(fspec.addrStep)

  object PcGen {
    def init: PcGen = {
      val x = PcGen()
      x.pc := fspec.resetPc
      x.pcTag := False
      x
    }
  }

  case class PcGen() extends Bundle {
    val pc = fspec.addrType()
    val pcTag = Bool()
  }

  val icache = new InstructionCache(
    InstructionCacheConfig(
      cacheSize = fspec.icacheSize,
      bytePerLine = 32,
      addressWidth = fspec.addrWidth.value,
      cpuDataWidth = fspec.insnWidth.value,
      memDataWidth = fspec.icacheMemPortDataWidth,
      catchIllegalAccess = false,
      catchAccessFault = false,
      asyncTagMemory = true,
      wayCount = 2,
      twoCycleCache = false
    ),
    MemoryTranslatorBusParameter()
  )

  val io = new Bundle {
    val memBus = icache.io.mem.toAxi4ReadOnly()
    val output = Stream(FetchPacket())
    val branchInfoFeedback = BranchInfoFeedback()
  }
  val exc = Machine.get[FullMachineException]

  icache.io.flush := False

  val pcStream = Stream(FetchPacket())
  val speculatedGlobalHistory = Reg(fspec.globalHistoryType()) init (0)

  case class BtbEntry() extends Bundle {
    val valid = Bool()
    val from = fspec.addrType()
    val to = fspec.addrType()
  }

  // PC generation
  val pcStreamGen = new Area {
    val valid = Reg(Bool()) init (true)
    val pc = Reg(PcGen()) init (PcGen.init)
    when(pcStream.fire) {
      Machine.report(Seq("PC stream: pc=", pc.pc, " pcTag=", pc.pcTag))
      pc.pc := (pc.pc & fspec.addrMask) + fspec.addrStep
    }

    pcStream.valid := valid
    pcStream.payload.pc := pc.pc
    pcStream.payload.pcTag := pc.pcTag
  }

  val lowLatencyPredictor = new Area {
    val btb = Mem(BtbEntry(), fspec.btbSize)
    if (fspec.initBranchPredictionBuffers)
      btb.init((0 until fspec.btbSize).map(_ => {
        val e = BtbEntry()
        e.valid := False
        e.from := 0
        e.to := 0
        e
      }))
    val btbEntry = btb(
      pcWordAddr(pcStreamGen.pc.pc).resize(fspec.btbWidth)
    )
    val btbHit = btbEntry.valid && btbEntry.from === pcStreamGen.pc.pc
    pcStream.payload.predictedBranchValid := btbHit
    pcStream.payload.predictedBranchTarget := btbEntry.to

    when(pcStream.fire && btbHit) {
      pcStreamGen.pc.pc := btbEntry.to
    }

    val btbWriteValid = False
    val btbWriteAddr = UInt(fspec.btbWidth) assignDontCare ()
    val btbWriteData = BtbEntry() assignDontCare ()
    btb.write(btbWriteAddr, btbWriteData, btbWriteValid)
  }

  // Fetch
  val s1 = new Area {
    val rescheduleTag = exc.exc.resetArea { Reg(Bool()) init (false) }
    val pcFetchStage = exc.exc.resetArea { pcStream.stage() }

    val out = Stream(FetchPacket())

    icache.io.cpu.prefetch.isValid := pcStream.valid
    icache.io.cpu.prefetch.pc := pcStream.payload.pc & fspec.addrMask

    icache.io.cpu.fetch.isValid := pcFetchStage.valid
    icache.io.cpu.fetch.pc := pcFetchStage.pc & fspec.addrMask
    icache.io.cpu.fetch.isStuck := out.isStall
    icache.io.cpu.fetch.isRemoved := False // ???
    icache.io.cpu.fetch.mmuRsp.physicalAddress := pcFetchStage.pc & fspec.addrMask

    val data = FetchPacket()

    // `cacheMiss` can be `X` on the first cycle.
    // XXX: Review this.
    data.cacheMiss := icache.io.cpu.fetch.cacheMiss || icache.io.cpu.prefetch.haltIt
    data.insn := icache.io.cpu.fetch.data
    data.pc := pcFetchStage.pc
    data.pcTag := pcFetchStage.pcTag
    data.globalHistory := speculatedGlobalHistory
    data.predictedBranchValid := pcFetchStage.predictedBranchValid
    data.predictedBranchTarget := pcFetchStage.predictedBranchTarget

    out << pcFetchStage
      .throwWhen(pcFetchStage.payload.pcTag =/= rescheduleTag)
      .translateWith(data)

    exc.exc.resetArea {
      // Payload can change during refill
      out.check()
    }

    io.output << out

    when(out.fire) {
      Machine.report(
        Seq(
          "Sent out fetch packet pc=",
          data.pc,
          " cacheMiss=",
          data.cacheMiss,
          " insn=",
          data.insn,
          " pcTag=",
          data.pcTag
        )
      )
    }
  }

  // Predict
  // XXX: Feedback path!
  val s2 = new Area {
    object GshareEntry {
      def idle: GshareEntry = {
        val x = GshareEntry()
        x.valid := False
        x.taken := False
        x
      }
    }
    case class GshareEntry() extends Bundle {
      val valid = Bool()
      val taken = Bool()
    }
    val gshareMem = Mem(GshareEntry(), fspec.globalHistorySize)
    if (fspec.initBranchPredictionBuffers)
      gshareMem.init((0 until fspec.globalHistorySize).map(_ => {
        val e = GshareEntry()
        e.valid := False
        e.taken := False
        e
      }))

    val gshareMemWriteValid = False
    val gshareMemWriteAddr = UInt(fspec.globalHistoryWidth) assignDontCare ()
    val gshareMemWriteData = GshareEntry() assignDontCare ()
    gshareMem.write(gshareMemWriteAddr, gshareMemWriteData, gshareMemWriteValid)

    val gshareQuery = gshareMem(
      (s1.data.globalHistory ^ pcWordAddr(s1.out.pc)
        .resize(fspec.globalHistoryWidth)
        .asBits).asUInt
    )
    when(s1.out.fire) {
      val decision = False
      when(io.branchInfoFeedback.isConditionalBranch) {
        when(gshareQuery.valid) {
          decision := gshareQuery.taken
        } otherwise {
          // Predict backward branches to be taken by default, if dynamic information is not available
          decision := io.branchInfoFeedback.backward
        }
        s1.data.predictedBranchValid := decision
        s1.data.predictedBranchTarget := io.branchInfoFeedback.target
        speculatedGlobalHistory := speculatedGlobalHistory(
          speculatedGlobalHistory.getWidth - 2 downto 0
        ) ## decision.asBits
      } elsewhen (io.branchInfoFeedback.isUnconditionalStaticBranch) {
        decision := True
        s1.data.predictedBranchValid := True
        s1.data.predictedBranchTarget := io.branchInfoFeedback.target
      }

      // Reschedule if:
      // - Our decision is different from the low-latency predictor's one, and
      // - This branch is not a dynamic branch (in which case we are not able to make a prediction)
      val reschedule =
        (decision =/= s1.pcFetchStage.predictedBranchValid ||
          (
            decision && io.branchInfoFeedback.target =/= s1.pcFetchStage.predictedBranchTarget
          )) && !io.branchInfoFeedback.isUnconditionalDynamicBranch

      when(reschedule) {
        val next = decision.mux(
          True -> io.branchInfoFeedback.target,
          False -> ((s1.out.pc & fspec.addrMask) + fspec.addrStep)
        )

        Machine.report(
          Seq(
            "Rescheduling based on prediction - pc=",
            s1.out.pc,
            " brTarget=",
            io.branchInfoFeedback.target,
            " nextPC=",
            next,
            " decision=",
            decision,
            " predicted=",
            s1.data.predictedBranchValid
          )
        )

        s1.rescheduleTag := !s1.rescheduleTag
        pcStreamGen.pc.pc := next
        pcStreamGen.pc.pcTag := !s1.rescheduleTag

        val btbEntry = BtbEntry()
        btbEntry.valid := decision
        btbEntry.from := s1.out.pc
        btbEntry.to := next

        lowLatencyPredictor.btbWriteValid := True
        lowLatencyPredictor.btbWriteAddr := pcWordAddr(s1.out.pc).resized
        lowLatencyPredictor.btbWriteData := btbEntry
      } otherwise {
        Machine.report(
          Seq(
            "Not rescheduling - pc=",
            s1.out.pc,
            " decision ",
            decision,
            " predicted ",
            s1.pcFetchStage.predictedBranchValid
          )
        )
      }
    }
  }

  // Refill
  val refillArea = new Area {
    val fetch = exc.lookup[FetchPacket]
    val icacheMiss =
      exc.exc.valid && exc.exc.code === MachineExceptionCode.INSN_CACHE_MISS
    icache.io.cpu.fill.valid := icacheMiss
    icache.io.cpu.fill.payload := fetch.pc
    val waitingRefill = Reg(
      Bool()
    ) init (false) clearWhen (!icache.io.cpu.prefetch.haltIt) setWhen (icache.io.cpu.fill.valid)
    val refillAddr = RegNextWhen(fetch.pc, icache.io.cpu.fill.valid)
    when(icache.io.cpu.fill.valid) {
      Machine.report(Seq("Requesting cache refill. PC=", fetch.pc))
    }
    when(waitingRefill) {
      Machine.report(Seq("Waiting for cache refill. PC=", refillAddr))
      assert(
        pcStreamGen.valid === False,
        "unexpected valid status for pc stream gen during refill"
      )
      assert(!exc.exc.valid, "unexpected exception during refill")
      assert(pcStreamGen.pc.pcTag === False, "invalid pcTag during refill")
      when(!icache.io.cpu.prefetch.haltIt) {
        Machine.report(Seq("Re-activating PC stream at ", refillAddr))
        pcStreamGen.valid := True
        pcStreamGen.pc.pc := refillAddr
      }
    }
  }

  // Restart
  val restartLogic = new Area {
    val x = Machine.tryGet[FetchRestartSignal]
    if (x.isDefined) {
      when(x.get.valid) {
        assert(
          pcStreamGen.valid === False,
          "unexpected valid status for pc stream gen during restart"
        )
        assert(!exc.exc.valid, "unexpected exception during restart")
        assert(pcStreamGen.pc.pcTag === False, "invalid pcTag during restart")
        pcStreamGen.valid := True
        pcStreamGen.pc.pc := x.get.pc
      }
    }
  }

  // Highest priority
  val pcExceptionHandler = new Area {
    when(exc.exc.valid) {
      val theirFetchPacket = exc.lookup[FetchPacket]
      val nextPCforTheirFetchPacket =
        ((theirFetchPacket.pc & fspec.addrMask) + fspec.addrStep)
      pcStreamGen.pc.pcTag := False
      when(exc.exc.code === MachineExceptionCode.BRANCH_MISS) {
        Machine.report(
          Seq(
            "Fixing branch miss - src=",
            theirFetchPacket.pc,
            " dst=",
            exc.exc.brDstAddr,
            " taken=",
            exc.exc.brTaken,
            " history.prev=",
            theirFetchPacket.globalHistory,
            " const=",
            exc.exc.brIsConst,
            " gs=",
            s2.gshareMemWriteAddr
          )
        )
        pcStreamGen.pc.pc := exc.exc.brTaken.mux(
          False -> nextPCforTheirFetchPacket,
          True -> exc.exc.brDstAddr.asUInt
        )
        s2.gshareMemWriteAddr := (theirFetchPacket.globalHistory.resize(
          s2.gshareMemWriteAddr.getWidth
        ) ^ pcWordAddr(theirFetchPacket.pc).asBits.resized).asUInt
        s2.gshareMemWriteData.valid := True
        s2.gshareMemWriteData.taken := exc.exc.brTaken

        when(exc.exc.brIsConst) {
          speculatedGlobalHistory := theirFetchPacket.globalHistory(
            speculatedGlobalHistory.getWidth - 2 downto 0
          ) ## exc.exc.brTaken.asBits
          s2.gshareMemWriteValid := True
        } otherwise {
          val btbEntry = BtbEntry()
          btbEntry.valid := True
          btbEntry.from := theirFetchPacket.pc
          btbEntry.to := exc.exc.brDstAddr.asUInt
          lowLatencyPredictor.btbWriteValid := True
          lowLatencyPredictor.btbWriteAddr := pcWordAddr(
            theirFetchPacket.pc
          ).resized
          lowLatencyPredictor.btbWriteData := btbEntry
        }
      } elsewhen (exc.exc.code === MachineExceptionCode.SERIALIZE) {
        Machine.report(
          Seq("Got serialization exception. Restarting at next pc.")
        )
        pcStreamGen.pc.pc := nextPCforTheirFetchPacket
      } elsewhen (exc.exc.code === MachineExceptionCode.INSN_CACHE_FLUSH) {
        Machine.report(Seq("Requested ICache flush."))
        pcStreamGen.pc.pc := nextPCforTheirFetchPacket
        icache.io.flush := True
      } otherwise {
        Machine.report(Seq("Got exception - IFetch lockup."))
        pcStreamGen.valid := False
      }
    }
  }
}