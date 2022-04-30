package magicore.lib.funit

import spinal.core._
import spinal.lib._
import magicore.util.PolymorphicDataChain
import magicore.control._
import magicore.util.MultiLaneFifo
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._
import magicore.util.MagiCoreExt._
import spinal.lib.bus.misc.SizeMapping

case class LsuConfig(
    storeBufferSize: Int = 16,
    ioMemoryRegions: Seq[SizeMapping] = Seq()
)

object LsuOperationSize extends SpinalEnum(binarySequential) {
  val BYTE, HALF, WORD, DOUBLE = newElement()

  def toAxSize(me: SpinalEnumCraft[LsuOperationSize.type]): UInt = {
    me.mux(
      BYTE -> B"000",
      HALF -> B"001",
      WORD -> B"010",
      DOUBLE -> B"011"
    ).asUInt
  }
}

case class LsuOperation() extends Bundle with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]
  def parentObjects = Seq()

  val isFence = Bool()
  val isStore = Bool()
  val offset = SInt(spec.dataWidth)
  val size = LsuOperationSize()
  val signExt = Bool()
}

trait LsuInstance extends FunctionUnitInstance {
  def io_axiMaster: Axi4
}

case class LsuBusySignal(busy: Bool)

class Lsu(staticTagData: => Data, c: LsuConfig) extends FunctionUnit {
  def staticTag: Data = staticTagData
  private val spec = Machine.get[MachineSpec]

  val byteOffsetWidth = log2Up(spec.dataWidth.value / 8) bits
  val storeBufferKeyWidth = (spec.addrWidth.value - byteOffsetWidth.value) bits
  val storeBufferKeyType = HardType(Bits(storeBufferKeyWidth))
  val strbWidth = (spec.dataWidth.value / 8) bits
  val strbType = HardType(Bits(strbWidth))

  def getStoreBufferKeyForAddr(addr: Bits): Bits = {
    addr(spec.addrWidth.value - 1 downto byteOffsetWidth.value)
  }

  private var effInst: EffectInstance = null

  override def inOrder: Boolean = true

  val axiConfig = Axi4Config(
    addressWidth = spec.addrWidth.value,
    dataWidth = spec.dataWidth.value,
    idWidth = spec.robEntryIndexWidth.value + spec.epochWidth.value,
    useId = true,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useCache = true,
    useSize = true,
    useQos = false,
    useLen = false,
    useLast = true,
    useResp = true,
    useProt = true,
    useStrb = true
  )

  assert(c.storeBufferSize <= spec.robSize)

  case class PendingStore() extends Bundle {
    val addr = Bits(spec.addrWidth)
    val strb = strbType()
    val size = LsuOperationSize()
  }

  case class OooLoadContext() extends Bundle {
    val addr = Bits(spec.addrWidth)
    val strb = strbType()
    val size = LsuOperationSize()
    val signExt = Bool()

    def shift = addr.resize(byteOffsetWidth).asUInt
  }

  case class StoreBufferEntry() extends Bundle {
    val valid = Bool()
    val posted = Bool()
    val key = storeBufferKeyType()
    val srcRobIndex = spec.robEntryIndexType()
  }

  object StoreBufferEntry {
    def idle: StoreBufferEntry = {
      val ret = StoreBufferEntry()
      ret.valid := False
      ret.posted := False
      ret.key.assignDontCare()
      ret.srcRobIndex.assignDontCare()
      ret
    }
  }

  case class LsuReq() extends Bundle {
    assert(spec.dataWidth.value == 32 || spec.dataWidth.value == 64)

    val addr = Bits(spec.addrWidth)
    val dataPhysRegIndex = spec.physRegIndexType
    val strb = Bits((spec.dataWidth.value / 8) bits)
    val isFence = Bool()
    val isStore = Bool()
    val token = CommitToken()
    val size = LsuOperationSize()
    val signExt = Bool()

    def setStrbFromSize(size: SpinalEnumCraft[LsuOperationSize.type]): Unit = {
      val baseStrb =
        if (spec.dataWidth.value == 32)
          size.mux(
            LsuOperationSize.BYTE -> B(0x1, strb.getWidth bits),
            LsuOperationSize.HALF -> B(0x3, strb.getWidth bits),
            default -> B(0xf, strb.getWidth bits)
          )
        else
          size.mux(
            LsuOperationSize.BYTE -> B(0x1, strb.getWidth bits),
            LsuOperationSize.HALF -> B(0x3, strb.getWidth bits),
            LsuOperationSize.WORD -> B(0xf, strb.getWidth bits),
            default -> B(0xff, strb.getWidth bits)
          )
      val shift = addr(byteOffsetWidth.value - 1 downto 0).asUInt
      strb := (baseStrb << shift).resized
    }
  }

  def strbToWordMask(strb: Bits): Bits = {
    val bits = Vec(
      strb.as(Vec(Bool(), strb.getWidth)).flatMap(x => (0 until 8).map(_ => x))
    )
    bits.asBits
  }

  def convertLoadOutputToRegValue(
      data: Bits,
      strb: Bits,
      byteShift: UInt,
      size: SpinalEnumCraft[LsuOperationSize.type],
      signExt: Bool
  ): Bits = {
    val x = (data & strbToWordMask(strb)) >> (byteShift << 3)
    val out = Bits(x.getWidth bits)
    when(signExt) {
      switch(size) {
        is(LsuOperationSize.BYTE) {
          out := x(7 downto 0).asSInt.resize(x.getWidth bits).asBits
        }
        is(LsuOperationSize.HALF) {
          out := x(15 downto 0).asSInt.resize(x.getWidth bits).asBits
        }
        is(LsuOperationSize.WORD) {
          out := x(31 downto 0).asSInt.resize(x.getWidth bits).asBits
        }
        default {
          out := x
        }
      }
    } otherwise {
      out := x
    }
    out
  }

  override def generate(
      hardType: HardType[_ <: PolymorphicDataChain]
  ): FunctionUnitInstance = {
    val prfIf = Machine.get[PrfInterface]
    new LsuInstance {

      val io_available = null
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))

      val io_axiMaster = Axi4(axiConfig)

      val epochMgr = Machine.get[EpochManager]

      val outStream_pipeline = Stream(CommitRequest(null))
      val outStream_storeWait = Stream(CommitRequest(null))
      val outStream_oooRead = Stream(CommitRequest(null))
      val arbitratedStream = StreamArbiterFactory.lowerFirst.on(
        Seq(
          outStream_oooRead.stage(),
          outStream_storeWait.stage(),
          outStream_pipeline.stage()
        )
      )
      arbitratedStream >> io_output

      val axiM = Axi4(axiConfig)
      axiM.ar >> io_axiMaster.ar
      axiM.aw >> io_axiMaster.aw
      axiM.w >> io_axiMaster.w
      axiM.r << io_axiMaster.r.s2mPipe()
      axiM.b << io_axiMaster.b

      val pendingStores = Mem(PendingStore(), spec.robSize)
      val pendingStoreData = Mem(spec.dataType, spec.robSize)

      val oooLoadContexts = Mem(OooLoadContext(), spec.robSize)

      // Effects are "posted" after commit - don't reset it.
      val effFifo = MultiLaneFifo(
        dataType = CommitEffect(),
        depth = spec.robSize,
        numLanes = spec.writebackWidth
      )

      val pendingStoreValid_posted = Vec(Reg(Bool()) init (false), spec.robSize)

      val resetArea =
        new ResetArea(reset = effInst.io_reset, cumulative = true) {
          val pendingStoreValid_scheduled =
            Vec(Reg(Bool()) init (false), spec.robSize)
        }
      val storeBuffer = Vec(
        Reg(StoreBufferEntry()) init (StoreBufferEntry.idle),
        c.storeBufferSize
      )

      val pendingStoreValid = Vec(
        pendingStoreValid_posted
          .zip(resetArea.pendingStoreValid_scheduled)
          .map(
            { case (a, b) => a || b }
          )
      )

      val busy = LsuBusySignal(pendingStoreValid.orR)
      Machine.provide(busy)

      // Validate pendingStoreValid invariants
      for (
        ((posted, scheduled), i) <- pendingStoreValid_posted
          .zip(resetArea.pendingStoreValid_scheduled)
          .zipWithIndex
      ) {
        assert(
          !(posted && scheduled),
          "conflicting POSTED and SCHEDULED pending stores at rob index " + i
        )
      }

      // Validate storeBuffer invariants
      for ((entry, i) <- storeBuffer.zipWithIndex) {
        assert(
          !entry.valid || (pendingStoreValid(
            entry.srcRobIndex
          ) && pendingStoreValid_posted(entry.srcRobIndex) === entry.posted),
          Seq(
            "store buffer invariant violation - rob index ",
            entry.srcRobIndex,
            " psv.scheduled=",
            resetArea.pendingStoreValid_scheduled(entry.srcRobIndex),
            " psv.posted=",
            pendingStoreValid_posted(entry.srcRobIndex),
            " entry.posted=",
            entry.posted,
            " storeBufferIndex=" + i
          )
        )
      }
      for ((psv, i) <- pendingStoreValid.zipWithIndex) {
        assert(
          !psv || storeBuffer.countForVerification(x =>
            x.valid && x.srcRobIndex === i
          ) === 1,
          "psv entry maps to zero or multiple entries in store buffer"
        )
      }

      val storeBufferAllocLogic = new Area {
        val indexType = HardType(UInt(log2Up(storeBuffer.size) bits))
        val addr = Bits(spec.addrWidth)
        val firstMatch = storeBuffer.zipWithIndex.firstWhere(
          hardType = indexType(),
          predicate =
            x => x._1.valid && x._1.key === getStoreBufferKeyForAddr(addr),
          generate = x => U(x._2, indexType().getWidth bits)
        )
        val firstEmpty = storeBuffer.zipWithIndex.firstWhere(
          hardType = indexType(),
          predicate = x => x._1.valid === False,
          generate = x => U(x._2, indexType().getWidth bits)
        )
      }

      val pendingStoresUtil = new Area {
        val srcRobIndexAtFirstMatch =
          storeBuffer(storeBufferAllocLogic.firstMatch._2).srcRobIndex
      }

      val writeAckLogic = new Area {
        val writePipe =
          new StreamFifoLowLatency(
            spec.robEntryIndexType(),
            spec.robSize,
            latency = 1
          )
        writePipe.io.pop.setBlocked()

        axiM.b.ready := True
        when(axiM.b.valid) {
          assert(writePipe.io.pop.valid, "AXI write ack with an empty pipe")
          writePipe.io.pop.ready := True

          val robIndex = writePipe.io.pop.payload
          assert(pendingStoreValid_posted(robIndex), "invalid store ack")
          pendingStoreValid_posted(robIndex) := False
          var ok = False
          for (entry <- storeBuffer) {
            val thisOk = False
            when(entry.valid && entry.srcRobIndex === robIndex) {
              entry.valid := False
              assert(!ok, "multiple robIndex matches")
              thisOk := True
            }
            ok = ok || thisOk
          }
          assert(ok, "did not find matching entry in store buffer")

          Machine.report(
            Seq(
              "store effect ack: robIndex=",
              robIndex
            )
          )
        }
      }

      val firstStageLogic = new Area {
        val in = io_input.payload
        val op = in.lookup[LsuOperation]
        val rename = in.lookup[RenameInfo]
        val dispatch = in.lookup[DispatchInfo]
        val issue = in.lookup[IssuePort[_]]

        val req = LsuReq()
        req.addr := (issue
          .srcRegData(0)
          .resize(spec.addrWidth)
          .asSInt + op.offset.resized).asBits
        req.dataPhysRegIndex := rename.physSrcRegs(1)
        req.isFence := op.isFence
        req.isStore := op.isStore
        req.size := op.size
        req.signExt := op.signExt
        req.setStrbFromSize(op.size)
        req.token := in.lookup[CommitToken]

        println("I/O memory regions: " + c.ioMemoryRegions)

        val robHead = Machine.get[RobHeadInfo]
        val isIoRegionLoad =
          !req.isFence && !req.isStore &&
            c.ioMemoryRegions.map(region => region.hit(req.addr.asUInt)).orR

        // Exception condition already checked by InO-IQ
        val out = io_input
          .translateWith(req)
          .continueWhen(
            !isIoRegionLoad || req.token.robIndex === robHead.headPtr
          )
          .pipelined(m2s = true, s2m = true)
      }

      case class StoreDataWaitReq() extends Bundle {
        val token = CommitToken()
        val dataPhysReg = spec.physRegIndexType
        val shift = UInt(byteOffsetWidth)
      }

      val storeDataWaitLogic = new Area {
        val input = Stream(StoreDataWaitReq())
        input.setIdle()

        val req = input.queueLowLatency(8, latency = 1)
        val dataAvailable =
          prfIf.state.table(req.payload.dataPhysReg).dataAvailable

        val commitReq = CommitRequest(null)
        commitReq.exception := MachineException.idle
        commitReq.regWriteValue.assignDontCare()
        commitReq.token := req.payload.token

        val epochMismatch = req.payload.token.epoch =/= epochMgr.currentEpoch
        outStream_storeWait << req
          .continueWhen(
            dataAvailable || epochMismatch
          )
          .translateWith(commitReq)

        val data = prfIf.readAsync(req.dataPhysReg).data
        pendingStoreData.write(
          address = req.payload.token.robIndex,
          data = (data << (req.shift << 3)).resize(data.getWidth),
          enable = req.fire
        )
      }

      val pipelineLogic = new Area {
        val req = firstStageLogic.out

        val pendingStore = PendingStore()
        pendingStore.addr := req.payload.addr
        pendingStore.strb := req.payload.strb
        pendingStore.size := req.payload.size

        val shouldWritePendingStore = False
        pendingStores.write(
          address = req.payload.token.robIndex,
          data = pendingStore,
          enable = shouldWritePendingStore
        )

        storeBufferAllocLogic.addr := req.payload.addr

        axiM.ar.setIdle()

        when(req.valid) {
          when(req.payload.isFence) {
            // FENCE path
            val commitReq = CommitRequest(null)
            commitReq.exception := MachineException.idle
            commitReq.regWriteValue.assignDontCare()
            commitReq.token := req.payload.token

            val ok = !busy.busy
            req.ready := ok && outStream_pipeline.ready
            outStream_pipeline.valid := ok
            outStream_pipeline.payload := commitReq
          } elsewhen (req.payload.isStore) {
            // STORE path
            // Wait for the previous store to complete
            val previousStoreCompleted = !pendingStoreValid(
              req.payload.token.robIndex
            )

            val dataReq = StoreDataWaitReq()
            dataReq.dataPhysReg := req.payload.dataPhysRegIndex
            dataReq.shift := req.payload.addr.resize(byteOffsetWidth).asUInt
            dataReq.token := req.payload.token

            val ok =
              previousStoreCompleted && storeBufferAllocLogic.firstEmpty._1
            outStream_pipeline.setIdle()

            storeDataWaitLogic.input.valid := ok
            storeDataWaitLogic.input.payload := dataReq
            req.ready := ok && storeDataWaitLogic.input.ready

            when(req.isStall) {
              Machine.report(
                Seq(
                  "store STALL - addr=",
                  req.payload.addr.asUInt,
                  " robIndex=",
                  req.payload.token.robIndex,
                  " epoch=",
                  req.payload.token.epoch,
                  " previousStoreCompleted=",
                  previousStoreCompleted
                )
              )
            }

            // Throw away outdated requests
            when(
              req.ready && req.payload.token.epoch === epochMgr.currentEpoch
            ) {
              // Write to store buffer
              val bufEntry = StoreBufferEntry()
              bufEntry.valid := True
              bufEntry.srcRobIndex := req.payload.token.robIndex
              bufEntry.key := getStoreBufferKeyForAddr(req.payload.addr)
              bufEntry.posted := False

              assert(
                !storeBuffer(
                  storeBufferAllocLogic.firstEmpty._2
                ).valid,
                "invalid allocated store buffer index"
              )
              storeBuffer(
                storeBufferAllocLogic.firstEmpty._2
              ) := bufEntry

              // Write pending store - defer the actual write to effect handling
              resetArea.pendingStoreValid_scheduled(
                req.payload.token.robIndex
              ) := True
              shouldWritePendingStore := True
              Machine.report(
                Seq(
                  "store scheduled - addr=",
                  req.payload.addr.asUInt,
                  " robIndex=",
                  req.payload.token.robIndex,
                  " epoch=",
                  req.payload.token.epoch
                )
              )
            }
          } otherwise {
            // LOAD path
            val found = storeBufferAllocLogic.firstMatch._1
            assert(
              !found || storeBuffer(
                storeBufferAllocLogic.firstMatch._2
              ).key === getStoreBufferKeyForAddr(req.payload.addr),
              "invalid store buffer index for load operation"
            )
            assert(
              !found || pendingStoreValid(
                pendingStoresUtil.srcRobIndexAtFirstMatch
              ),
              "load operation got invalid src rob index"
            )

            outStream_pipeline.setIdle()

            when(found) {
              req.ready := False
            } otherwise {
              // OoO memory read issue
              val ar = Axi4Ar(axiConfig)
              ar.id := (req.payload.token.epoch ## req.payload.token.robIndex).asUInt
              ar.addr := req.payload.addr.asUInt
              ar.cache := B"1111"
              ar.prot := B"000"
              ar.size := LsuOperationSize.toAxSize(req.payload.size)
              axiM.ar.valid := True
              axiM.ar.payload := ar
              req.ready := axiM.ar.ready

              val ctx = OooLoadContext()
              ctx.strb := req.payload.strb
              ctx.addr := req.payload.addr
              ctx.size := req.payload.size
              ctx.signExt := req.payload.signExt

              oooLoadContexts.write(
                address = req.payload.token.robIndex,
                data = ctx
              )

              when(req.ready) {
                Machine.report(
                  Seq(
                    "issueing ooo load at addr ",
                    req.payload.addr.asUInt,
                    " found=",
                    found,
                    " robIndex=",
                    req.payload.token.robIndex,
                    " epoch=",
                    req.payload.token.epoch,
                    " strb=",
                    req.payload.strb,
                    " size=",
                    req.payload.size
                  )
                )
              }
            }
          }
        } otherwise {
          outStream_pipeline.setIdle()
          req.setBlocked()
        }
      }

      // Effect apply
      val effectLogic = new Area {
        assert(!effFifo.io.push.isStall, "effect fifo must not stall")

        // XXX: Previously we incorrectly checked the `pendingStoreValid_scheduled` flag on the POP side instead of PUSH.
        // This causes non-store LSU operations to also be pushed into the queue and fill it up above the allocated
        // capacity (managed by `pendingStoreValid`), causing real store operations to be dropped.
        //
        // Unable to reproduce in simulation so far - TODO.
        val effStoreScheduled = effInst.io_effect.map(x =>
          resetArea.pendingStoreValid_scheduled(
            x.payload.robIndex
          )
        )

        effFifo.io.push.valid := False
        for ((eff, i) <- effInst.io_effect.zipWithIndex) {
          val storeScheduled = effStoreScheduled(i)
          effFifo.io.push.payload(i).payload := eff.payload
          when(eff.valid && storeScheduled) {
            effFifo.io.push.valid := True
            effFifo.io.push.payload(i).valid := True
            storeScheduled := False
            pendingStoreValid_posted(eff.payload.robIndex) := True
            for (entry <- storeBuffer) {
              when(
                entry.valid && entry.srcRobIndex === eff.payload.robIndex
              ) {
                assert(!entry.posted, "posting a store buffer entry twice");
                entry.posted := True
              }
            }
          } otherwise {
            effFifo.io.push.payload(i).valid := False
          }
        }

        val robIndex = effFifo.io.pop.payload.robIndex
        val store = pendingStores(robIndex)
        val popStream = effFifo.io.pop
        assert(
          !popStream.valid || pendingStoreValid_posted(robIndex),
          "popped effect was not posted"
        )
        val (popToAw, popToW) = StreamFork2(popStream)

        val aw = Axi4Aw(axiConfig)
        aw.id := 0
        aw.addr := store.addr.asUInt
        aw.cache := B"1111"
        aw.size := LsuOperationSize.toAxSize(store.size)
        aw.prot := B"000"
        axiM.aw << popToAw.translateWith(aw)

        val w = Axi4W(axiConfig)
        w.data := pendingStoreData(robIndex)
        w.strb := store.strb
        w.last := True
        axiM.w << popToW.translateWith(w)

        writeAckLogic.writePipe.io.push.valid := popStream.fire
        writeAckLogic.writePipe.io.push.payload := robIndex
        assert(
          !writeAckLogic.writePipe.io.push.isStall,
          "writePipe push must not stall"
        )

        when(popStream.fire) {
          Machine.report(
            Seq(
              "store effect fire: robIndex=",
              robIndex,
              " addr=",
              store.addr,
              ", data=",
              w.data,
              ", strb=",
              w.strb
            )
          )
        }
      }

      val readAckLogic = new Area {
        val commitReq = CommitRequest(null)
        commitReq.exception := MachineException.idle
        commitReq.token.epoch := axiM.r.payload.id(
          axiM.r.payload.id.getWidth - 1 downto spec.robEntryIndexWidth.value
        )
        commitReq.token.robIndex := axiM.r.payload.id(
          spec.robEntryIndexWidth.value - 1 downto 0
        )

        val ctx = oooLoadContexts(commitReq.token.robIndex)

        commitReq.exception.memoryError_accessAddr := ctx.addr
        when(axiM.r.payload.resp =/= 0) {
          commitReq.exception.valid := True
          commitReq.exception.code := MachineExceptionCode.MEMORY_ERROR

          when(outStream_oooRead.fire) {
            Machine.report(
              Seq("OoO read error: ", axiM.r.payload.resp, " addr=", ctx.addr)
            )
          }
        }

        val data =
          convertLoadOutputToRegValue(
            axiM.r.payload.data,
            ctx.strb,
            ctx.shift,
            ctx.size,
            ctx.signExt
          )
        commitReq.regWriteValue(0) := data // TODO: Sign extension

        axiM.r.translateWith(commitReq) >> outStream_oooRead
        when(outStream_oooRead.fire) {
          Machine.report(
            Seq(
              "load ooo ok - data: ",
              outStream_oooRead.payload.regWriteValue(0),
              " robIndex=",
              commitReq.token.robIndex,
              " epoch=",
              commitReq.token.epoch,
              " raw=",
              axiM.r.payload.data,
              " strb=",
              ctx.strb,
              " shift=",
              ctx.shift,
              " size=",
              ctx.size,
              " signExt=",
              ctx.signExt
            )
          )
        }
      }

      // Clear un-posted pending stores in the store buffer.
      when(effInst.io_reset) {
        for (entry <- storeBuffer) {
          when(entry.valid && !entry.posted) {
            entry.valid := False

            Machine.report(
              Seq(
                "clearing un-posted pending store - rob index ",
                entry.srcRobIndex
              )
            )
          }
        }
      }
    }
  }

  override def generateEffect(): Option[EffectInstance] = {
    val spec = Machine.get[MachineSpec]
    effInst = new EffectInstance {}
    Some(effInst)
  }
}
