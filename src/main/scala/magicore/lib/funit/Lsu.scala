package magicore.lib.funit

import spinal.core._
import spinal.lib._
import magicore.util.PolymorphicDataChain
import magicore.control._
import magicore.util.MultiLaneFifo
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._
import magicore.util.MagiCoreExt._

case class LsuConfig(
    storeBufferSize: Int = 16
)

object LsuOperationSize extends SpinalEnum(binarySequential) {
  val BYTE, HALF, WORD = newElement()
}

case class LsuOperation() extends Bundle with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]
  def parentObjects = Seq()

  val isFence = Bool()
  val isStore = Bool()
  val offset = SInt(32 bits)
  val size = LsuOperationSize()
  val signExt = Bool()
}

trait LsuInstance extends FunctionUnitInstance {
  def io_axiMaster: Axi4
}

class Lsu(staticTagData: => Data, c: LsuConfig) extends FunctionUnit {
  def staticTag: Data = staticTagData
  private val spec = Machine.get[MachineSpec]

  val byteOffsetWidth = log2Up(spec.dataWidth.value / 8) bits
  val storeBufferKeyWidth = 10 bits
  val storeBufferKeyType = HardType(Bits(storeBufferKeyWidth))
  val strbWidth = (spec.dataWidth.value / 8) bits
  val strbType = HardType(Bits(strbWidth))

  def getStoreBufferKeyForAddr(addr: Bits): Bits = {
    addr(
      byteOffsetWidth.value + storeBufferKeyWidth.value - 1 downto byteOffsetWidth.value
    )
  }

  private var effInst: EffectInstance = null

  override def inOrder: Boolean = true

  val axiConfig = Axi4Config(
    addressWidth = spec.dataWidth.value,
    dataWidth = spec.dataWidth.value,
    idWidth = spec.robEntryIndexWidth.value + spec.epochWidth.value,
    useId = true,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useCache = false,
    useSize = false,
    useQos = false,
    useLen = false,
    useLast = true,
    useResp = true,
    useProt = false,
    useStrb = true
  )

  assert(c.storeBufferSize <= spec.robSize)

  case class PendingStore() extends Bundle {
    val addr = spec.dataType
    val data = spec.dataType
    val strb = strbType()
  }

  case class OooLoadContext() extends Bundle {
    val addr = spec.dataType
    val strb = strbType()
    val size = LsuOperationSize()
    val signExt = Bool()

    def shift = addr.resize(byteOffsetWidth).asUInt
  }

  case class StoreBufferEntry() extends Bundle {
    val valid = Bool()
    val key = storeBufferKeyType()
    val srcRobIndex = spec.robEntryIndexType()
  }

  object StoreBufferEntry {
    def idle: StoreBufferEntry = {
      val ret = StoreBufferEntry()
      ret.valid := False
      ret.key.assignDontCare()
      ret.srcRobIndex.assignDontCare()
      ret
    }
  }

  case class LsuReq() extends Bundle {
    val addr = spec.dataType
    val data = spec.dataType
    val strb = Bits((spec.dataWidth.value / 8) bits)
    val isFence = Bool()
    val isStore = Bool()
    val token = CommitToken()
    val size = LsuOperationSize()
    val signExt = Bool()

    def setStrbFromSize(size: SpinalEnumCraft[LsuOperationSize.type]): Unit = {
      val baseStrb = size.mux(
        LsuOperationSize.BYTE -> B(0x1, strb.getWidth bits),
        LsuOperationSize.HALF -> B(0x3, strb.getWidth bits),
        LsuOperationSize.WORD -> B(0xf, strb.getWidth bits)
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
    new LsuInstance {

      val io_available = null
      val io_input = Stream(hardType())
      val io_output = Stream(CommitRequest(null))

      val io_axiMaster = Axi4(axiConfig)

      val epochMgr = Machine.get[EpochManager]

      val outStream_pipeline = Stream(CommitRequest(null))
      val outStream_loadPipe = Stream(CommitRequest(null))
      val outStream_oooRead = Stream(CommitRequest(null))
      val arbitratedStream = StreamArbiterFactory.lowerFirst.on(
        Seq(
          outStream_oooRead.stage(),
          outStream_loadPipe.stage(),
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
          !entry.valid || pendingStoreValid(entry.srcRobIndex),
          Seq(
            "store buffer invariant violation - rob index ",
            entry.srcRobIndex,
            " psv.scheduled=",
            resetArea.pendingStoreValid_scheduled(entry.srcRobIndex),
            " psv.posted=",
            pendingStoreValid_posted(entry.srcRobIndex),
            " storeBufferIndex=" + i
          )
        )
      }

      val storeBufferAllocLogic = new Area {
        val indexType = HardType(UInt(log2Up(storeBuffer.size) bits))
        val addr = spec.dataType
        val allocOk = Bool()
        val allocIndex = indexType()
        allocIndex.assignDontCare()
        val firstReplace = storeBuffer.zipWithIndex.firstWhere(
          hardType = indexType(),
          predicate =
            x => x._1.valid && x._1.key === getStoreBufferKeyForAddr(addr),
          generate = x => U(x._2, allocIndex.getWidth bits)
        )
        val firstEmpty = storeBuffer.zipWithIndex.firstWhere(
          hardType = indexType(),
          predicate = x => x._1.valid === False,
          generate = x => U(x._2, allocIndex.getWidth bits)
        )

        when(firstReplace._1) {
          allocIndex := firstReplace._2
        } elsewhen (firstEmpty._1) {
          allocIndex := firstEmpty._2
        }
        allocOk := firstReplace._1 || firstEmpty._1
      }

      val pendingStoresUtil = new Area {
        val srcRobIndexAtFirstReplace =
          storeBuffer(storeBufferAllocLogic.firstReplace._2).srcRobIndex
        val storeAtFirstReplace = pendingStores(srcRobIndexAtFirstReplace)
      }

      // This logic block may invalidate a `storeBuffer` entry. So, this logic needs to be
      // before the `pipelineLogic` block which may write to (update) the same entry.
      val writeAckLogic = new Area {
        axiM.b.ready := True
        val valid = RegNext(axiM.b.valid, False)
        when(axiM.b.valid) {
          val robIndex =
            axiM.b.payload.id.resize(spec.robEntryIndexWidth)
          assert(pendingStoreValid_posted(robIndex), "invalid store ack")
          pendingStoreValid_posted(robIndex) := False
          val ok = False
          for (entry <- storeBuffer) {
            when(entry.srcRobIndex === robIndex) {
              entry.valid := False
              ok := True
            }
          }
          when(ok) {
            Machine.report(
              Seq(
                "store effect ack: robIndex=",
                robIndex
              )
            )
          }
        }
      }

      val firstStageLogic = new Area {
        val in = io_input.payload
        val op = in.lookup[LsuOperation]
        val dispatch = in.lookup[DispatchInfo]
        val issue = in.lookup[IssuePort[_]]

        val req = LsuReq()
        req.addr := (issue.srcRegData(0).asSInt + op.offset.resized).asBits
        req.data := (issue.srcRegData(1) << (req
          .addr(byteOffsetWidth.value - 1 downto 0)
          .asUInt << 3)).resized
        req.isFence := op.isFence
        req.isStore := op.isStore
        req.size := op.size
        req.signExt := op.signExt
        req.setStrbFromSize(op.size)
        req.token := in.lookup[CommitToken]
        val out = io_input.translateWith(req).pipelined(m2s = true, s2m = true)
      }

      case class LoadReq() extends Bundle {
        val storeBypassValid = Bool()
        val store = PendingStore()
        val addr = spec.dataType
        val strb = Bits((spec.dataWidth.value / 8) bits)
        val token = CommitToken()
        val size = LsuOperationSize()
        val signExt = Bool()
      }

      val loadPipeline = new Area {
        val input = Stream(LoadReq())
        input.setIdle()

        val inputDemux = StreamDemux(input, input.storeBypassValid.asUInt, 2)
        val oooReq = inputDemux(0)
        val bypassReq = inputDemux(1).stage()

        val bypassPath = new Area {
          val store = bypassReq.store
          val commitReq = CommitRequest(null)
          commitReq.exception := MachineException.idle

          val storeData = convertLoadOutputToRegValue(
            store.data,
            bypassReq.payload.strb,
            bypassReq.payload
              .addr(byteOffsetWidth.value - 1 downto 0)
              .asUInt,
            bypassReq.payload.size,
            bypassReq.payload.signExt
          )
          commitReq.regWriteValue(0) := storeData
          commitReq.token := bypassReq.payload.token
          outStream_loadPipe << bypassReq.translateWith(commitReq)
        }

        val oooPath = new Area {
          val ar = Axi4Ar(axiConfig)
          ar.id := (oooReq.payload.token.epoch ## oooReq.payload.token.robIndex).asUInt
          ar.addr := oooReq.payload.addr.asUInt
          axiM.ar << oooReq.translateWith(ar)

          val ctx = OooLoadContext()
          ctx.strb := oooReq.payload.strb
          ctx.addr := oooReq.payload.addr
          ctx.size := oooReq.payload.size
          ctx.signExt := oooReq.payload.signExt

          oooLoadContexts.write(
            address = oooReq.payload.token.robIndex,
            data = ctx,
            enable = oooReq.fire
          )

          when(oooReq.fire) {
            Machine.report(
              Seq(
                "issueing ooo load at addr ",
                oooReq.payload.addr.asUInt,
                " robIndex=",
                oooReq.payload.token.robIndex,
                " epoch=",
                oooReq.payload.token.epoch
              )
            )
          }
        }
      }

      val pipelineLogic = new Area {
        val req = firstStageLogic.out

        val pendingStore = PendingStore()
        pendingStore.addr := req.payload.addr
        pendingStore.data := req.payload.data
        pendingStore.strb := req.payload.strb

        val shouldWritePendingStore = False
        pendingStores.write(
          address = req.payload.token.robIndex,
          data = pendingStore,
          enable = shouldWritePendingStore
        )

        storeBufferAllocLogic.addr := req.payload.addr

        when(req.valid) {
          when(req.payload.isFence) {
            // FENCE path
            val commitReq = CommitRequest(null)
            commitReq.exception := MachineException.idle
            commitReq.regWriteValue.assignDontCare()
            commitReq.token := req.payload.token

            val ok = !pendingStoreValid.orR
            req.ready := ok && outStream_pipeline.ready
            outStream_pipeline.valid := ok
            outStream_pipeline.payload := commitReq
          } elsewhen (req.payload.isStore) {
            // STORE path
            // Wait for the previous store to complete
            val previousStoreCompleted = !pendingStoreValid(
              req.payload.token.robIndex
            )

            val commitReq = CommitRequest(null)
            commitReq.exception := MachineException.idle
            commitReq.regWriteValue.assignDontCare()
            commitReq.token := req.payload.token

            val ok =
              previousStoreCompleted && storeBufferAllocLogic.allocOk
            outStream_pipeline.valid := ok
            outStream_pipeline.payload := commitReq
            req.ready := ok && outStream_pipeline.ready

            when(req.isStall) {
              Machine.report(
                Seq(
                  "store STALL - addr=",
                  req.payload.addr.asUInt,
                  " data=",
                  req.payload.data,
                  " robIndex=",
                  req.payload.token.robIndex,
                  " epoch=",
                  req.payload.token.epoch,
                  " previousStoreCompleted=",
                  previousStoreCompleted,
                  " allocOk=",
                  storeBufferAllocLogic.allocOk
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

              assert(
                !storeBuffer(
                  storeBufferAllocLogic.allocIndex
                ).valid || storeBuffer(
                  storeBufferAllocLogic.allocIndex
                ).key === getStoreBufferKeyForAddr(req.payload.addr),
                "invalid allocated store buffer index"
              )
              storeBuffer(
                storeBufferAllocLogic.allocIndex
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
                  " data=",
                  req.payload.data,
                  " robIndex=",
                  req.payload.token.robIndex,
                  " epoch=",
                  req.payload.token.epoch
                )
              )
            }
          } otherwise {
            // LOAD path
            val foundBypass = storeBufferAllocLogic.firstReplace._1
            assert(
              !foundBypass || storeBuffer(
                storeBufferAllocLogic.firstReplace._2
              ).key === getStoreBufferKeyForAddr(req.payload.addr),
              "invalid store buffer index for load operation"
            )
            assert(
              !foundBypass || pendingStoreValid(
                pendingStoresUtil.srcRobIndexAtFirstReplace
              ),
              "load operation got invalid src rob index"
            )
            val store = pendingStoresUtil.storeAtFirstReplace
            val bypassDataReady =
              store.strb === req.payload.strb && store.addr === req.payload.addr

            val loadReq = LoadReq()
            loadReq.assignSomeByName(req.payload)
            loadReq.storeBypassValid := foundBypass
            loadReq.store := store

            when(req.ready) {
              Machine.report(
                Seq(
                  "Firing load request at addr ",
                  req.payload.addr.asUInt,
                  " foundBypass=",
                  foundBypass,
                  " robIndex=",
                  req.payload.token.robIndex,
                  " epoch=",
                  req.payload.token.epoch
                )
              )
            }
            req
              .translateWith(loadReq)
              .continueWhen(
                !foundBypass || bypassDataReady
              ) >> loadPipeline.input
            outStream_pipeline.setIdle()
          }
        } otherwise {
          outStream_pipeline.setIdle()
          req.setBlocked()
        }
      }

      // Effect apply
      val effectLogic = new Area {
        assert(!effFifo.io.push.isStall, "effect fifo must not stall")

        effFifo.io.push.valid := effInst.io_effect
          .map(x => x.valid)
          .orR
        effFifo.io.push.payload := effInst.io_effect
        when(effFifo.io.push.fire) {
          for (eff <- effInst.io_effect) {
            when(eff.valid) {
              val scheduled = resetArea.pendingStoreValid_scheduled(
                eff.payload.robIndex
              )
              scheduled := False
              pendingStoreValid_posted(eff.payload.robIndex) := scheduled
            }
          }
        }

        val robIndex = effFifo.io.pop.payload.robIndex
        val isStore = pendingStoreValid_posted(robIndex)
        val store = pendingStores(robIndex)
        val popStream = effFifo.io.pop.throwWhen(!isStore)
        val (popToAw, popToW) = StreamFork2(popStream)

        val aw = Axi4Aw(axiConfig)
        aw.id := robIndex.resized
        aw.addr := store.addr.asUInt
        axiM.aw << popToAw.translateWith(aw)

        val w = Axi4W(axiConfig)
        w.data := store.data
        w.strb := store.strb
        w.last := True
        axiM.w << popToW.translateWith(w)

        when(popStream.fire) {
          Machine.report(
            Seq(
              "store effect fire: robIndex=",
              robIndex,
              " addr=",
              store.addr,
              ", data=",
              store.data
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
              commitReq.token.epoch
            )
          )
        }
      }

      // Clear un-posted pending stores in the store buffer.
      when(effInst.io_reset) {
        for (entry <- storeBuffer) {
          when(
            !pendingStoreValid_posted(
              entry.srcRobIndex
            )
          ) {
            // Also invalidate the entry from the same cycle.
            entry.valid := False
            when(entry.valid) {
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
  }

  override def generateEffect(): Option[EffectInstance] = {
    val spec = Machine.get[MachineSpec]
    effInst = new EffectInstance {}
    Some(effInst)
  }
}
