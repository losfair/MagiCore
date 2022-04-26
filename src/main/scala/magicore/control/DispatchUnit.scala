package magicore.control

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import scala.reflect._
import magicore.lib.funit.AluBranchContext
import magicore.frontend.FetchPacket
import scala.collection.mutable.ArrayBuffer

case class DispatchInfo(hardType: HardType[_ <: PolymorphicDataChain])
    extends Bundle
    with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]
  val robIndex = spec.robEntryIndexType()
  val epoch = spec.epochType()
  val parentObjects = if (hardType != null) Seq(hardType()) else Seq()

  override def decodeAs[T <: AnyRef](ctag: ClassTag[T]): Option[T] = {
    if (ctag == classTag[CommitToken]) {
      val t = CommitToken()
      t.robIndex := robIndex
      t.epoch := epoch
      return Some(t.asInstanceOf[T])
    }
    return None
  }
}

case class CommitToken() extends Bundle with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]
  val robIndex = spec.robEntryIndexType()
  val epoch = spec.epochType()
  val parentObjects = Seq()
}

case class CommitRequest(
    hardType: HardType[_ <: PolymorphicDataChain],
    genRegWriteValue: Boolean = true
) extends Bundle
    with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]
  val token = CommitToken()
  val regWriteValue =
    if (genRegWriteValue)
      Vec(
        (0 until spec.maxNumDstRegsPerInsn).map(_ => spec.dataType)
      )
    else null
  val exception = MachineException()
  val ctx: PolymorphicDataChain = if (hardType != null) hardType() else null
  def parentObjects = Seq(ctx, exception)
}

case class RobEntry(hardType: HardType[_ <: PolymorphicDataChain])
    extends Bundle {
  val commitRequest = CommitRequest(
    hardType = hardType,
    genRegWriteValue = false
  ) setCompositeName (this, "cr")
  val completed = Bool()
}

case class FullCommitRequestType(ty: HardType[CommitRequest])

case class RobHeadInfo() extends Area {
  private val spec = Machine.get[MachineSpec]
  val headPtr = spec.robEntryIndexType()
}

case class DispatchPerfCounters(instRetired: UInt)

case class DispatchUnit[T <: PolymorphicDataChain](
    dataType: HardType[T]
) extends Area {

  private val spec = Machine.get[MachineSpec]
  private val sem = Machine.get[MachineSemantics]

  def commitRequestType = CommitRequest(null)
  def fullCommitRequestType = CommitRequest(dataType, genRegWriteValue = false)
  def outType = DispatchInfo(dataType())
  def robEntryType = RobEntry(dataType())

  Machine.provide(FullCommitRequestType(fullCommitRequestType))

  case class ReadOutput() extends Bundle {
    val addr = spec.robEntryIndexType()
    val data = robEntryType
  }

  val io = new Bundle {
    val input = Stream(dataType())
    val oooOutput = Stream(outType)
    val inOrderOutput = Stream(outType)
    val commitOoO =
      Vec(Stream(commitRequestType), sem.functionUnits.count(x => !x.inOrder))
    val commitInO =
      Vec(Stream(commitRequestType), sem.functionUnits.count(x => x.inOrder))
    val writebackMonitor = Vec(
      Flow(CommitRequest(dataType, genRegWriteValue = false)),
      spec.writebackWidth
    )
  }

  val epochMgr = Machine.get[EpochManager]
  val commitGroups = Seq(
    (io.commitOoO, epochMgr.decReq_ooo),
    (io.commitInO, epochMgr.decReq_ino)
  )

  val exception = Reg(
    FullMachineException(fullCommitRequestType)
  ) init (FullMachineException.idle(fullCommitRequestType))
  Machine.provide(exception)
  Machine.provide(exception.exc)
  exception.exc.valid := False

  when(exception.exc.valid) {
    Machine.report(Seq("exception HIGH: ", exception.exc.code))
  }

  val reset = exception.exc.valid

  val rob = new Area {

    val debugCyc = Reg(UInt(64 bits)) init (0)
    debugCyc := debugCyc + 1

    def getBankIndexForPtr(p: UInt) = p(
      log2Up(spec.writebackWidth) - 1 downto 0
    )
    def getEntryIndexForPtr(p: UInt) = p(
      p.getWidth - 1 downto log2Up(spec.writebackWidth)
    )
    assert(spec.robSize % spec.writebackWidth == 0)
    val robBankSize = spec.robSize / spec.writebackWidth

    val resetArea = new ResetArea(reset = reset, cumulative = true) {
      val risingOccupancy = Reg(Bool()) init (false)
      val pushPtr = Reg(spec.robEntryIndexType()) init (0)
      val popPtr = Reg(spec.robEntryIndexType()) init (0)
    }

    val headInfo = RobHeadInfo()
    headInfo.headPtr := resetArea.popPtr
    Machine.provide(headInfo)

    val risingOccupancy = resetArea.risingOccupancy
    val pushPtr = resetArea.pushPtr
    val popPtr = resetArea.popPtr

    val ptrEq = popPtr === pushPtr
    val empty = ptrEq && !risingOccupancy
    val full = ptrEq && risingOccupancy
    val banks =
      (0 until spec.writebackWidth).map(_ =>
        LvtMem(HardType(robEntryType), robBankSize)
      )
    val prfIf = Machine.get[PrfInterface]

    val dispatchPushLogic = new Area {
      val output = outType
      output.parentObjects(0) := io.input.payload
      output.robIndex := pushPtr
      output.epoch := epochMgr.currentEpoch

      val outputStream = io.input.translateWith(output).continueWhen(!full)

      val decodeInfo = io.input.payload.lookup[DecodeInfo]

      val inOrder = False
      for (fu <- sem.functionUnits) {
        if (fu.inOrder) {
          when(fu.staticTag === decodeInfo.functionUnitTag) {
            inOrder := True
          }
        }
      }

      when(inOrder) {
        io.inOrderOutput << outputStream
        io.oooOutput.setIdle()
      } otherwise {
        io.inOrderOutput.setIdle()
        io.oooOutput << outputStream
      }

      val assignedBankIndex = getBankIndexForPtr(pushPtr)
      val newEntry = robEntryType
      newEntry.commitRequest.assignDontCare()
      newEntry.commitRequest.ctx := io.input.payload
      newEntry.completed := False
      for ((b, i) <- banks.zipWithIndex) {
        b.write(
          address = getEntryIndexForPtr(pushPtr),
          data = newEntry,
          enable = assignedBankIndex === i && io.input.fire
        )
      }

      when(io.input.fire) {
        risingOccupancy := True
        pushPtr := (pushPtr === spec.robSize - 1).mux(
          True -> U(0),
          False -> (pushPtr + 1)
        )

        Machine.report(
          Seq(
            "Dispatch push: pushPtr=",
            pushPtr,
            " popPtr=",
            popPtr,
            " risingOccupancy=",
            risingOccupancy,
            " inOrder=",
            inOrder
          )
        )
      }

      when(io.input.isStall) {
        Machine.report(
          Seq("Dispatch STALL: full=", full)
        )
      }
    }

    val commitLogic = new Area {
      case class BankWrite() extends Bundle {
        val address = UInt(log2Up(robBankSize) bits)
        val data = robEntryType
        val enable = Bool()
      }
      val bankWrites = banks
        .map { b =>
          val w = BankWrite()
          w.assignDontCare()
          w.enable := False
          w
        }
        .to[ArrayBuffer]
      // Arbitrated commit
      for (
        ((commitGroup, decReq), commitGroupIndex) <- commitGroups.zipWithIndex
      ) {
        val commit = {
          // `StreamArbiter` breaks if `valid` goes down without `ready`
          val c =
            StreamArbiterFactory.roundRobin.on(commitGroup.map(x => x.check()))

          // Decrement epoch counter for both up-to-date and outdated commits
          when(c.fire) {
            Machine.report(
              Seq(
                "epoch count dec: epoch ",
                c.payload.token.epoch,
                " prev ",
                epochMgr.epochTable(c.payload.token.epoch),
                " commitGroup " + commitGroupIndex
              )
            )
            decReq.valid := True
            decReq.payload := c.payload.token.epoch
          }

          c.throwWhen(c.payload.token.epoch =/= epochMgr.currentEpoch)
        }
        commit.setCompositeName(
          this,
          postfix = "commitGroup_" + commitGroupIndex + "arbitratedCommit"
        )

        val assignedBankIndex = getBankIndexForPtr(
          commit.payload.token.robIndex
        ).setCompositeName(
          this,
          postfix = "commitGroup_" + commitGroupIndex + "assignedBankIndex"
        )
        val assignedEntryIndex = getEntryIndexForPtr(
          commit.payload.token.robIndex
        ).setCompositeName(
          this,
          postfix = "commitGroup_" + commitGroupIndex + "assignedEntryIndex"
        )

        val selectedEntry = robEntryType
        val selectedEntryValid = False
        commit.ready := selectedEntryValid

        selectedEntry.assignDontCare()

        if (commitGroupIndex == 0) {
          println("ROB entry width: " + selectedEntry.getBitsWidth)
          println("CommitReq width: " + commit.payload.getBitsWidth)
        }

        for ((b, i) <- banks.zipWithIndex) {
          val oldEntry = b.readAsync(address = assignedEntryIndex)
          val newEntry = robEntryType
          newEntry.completed := True
          newEntry.commitRequest.ctx := oldEntry.commitRequest.ctx
          newEntry.commitRequest.exception := commit.payload.exception
          newEntry.commitRequest.token := commit.payload.token

          val requestWrite = assignedBankIndex === i && commit.valid
          requestWrite.setCompositeName(
            this,
            postfix =
              "commitGroup_" + commitGroupIndex + "_bank_" + i + "_requestWrite"
          )
          val prevWrite = bankWrites(i)
          val newWrite = BankWrite()
          newWrite.address := assignedEntryIndex
          newWrite.data := newEntry
          newWrite.enable := requestWrite
          bankWrites.update(
            i,
            prevWrite.enable.mux(
              False -> newWrite,
              True -> prevWrite
            )
          )
          val fireNow = !prevWrite.enable && requestWrite
          fireNow.setCompositeName(
            this,
            postfix =
              "commitGroup_" + commitGroupIndex + "_bank_" + i + "_fireNow"
          )

          when(fireNow) {
            assert(
              !oldEntry.completed,
              "Commit request for completed ROB entry"
            )
            selectedEntry := oldEntry
            selectedEntryValid := True
          }
        }

        val renameInfo = selectedEntry.commitRequest.lookup[RenameInfo]
        val decodeInfo = selectedEntry.commitRequest.lookup[DecodeInfo]

        // Write to physical regfile
        for (
          ((dstRegPhys, dstRegArch), valueToWrite) <- renameInfo.physDstRegs
            .zip(
              decodeInfo.archDstRegs
            )
            .zip(commit.regWriteValue)
        ) {
          val prfItem = PrfItem()
          prfItem.data := valueToWrite

          val shouldWrite = dstRegArch.valid && selectedEntryValid
          prfIf.write(
            address = dstRegPhys,
            data = prfItem,
            enable = shouldWrite
          )

          new ResetArea(reset = reset, cumulative = true) {
            prfIf.notify_callerHandlesReset(
              enable = shouldWrite,
              index = dstRegPhys
            )
          }

          when(shouldWrite) {
            val st = prfIf.state.table(dstRegPhys)
            assert(st.busy, "Physical register not busy")
            assert(!st.dataAvailable, "Physical register already has data")

            st.busy := False
            st.dataAvailable := True
          }
        }

        when(selectedEntryValid) {
          Machine.report(
            Seq(
              "commit rob entry cyc=",
              debugCyc,
              " at ",
              commit.payload.token.robIndex,
              " epoch ",
              commit.payload.token.epoch,
              " group " + commitGroupIndex
            ) ++ renameInfo.physDstRegs
              .zip(
                decodeInfo.archDstRegs
              )
              .zip(commit.payload.regWriteValue)
              .flatMap(arg => {
                val ((phys, arch), value) = arg
                Seq(
                  "[v=",
                  arch.valid,
                  ",phys=",
                  phys,
                  ",arch=",
                  arch.index,
                  ",value=",
                  value,
                  "]"
                )
              })
          )
        }
      }

      for ((w, b) <- bankWrites.zip(banks)) {
        b.write(address = w.address, data = w.data, enable = w.enable)
      }
    }

    val popLogic = new Area {
      val currentBankIndex = getBankIndexForPtr(popPtr)

      // Read and re-permutate data from the ROB
      val readOutput = Vec(
        Vec((0 until banks.size).map(i => getEntryIndexForPtr(popPtr + i)))
          .rotateRight(currentBankIndex)
          .zipWithIndex
          .map(arg => {
            var (addr_, index) = arg
            val out = ReadOutput()
            out.addr := (addr_.asBits ## B(
              index,
              log2Up(spec.writebackWidth) bits
            )).asUInt
            out.data := banks(index).readAsync(address = addr_)
            out
          })
      )
        .rotateLeft(currentBankIndex)

      // Continuous ready entries
      // If `reset` is high on the current cycle, our internal state is a bit messy - don't expose it.
      var entryReady = !reset
      val renameIf = Machine.get[RenameInterface]
      var cmtSnapshot = renameIf.unit.cmt

      val effects = sem.functionUnits
        .map(x => (x, x.generateEffect()))
        .filter(x => x._2.isDefined)
        .map(x => (x._1, x._2.get))
      effects
        .foreach(x => {
          x._2.io_reset := reset
        })

      var retireCount = UInt(log2Up(spec.writebackWidth + 1) bits)
      retireCount := 0

      for (i <- 0 until spec.writebackWidth) {
        val entryData = readOutput(i)
        val localEmpty =
          entryData.addr === pushPtr && (!risingOccupancy || Bool(i != 0))
        entryReady = entryReady && !localEmpty && entryData.data.completed

        val nextEpoch = epochMgr.currentEpoch + 1
        val nextEpochAvailable = epochMgr.epochTable(nextEpoch) === 0

        // If this entry triggers an exception, ensure that the next epoch is available
        entryReady =
          entryReady && !(entryData.data.commitRequest.exception.valid && !nextEpochAvailable)

        // Commits following an exception should be ignored
        if (i != 0) {
          entryReady =
            entryReady && !readOutput(i - 1).data.commitRequest.exception.valid
        }

        // On an exception:
        // - Trigger a reset (through `exception.valid`)
        // - Advance the epoch number
        when(entryReady && entryData.data.commitRequest.exception.valid) {
          exception.ctx := entryData.data.commitRequest
          epochMgr.currentEpoch := nextEpoch

          Machine.report(Seq("triggering exception"))
        }

        val renameInfo = entryData.data.commitRequest.lookup[RenameInfo]
        val decodeInfo = entryData.data.commitRequest.lookup[DecodeInfo]

        val lastCmtSnapshot = cmtSnapshot

        cmtSnapshot = Vec(cmtSnapshot.map(x => {
          val v = spec.physRegIndexType
          v := x
          v
        }))

        for (
          (dstRegPhys, dstRegArch) <- renameInfo.physDstRegs
            .zip(
              decodeInfo.archDstRegs
            )
        ) {
          // The register should be made "persistent" if:
          // - It is requested to be written to
          // - The current entry is ready
          // - This entry does not cause an exception (excluding branches & serialize)
          val shouldWrite =
            dstRegArch.valid && entryReady && (!entryData.data.commitRequest.exception.valid ||
              !MachineExceptionCode.shouldSuppressWriteback(
                entryData.data.commitRequest.exception.code
              ))
          when(shouldWrite) {
            val st = prfIf.state.table(dstRegPhys)
            assert(!st.busy)
            assert(st.dataAvailable)
            assert(!st.allocatable)

            st.allocatable := True
            Machine.report(
              Seq(
                "DispatchUnit.popLogic writing cmt: dst.arch=",
                dstRegArch.index,
                " dst.phys=",
                dstRegPhys,
                " prevPhys=",
                lastCmtSnapshot(dstRegArch.index)
              )
            )
            cmtSnapshot.write(dstRegArch.index, dstRegPhys)
            renameIf.unit.cmtAllowMask
              .write(lastCmtSnapshot(dstRegArch.index), True)
            renameIf.unit.cmtAllowMask.write(dstRegPhys, False)
          }
        }

        io.writebackMonitor(i).valid := entryReady
        io.writebackMonitor(i).payload := entryData.data.commitRequest

        val eff = CommitEffect(fullCommitRequestType)
        eff.robIndex := entryData.addr
        eff.data := entryData.data.commitRequest

        effects.foreach(x => {
          x._2
            .io_effect(i)
            .valid := entryReady && decodeInfo.functionUnitTag === x._1.staticTag
          x._2.io_effect(i).payload := eff
        })

        val physValue_debug = Machine.debugGen {
          renameInfo.physDstRegs.map(phys => prfIf.readAsync(phys))
        }

        // Pop the item from the queue
        when(entryReady) {
          risingOccupancy := False
          popPtr := entryData.addr + 1

          val brCtx = entryData.data.commitRequest.tryLookup[AluBranchContext]
          val fetchPkt = entryData.data.commitRequest.tryLookup[FetchPacket]

          Machine.report(
            Seq(
              "writeback rob entry cyc=",
              debugCyc,
              " at ",
              entryData.addr,
              " exc.valid ",
              entryData.data.commitRequest.exception.valid,
              " exc.code ",
              entryData.data.commitRequest.exception.code
            ) ++
              (if (brCtx.isDefined) Seq(" pc ", brCtx.get.pc)
               else Seq()) ++
              (if (fetchPkt.isDefined)
                 Seq(" insn ", fetchPkt.get.insn)
               else Seq()) ++
              renameInfo.physDstRegs
                .zip(
                  decodeInfo.archDstRegs
                )
                .zip(physValue_debug)
                .flatMap(arg => {
                  val ((phys, arch), value) = arg
                  Seq(
                    "[v=",
                    arch.valid,
                    ",phys=",
                    phys,
                    ",arch=",
                    arch.index,
                    ",value=",
                    value.data,
                    "]"
                  )
                })
          )
        }

        retireCount = entryReady ? (retireCount + 1) | retireCount
      }

      renameIf.unit.cmt := cmtSnapshot
      Machine
        .tryGet[DispatchPerfCounters]
        .foreach(x => x.instRetired := x.instRetired + retireCount.resized)
    }
  }
}
