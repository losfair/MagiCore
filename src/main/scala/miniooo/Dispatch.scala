package miniooo

import spinal.core._
import spinal.lib._
import MiniOoOExt._

case class DispatchedInsn(
    ctx: MachineContext,
    parentContextType: () => Bundle with DerefToInsn
) extends Bundle
    with DerefToInsn {
  val insn = ctx.sem.newDecodedInsn()
  val parentContext = parentContextType()
  val robIndex = ctx.cfg.robEntryIndexType
}

case class RobEntry(
    ctx: MachineContext,
    parentContextType: () => Bundle with DerefToInsn
) extends Bundle {
  val parentContext = parentContextType()
  val commitRequest = CommitRequest(ctx.cfg)
  val completed = Bool()
}

case class Dispatch(
    ctx: MachineContext,
    insnInputType: () => Bundle with DerefToInsn
) extends Area {
  val outInsnType = () => DispatchedInsn(ctx, insnInputType)

  val io = new Bundle {
    val insnInput = Stream(insnInputType())
    val insnOutput = Stream(outInsnType())
    val commitInput =
      Vec(Stream(CommitRequest(ctx.cfg)), ctx.sem.functionUnits.size)
  }

  val rob = new Area {
    def getBankIndexForPtr(p: UInt) = p(
      log2Up(ctx.cfg.commitWidth) - 1 downto 0
    )
    def getEntryIndexForPtr(p: UInt) = p(
      p.getWidth - 1 downto log2Up(ctx.cfg.commitWidth)
    )
    assert(ctx.cfg.robSize % ctx.cfg.commitWidth == 0)
    val robBankSize = ctx.cfg.robSize / ctx.cfg.commitWidth

    val risingOccupancy = Reg(Bool()) init (false)
    val pushPtr = Reg(ctx.cfg.robEntryIndexType) init (0)
    val popPtr = Reg(ctx.cfg.robEntryIndexType) init (0)
    val ptrEq = popPtr === pushPtr
    val empty = ptrEq && !risingOccupancy
    val full = ptrEq && risingOccupancy
    val banks =
      (0 until ctx.cfg.commitWidth).map(_ =>
        RatMem(RobEntry(ctx, insnInputType), robBankSize)
      )

    val dispatchPushLogic = new Area {
      val output = outInsnType()
      output.insn := io.insnInput.payload.insn
      output.parentContext := io.insnInput.payload
      output.robIndex := pushPtr
      io.insnOutput << io.insnInput.translateWith(output).continueWhen(!full)

      val assignedBankIndex = getBankIndexForPtr(pushPtr)
      val newEntry = RobEntry(ctx, insnInputType)
      newEntry.parentContext := io.insnInput.payload
      newEntry.commitRequest.assignDontCare()
      newEntry.completed := False
      for ((b, i) <- banks.zipWithIndex) {
        b.write(
          address = getEntryIndexForPtr(pushPtr),
          data = newEntry,
          enable = assignedBankIndex === i && io.insnInput.fire
        )
      }

      when(io.insnInput.fire) {
        risingOccupancy := True
        pushPtr := (pushPtr === ctx.cfg.robSize - 1).mux(
          True -> U(0),
          False -> (pushPtr + 1)
        )
      }
    }

    val commitLogic = new Area {
      // Arbitrated commit
      val commit = StreamArbiterFactory.roundRobin.build(
        CommitRequest(ctx.cfg),
        io.commitInput.size
      )
      for ((dst, src) <- commit.io.inputs.zip(io.commitInput)) {
        dst << src
      }

      val assignedBankIndex = getBankIndexForPtr(
        commit.io.output.payload.robAddr
      )
      val assignedEntryIndex = getEntryIndexForPtr(
        commit.io.output.payload.robAddr
      )

      commit.io.output.freeRun()

      val selectedEntry = RobEntry(ctx, insnInputType)
      val selectedEntryValid = False

      for ((b, i) <- banks.zipWithIndex) {
        val value = b.readAsync(address = assignedEntryIndex)
        value.completed := True
        value.commitRequest := commit.io.output.payload

        val fireNow = assignedBankIndex === i && commit.io.output.valid
        b.write(
          address = assignedEntryIndex,
          data = value,
          enable = fireNow
        )

        when(fireNow) {
          assert(!value.completed, "Commit request for completed ROB entry")
          selectedEntry := value
          selectedEntryValid := True
        }
      }

      val renameCtx = selectedEntry.parentContext.chainLookup[RenamedInsn]()
      val insn = selectedEntry.parentContext.insn

      // Write to physical regfile
      for (
        (dstRegPhys, dstRegArch) <- renameCtx.physDstRegs.zip(
          insn.archDstRegs
        )
      ) {
        val prfItem = PrfItem(ctx)
        prfItem.data := commit.io.output.regWriteValue

        val shouldWrite = dstRegArch.valid && selectedEntryValid
        ctx.prfContent.write(
          address = dstRegPhys,
          data = prfItem,
          enable = shouldWrite
        )
        when(shouldWrite) {
          val st = ctx.prfState.table(dstRegPhys)
          assert(st.busy)
          assert(!st.dataAvailable)

          st.busy := False
          st.dataAvailable := True
        }
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
            new Bundle {
              val addr = addr_
              val data = banks(index).readAsync(address = addr)
            }
          })
      )
        .rotateLeft(currentBankIndex)

      // Continuous ready entries
      var entryReady = True

      for (i <- 0 until ctx.cfg.commitWidth) {
        val entryData = readOutput(i)
        val localEmpty =
          entryData.addr === pushPtr && (!risingOccupancy || Bool(i != 0))
        entryReady = entryReady && !localEmpty && entryData.data.completed

        val renameCtx = entryData.data.parentContext.chainLookup[RenamedInsn]()
        val insn = entryData.data.parentContext.insn

        for (
          (dstRegPhys, dstRegArch) <- renameCtx.physDstRegs.zip(
            insn.archDstRegs
          )
        ) {
          val prfItem = PrfItem(ctx)
          prfItem.data := entryData.data.commitRequest.regWriteValue

          val shouldWrite = dstRegArch.valid && entryReady
          when(shouldWrite) {
            val st = ctx.prfState.table(dstRegPhys)
            assert(!st.busy)
            assert(st.dataAvailable)
            assert(!st.allocatable)

            st.allocatable := True
            ctx.rename.cmt.write(dstRegArch.index, dstRegPhys)
            ctx.rename.cmtAllowMask.write(ctx.rename.cmt(dstRegArch.index), True)
            ctx.rename.cmtAllowMask.write(dstRegPhys, False)
          }
        }

        // Pop the item from the queue
        when(entryReady) {
          risingOccupancy := False
          popPtr := entryData.addr + 1
        }
      }
    }
  }
}
