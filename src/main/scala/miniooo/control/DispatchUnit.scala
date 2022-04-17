package miniooo.control

import spinal.core._
import spinal.lib._
import miniooo.util._
import MiniOoOExt._

case class DispatchInfo(hardType: HardType[_ <: PolymorphicDataChain])
    extends Bundle
    with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]
  val robIndex = spec.robEntryIndexType()
  val parentObjects = if (hardType != null) Seq(hardType()) else Seq()
}

case class CommitRequest(hardType: HardType[_ <: PolymorphicDataChain])
    extends Bundle
    with PolymorphicDataChain {
  private val spec = Machine.get[MachineSpec]
  val robAddr = spec.robEntryIndexType()
  val regWriteValue = Vec(
    (0 until spec.maxNumDstRegsPerInsn).map(_ => spec.dataType)
  )
  val parentObjects = if (hardType != null) Seq(hardType()) else Seq()
}

case class RobEntry(hardType: HardType[_ <: PolymorphicDataChain])
    extends Bundle {
  val commitRequest = CommitRequest(hardType) setCompositeName (this, "cr")
  val completed = Bool()
}

case class DispatchUnit[T <: PolymorphicDataChain](
    dataType: HardType[T]
) extends Area {

  private val spec = Machine.get[MachineSpec]
  private val sem = Machine.get[MachineSemantics]

  def commitRequestType = CommitRequest(null)
  def outType = DispatchInfo(dataType())
  def robEntryType = RobEntry(dataType())

  case class ReadOutput() extends Bundle {
    val addr = spec.robEntryIndexType()
    val data = robEntryType
  }

  val io = new Bundle {
    val input = Stream(dataType())
    val output = Stream(outType)
    val commit = Vec(Stream(commitRequestType), sem.numFunctionUnits)
    val writebackMonitor = Vec(Flow(CommitRequest(dataType)), spec.commitWidth)
  }

  val rob = new Area {

    val debugCyc = Reg(UInt(64 bits)) init (0)
    debugCyc := debugCyc + 1

    def getBankIndexForPtr(p: UInt) = p(
      log2Up(spec.commitWidth) - 1 downto 0
    )
    def getEntryIndexForPtr(p: UInt) = p(
      p.getWidth - 1 downto log2Up(spec.commitWidth)
    )
    assert(spec.robSize % spec.commitWidth == 0)
    val robBankSize = spec.robSize / spec.commitWidth

    val risingOccupancy = Reg(Bool()) init (false)
    val pushPtr = Reg(spec.robEntryIndexType()) init (0)
    val popPtr = Reg(spec.robEntryIndexType()) init (0)
    val ptrEq = popPtr === pushPtr
    val empty = ptrEq && !risingOccupancy
    val full = ptrEq && risingOccupancy
    val banks =
      (0 until spec.commitWidth).map(_ =>
        RatMem(HardType(robEntryType), robBankSize)
      )
    val prfIf = Machine.get[PrfInterface]

    val dispatchPushLogic = new Area {
      val output = outType
      output.parentObjects(0) := io.input.payload
      output.robIndex := pushPtr
      io.output << io.input.translateWith(output).continueWhen(!full)

      val assignedBankIndex = getBankIndexForPtr(pushPtr)
      val newEntry = robEntryType
      newEntry.commitRequest.assignDontCare()
      newEntry.commitRequest.parentObjects(0) := io.input.payload
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
      }
    }

    val commitLogic = new Area {
      // Arbitrated commit
      val commit = StreamArbiterFactory.roundRobin.on(io.commit)

      val assignedBankIndex = getBankIndexForPtr(
        commit.payload.robAddr
      )
      val assignedEntryIndex = getEntryIndexForPtr(
        commit.payload.robAddr
      )

      commit.freeRun()

      val selectedEntry = robEntryType
      val selectedEntryValid = False

      selectedEntry.assignDontCare()
      println("ROB entry width: " + selectedEntry.getBitsWidth)
      println("CommitReq width: " + commit.payload.getBitsWidth)

      for ((b, i) <- banks.zipWithIndex) {
        val oldEntry = b.readAsync(address = assignedEntryIndex)
        val newEntry = robEntryType
        newEntry.completed := True
        newEntry.commitRequest.parentObjects
          .zip(oldEntry.commitRequest.parentObjects)
          .foreach { case (n, o) =>
            n := o
          }
        newEntry.commitRequest.regWriteValue := commit.payload.regWriteValue
        newEntry.commitRequest.robAddr := commit.payload.robAddr

        val fireNow = assignedBankIndex === i && commit.valid
        b.write(
          address = assignedEntryIndex,
          data = newEntry,
          enable = fireNow
        )

        when(fireNow) {
          assert(!oldEntry.completed, "Commit request for completed ROB entry")
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
        prfIf.notify(enable = shouldWrite, index = dstRegPhys)
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
            commit.payload.robAddr
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
              log2Up(spec.commitWidth) bits
            )).asUInt
            out.data := banks(index).readAsync(address = addr_)
            out
          })
      )
        .rotateLeft(currentBankIndex)

      // Continuous ready entries
      var entryReady = True
      val renameIf = Machine.get[RenameInterface]
      var cmtSnapshot = renameIf.unit.cmt

      for (i <- 0 until spec.commitWidth) {
        val entryData = readOutput(i)
        val localEmpty =
          entryData.addr === pushPtr && (!risingOccupancy || Bool(i != 0))
        entryReady = entryReady && !localEmpty && entryData.data.completed

        val renameInfo = entryData.data.commitRequest.lookup[RenameInfo]
        val decodeInfo = entryData.data.commitRequest.lookup[DecodeInfo]

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
          val shouldWrite = dstRegArch.valid && entryReady
          when(shouldWrite) {
            val st = prfIf.state.table(dstRegPhys)
            assert(!st.busy)
            assert(st.dataAvailable)
            assert(!st.allocatable)

            st.allocatable := True
            cmtSnapshot.write(dstRegArch.index, dstRegPhys)
            renameIf.unit.cmtAllowMask
              .write(cmtSnapshot(dstRegArch.index), True)
            renameIf.unit.cmtAllowMask.write(dstRegPhys, False)
          }
        }

        io.writebackMonitor(i).valid := entryReady
        io.writebackMonitor(i).payload := entryData.data.commitRequest

        // Pop the item from the queue
        when(entryReady) {
          risingOccupancy := False
          popPtr := entryData.addr + 1

          Machine.report(
            Seq(
              "writeback rob entry cyc=",
              debugCyc,
              " at ",
              entryData.addr
            ) ++ renameInfo.physDstRegs
              .zip(
                decodeInfo.archDstRegs
              )
              .zip(entryData.data.commitRequest.regWriteValue)
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

      renameIf.unit.cmt := cmtSnapshot
    }
  }
}
