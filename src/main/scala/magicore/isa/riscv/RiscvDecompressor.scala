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
import vexriscv.plugin.RvcDecompressor

case class RiscvDecompressor() extends Area {
  val input = Stream(FetchPacket())
  val output = Stream(FetchPacket())

  val exc = Machine.get[MachineException]
  val sliceWidth = 16 bits
  val bufferSize = 4

  case class Slice() extends Bundle {
    val fetch = FetchPacket(hasInsn = false)
    val data = Bits(sliceWidth)
    val headOfLine = Bool()
  }

  assert(input.payload.insn.getWidth % sliceWidth.value == 0)
  val slicesPerInsn = input.payload.insn.getWidth / sliceWidth.value
  val sliceSubIndexRange =
    (log2Up(sliceWidth.value / 8) + log2Up(slicesPerInsn) - 1) downto log2Up(
      sliceWidth.value / 8
    )

  def provide() {
    Machine.provide(
      FetchDecompressor(input = input, output = output, latency = 1)
    )
  }

  val state = exc.resetArea {
    new Area {
      val buffer = Vec(Reg(Slice()), bufferSize)
      val bufferTop = Reg(UInt(log2Up(bufferSize + 1) bits)) init (0)
    }
  }

  val bufferIsFull = state.bufferTop > bufferSize - slicesPerInsn
  input.ready := !bufferIsFull

  val nextBuffer = Vec(Slice(), bufferSize)
  nextBuffer := state.buffer

  val bufferTopAfterPush = U(0, state.bufferTop.getWidth bits)
  val bufferShiftAmount = U(0, state.bufferTop.getWidth bits)
  for (i <- 0 until bufferSize)
    state.buffer(i) := nextBuffer(
      (bufferShiftAmount + i).resize(log2Up(nextBuffer.size) bits)
    )
  state.bufferTop := bufferTopAfterPush - bufferShiftAmount

  assert(
    bufferTopAfterPush >= bufferShiftAmount,
    "decompression buffer invariant violation"
  )

  // Shift in the new instruction
  val bufferGenLogic = new Area {
    when(input.fire) {
      val slices =
        input.payload.insn.subdivideIn(sliceCount = SlicesCount(slicesPerInsn))
      val startAt = input.payload.pc(sliceSubIndexRange)
      var subCounter = UInt(state.bufferTop.getWidth bits)
      subCounter := state.bufferTop

      for (i <- 0 until slicesPerInsn) {
        val enable = startAt <= i
        val slice = Slice()
        val higherBits = input.payload
          .pc(
            (input.payload.pc.getWidth - 1) downto (sliceSubIndexRange.start + 1)
          )
          .asBits ## B(i, sliceSubIndexRange.numRangeElements bits)
        val lowerBits = B(0, 1 bits)
        slice.fetch.pc := (higherBits ## lowerBits).asUInt
        slice.fetch.assignUnassignedByName(input.payload)
        slice.data := slices(i)
        slice.headOfLine := Bool(i == 0)

        when(enable) {
          assert(subCounter < nextBuffer.size, "subCounter overflow")
          nextBuffer(subCounter.resize(log2Up(nextBuffer.size) bits)) := slice
          Machine.report(
            Seq(
              "Decompressor buffer push: pc=",
              slice.fetch.pc,
              " data=",
              slice.data
            )
          )
        }
        subCounter = enable.mux(
          False -> subCounter,
          True -> (subCounter + 1)
        )
      }
      bufferTopAfterPush := subCounter
    } otherwise {
      bufferTopAfterPush := state.bufferTop
    }
  }

  val outputLogic = new Area {
    val first = state.buffer(0)
    val firstValid = state.bufferTop =/= 0
    val second = state.buffer(1)
    val secondValid = state.bufferTop =/= 1

    // Integrity cases:
    // 1. 4-byte instruction, head-of-line aligned, branch predicted - OK.
    // 2. 4-byte instruction, NOT aligned:
    //    a. Branched predicted on the first half - ERROR. The second half is not part of this instruction.
    //    b. Rescheduling tag mismatches between the two halves - ERROR. The second half is not part of this instruction.
    // 3. 2-byte instruction, head-of-line aligned, branch predicted -
    //    If not a branch, suppress its own branch signal.
    //    Otherwise, prevent the second half from being issued.
    // 4. 2-byte instruction, NOT aligned, branch predicted - OK.

    output.setIdle()
    output.payload.assignSomeByName(first.fetch)

    when(firstValid) {
      when(first.data(1 downto 0) === B"11") {
        // Uncompressed - wait & issue
        when(secondValid) {
          output.valid := True
          output.payload.insn := second.data ## first.data
          // Integrity case 2
          output.payload.decompressionIntegrityError := !first.headOfLine && (first.fetch.predictedBranchValid || first.fetch.pcTag =/= second.fetch.pcTag)

          output.payload.cacheMiss := first.fetch.cacheMiss || second.fetch.cacheMiss
          output.payload.cacheMissOffset := first.fetch.cacheMiss ? U(0) | U(
            2
          )

          when(output.fire) {
            Machine.report(
              Seq(
                "Decompressor issue (NC): pc=",
                output.payload.pc,
                " insn=",
                output.payload.insn,
                " integrityError=",
                output.payload.decompressionIntegrityError
              )
            )
            bufferShiftAmount := output.payload.decompressionIntegrityError ? U(
              1,
              bufferShiftAmount.getWidth bits
            ) | U(2, bufferShiftAmount.getWidth bits)
          }
        }
      } otherwise {
        // Compressed - decompress & issue
        output.valid := True
        val decompressed =
          RvcDecompressor(first.data, rvf = false, rvd = false, xlen = 64)
        output.payload.insn := decompressed.illegal ? first.data.resize(
          output.payload.insn.getWidth bits
        ) | decompressed.inst
        output.payload.compressed := True
        when(output.fire) {
          Machine.report(
            Seq(
              "Decompressor issue (C): pc=",
              output.payload.pc,
              " insn=",
              output.payload.insn
            )
          )
          bufferShiftAmount := 1
        }

        // Integrity case 3
        when(first.headOfLine) {
          assert(secondValid, "first is headOfLine but second is not valid")
          assert(
            first.fetch.predictedBranchValid === second.fetch.predictedBranchValid,
            "inconsistent PBV"
          )

          switch(output.payload.insn) {
            is(
              RvEncoding.JAL(true),
              RvEncoding.JALR,
              RvEncoding.BEQ(true),
              RvEncoding.BNE(true)
            ) {
              when(output.fire && first.fetch.predictedBranchValid) {
                bufferShiftAmount := 2
              }
            }
            default {
              output.payload.predictedBranchValid := False
            }
          }
        }
      }
    }
  }
}
