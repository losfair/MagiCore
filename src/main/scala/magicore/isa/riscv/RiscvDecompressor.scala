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

case class RiscvDecompressor() extends Area {
  val input = Stream(FetchPacket())
  val output = Stream(FetchPacket())

  val exc = Machine.get[MachineException]
  val sliceWidth = 16 bits
  val bufferSize = 4

  case class Slice() extends Bundle {
    val fetch = FetchPacket(hasInsn = false)
    val data = Bits(sliceWidth)
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

        when(enable) {
          assert(subCounter < nextBuffer.size, "subCounter overflow")
          nextBuffer(subCounter.resize(log2Up(nextBuffer.size) bits)) := slice
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

    output.setIdle()

    when(firstValid) {
      when(first.data(1 downto 0) === B"11") {
        // Uncompressed - wait & issue
        when(secondValid) {
          output.valid := True
          output.payload.assignSomeByName(first.fetch)
          output.payload.insn := second.data ## first.data
          when(output.ready) {
            bufferShiftAmount := 2
          }
        }
      } otherwise {
        // Compressed - decompress & issue
        output.valid := True
        output.payload.assignSomeByName(first.fetch)
        output.payload.insn := RiscvDecompressorUtil.decompressOnce(first.data)
        when(output.ready) {
          bufferShiftAmount := 1
        }
      }
    }
  }
}

object RiscvDecompressorUtil {
  def decompressRegIndex(from: Bits): Bits = {
    assert(from.getWidth == 3)
    B"01" ## from
  }

  def decompressOnce(from: Bits): Bits = {
    val E = RvEncoding

    assert(from.getWidth == 16)
    val out = Bits(32 bits)

    def throwAsIllegal() {
      out(31 downto 16) := 0
      out(15 downto 0) := from
    }

    switch(from) {
      is(M"000-----------00") {
        // C.ADDI4SPN
        val nzuimm = B(0, 12 bits)
        nzuimm(5 downto 4) := from(12 downto 11)
        nzuimm(9 downto 6) := from(10 downto 7)
        nzuimm(2) := from(6)
        nzuimm(3) := from(5)

        when(nzuimm === 0) {
          throwAsIllegal()
        } otherwise {
          out := E.ADDI.value
          out(31 downto 20) := nzuimm
          out(E.rdRange) := decompressRegIndex(from(4 downto 2))
          out(E.rs1Range) := 2
        }

      }
      is(M"010-----------00") {
        // C.LW
        val uimm = B(0, 12 bits)
        uimm(5 downto 3) := from(12 downto 10)
        uimm(2) := from(6)
        uimm(6) := from(5)

        out := E.LW.value
        out(31 downto 20) := uimm
        out(E.rdRange) := decompressRegIndex(from(4 downto 2))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      is(M"011-----------00") {
        // C.LD
        val uimm = B(0, 12 bits)
        uimm(5 downto 3) := from(12 downto 10)
        uimm(7) := from(6)
        uimm(6) := from(5)

        out := E.LD.value
        out(31 downto 20) := uimm
        out(E.rdRange) := decompressRegIndex(from(4 downto 2))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      is(M"110-----------00") {
        // C.SW
        val uimm = B(0, 12 bits)
        uimm(5 downto 3) := from(12 downto 10)
        uimm(2) := from(6)
        uimm(6) := from(5)

        out := E.SW.value
        out(31 downto 20) := uimm
        out(E.rs2Range) := decompressRegIndex(from(4 downto 2))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      is(M"111-----------00") {
        // C.SD
        val uimm = B(0, 12 bits)
        uimm(5 downto 3) := from(12 downto 10)
        uimm(7) := from(6)
        uimm(6) := from(5)

        out := E.SD.value
        out(31 downto 20) := uimm
        out(E.rs2Range) := decompressRegIndex(from(4 downto 2))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      default {
        throwAsIllegal()
      }
    }
    out
  }
}
