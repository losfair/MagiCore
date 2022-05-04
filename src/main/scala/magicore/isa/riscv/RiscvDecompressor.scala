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
    output.payload.assignSomeByName(first.fetch)

    when(firstValid) {
      when(first.data(1 downto 0) === B"11") {
        // Uncompressed - wait & issue
        when(secondValid) {
          output.valid := True
          output.payload.insn := second.data ## first.data
          when(output.ready) {
            bufferShiftAmount := 2
          }
        }
      } otherwise {
        // Compressed - decompress & issue
        output.valid := True
        output.payload.insn := RiscvDecompressorUtil.decompressOnce(first.data)
        output.payload.compressed := True
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
    val rv64 = Machine.get[MachineSpec].dataWidth.value == 64
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
        out(31 downto 25) := uimm(11 downto 5)
        out(11 downto 7) := uimm(4 downto 0)
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
        out(31 downto 25) := uimm(11 downto 5)
        out(11 downto 7) := uimm(4 downto 0)
        out(E.rs2Range) := decompressRegIndex(from(4 downto 2))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      is(M"000-----------01") {
        // C.NOP / C.ADDI
        val imm_ = B(0, 6 bits)
        imm_(5) := from(12)
        imm_(4 downto 0) := from(6 downto 2)
        val imm = imm_.asSInt.resize(12 bits).asBits

        out := E.ADDI.value
        out(31 downto 20) := imm
        out(E.rdRange) := from(11 downto 7)
        out(E.rs1Range) := from(11 downto 7)
      }
      if (rv64) {
        is(M"001-----------01") {
          // C.ADDIW
          // Is C.JAL on RV32 - not implemented
          val imm_ = B(0, 6 bits)
          imm_(5) := from(12)
          imm_(4 downto 0) := from(6 downto 2)
          val imm = imm_.asSInt.resize(12 bits).asBits

          out := E.ADDIW.value
          out(31 downto 20) := imm
          out(E.rdRange) := from(11 downto 7)
          out(E.rs1Range) := from(11 downto 7)
        }
      }
      is(M"010-----------01") {
        // C.LI
        val imm_ = B(0, 6 bits)
        imm_(5) := from(12)
        imm_(4 downto 0) := from(6 downto 2)
        val imm = imm_.asSInt.resize(12 bits).asBits

        out := E.ADDI.value
        out(31 downto 20) := imm
        out(E.rdRange) := from(11 downto 7)
      }
      is(M"011-----------01") {
        when(from(11 downto 7) === 2) {
          // C.ADDI16SP
          val nzimm_ = B(0, 10 bits)
          nzimm_(9) := from(12)
          nzimm_(4) := from(6)
          nzimm_(6) := from(5)
          nzimm_(8 downto 7) := from(4 downto 3)
          nzimm_(5) := from(2)

          out := E.ADDI.value
          out(31 downto 20) := nzimm_.asSInt.resize(12 bits).asBits
          out(E.rdRange) := decompressRegIndex(from(4 downto 2))
          out(E.rs1Range) := 2
        } otherwise {
          // C.LUI
          val nzimm_ = B(0, 18 bits)
          nzimm_(17) := from(12)
          nzimm_(16 downto 12) := from(6 downto 2)
          out := E.LUI.value
          out(31 downto 12) := nzimm_(17 downto 12).asSInt
            .resize(20 bits)
            .asBits
        }
      }
      is(M"100-00--------01") {
        // C.SRLI
        val uimm = B(0, 12 bits)
        uimm(5) := from(12)
        uimm(4 downto 0) := from(6 downto 2)

        out := E.SRLI.value
        out(31 downto 20) := uimm
        out(E.rdRange) := decompressRegIndex(from(9 downto 7))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      is(M"100-01--------01") {
        // C.SRAI
        val uimm = B(0, 12 bits)
        uimm(5) := from(12)
        uimm(4 downto 0) := from(6 downto 2)

        out := E.SRAI.value
        out(31 downto 20) := uimm
        out(E.rdRange) := decompressRegIndex(from(9 downto 7))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      is(M"100-10--------01") {
        // C.ANDI
        val imm_ = B(0, 6 bits)
        imm_(5) := from(12)
        imm_(4 downto 0) := from(6 downto 2)
        val imm = imm_.asSInt.resize(12 bits).asBits

        out := E.ANDI.value
        out(31 downto 20) := imm
        out(E.rdRange) := decompressRegIndex(from(9 downto 7))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      is(M"100011---00---01") {
        // C.SUB
        out := E.SUB.value
        out(E.rdRange) := decompressRegIndex(from(9 downto 7))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
        out(E.rs2Range) := decompressRegIndex(from(4 downto 2))
      }
      is(M"100011---01---01") {
        // C.XOR
        out := E.XOR.value
        out(E.rdRange) := decompressRegIndex(from(9 downto 7))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
        out(E.rs2Range) := decompressRegIndex(from(4 downto 2))
      }
      is(M"100011---10---01") {
        // C.OR
        out := E.OR.value
        out(E.rdRange) := decompressRegIndex(from(9 downto 7))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
        out(E.rs2Range) := decompressRegIndex(from(4 downto 2))
      }
      is(M"100011---11---01") {
        // C.AND
        out := E.AND.value
        out(E.rdRange) := decompressRegIndex(from(9 downto 7))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
        out(E.rs2Range) := decompressRegIndex(from(4 downto 2))
      }
      is(M"100111---00---01") {
        // C.SUBW
        out := E.SUBW.value
        out(E.rdRange) := decompressRegIndex(from(9 downto 7))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
        out(E.rs2Range) := decompressRegIndex(from(4 downto 2))
      }
      is(M"100111---01---01") {
        // C.ADDW
        out := E.ADDW.value
        out(E.rdRange) := decompressRegIndex(from(9 downto 7))
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
        out(E.rs2Range) := decompressRegIndex(from(4 downto 2))
      }
      is(M"101-----------01") {
        // C.J
        val imm_ = B(0, 12 bits)
        imm_(11) := from(12)
        imm_(4) := from(11)
        imm_(9 downto 8) := from(10 downto 9)
        imm_(10) := from(8)
        imm_(6) := from(7)
        imm_(7) := from(6)
        imm_(3 downto 1) := from(5 downto 3)
        imm_(5) := from(2)

        val imm = imm_.asSInt.resize(21 bits).asBits

        val target = B(0, 32 bits)
        target(31) := imm(20)
        target(30 downto 21) := imm(10 downto 1)
        target(20) := imm(11)
        target(19 downto 12) := imm(19 downto 12)

        out := E.JAL(true).value
        out(31 downto 12) := target(31 downto 12)
        out(E.rdRange) := 0
      }
      is(M"110-----------01") {
        // C.BEQZ
        val imm_ = B(0, 9 bits)
        imm_(8) := from(12)
        imm_(4 downto 3) := from(11 downto 10)
        imm_(7 downto 6) := from(6 downto 5)
        imm_(2 downto 1) := from(4 downto 3)
        imm_(5) := from(2)

        val imm = imm_.asSInt.resize(13 bits).asBits

        out := E.BEQ(true).value
        out(31) := imm(12)
        out(30 downto 25) := imm(10 downto 5)
        out(11 downto 8) := imm(4 downto 1)
        out(7) := imm(11)
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      is(M"111-----------01") {
        // C.BNEZ
        val imm_ = B(0, 9 bits)
        imm_(8) := from(12)
        imm_(4 downto 3) := from(11 downto 10)
        imm_(7 downto 6) := from(6 downto 5)
        imm_(2 downto 1) := from(4 downto 3)
        imm_(5) := from(2)

        val imm = imm_.asSInt.resize(13 bits).asBits

        out := E.BNE(true).value
        out(31) := imm(12)
        out(30 downto 25) := imm(10 downto 5)
        out(11 downto 8) := imm(4 downto 1)
        out(7) := imm(11)
        out(E.rs1Range) := decompressRegIndex(from(9 downto 7))
      }
      is(M"000-----------10") {
        // C.SLLI
        val uimm = B(0, 12 bits)
        uimm(5) := from(12)
        uimm(4 downto 0) := from(6 downto 2)

        out := E.SLLI.value
        out(31 downto 20) := uimm
        out(E.rdRange) := from(11 downto 7)
        out(E.rs1Range) := from(11 downto 7)
      }
      is(M"010-----------10") {
        // C.LWSP
        val uimm = B(0, 12 bits)
        uimm(5) := from(12)
        uimm(4 downto 2) := from(6 downto 4)
        uimm(7 downto 6) := from(3 downto 2)

        out := E.LW.value
        out(31 downto 20) := uimm
        out(E.rdRange) := from(11 downto 7)
        out(E.rs1Range) := 2
      }
      if (rv64) {
        is(M"011-----------10") {
          // C.LDSP
          val uimm = B(0, 12 bits)
          uimm(5) := from(12)
          uimm(4 downto 3) := from(6 downto 5)
          uimm(8 downto 6) := from(4 downto 2)

          out := E.LW.value
          out(31 downto 20) := uimm
          out(E.rdRange) := from(11 downto 7)
          out(E.rs1Range) := 2
        }
      }
      is(M"1000----------10") {
        when(from(6 downto 2) === 0) {
          // C.JR
          when(from(11 downto 7) === 0) {
            throwAsIllegal()
          } otherwise {
            out := E.JALR.value
            out(E.rs1Range) := from(11 downto 7)
          }
        } otherwise {
          // C.MV
          out := E.ADD.value
          out(E.rs2Range) := from(6 downto 2)
          out(E.rdRange) := from(11 downto 7)
        }
      }
      is(M"1001----------10") {
        when(from(6 downto 2) === 0) {
          when(from(11 downto 7) === 0) {
            // C.EBREAK - not implemented
            throwAsIllegal()
          } otherwise {
            // C.JALR
            out := E.JALR.value
            out(E.rdRange) := 1
            out(E.rs1Range) := from(11 downto 7)
          }
        } otherwise {
          // C.ADD
          out := E.ADD.value
          out(E.rs2Range) := from(6 downto 2)
          out(E.rs1Range) := from(11 downto 7)
          out(E.rdRange) := from(11 downto 7)
        }
      }
      is(M"110-----------10") {
        // C.SWSP
        val uimm = B(0, 12 bits)
        uimm(5 downto 2) := from(12 downto 9)
        uimm(7 downto 6) := from(8 downto 7)

        out := E.SW.value
        out(31 downto 25) := uimm(11 downto 5)
        out(11 downto 7) := uimm(4 downto 0)
        out(E.rs2Range) := from(6 downto 2)
        out(E.rs1Range) := 2
      }
      is(M"111-----------10") {
        // C.SDSP
        val uimm = B(0, 12 bits)
        uimm(5 downto 3) := from(12 downto 10)
        uimm(8 downto 6) := from(9 downto 7)

        out := E.SD.value
        out(31 downto 25) := uimm(11 downto 5)
        out(11 downto 7) := uimm(4 downto 0)
        out(E.rs2Range) := from(6 downto 2)
        out(E.rs1Range) := 2
      }
      default {
        throwAsIllegal()
      }
    }
    out
  }
}
