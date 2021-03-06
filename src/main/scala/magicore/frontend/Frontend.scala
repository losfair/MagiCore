package magicore.frontend

import spinal.core._
import spinal.lib._
import magicore.util._
import magicore.control._

case class FrontendSpec(
    icacheSize: Int,
    icacheMemPortDataWidth: Int,
    insnWidth: BitCount = 32 bits,
    addrWidth: BitCount = 32 bits,
    resetPc: BigInt = 0x0,
    globalHistorySize: Int = 1024,
    btbSize: Int = 64,
    initBranchPredictionBuffers: Boolean = false,
    compressed: Boolean = false
) {
  val insnType = HardType(Bits(insnWidth))
  val addrType = HardType(UInt(addrWidth))

  def addrMask = ~U((insnWidth.value / 8) - 1, addrWidth)
  val addrStep = insnWidth.value / 8

  assert(isPow2(globalHistorySize))
  val globalHistoryWidth = log2Up(globalHistorySize) bits
  val globalHistoryType = HardType(Bits(globalHistoryWidth))

  assert(isPow2(btbSize))
  val btbWidth = log2Up(btbSize) bits
}

abstract class FrontendSemantics {
  def decoder: DecoderInstance
}

trait DecoderInstance extends Area {}
