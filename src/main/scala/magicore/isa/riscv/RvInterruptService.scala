package magicore.isa.riscv

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import magicore.control._
import magicore.frontend._
import magicore.lib.funit._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

case class RvInterruptSourceState() extends Bundle {
  val enable = Bool()
  val pending = Bool()
}

object RvInterruptSourceState {
  def init: RvInterruptSourceState = {
    val x = RvInterruptSourceState()
    x.enable := False
    x.pending := False
    x
  }
}

case class RvInterruptLines() extends Bundle {
  val external = Bool()
  val timer = Bool()
  val software = Bool()
}

case class RvInterruptService() extends Area {
  val state = new Area {
    val external =
      Reg(RvInterruptSourceState()) init (RvInterruptSourceState.init)
    val timer = Reg(RvInterruptSourceState()) init (RvInterruptSourceState.init)
    val software =
      Reg(RvInterruptSourceState()) init (RvInterruptSourceState.init)
  }

  def setLines(lines: RvInterruptLines) {
    state.external.pending := lines.external
    state.timer.pending := lines.timer
    state.software.pending := lines.software
  }

  val csrMip = new Area {
    val encoding = Bits(32 bits)
    encoding := 0
    encoding(3) := state.software.pending // MSIP
    encoding(7) := state.timer.pending // MTIP
    encoding(11) := state.external.pending // MEIP
  }

  val csrMie = new Area {
    val encoding = Bits(32 bits)
    encoding := 0
    encoding(3) := state.software.enable // MSIE
    encoding(7) := state.timer.enable // MTIE
    encoding(11) := state.external.enable // MEIE

    def decodeAndSet(x: Bits) {
      state.software.enable := x(3)
      state.timer.enable := x(7)
      state.external.enable := x(11)
    }
  }

  val mie = Reg(Bool()) init (false)
  val active = False
  val trigger = active && mie
  val cause = U(0, 4 bits)

  // 3.1.9:
  // Multiple simultaneous interrupts destined for M-mode are handled in the following decreasing
  // priority order: MEI, MSI, MTI, SEI, SSI, STI.
  when(state.external.enable && state.external.pending) {
    active := True
    cause := 11
  } elsewhen (state.software.enable && state.software.pending) {
    active := True
    cause := 3
  } elsewhen (state.timer.enable && state.timer.pending) {
    active := True
    cause := 7
  }
}
