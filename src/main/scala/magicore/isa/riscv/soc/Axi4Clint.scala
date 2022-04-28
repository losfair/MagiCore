package magicore.isa.riscv.soc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.misc.Clint

case class Axi4Clint(hartCount : Int, idWidth: Int, dataWidth: Int) extends Component{
  val io = new Bundle {
    val bus = slave(Axi4(Axi4Config(addressWidth = 16, dataWidth = dataWidth, idWidth = idWidth)))
    val timerInterrupt = out Bits(hartCount bits)
    val softwareInterrupt = out Bits(hartCount bits)
    val time = out UInt(64 bits)
  }

  val factory = new Axi4SlaveFactory(io.bus)
  val logic = Clint(hartCount)
  logic.driveFrom(factory)

  for(hartId <- 0 until hartCount){
    io.timerInterrupt(hartId) := logic.harts(hartId).timerInterrupt
    io.softwareInterrupt(hartId) := logic.harts(hartId).softwareInterrupt
  }

  io.time := logic.time
}
