package spinal.lib.com.uart

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

object Axi4UartCtrl {
  def getAxi4Config(idWidth: Int) = Axi4Config(
    addressWidth = 5,
    dataWidth = 32,
    idWidth = idWidth
  )
}

case class Axi4UartCtrl(idWidth: Int, config: UartCtrlMemoryMappedConfig)
    extends Component {
  val io = new Bundle {
    val axi = slave(Axi4(Axi4UartCtrl.getAxi4Config(idWidth)))
    val uart = master(Uart())
    val interrupt = out Bool ()
  }

  val uartCtrl = new UartCtrl(config.uartCtrlConfig)
  io.uart <> uartCtrl.io.uart

  val busCtrl = new Axi4SlaveFactory(io.axi)
  val bridge = uartCtrl.driveFrom32(busCtrl, config)
  io.interrupt := bridge.interruptCtrl.interrupt
}
