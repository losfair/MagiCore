package magicore.isa.riscv.soc

import spinal.core._
import spinal.lib._
import spinal.lib.misc.InterruptCtrl
import spinal.lib.bus.amba4.axi._

case class Axi4InterruptCtrl(width: Int, idWidth: Int) extends Component {
  val io = new Bundle {
    val bus = slave(
      Axi4(
        Axi4Config(
          addressWidth = 5,
          dataWidth = 32,
          idWidth = idWidth
        )
      )
    )
    val inputs = in Bits (width bits)
    val pendings = out Bits (width bits)
  }

  val ctrl = InterruptCtrl(width)
  ctrl.io.inputs <> io.inputs
  ctrl.io.pendings <> io.pendings

  val factory = new Axi4SlaveFactory(io.bus)
  ctrl.driveFrom(factory, 0)
}
