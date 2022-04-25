package miniooo.isa.riscv.soc

import spinal.core._
import spinal.lib._
import miniooo.util._
import MiniOoOExt._
import miniooo.control._
import miniooo.frontend._
import miniooo.lib.funit._
import miniooo.isa.riscv._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping

case class MiniRv32() extends Component {
  val debug = false
  val processor = RiscvProcessor(
    resetPc = 0x00010000,
    debug = debug,
    initBranchPredictionBuffers = false
  )

  val slaveIdWidth = 16

  val bootrom = Axi4SharedOnChipRam(
    dataWidth = 32,
    byteCount = 16384,
    idWidth = slaveIdWidth
  )
  bootrom.ram.initFromFile("./fsbl/firmware.bin")

  val ocram = Axi4SharedOnChipRam(
    dataWidth = 32,
    byteCount = 65536,
    idWidth = slaveIdWidth
  )

  val io = new Bundle {
    val bus = master(Axi4(Axi4Config(
      addressWidth = 32,
      dataWidth = 32,
      idWidth = slaveIdWidth
    )))
  }

  val bootromReadOnly = bootrom.io.axi.toAxi4ReadOnly()

  var axiCrossbar = Axi4CrossbarFactory()
  axiCrossbar.addSlaves(
    bootromReadOnly -> SizeMapping(0x00010000, 16384),
    ocram.io.axi -> SizeMapping(0x00020000, 65536),
    io.bus -> SizeMapping(0x40000000, 2 GiB)
  )
  axiCrossbar.addConnections(
    processor.io.iBus -> Seq(bootromReadOnly, ocram.io.axi, io.bus),
    processor.io.dBus -> Seq(bootromReadOnly, ocram.io.axi, io.bus)
  )
  axiCrossbar.build()
}
