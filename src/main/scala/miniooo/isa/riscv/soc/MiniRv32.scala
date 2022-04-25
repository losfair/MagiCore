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
import spinal.lib.com.uart.Axi4UartCtrl
import spinal.lib.com.uart.UartCtrlMemoryMappedConfig
import spinal.lib.com.uart.Uart

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

  val uart = Axi4UartCtrl(
    idWidth = slaveIdWidth,
    config = UartCtrlMemoryMappedConfig(
      baudrate = 115200,
      txFifoDepth = 16,
      rxFifoDepth = 16,
      writeableConfig = true,
      clockDividerWidth = 20
    )
  )

  val io = new Bundle {
    val bus = master(
      Axi4(
        Axi4Config(
          addressWidth = 32,
          dataWidth = 32,
          idWidth = slaveIdWidth
        )
      )
    )
    val uart = master(Uart())
  }

  io.uart <> uart.io.uart

  var axiCrossbar = Axi4CrossbarFactory()
  axiCrossbar.addSlaves(
    bootrom.io.axi -> SizeMapping(0x00010000, 16384), // TODO: Error on writes
    ocram.io.axi -> SizeMapping(0x00020000, 65536),
    io.bus -> SizeMapping(0x40000000, 2 GiB),
    uart.io.axi -> SizeMapping(BigInt("ff010000", 16), 0x1000)
  )
  axiCrossbar.addConnections(
    processor.io.iBus -> Seq(bootrom.io.axi, ocram.io.axi, io.bus),
    processor.io.dBus -> Seq(bootrom.io.axi, ocram.io.axi, io.bus, uart.io.axi)
  )
  axiCrossbar.build()
}

object MiniRv32SyncReset {
  object SyncResetSpinalConfig
      extends SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC),
        defaultClockDomainFrequency = FixedFrequency(50 MHz)
      )

  def main(args: Array[String]) {
    SyncResetSpinalConfig.generateVerilog(Machine.build {
      new MiniRv32()
    })
  }
}
