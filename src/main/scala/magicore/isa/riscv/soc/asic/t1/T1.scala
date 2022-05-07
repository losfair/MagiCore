package magicore.isa.riscv.soc.asic.t1

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import magicore.control._
import magicore.frontend._
import magicore.lib.funit._
import magicore.isa.riscv._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.uart.Axi4UartCtrl
import spinal.lib.com.uart.UartCtrlMemoryMappedConfig
import spinal.lib.com.uart.Uart
import spinal.lib.misc.InterruptCtrl
import spinal.lib.misc.plic.PlicMapping
import magicore.isa.riscv.soc.Axi4Clint
import magicore.isa.riscv.soc.Axi4InterruptCtrl

class MagiSoC(
    debug: Boolean,
    rv64: Boolean,
    amo: Boolean,
    downsizeExternalBus: Boolean,
    bootrom: String
) extends Component {
  val dataWidth = if (rv64) 64 else 32

  val processor = RiscvProcessor(
    resetPc = 0x00010000,
    debug = debug,
    initBranchPredictionBuffers = false,
    rv64 = rv64,
    compressed = false,
    amo = amo,
    ioMemoryRegions = Seq(
      SizeMapping(BigInt("ff000000", 16), 0x800000), // Internal I/O
      SizeMapping(BigInt("e0000000", 16), 0x4000000), // PLIC
      SizeMapping(BigInt("40000000", 16), 0x10000000) // AXI I/O devices
    )
  )

  val slaveIdWidth = 16
  val numExternalInterrupts = 16
  val numInternalInterrupts = 2

  val bootromBackingStore =
    if (rv64) Mem(Bits(64 bits), 1024) else Mem(Bits(32 bits), 2048)
  bootromBackingStore.initFromFile(bootrom)
  val bootromAxi = Axi4Rom(
    mem = bootromBackingStore,
    config = Axi4Config(
      addressWidth = 32,
      dataWidth = dataWidth,
      idWidth = slaveIdWidth
    )
  )

  val ocram = Axi4SharedOnChipRam(
    dataWidth = dataWidth,
    byteCount = 16384,
    idWidth = slaveIdWidth
  )

  val uart = Axi4UartCtrl(
    idWidth = slaveIdWidth,
    config = UartCtrlMemoryMappedConfig(
      baudrate = 115200,
      txFifoDepth = 32,
      rxFifoDepth = 32,
      writeableConfig = true,
      clockDividerWidth = 20
    )
  )

  val clint =
    new Axi4Clint(hartCount = 1, idWidth = slaveIdWidth, dataWidth = dataWidth)

  val io = new Bundle {
    val bus = master(
      Axi4(
        Axi4Config(
          addressWidth = 32,
          dataWidth = if (downsizeExternalBus) 32 else dataWidth,
          idWidth = slaveIdWidth
        )
      )
    )
    val interrupts = in(Bits(numExternalInterrupts bits))
    val uart = master(Uart())
  }

  val plicInterruptLine = Bool()
  val plic = Axi4PlicGenerator(
    Axi4Config(
      addressWidth = 32,
      dataWidth = dataWidth,
      idWidth = slaveIdWidth
    )
  )
  plic.priorityWidth.load(2)
  plic.mapping.load(PlicMapping.sifive)
  plic.addTarget(plicInterruptLine)

  val intrController =
    new Axi4InterruptCtrl(
      width = numExternalInterrupts + numInternalInterrupts,
      idWidth = slaveIdWidth
    )
  intrController.io.inputs(numExternalInterrupts - 1 downto 0) := io.interrupts
  intrController.io.inputs(numExternalInterrupts + 0) := False
  intrController.io.inputs(numExternalInterrupts + 1) := uart.io.interrupt

  for ((input, id) <- intrController.io.inputs.asBools.zipWithIndex) {
    plic.addInterrupt(input, id)
  }

  val plicAxi4 = plic.ctrl.await()

  io.uart <> uart.io.uart
  processor.io.interrupt.external := intrController.io.pendings.orR | plicInterruptLine
  processor.io.interrupt.timer := clint.io.timerInterrupt(0)
  processor.io.interrupt.software := clint.io.softwareInterrupt(0)

  val uartAxi = uart.io.axi.createDownsizerOnSlaveSide(dataWidth)
  val intrControllerAxi =
    intrController.io.bus.createDownsizerOnSlaveSide(dataWidth)

  val extBus = Axi4(io.bus.config.copy(dataWidth = dataWidth))
  if (downsizeExternalBus) {
    extBus >> io.bus.createDownsizerOnSlaveSide(dataWidth)
  } else {
    extBus >> io.bus
  }

  var axiCrossbar = Axi4CrossbarFactory()
  axiCrossbar.addSlaves(
    bootromAxi -> SizeMapping(0x00010000, bootromBackingStore.byteCount),
    ocram.io.axi -> SizeMapping(0x00020000, ocram.byteCount),
    extBus -> SizeMapping(
      0x10000000,
      BigInt("c0000000", 16) - 0x10000000
    ), // Zynq DDR + PL fabric range
    uartAxi -> SizeMapping(BigInt("ff010000", 16), 0x100),
    intrControllerAxi -> SizeMapping(BigInt("ff010200", 16), 0x100),
    clint.io.bus -> SizeMapping(BigInt("ff020000", 16), 0x10000),
    plicAxi4 -> SizeMapping(BigInt("e0000000", 16), 0x4000000)
  )
  axiCrossbar.addConnections(
    processor.io.iBus -> Seq(bootromAxi, ocram.io.axi, extBus),
    processor.io.dBus -> Seq(
      bootromAxi,
      ocram.io.axi,
      extBus,
      uartAxi,
      clint.io.bus,
      intrControllerAxi,
      plicAxi4
    )
  )

  axiCrossbar.build()
}

class MagiSoC_T1()
    extends MagiSoC(
      debug = false,
      rv64 = false,
      amo = true,
      downsizeExternalBus = false,
      bootrom = "./fsbl/firmware_32.bin"
    )

object T1SpinalConfig
    extends SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC),
      defaultClockDomainFrequency = FixedFrequency(80 MHz)
    )

object GenT1 {
  def main(args: Array[String]) {
    T1SpinalConfig.generateVerilog(Machine.build {
      new MagiSoC_T1()
    })
  }
}
