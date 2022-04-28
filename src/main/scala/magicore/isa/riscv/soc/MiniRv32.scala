package magicore.isa.riscv.soc

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
import magicore.lib.mas.MicroarchSampler
import magicore.lib.mas.Axi4MicroarchSamplerCtrl
import spinal.lib.misc.InterruptCtrl

case class MiniRv32() extends Component {
  val debug = false
  val rv64 = true
  val dataWidth = if (rv64) 64 else 32

  val processor = RiscvProcessor(
    resetPc = 0x00010000,
    debug = debug,
    initBranchPredictionBuffers = false,
    rv64 = rv64
  )

  val slaveIdWidth = 16
  val numExternalInterrupts = 16
  val numInternalInterrupts = 2

  val bootromBackingStore =
    if (rv64) Mem(Bits(64 bits), 2048) else Mem(Bits(32 bits), 4096)
  bootromBackingStore.initFromFile("./fsbl/firmware.bin")
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

  val mas = new MicroarchSampler(
    sampleWidth = 32 bits,
    bufferSizeInBytes = 256 KiB,
    intrThreshold = 1000
  )
  mas.io.enable := processor.csr.csrFile.priv === RvPrivLevel.U
  mas.addSignal(
    "dispatch.ooo.fire",
    processor.pipeline.dispatch.io.oooOutput.fire
  )
  mas.addSignal(
    "dispatch.ino.fire",
    processor.pipeline.dispatch.io.inOrderOutput.fire
  )
  for ((port, i) <- processor.pipeline.oooIssue.io.issuePorts.zipWithIndex) {
    mas.addSignal(
      s"issue.ooo.port_$i.fire",
      port.fire
    )
  }
  for (
    (port, i) <- processor.pipeline.inOrderIssue.io.issuePorts.zipWithIndex
  ) {
    mas.addSignal(
      s"issue.ino.port_$i.fire",
      port.fire
    )
  }
  mas.addSignal(
    "commit.ooo.fire",
    processor.pipeline.dispatch.io.commitOoO.map(x => x.fire).orR
  )
  mas.addSignal(
    "commit.ino.fire",
    processor.pipeline.dispatch.io.commitInO.map(x => x.fire).orR
  )
  for (
    (lane, i) <- processor.pipeline.dispatch.io.writebackMonitor.zipWithIndex
  ) {
    mas.addSignal(s"retire.lane_$i.fire", lane.fire)
  }

  val exc = Machine.get[MachineException]
  mas.addSignal(
    "exception.fire",
    exc.valid
  )
  mas.addSignal(
    "exception.code",
    exc.valid ? exc.code.asBits.resize(5 bits) | 0
  )

  val masCtrl =
    new Axi4MicroarchSamplerCtrl(sampler = mas, idWidth = slaveIdWidth)

  val io = new Bundle {
    val bus = master(
      Axi4(
        Axi4Config(
          addressWidth = 32,
          dataWidth = dataWidth,
          idWidth = slaveIdWidth
        )
      )
    )
    val interrupts = in(Bits(numExternalInterrupts bits))
    val uart = master(Uart())
  }

  val intrController =
    new Axi4InterruptCtrl(
      width = numExternalInterrupts + numInternalInterrupts,
      idWidth = slaveIdWidth
    )
  intrController.io.inputs(numExternalInterrupts - 1 downto 0) := io.interrupts
  intrController.io.inputs(numExternalInterrupts + 0) := mas.io.irq
  intrController.io.inputs(numExternalInterrupts + 1) := uart.io.interrupt

  io.uart <> uart.io.uart
  processor.io.interrupt.external := intrController.io.pendings.orR
  processor.io.interrupt.timer := clint.io.timerInterrupt(0)
  processor.io.interrupt.software := clint.io.softwareInterrupt(0)

  val masCtrlAxi = masCtrl.io.bus.createDownsizerOnSlaveSide(dataWidth)
  var masDataAxi =
    mas.toAxi4ReadOnly(slaveIdWidth).createDownsizerOnSlaveSide(dataWidth)
  val uartAxi = uart.io.axi.createDownsizerOnSlaveSide(dataWidth)
  val clintAxi = clint.io.bus.createDownsizerOnSlaveSide(dataWidth)
  val intrControllerAxi =
    intrController.io.bus.createDownsizerOnSlaveSide(dataWidth)

  var axiCrossbar = Axi4CrossbarFactory()
  axiCrossbar.addSlaves(
    bootromAxi -> SizeMapping(0x00010000, bootromBackingStore.byteCount),
    ocram.io.axi -> SizeMapping(0x00020000, ocram.byteCount),
    io.bus -> SizeMapping(0x40000000, 2 GiB),
    uartAxi -> SizeMapping(BigInt("ff010000", 16), 0x100),
    masCtrlAxi -> SizeMapping(BigInt("ff010100", 16), 0x100),
    intrControllerAxi -> SizeMapping(BigInt("ff010200", 16), 0x100),
    clintAxi -> SizeMapping(BigInt("ff020000", 16), 0x10000),
    masDataAxi -> SizeMapping(BigInt("ff100000", 16), mas.bufferSizeInBytes)
  )
  axiCrossbar.addConnections(
    processor.io.iBus -> Seq(bootromAxi, ocram.io.axi, io.bus),
    processor.io.dBus -> Seq(
      bootromAxi,
      ocram.io.axi,
      io.bus,
      uartAxi,
      clintAxi,
      intrControllerAxi,
      masCtrlAxi,
      masDataAxi
    )
  )

  // Simulate memory latency
  /*
  axiCrossbar.addPipelining(ocram.io.axi)((crossbar, device) => {
    crossbar.arw
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true) >> device.arw
    crossbar.w
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true) >> device.w
    device.r >> crossbar.r
    device.b >> crossbar.b
  })
  axiCrossbar.addPipelining(io.bus)((crossbar, device) => {
    crossbar.ar
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true) >> device.ar
    device.r >> crossbar.r
  })((crossbar, device) => {
    crossbar.aw
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true) >> device.aw
    crossbar.w
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true)
      .pipelined(m2s = true, s2m = true) >> device.w
    device.b >> crossbar.b
  })
   */

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
