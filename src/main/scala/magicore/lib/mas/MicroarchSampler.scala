package magicore.lib.mas

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.bram.BRAM
import magicore.util.Axi4Rom

case class MicroarchSampler(
    sampleWidth: BitCount,
    bufferSizeInBytes: BigInt,
    intrThreshold: Int
) extends Area {
  case class SampledSignal(signal: Data, range: Range.Inclusive)

  val io = new Bundle {
    val irq = Bool()
    val enable = Bool()
  }

  assert(isPow2(sampleWidth.value))
  assert(sampleWidth.value % 8 == 0)
  val sampleWidthInBytes = sampleWidth.value / 8

  assert(bufferSizeInBytes > 0)
  assert(bufferSizeInBytes % sampleWidthInBytes == 0)

  val numSamples = bufferSizeInBytes / sampleWidthInBytes
  assert(
    numSamples >= intrThreshold * 2,
    "Buffer size must be at least twice the interrupt threshold"
  )

  val sampleType = HardType(Bits(sampleWidth))

  val buffer =
    new Mem(sampleType(), numSamples.toInt)
  val bufferPtr = Reg(UInt(log2Up(numSamples) bits)) init (0)
  val sample = Bits(sampleWidth)
  sample := 0

  var totalWidth = 0
  val signals = new mutable.LinkedHashMap[String, SampledSignal]()

  val regSample = RegNext(RegNext(sample, B(0, sampleWidth)), B(0, sampleWidth))
  val softEnable = Reg(Bool()) init (false)
  val isEnabled = softEnable && io.enable

  io.irq := isEnabled && bufferPtr >= numSamples - intrThreshold

  when(isEnabled) {
    buffer.write(bufferPtr, regSample)
    when(bufferPtr =/= bufferPtr.maxValue) {
      bufferPtr := bufferPtr + 1
    }
  }

  def addSignal(name: String, signal: Data): Unit = {
    if (signals.contains(name)) {
      throw new Exception(s"Signal $name already exists")
    }

    val width = signal.getBitsWidth
    assert(width > 0)

    val range = (totalWidth + width - 1) downto totalWidth
    println(
      s"Adding signal '$name' to microarch sampler; width: $width, range: [${range.start}:${range.end}]"
    )
    signals.update(name, SampledSignal(signal, range))
    totalWidth += width
    assert(
      totalWidth <= sampleWidth.value,
      "Total signal width exceeds sample width."
    )

    sample(range) := signal.asBits
  }

  def toAxi4ReadOnly(idWidth: Int): Axi4ReadOnly = {
    val port = Axi4Rom(
      buffer,
      Axi4Config(
        addressWidth = 32,
        dataWidth = sampleWidth.value,
        idWidth = idWidth
      )
    )
    val out = Axi4ReadOnly(port.config)
    out.ar.pipelined(m2s = true, s2m = true) >> port.ar
    port.r.pipelined(m2s = true, s2m = true) >> out.r

    out
  }
}

case class Axi4MicroarchSamplerCtrl(
    sampler: MicroarchSampler,
    idWidth: Int,
    baseAddress: BigInt = 0
) extends Area {
  val io = new Bundle {
    val bus =
      Axi4(Axi4Config(addressWidth = 5, dataWidth = 32, idWidth = idWidth))
  }

  val factory = new Axi4SlaveFactory(io.bus)

  factory.read(sampler.bufferPtr, baseAddress + 0)
  factory.write(sampler.bufferPtr, baseAddress + 0)

  factory.read(sampler.softEnable, baseAddress + 4)
  factory.write(sampler.softEnable, baseAddress + 4)
}
