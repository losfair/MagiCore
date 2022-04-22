package miniooo.testutil

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.amba4.axi.Axi4Shared
import spinal.lib.bus.amba4.axi.Axi4WriteOnly

object TestExt {

  implicit class StreamExt[T <: Data](stream: Stream[T]) {
    def simWrite(
        dut: Component,
        generate: T => Unit,
        giveUp: => Boolean = false
    ): Boolean = {
      if (giveUp) {
        dut.clockDomain.waitSampling()
        return false
      }
      stream.valid #= true
      generate(stream.payload)
      dut.clockDomain.waitSamplingWhere(stream.ready.toBoolean || giveUp)
      stream.valid #= false
      stream.ready.toBoolean
    }

    def simContinuousRead(
        dut: Component,
        latency: Int,
        generate: T => Unit
    ): Unit = {
      stream.ready #= latency == 0

      fork {
        while (true) {
          dut.clockDomain.waitSampling()

          if (stream.valid.toBoolean) {
            if (latency != 0) {
              dut.clockDomain.waitSampling(latency - 1)
              stream.ready #= true
            }
            generate(stream.payload)
            if (latency != 0) {
              dut.clockDomain.waitSampling(1)
              stream.ready #= false
            }
          }
        }
      }
    }
  }

  implicit class Axi4WriteOnlyExt(that: Axi4WriteOnly) {
    def simWriteBytes(dut: Component, addr: Long, data: Seq[Byte]) {
      assert(data.length > 0 && data.length <= 256)

      that.aw.valid #= true
      that.aw.payload.addr #= addr
      that.aw.payload.size #= 0x0
      that.aw.payload.len #= data.length - 1
      that.aw.payload.burst #= 1

      // Deadlock when used with the crossbar if we wait for `aw.ready` synchronously here
      val awWaiter = fork {
        dut.clockDomain.waitSamplingWhere(that.aw.ready.toBoolean)
        that.aw.valid #= false
      }

      for ((b_, i) <- data.zipWithIndex) {
        val b = b_.toShort & 0xff
        val shiftBytes =
          (addr.toInt + i) % (that.config.dataWidth / 8)
        val strb = 1 << shiftBytes
        val shiftedData = BigInt(b) << (shiftBytes * 8)
        that.w.valid #= true
        that.w.payload.data #= shiftedData
        that.w.payload.strb #= strb
        that.w.payload.last #= i + 1 == data.length
        dut.clockDomain.waitSamplingWhere(that.w.ready.toBoolean)
      }
      that.w.valid #= false
      dut.clockDomain.waitSamplingWhere(that.b.valid.toBoolean)
      that.b.ready #= true
      dut.clockDomain.waitSampling()
      that.b.ready #= false

      while (!awWaiter.done) {
        dut.clockDomain.waitSampling()
      }
    }
  }
}
