package miniooo.testutil

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

object TestExt {
  
  implicit class StreamExt[T <: Data](stream: Stream[T]) {
    def simWrite(dut: Component, generate: T => Unit, giveUp: => Boolean = false): Boolean = {
      if(giveUp) {
        dut.clockDomain.waitSampling()
        return false
      }
      stream.valid #= true
      generate(stream.payload)
      dut.clockDomain.waitSamplingWhere(stream.ready.toBoolean || giveUp)
      stream.valid #= false
      stream.ready.toBoolean
    }

    def simContinuousRead(dut: Component, latency: Int, generate: T => Unit): Unit = {
      stream.ready #= latency == 0

      fork {
        while(true) {
          dut.clockDomain.waitSampling()

          if(stream.valid.toBoolean) {
            if(latency != 0) {
              dut.clockDomain.waitSampling(latency - 1)
              stream.ready #= true
            }
            generate(stream.payload)
            if(latency != 0) {
              dut.clockDomain.waitSampling(1)
              stream.ready #= false
            }
          }
        }
      }
    }
  }
}