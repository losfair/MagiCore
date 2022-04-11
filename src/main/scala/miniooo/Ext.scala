package miniooo

import spinal.core._
import spinal.lib._

object MiniOoOExt {
  implicit class StreamExt[T <: Data](stream: Stream[T]) {
    def check(
        payloadInvariance: Boolean = false,
        payloadChangeFormatter: (T, T) => Seq[Any] = null
    ): Stream[T] = {
      val rValid = RegInit(False) setWhen (stream.valid) clearWhen (stream.fire)
      val rData = RegNextWhen(stream.payload, stream.valid && !rValid)

      val stack = ScalaLocated.long.replace("\n", "\\n")
      assert(
        !(!stream.valid && rValid),
        "Stream transaction disappeared:\\n" + stack
      )
      if (payloadInvariance) {
        val baseMsg = "Stream transaction payload changed:\\n" + stack
        val msg: Seq[Any] =
          if (payloadChangeFormatter == null) Seq(baseMsg)
          else Seq(baseMsg) ++ payloadChangeFormatter(rData, stream.payload)
        assert(
          !rValid || rData === stream.payload,
          msg
        )
      }

      stream
    }
  }

  def initWithValidFalse[T <: Bundle](fresh: T): T = {
    fresh.assignDontCare()
    fresh.assignSomeByName(new Bundle {
      val valid = False
    })
    fresh
  }

  implicit class VecExt[T <: Data](vec: Vec[T]) {
    def rotateLeft(n: UInt): Vec[T] = {
      assert(isPow2(vec.size))
      val out = Vec(vec.dataType, vec.size)
      for (i <- 0 until vec.size) {
        out.write(i, vec(n.resize(log2Up(vec.size)) + i))
      }
      out
    }

    def rotateRight(n: UInt): Vec[T] = {
      assert(isPow2(vec.size))
      val out = Vec(vec.dataType, vec.size)
      for (i <- 0 until vec.size) {
        out.write(
          i,
          vec(
            n.resize(log2Up(vec.size))
              .twoComplement(True)
              .asUInt + (i + vec.size)
          )
        )
      }
      out
    }
  }
}
