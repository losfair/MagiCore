package miniooo.util

import spinal.core._
import spinal.lib._
import scala.collection.IterableLike

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
      val out = (0 until vec.size).map(i => vec(n.resize(log2Up(vec.size)) + i))
      Vec(out)
    }

    def rotateRight(n: UInt): Vec[T] = {
      assert(isPow2(vec.size))
      val out = (0 until vec.size).map(i =>
        vec(
          (n.resize(log2Up(vec.size))
            .twoComplement(True)
            .asUInt + (i + vec.size)).resize(log2Up(vec.size))
        )
      )
      Vec(out)
    }

    def countForVerification(predicate: T => Bool): UInt = {
      var out = U(0, 32 bits)
      for (item <- vec) {
        out = out + predicate(item).asUInt.resized
      }
      out
    }

    def firstWhere(predicate: T => Bool): (Bool, T) = {
      val sat = vec.map(predicate)
      Vec(sat)
        .zip(vec)
        .reduceBalancedTree((l, r) => {
          val ok = False
          val out = vec.dataType()
          out.assignDontCare()
          when(l._1) {
            ok := l._1
            out := l._2
          } otherwise {
            ok := r._1
            out := r._2
          }
          (ok, out)
        })
    }
  }

  implicit class SeqExt[T](seq: Seq[T]) {
    def firstWhere[H <: Data](
        hardType: HardType[H],
        predicate: T => Bool,
        generate: T => H
    ): (Bool, H) = {
      val sat = seq.map(predicate)
      sat
        .zip(seq.map(generate))
        .reduceBalancedTree((l, r) => {
          val ok = False
          val out = hardType()
          out.assignDontCare()
          when(l._1) {
            ok := l._1
            out := l._2
          } otherwise {
            ok := r._1
            out := r._2
          }
          (ok, out)
        })
    }
  }
}
