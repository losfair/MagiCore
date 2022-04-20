package miniooo.control

import spinal.core._
import spinal.lib._

final case class EpochManager() extends Area {
  private val spec = Machine.get[MachineSpec]
  val epochTable = Vec(Reg(spec.epochCounterType()) init (0), spec.numEpochs)
  val currentEpoch = Reg(spec.epochType()) init (0)

  val decReq = Flow(spec.epochType())
  decReq.setIdle()

  val incReq_ooo = Flow(spec.epochType())
  incReq_ooo.setIdle()

  val incReq_ino = Flow(spec.epochType())
  incReq_ino.setIdle()

  def matches(f: Flow[UInt], i: Int): Bool = {
    f.valid && f.payload === i
  }

  for (i <- 0 until spec.numEpochs) {
    val d = matches(decReq, i)
    val i1 = matches(incReq_ooo, i)
    val i2 = matches(incReq_ino, i)
    val v = epochTable(i)
    when(!d && i1 && i2) {
      assert(
        v =/= v.maxValue && v =/= v.maxValue - 1,
        "epoch counter overflow: +2"
      )
      v := v + 2
    }
    when((d && i1 && i2) || (!d && (i1 ^ i2))) {
      assert(v =/= v.maxValue, "epoch counter overflow: +1")
      v := v + 1
    }
    when(d && !i1 && !i2) {
      assert(v =/= 0, "epoch counter underflow: -1")
      v := v - 1
    }
  }
}
