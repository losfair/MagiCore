package magicore.control

import spinal.core._
import spinal.lib._

final case class EpochManager() extends Area {
  private val spec = Machine.get[MachineSpec]
  val epochTable = Vec(Reg(spec.epochCounterType()) init (0), spec.numEpochs)
  val currentEpoch = Reg(spec.epochType()) init (0)

  val decReq_ooo = Flow(spec.epochType())
  decReq_ooo.setIdle()

  val decReq_ino = Flow(spec.epochType())
  decReq_ino.setIdle()

  val incReq_ooo = Flow(spec.epochType())
  incReq_ooo.setIdle()

  val incReq_ino = Flow(spec.epochType())
  incReq_ino.setIdle()

  def matches(f: Flow[UInt], i: Int): Bool = {
    f.valid && f.payload === i
  }

  for (i <- 0 until spec.numEpochs) {
    val d1 = matches(decReq_ooo, i)
    val d2 = matches(decReq_ino, i)
    val i1 = matches(incReq_ooo, i)
    val i2 = matches(incReq_ino, i)
    val v = epochTable(i)

    val vec = i1 ## i2 ## d1 ## d2
    val delta = SInt(v.getWidth bits)
    switch(vec) {
      is(B"1100") {
        assert(v <= v.maxValue - 2, "epoch counter overflow: 2")
        delta := 2
      }
      is(B"1000", B"0100", B"1101", B"1110") {
        assert(v <= v.maxValue - 1, "epoch counter overflow: 1")
        delta := 1
      }
      is(B"0000", B"0101", B"0110", B"1001", B"1010", B"1111") {
        delta := 0
      }
      is(B"0001", B"0010", B"1011", B"0111") {
        assert(v >= 1, "epoch counter underflow: -1")
        delta := -1
      }
      is(B"0011") {
        assert(v >= 2, "epoch counter underflow: -2")
        delta := -2
      }
    }
    v := v + delta.asUInt
  }
}
