package miniooo.util

import spinal.core._
import spinal.lib._
import MiniOoOExt._

case class MultiLaneFifo[T <: Data](
    dataType: HardType[T],
    depth: Int,
    numLanes: Int
) extends Component {
  assert(numLanes > 1)

  val io = new Bundle {
    val push = slave(Stream(Vec(Flow(dataType()), numLanes)))
    val pop = master(Stream(dataType()))
  }
  val backing =
    new StreamFifoLowLatency(
      dataType = Vec(Flow(dataType()), numLanes),
      depth = depth,
      latency = 1
    )
  val currentLane = Reg(UInt(log2Up(numLanes) bits)) init (0)

  assert(
    !io.push.valid || io.push.payload.map(x => x.valid).orR,
    "at least one lane should be valid"
  )
  io.push >> backing.io.push

  var popped = False
  val hasRemaining = False
  val poppedLane = UInt(log2Up(numLanes) bits)
  poppedLane.assignDontCare()

  for ((x, i) <- backing.io.pop.payload.zipWithIndex) {
    val thisValid = currentLane <= i && x.valid
    val canPop = thisValid && !popped
    when(canPop) {
      poppedLane := i
    }

    // There's something more in this row
    when(thisValid && popped) {
      hasRemaining := True
    }

    popped = canPop.mux(
      False -> popped,
      True -> True
    )
  }

  backing.io.pop.ready := False
  when(io.pop.fire) {
    currentLane := hasRemaining.mux(
      False -> U(0, currentLane.getWidth bits),
      True -> (poppedLane + 1)
    )
    when(!hasRemaining) {
      backing.io.pop.ready := True
    }
    assert(
      !hasRemaining || poppedLane =/= numLanes - 1,
      "hasRemaining but poppedLane is the last lane - invalid!"
    )
  }

  io.pop.valid := backing.io.pop.valid && popped
  io.pop.payload := backing.io.pop.payload(poppedLane).payload

  assert(
    !io.pop.valid || backing.io.pop.payload(poppedLane).valid,
    "invalid MultiLaneFifo pop"
  )
}
