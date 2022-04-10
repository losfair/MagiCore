
package miniooo

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import org.scalatest.funsuite.AnyFunSuite

class SimTrivial extends AnyFunSuite {
  test("SimTrivial") {
    SimConfig.doSim(new MiniOoO()) { dut =>
    }
  }
}
