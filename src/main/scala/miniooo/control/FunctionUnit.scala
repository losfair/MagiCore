package miniooo.control

import spinal.core._
import spinal.lib._
import miniooo.util.PolymorphicDataChain

trait FunctionUnit {
  def lowLatency: Boolean = false
  def staticTag: Data
  def generate(hardType: HardType[_ <: PolymorphicDataChain]): FunctionUnitInstance
}

trait FunctionUnitInstance extends Area {
  def io_available: Bool
  def io_input: Stream[_ <: PolymorphicDataChain]
  def io_output: Stream[CommitRequest]
}
