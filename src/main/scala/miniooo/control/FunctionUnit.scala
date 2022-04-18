package miniooo.control

import spinal.core._
import spinal.lib._
import miniooo.util.PolymorphicDataChain

trait FunctionUnit {
  def staticTag: Data
  def warnOnBlockedIssue: Boolean = false
  def isAlu: Boolean = false // The ALU receives special treatment for low-latency issueing.
  def generate(hardType: HardType[_ <: PolymorphicDataChain]): FunctionUnitInstance
}

trait FunctionUnitInstance extends Area {
  def io_available: Bool
  def io_input: Stream[_ <: PolymorphicDataChain]
  def io_output: Stream[CommitRequest]
}
