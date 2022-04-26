package magicore.util

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

// Async mem with Live Value Table.
case class LvtMem[T <: Data](wordType: HardType[T], wordCount: Int)
    extends Area {
  val readPorts = new ArrayBuffer[(UInt, T)]() // (address, output)
  val writePorts =
    new ArrayBuffer[(UInt, T, Bool)]() // (address, input, enable)
  def readAsync(address: UInt): T = {
    val out = wordType()
    out.setCompositeName(this, postfix = "read_" + readPorts.size)
    readPorts += ((address, out))
    out
  }
  def write(
      address: UInt,
      data: T,
      enable: Bool
  ): Unit = {
    writePorts += ((address, data, enable))
  }

  Component.current.afterElaboration {
    val mems = writePorts.map(_ => Mem(wordType, wordCount))
    val lvt =
      if (writePorts.size > 1)
        Vec(Reg(UInt(log2Up(writePorts.size) bits)) init (0), wordCount)
      else null

    for (((mem, wp), i) <- mems.zip(writePorts).zipWithIndex) {
      val (address, input, enable) = wp
      mem.write(address = address, data = input, enable = enable)
      if (lvt != null) {
        when(enable) {
          lvt(address) := U(i, log2Up(writePorts.size) bits)
        }
      }
    }

    for (rp <- readPorts) {
      val (address, output) = rp
      val readRsps = Vec(
        mems.map(mem =>
          mem.readAsync(address = address, readUnderWrite = writeFirst)
        )
      )
      if (lvt != null) {
        output := readRsps(lvt(address))
      } else {
        if (readRsps.size == 0) {
          output.assignDontCare()
        } else {
          assert(readRsps.size == 1)
          output := readRsps(0)
        }
      }
    }

    /*for(i <- 0 until readPorts.size) {
      println("Read port " + i + " name: " + readPorts(i)._2.getName())
    }*/
  }
}
