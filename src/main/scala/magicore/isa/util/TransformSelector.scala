package magicore.isa.util

import spinal.core._
import spinal.lib._
import magicore.util._
import MagiCoreExt._
import magicore.control._
import magicore.frontend._
import magicore.lib.funit._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping

object TransformSelector {
  def create(
      sel: UInt,
      transforms: Seq[(Int, PostFetchTransform)]
  ): PostFetchTransform = {
    assert(transforms.size > 0)
    assert(sel.maxValue >= transforms.size - 1)

    val input = Stream(FetchPacket())
    val defaultOutput = Stream(FetchPacket())
    defaultOutput.setIdle()

    transforms.foreach(x => x._2.input.setIdle())

    switch(sel) {
      for ((k, trans) <- transforms) {
        is(U(k, sel.getWidth bits)) {
          input >> trans.input
        }
      }
      default {
        input >> defaultOutput
      }
    }

    val output =
      StreamArbiterFactory.lowerFirst.on(
        transforms.map(x => x._2.output) ++ Seq(defaultOutput)
      )

    PostFetchTransform(input = input, output = output, latency = 1)
  }
}
