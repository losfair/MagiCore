package magicore.util

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

object Axi4Rom {
  def apply(mem: Mem[_ <: Data], config: Axi4Config): Axi4ReadOnly = {
    val wordWidth = mem.wordType().getBitsWidth
    assert(wordWidth % 8 == 0 && isPow2(wordWidth))
    val byteIndexSize = log2Up(wordWidth / 8)

    val axi = Axi4ReadOnly(config)

    val (reqToStage, reqToRead) = StreamFork2(axi.ar)

    val output =
      mem.streamReadSync(
        reqToRead.translateWith(
          reqToRead.payload.addr(
            (byteIndexSize + log2Up(mem.wordCount) - 1) downto byteIndexSize
          )
        )
      )
    val reqStaged = reqToStage.stage()

    val to = Axi4R(config)
    if (config.useId) to.id := reqStaged.id
    if (config.useLast) to.last := True
    to.data := output.payload.asBits
    to.setOKAY()
    if (config.useRUser) to.user := reqStaged.user

    axi.r <-/< StreamJoin(output, reqStaged).translateWith(to)
    axi
  }
}
