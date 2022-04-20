package miniooo.frontend

import spinal.core._
import spinal.lib._
import miniooo.util._
import miniooo.control._
import vexriscv.ip.InstructionCache
import vexriscv.ip.MemoryTranslatorBusParameter
import vexriscv.ip.InstructionCacheConfig

case class FetchUnit() extends Area {
  private val mspec = Machine.get[MachineSpec]
  private val fspec = Machine.get[FrontendSpec]
  val icache = new InstructionCache(InstructionCacheConfig(
    cacheSize = fspec.icacheSize,
    bytePerLine = 32,
    addressWidth = mspec.addrWidth.value,
    cpuDataWidth = mspec.dataWidth.value,
    memDataWidth = fspec.icacheMemPortDataWidth,
    catchIllegalAccess = false,
    catchAccessFault = false,
    asyncTagMemory = true,
    wayCount = 2
  ), MemoryTranslatorBusParameter())
}

/*
                                   bytePerLine : Int,
                                   wayCount : Int,
                                   addressWidth : Int,
                                   cpuDataWidth : Int,
                                   memDataWidth : Int,
                                   catchIllegalAccess : Boolean,
                                   catchAccessFault : Boolean,
                                   asyncTagMemory : Boolean,
*/