package miniooo

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

case class MachineContext(
    cfg: MachineConfig,
    sem: MachineSemantics
) {
  val prf = PhysicalRegisterFile(this)
  val regfileReadPorts = new ArrayBuffer[(Int, Boolean, PrfReadPort)]()
  val regfileWritePorts = new ArrayBuffer[(Int, Boolean, PrfWritePort)]()

  val rename = Rename(this, sem.newDecodedInsn)
  
  def usePrfReadPort(arbitrationGroup: Int, unique: Boolean = true): PrfReadPort = {
    val port = new PrfReadPort(this)
    regfileReadPorts += ((arbitrationGroup, unique, port))
    port
  }

  def usePrfWritePort(arbitrationGroup: Int, unique: Boolean = true): PrfWritePort = {
    val port = new PrfWritePort(this)
    regfileWritePorts += ((arbitrationGroup, unique, port))
    port
  }
}

case class PrfReadReq(ctx: MachineContext) extends Bundle {
  val index = ctx.cfg.physRegIndexType
}

case class PrfReadRsp(ctx: MachineContext) extends Bundle {
  val data = Bits(ctx.cfg.dataWidth)
}

case class PrfWriteReq(ctx: MachineContext) extends Bundle {
  val index = ctx.cfg.physRegIndexType
  val data = Bits(ctx.cfg.dataWidth)
}

class PrfReadPort(ctx: MachineContext) {
  val reqStream = Stream(PrfReadReq(ctx))
  val rspWire = PrfReadRsp(ctx) // valid when `reqStream.valid && reqStream.ready`
}

class PrfWritePort(ctx: MachineContext) {
  val reqStream = Stream(PrfWriteReq(ctx))
}
