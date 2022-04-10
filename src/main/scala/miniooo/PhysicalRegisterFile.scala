package miniooo

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import MiniOoOExt._

case class PrfItem(ctx: MachineContext) extends Bundle {
  val data = Bits(ctx.cfg.dataWidth)
}

object PrfBusyTableSnapshot {
  def zeroed(ctx: MachineContext): PrfBusyTableSnapshot = {
    val v = PrfBusyTableSnapshot(ctx)
    v.table.foreach(_ := False)
    v
  }
}

case class PrfBusyTableSnapshot(ctx: MachineContext) extends Bundle {
  val table = Vec(Bool(), ctx.cfg.numPhysicalRegs)

  def findFreeReg(): (Bool, UInt) = {
    val (busy, index) = this.table.zipWithIndex
      .map(x => (x._1, U(x._2, ctx.cfg.archRegIndexWidth)))
      .reduceBalancedTree((a, b) => {
        val busy = Bool()
        val index = ctx.cfg.physRegIndexType
        when(!a._1) {
          busy := False
          index := a._2
        } elsewhen (!b._1) {
          busy := False
          index := b._2
        } otherwise {
          busy := True
          index.assignDontCare()
        }
        (busy, index)
      })
    (!busy, index)
  }

  def markAsBusy(index: UInt): PrfBusyTableSnapshot = {
    assert(!this.table(index), Seq("Register already busy: ", index))
    val snapshot = PrfBusyTableSnapshot(ctx)
    snapshot.table := Vec(this.table.zipWithIndex.map(arg => {
      val (x, i) = arg
      val v = Bool()
      when(index === i) {
        v := True
      } otherwise {
        v := x
      }
      v
    }))
    snapshot
  }
}

case class PhysicalRegisterFile(ctx: MachineContext) extends Area {
  assert(
    ctx.cfg.maxNumDstRegsPerInsn == 1,
    "Only one destination register supported"
  )
  val mem = Mem(PrfItem(ctx), ctx.cfg.numPhysicalRegs)
  val busyTable =
    Reg(PrfBusyTableSnapshot(ctx)) init (PrfBusyTableSnapshot.zeroed(ctx))

  Component.current.afterElaboration {
    val portsInArbitrationGroup =
      ctx.regfileReadPorts
        .groupBy(_._1)
        .map(x => (x._1, x._2.map(y => (y._2, y._3))))
        .toMap
    val arbGroups = (0 until ctx.cfg.maxNumSrcRegsPerInsn).map(i =>
      new StreamArbiterFactory().lowerFirst
        .build(PrfReadReq(ctx), portsInArbitrationGroup(i).size)
    )
    for ((arbGroup, arbGroupIndex) <- arbGroups.zipWithIndex) {
      when(arbGroup.io.output.valid) {
        // Issuing a busy register is invalid
        assert(!busyTable.table(arbGroup.io.output.payload.index))
      }
      arbGroup.io.output.freeRun()
      val portList = portsInArbitrationGroup(arbGroupIndex)
      if (portList.size == 1 && portList(0)._1) {
        // unique
        val port = portList(0)._2
        val rsp = mem.readAsync(
          address = port.reqStream.payload.index,
          readUnderWrite = writeFirst
        )
        port.rspWire.data := rsp.data
      } else {
        val rsp = mem.readAsync(
          address = arbGroup.io.output.payload.index,
          readUnderWrite = writeFirst
        )
        for (((unique, port), portIndex) <- portList.zipWithIndex) {
          assert(!unique)
          arbGroup.io.inputs(portIndex) << port.reqStream
            .check(payloadInvariance = true)
          port.rspWire.data := rsp.data
        }
      }
    }

    val writeArbGroup = new StreamArbiterFactory().lowerFirst
      .build(PrfWriteReq(ctx), ctx.regfileWritePorts.size)
    for (
      ((arbGroupIndex, _, port), portIndex) <-
        ctx.regfileWritePorts.zipWithIndex
    ) {
      assert(arbGroupIndex == 0)
      writeArbGroup.io.inputs(portIndex) << port.reqStream
        .check(payloadInvariance = true)
    }
    writeArbGroup.io.output.freeRun()
    when(writeArbGroup.io.output.valid) {
      assert(
        busyTable.table(writeArbGroup.io.output.payload.index),
        Seq(
          "Register to commit was not busy: ",
          writeArbGroup.io.output.payload.index
        )
      )
      busyTable.table(writeArbGroup.io.output.payload.index) := False
    }
    val writeItem = PrfItem(ctx)
    writeItem.data := writeArbGroup.io.output.payload.data
    mem.write(
      address = writeArbGroup.io.output.payload.index,
      data = writeItem,
      enable = writeArbGroup.io.output.valid
    )
  }
}
