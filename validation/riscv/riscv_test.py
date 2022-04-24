import sys
import cocotb
from cocotb.triggers import Timer
from cocotb.triggers import RisingEdge, FallingEdge
from cocotb.clock import Clock
from cocotbext.axi import AddressSpace, MemoryRegion, Region
from cocotbext.axi import AxiBus, AxiReadBus, AxiLiteMaster, AxiSlave, AxiSlaveRead, AxiSlaveWrite, AxiAWBus, AxiARBus, AxiRBus
import struct

async def monitor_clk(dut):
    while True:
        await FallingEdge(dut.clk)
        print("*** clock ***")


class AxiDebugControllerRegion(Region):
    def __init__(self, size=4096, **kwargs):
        super().__init__(size, **kwargs)
        self.stop = False

    async def _read(self, address, length, **kwargs):
        print("AXI DEBUG read: address=0x{:08x}, length={}".format(address, length))
        return 0

    async def _write(self, address, data, **kwargs):
        assert len(data) == 4
        value = struct.unpack("<I", data)[0]
        if address == 0:
            sys.stdout.write(chr(value))
            sys.stdout.flush()
        elif address == 4:
            self.stop = True
        elif address == 8:
            pass
        else:
            raise Exception("AXI DEBUG write error: address=0x{:08x}, value=0x{:08x}".format(address, value))

@cocotb.test()
async def first_test(dut):
    addressSpace = AddressSpace(2 ** 32)

    insnMem = MemoryRegion(1048576)
    addressSpace.register_region(insnMem, 0x08000000)

    with open("./snippets/hello_world.bin", "rb") as f:
        await insnMem.write(0, f.read())

    dataMem = MemoryRegion(16777216)
    addressSpace.register_region(dataMem, 0x20000000)

    debugCtrl = AxiDebugControllerRegion()
    addressSpace.register_region(debugCtrl, 0xfe000000)

    axiSlavePort_iBus = AxiSlaveRead(AxiReadBus.from_prefix(dut, "iBus"), dut.clk, dut.rst, target=addressSpace)
    axiSlavePort_dBus = AxiSlave(AxiBus.from_prefix(dut, "dBus"), dut.clk, dut.rst, target=addressSpace)

    print("begin it")
    dut.rst.value = 1
    cocotb.start_soon(Clock(dut.clk, 10, units="ns").start())
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    dut.rst.value = 0
    print("reset ok")

    #await cocotb.start(monitor_clk(dut))

    while True:
        await RisingEdge(dut.clk)
        if debugCtrl.stop:
            break
