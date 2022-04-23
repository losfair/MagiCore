import cocotb
from cocotb.triggers import Timer
from cocotb.triggers import RisingEdge, FallingEdge
from cocotb.clock import Clock
from cocotbext.axi import AddressSpace, MemoryRegion
from cocotbext.axi import AxiBus, AxiReadBus, AxiLiteMaster, AxiSlave, AxiSlaveRead, AxiAWBus, AxiARBus, AxiRBus

async def monitor_clk(dut):
    while True:
        await FallingEdge(dut.clk)
        print("*** clock ***")

@cocotb.test()
async def first_test(dut):
    """Try accessing the design."""
    addressSpace = AddressSpace(2 ** 32)

    insnMem = MemoryRegion(1048576)
    addressSpace.register_region(insnMem, 0x08000000)

    dataMem = MemoryRegion(16777216)
    addressSpace.register_region(dataMem, 0x20000000)

    axiSlavePort_iBus = AxiSlaveRead(AxiReadBus.from_prefix(dut, "iBus"), dut.clk, dut.rst, target=addressSpace)
    axiSlavePort_dBus = AxiSlave(AxiBus.from_prefix(dut, "dBus"), dut.clk, dut.rst, target=addressSpace)

    dut.rst.value = 1
    cocotb.start_soon(Clock(dut.clk, 1, units="ns").start())
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    dut.rst.value = 0
    dut._log.info("reset ok")

    await cocotb.start(monitor_clk(dut))

    for i in range(0, 200):
        await RisingEdge(dut.clk)
