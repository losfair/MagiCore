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


async def monitor_uart(dut):
    duration = 1000000000 // 921600
    rx = dut.uart_txd

    # https://github.com/wallento/cocotbext-uart/blob/master/cocotbext/uart/base.py
    while True:
        data = 0

        # Wait for start of character
        await FallingEdge(rx)

        # Sample on the center of the start bit
        await Timer(duration/2, "ns")

        # Malformed start bit
        if rx != 0:
            raise Exception("UART start bit error")

        # Sample all bits
        for b in range(8):
            await Timer(duration, "ns")
            if rx == 1:
                data = data | (1 << b)

        # Stopbit(s)
        for b in range(1):
            await Timer(duration, "ns")
            if rx != 1:
                raise Exception("UART stop bit error")
        sys.stdout.write(chr(data))
        sys.stdout.flush()

@cocotb.test()
async def first_test(dut):
    addressSpace = AddressSpace(2 ** 32)

    dataMem = MemoryRegion(16777216)
    addressSpace.register_region(dataMem, 0x20000000)

    axiSlavePort = AxiSlave(AxiBus.from_prefix(
        dut, "dBus"), dut.clk, dut.rst, target=addressSpace)

    print("begin it")
    dut.rst.value = 1
    dut.uart_rxd.value = 1  # prevent break
    cocotb.start_soon(Clock(dut.clk, 20, units="ns").start())
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    dut.rst.value = 0
    print("reset ok")

    # await cocotb.start(monitor_clk(dut))
    await cocotb.start(monitor_uart(dut))

    while True:
        await RisingEdge(dut.clk)
