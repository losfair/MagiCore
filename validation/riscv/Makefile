# Makefile

# defaults
SIM ?= verilator
EXTRA_ARGS += -Wno-WIDTH -Wno-CMPCONST -Wno-PINMISSING
#SIM ?= icarus
TOPLEVEL_LANG ?= verilog

VERILOG_SOURCES += $(PWD)/RvTop.v
VERILOG_SOURCES += $(PWD)/../../RiscvProcessor.v
# use VHDL_SOURCES for VHDL files

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
TOPLEVEL = RvTop

# MODULE is the basename of the Python test file
MODULE = riscv_test

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim
