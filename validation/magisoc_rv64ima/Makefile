# Makefile

# defaults
SIM ?= verilator
EXTRA_ARGS += -Wno-WIDTH -Wno-CMPCONST -Wno-PINMISSING # --trace --trace-structs
#SIM ?= icarus
TOPLEVEL_LANG ?= verilog

VERILOG_SOURCES += $(PWD)/Top.v
VERILOG_SOURCES += $(PWD)/MagiSoC_RV64IMA.v
# use VHDL_SOURCES for VHDL files

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
TOPLEVEL = Top

# MODULE is the basename of the Python test file
MODULE = magisoc_rv64ima

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim
