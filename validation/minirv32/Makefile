# Makefile

# defaults
SIM ?= verilator
EXTRA_ARGS += -Wno-WIDTH -Wno-CMPCONST -Wno-PINMISSING -DSYNTHESIS=1 # --trace --trace-structs
#SIM ?= icarus
TOPLEVEL_LANG ?= verilog

VERILOG_SOURCES += $(PWD)/Top.v
VERILOG_SOURCES += $(PWD)/MiniRv32.v
# use VHDL_SOURCES for VHDL files

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
TOPLEVEL = Top

# MODULE is the basename of the Python test file
MODULE = minirv32

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim
