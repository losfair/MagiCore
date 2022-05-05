#!/bin/bash

export VERILATOR_ROOT=${HOME}/Projects/verilator
export PATH="${VERILATOR_ROOT}/bin:${PATH}"
verilator --version

export COCOTB_LOG_LEVEL=ERROR

exec make
