#!/bin/bash

set -e
cd "$(dirname $0)"

cargo xbuild --release --target ./riscv32im-unknown-none-elf.json
llvm-objcopy \
  ./target/riscv32im-unknown-none-elf/release/fsbl \
  --binary-architecture=riscv32 --strip-all -O binary \
  firmware.bin
llvm-objdump --mattr=+m -D ./target/riscv32im-unknown-none-elf/release/fsbl > firmware.dump
