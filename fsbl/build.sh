#!/bin/bash

set -e
cd "$(dirname $0)"

MODE="64"
if [ "$1" == "32" ]; then
  MODE="32"
fi

echo "Building for $MODE-bit mode."

cargo xbuild --release --target ./riscv${MODE}ima-unknown-none-elf.json
llvm-objcopy \
  ./target/riscv${MODE}ima-unknown-none-elf/release/fsbl \
  --binary-architecture=riscv${MODE} --strip-all -O binary \
  firmware_${MODE}.bin
llvm-objdump --mattr=+m,+a -D ./target/riscv${MODE}ima-unknown-none-elf/release/fsbl > firmware_${MODE}.dump
