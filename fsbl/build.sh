#!/bin/bash

set -e
cd "$(dirname $0)"

MODE="64"
if [ "$1" == "32" ]; then
  MODE="32"
fi

echo "Building for $MODE-bit mode."

cargo xbuild --release --target ./riscv${MODE}im-unknown-none-elf.json
llvm-objcopy \
  ./target/riscv${MODE}im-unknown-none-elf/release/fsbl \
  --binary-architecture=riscv${MODE} --strip-all -O binary \
  firmware.bin
llvm-objdump --mattr=+m -D ./target/riscv${MODE}im-unknown-none-elf/release/fsbl > firmware.dump
