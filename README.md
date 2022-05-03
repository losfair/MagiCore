# MagiCore

An out-of-order processor that supports multiple instruction sets. My playground for experimenting with new microarchitecture & ISA ideas.

![Architecture](res/arch.svg)

## ISA support status

- [x] RISC-V RV32IMAU
- [x] RISC-V RV64IMAU
- [ ] eBPF
- [ ] MIPS

## Performance

Currently MagiCore's frontend (IFetch/Decode) is not superscalar so the performance is limited to <1 IPC. 2.27 CoreMark/MHz, ~106MHz on Artix 7.
