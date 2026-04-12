# Context-Aware Optimization Synthesis for Production Compilers

By Ashley Sheng ('26)

## Description

Prototype of [Cranelift's](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift) mid-end optimizer.

Scoped to integer-types, and introduces context-aware (path-sensitive) mechanisms to introduce a strictly larger class of optimizations.

Additions include:
* Scoped range assumptions
* Conditional unions
* Context-aware extraction/elaboration

## How To Run Benchmarks
```cargo test --release time_benchmarks -- --nocapture```
This command runs the compilation time benchmarks.

```cargo test --release run_benchmarks -- --nocapture```
This command runs the test suite and their post-optimization instruction costs, comparing context-aware (path-sensitive) implementation with the baseline.
