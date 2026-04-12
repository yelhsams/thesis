# Context-Aware Optimization Synthesis for Production Compilers

By Ashley Sheng ('26)

## Description

Prototype of a integer-scoped version of [Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift).

Additions include:
* range analysis
* conditional unions
* context-aware extraction/elaboration

## How To Run Benchmarks
```cargo test --release time_benchmarks -- --nocapture```
This command runs the compilation time benchmarks.

```cargo test --release run_benchmarks -- --nocapture```
This command runs the test suite and their post-optimization instruction costs, comparing context-aware (path-sensitive) implementation with the baseline.
