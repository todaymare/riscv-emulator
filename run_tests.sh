#!/usr/bin/env bash


for f in riscv-tests/isa/*-p-*.bin riscv-tests/isa/*-pt-*.bin; do
    ./target/debug/riscv-emulator "$f"
done
