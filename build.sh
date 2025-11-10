#!/usr/bin/env bash
set -euo pipefail

# --- CONFIG -------------------------------------------------
ARCH=rv64im        # instruction set
ABI=lp64           # 64-bit ABI
BASE=0x80000000    # where your code will live in memory
INPUT=${1:-boot.S} # default source file if none given
OUT=${INPUT%.*}    # strip .S or .s
# ------------------------------------------------------------

echo "[1/3] Assembling ${INPUT}..."
riscv64-unknown-elf-as -march=${ARCH} -mabi=${ABI} -o ${OUT}.o ${INPUT}

echo "[2/3] Linking..."
riscv64-unknown-elf-ld -Ttext=${BASE} -o ${OUT}.elf ${OUT}.o

echo "[3/3] Generating raw binary..."
riscv64-unknown-elf-objcopy -O binary ${OUT}.elf ${OUT}.bin

echo "✅ Done!"
echo "ELF: ${OUT}.elf"
echo "BIN: ${OUT}.bin"

riscv64-unknown-elf-objdump -D -b binary -m riscv:rv64 -M no-aliases --adjust-vma=0x80000000  boot.bin
