    .section .text
    .globl _start
_start:
    li   a1, -5
    add  a0, a0, a1
    lw a0, 0xA0000000(a0)
    j   _start                # infinite loop
