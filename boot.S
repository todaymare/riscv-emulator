    .section .text
    .globl _start
_start:
    li   a0, 42
    li   a1, 99
    add  a2, a0, a1
    j   _start                # infinite loop
