    .section .text
    .globl _start
_start:
    li   a1, -5
    add  a0, a0, a1
    j   _start                # infinite loop
