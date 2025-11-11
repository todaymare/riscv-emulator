    .section .text
    .globl _start

_start:
    # Argument: a0 = 30
    li      a0, 40

    # Call fib(30)
    call    fib

    # a0 now holds the result — exit via ecall (a7=93 for emulator exit)
    li      a7, 93          # syscall: exit
    ecall


fib:
        addi    sp,sp,-32
        sd      ra,24(sp)
        sd      s0,16(sp)
        mv      s0,a0
        li      a5,1
        bgt     a0,a5,.L4
.L2:
        mv      a0,s0
        ld      ra,24(sp)
        ld      s0,16(sp)
        addi    sp,sp,32
        jr      ra
.L4:
        sd      s1,8(sp)
        addiw   a0,a0,-1
        call    fib
        mv      s1,a0
        addiw   a0,s0,-2
        call    fib
        addw    s0,s1,a0
        ld      s1,8(sp)
        j       .L2


