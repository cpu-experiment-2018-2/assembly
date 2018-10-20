invsqrt:
    fli %r4,0.5
    fmul %r5, %r3, %r4
    srawi %r6,%r3, 1
    li %r7,1597463007
    sub %r7,%r7,%r6

    fmul %r11,%r7,%r7
    fmul %r12,%r11,%r5
    fli %r10,1.5
    fsub %r13,%r10,%r12
    fmul %r7,%r13,%r7

    fmul %r11,%r7,%r7
    fmul %r12,%r11,%r5
    fsub %r13,%r10,%r12
    fmul %r7,%r13,%r7

    fmul %r11,%r7,%r7
    fmul %r12,%r11,%r5
    fsub %r13,%r10,%r12
    fmul %r7,%r13,%r7

    fmul %r11,%r7,%r7
    fmul %r12,%r11,%r5
    fsub %r13,%r10,%r12
    fmul %r3,%r13,%r7
    (*とりあえず三回 *)

    blr
sqrt:
    mr %r30,%lr
    bl invsqrt
    fli %r4,1.0
    fdiv %r3,%r4,%r3
    mr %lr,%r30
    blr


