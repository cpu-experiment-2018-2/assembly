reduction:
    fli %r4,6.28318530718
    li %r5, 0
@@reduction_loop:
    itof %r7, %r5
    fmul %r7, %r7,%r4 
    cmpf %r3,%r7
    ble @@reduction_end
    inc %r5
    jump  @@reduction_loop
@@reduction_end:
    dec %r5 
    itof %r5,%r5
    fmul %r5,%r5,%r4
    fsub %r3,%r3,%r5
    blr

kernel_atan:
    fmul %r4,%r3,%r3
    fli %r5, 0.060035485
    fli %r6, 0.08976446

    fmul %r5, %r5, %r4
    fsub %r5, %r6, %r5

    fli %r6, 0.111111104
    fmul %r5, %r5, %r4
    fsub %r5, %r6, %r5

    fli %r6, 0.142857142
    fmul %r5, %r5, %r4
    fsub %r5, %r6, %r5

    fli %r6, 0.2
    fmul %r5, %r5, %r4
    fsub %r5, %r6, %r5

    fli %r6,0.333333
    fmul %r5, %r5, %r4
    fsub %r5, %r6, %r5

    fli %r6,1.0
    fmul %r5, %r5, %r4
    fsub %r5, %r6, %r5

    fmul %r3, %r5, %r3
    blr

