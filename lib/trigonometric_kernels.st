reduction:
    fli %r4,6.28318530718
    li %r5, 0
    li %r6, 1258291200
@@reduction_loop:
    mr %r7,%r5
    add %r7,%r7,%r6 
    fsub %r7,%r7,%r6
    fmul %r7, %r7,%r4 
    subi %r7,%r7,1 
    cmpd %r3,%r7
    ble @@reduction_end
    inc %r5
    jump  @@reduction_loop
@@reduction_end:
    dec %r5 
    add %r5,%r5,%r6 
    fsub %r5,%r5,%r6
    fmul %r5,%r5,%r4
    fsub %r3,%r3,%r5
    blr
kernel_sin:
    fmul %r4,%r3,%r3
    
    fli %r5, 0.008332824
    fli %r6, 0.00019587841 
    fmul %r6,%r6,%r4
    fsub %r5,%r5,%r6
    fli %r6, 0.16666668
    fmul %r5,%r5,%r4
    fsub %r6,%r6,%r5
    fli %r5, 1.0
    fmul %r6,%r6,%r4
    fsub %r5,%r5,%r6
    fmul %r3,%r3,%r5
    blr
kernel_cos:
    fmul %r4,%r3,%r3
    fli %r6, 0.0013695068
    fli %r5, 0.04166368 
    fmul %r6,%r6,%r4
    fsub %r5,%r5,%r6
    fli %r6, 0.5
    fmul %r5,%r5,%r4
    fsub %r6,%r6,%r5
    fli %r5, 1.0
    fmul %r6,%r6,%r4
    fsub %r5,%r5,%r6
    mr %r3,%r5
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

