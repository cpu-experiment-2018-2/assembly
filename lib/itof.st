float_of_int:
    li %r4, 8388607
    li %r9,0
    cmpd %r9,%r3
    li %r11, 1065353216  (* 1.0の表現 *)
    ble @@positive(*正の数*)
    addi %r5, %r3, 0
    sub %r3,%r3,%r5
    sub %r3,%r3,%r5
    li %r11,-1082130432  (* -1.0の表現 *)
@@positive:
    cmpd %r3,%r4
    ble @@small
    srawi %r6,%r3,23
    li %r5,1258291200
    li %r7,8388608
    li %r8,0
@@larger:
    fadd %r8, %r8, %r5
    subi %r6, %r6, 1
    sub  %r3, %r3, %r7
    cmpd %r9, %r6
    beq @@last2
    jump @@larger
@@small:
    li %r5,1258291200
    add %r3,%r3,%r5 
    fsub %r3,%r3,%r5
    jump @@last
@@last2:
    add %r3,%r3,%r5 
    fsub %r3,%r3,%r5
    fadd %r3,%r3,%r8
@@last:
    fmul %r3, %r3, %r11
    blr
