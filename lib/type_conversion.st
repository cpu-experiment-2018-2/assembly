floor:
(* マイナスはまだバグってるので注意 *)
    mr %r30,%lr
    mr %r29,%r3


    bl int_of_float 
    bl float_of_int 

    cmpd %r3,%r29
    ble @@ok
    fli %r4,1.0
    fsub %r3,%r3,%r4
    cmpd %r3,%r29
    ble @@ok
    fsub %r3,%r3,%r4
@@ok:
    mr %lr,%r30
    blr


int_of_float:
    li %r5,0
    li %r6,0
    cmpd %r5,%r3
    ble @@int_of_float_plus
    li %r6,1
    fsub %r3, %r5, %r3
@@int_of_float_plus:
    li %r4, 1258291200
    cmpd %r4,%r3
    ble @@f2i_big
    fadd %r3,%r3,%r4
    sub %r3,%r3,%r4
    jump @@f2i_end
@@f2i_big:
    (* unreachbleだと信じたい
       そのうちね
    *)
@@f2i_end:
    cmpd %r5, %r6
    beq @@f2i_end2
    sub %r3,%r5,%r3
@@f2i_end2:
    blr


float_of_int:
    li %r4, 8388607
    li %r9,0
    cmpd %r9,%r3
    fli %r11, 1.0  (* 1.0の表現 *)
    ble @@positive(*正の数*)
    addi %r5, %r3, 0
    sub %r3,%r3,%r5
    sub %r3,%r3,%r5
    fli %r11, -1.0  (* -1.0の表現 *)
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

