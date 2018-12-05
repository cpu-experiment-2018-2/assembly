floor : (* Global Fun  ? somewhere  *)
	addi %fp,%sp,0 (*  ? somewhere  *)
	store %lr,%fp, 1 (*  ? somewhere  *)
	addi %sp,%sp,8 (*  ? somewhere  *)
	store %fp,%sp, 0 (*  ? somewhere  *)
	store %r3,%fp, 2 (*  ? somewhere  *)
	bl ftoi (* at line 3 *)
	addi %r3,%r3,0 (*  ? somewhere  *)
	bl itof (* at line 3 *)
	addi %r28,%r3,0 (*  ? somewhere  *)
	store %r28,%fp, 3 (*  ? somewhere  *)
	li %r27,1065353216 (* at line 4 *)
	fadd %r4, %r28,%r27 (* at line 4 *)
	load %r3, %fp ,2 (*  ? somewhere  *)
	bl fless (* at line 4 *)
	addi %r27,%r3,0 (*  ? somewhere  *)
	li %r28,0 (* at line 4 *)
	store %r28,%fp, 4 (*  ? somewhere  *)
	store %r27,%fp, 5 (*  ? somewhere  *)
	cmpd %r27,%r28(* at line 4 *)
	beq @@label38.if.true (* at line 4 *)
@@label38.if.false : (* Local Label at line 4 *)
	load %r4, %fp ,3 (*  ? somewhere  *)
	load %r3, %fp ,2 (*  ? somewhere  *)
	bl fless (* at line 5 *)
	addi %r26,%r3,0 (*  ? somewhere  *)
	li %r28,0 (* at line 5 *)
	store %r28,%fp, 6 (*  ? somewhere  *)
	store %r26,%fp, 7 (*  ? somewhere  *)
	cmpd %r26,%r28(* at line 5 *)
	beq @@label40.if.true (* at line 5 *)
@@label40.if.false : (* Local Label at line 5 *)
	li %r27,1065353216 (* at line 6 *)
	load %r28, %fp ,3 (*  ? somewhere  *)
	fsub %r3, %r28,%r27 (* at line 6 *)
	jump @@label41 (* at line 5 *)
@@label40.if.true : (* Local Label at line 5 *)
	load %r27, %fp ,3 (*  ? somewhere  *)
	addi %r3,%r27,0 (* at line 7 *)
@@label41 : (* Local Label at line 5 *)
	jump @@label39 (* at line 4 *)
@@label38.if.true : (* Local Label at line 4 *)
	li %r26,1065353216 (* at line 8 *)
	load %r28, %fp ,3 (*  ? somewhere  *)
	fadd %r3, %r28,%r26 (* at line 8 *)
@@label39 : (* Local Label at line 4 *)
	load %lr, %fp ,1 (*  ? somewhere  *)
	addi %sp,%fp,0 (*  ? somewhere  *)
	load %fp, %fp ,0 (*  ? somewhere  *)
	blr
ftoi:
    li %r5,0
    li %r6,0
    subi %r3, %r3, 1
    cmpd %r5,%r3
    ble @@ftoiplus
    li %r6,1
    fsub %r3, %r5, %r3
@@ftoiplus:
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
    jump itof

int_of_float : (* Global Fun  ? somewhere  *)
	addi %fp,%sp,0 (*  ? somewhere  *)
	store %lr,%fp, 1 (*  ? somewhere  *)
	addi %sp,%sp,5 (*  ? somewhere  *)
	store %fp,%sp, 0 (*  ? somewhere  *)
	store %r3,%fp, 2 (*  ? somewhere  *)
	bl fisneg (* at line 2 *)
	addi %r28,%r3,0 (*  ? somewhere  *)
	li %r27,0 (* at line 2 *)
	store %r28,%fp, 3 (*  ? somewhere  *)
	store %r27,%fp, 4 (*  ? somewhere  *)
	cmpd %r28,%r27(* at line 2 *)
	beq @@label23.if.true (* at line 2 *)
@@label23.if.false : (* Local Label at line 2 *)
	li %r26,1056964608 (* at line 2 *)
	load %r28, %fp ,2 (*  ? somewhere  *)
	fadd %r3, %r28,%r26 (* at line 2 *)
	bl ftoi (* at line 2 *)
	addi %r3,%r3,0 (*  ? somewhere  *)
	jump @@label24 (* at line 2 *)
@@label23.if.true : (* Local Label at line 2 *)
	li %r26,1056964608 (* at line 2 *)
	load %r28, %fp ,2 (*  ? somewhere  *)
	fsub %r3, %r28,%r26 (* at line 2 *)
	bl ftoi (* at line 2 *)
	addi %r3,%r3,0 (*  ? somewhere  *)
@@label24 : (* Local Label at line 2 *)
	load %lr, %fp ,1 (*  ? somewhere  *)
	addi %sp,%fp,0 (*  ? somewhere  *)
	load %fp, %fp ,0 (*  ? somewhere  *)
	blr


itof:
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

