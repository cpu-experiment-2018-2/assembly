neg:
    li %r4,0
    sub %r3,%r4,%r3
    blr
fneg:
    fli %r4,0.0
    fsub %r3,%r4,%r3
    blr

read_int:
    inuh %r3
    inul %r3
    inlh %r3
    inll %r3
    blr

read_float:
    inuh %r3
    inul %r3
    inlh %r3
    inll %r3
    blr
truncate:
    mr %r20,%lr
    bl floor 
    bl int_of_float
    mr %lr,%r20
    blr


div2:
    srawi %r3,%r3, 1
    blr
    
fsqr:
    fmul %r3,%r3,%r3
    blr

abs:
    jump fabs

abs_float:
    jump fabs

fabs:
    li %r4,0
    cmpd %r4,%r3
    ble @@fabsplus
    fsub %r3,%r4,%r3
@@fabsplus:
    blr
    

fhalf:
    fli %r4,0.5
    fmul %r3,%r4,%r3
    blr

fisneg:
    li %r4,0
    cmpd %r4,%r3
    ble @@fisneg_pos
    li %r3,1
    jump @@fisneg_end
@@fisneg_pos:
    li %r3,0
@@fisneg_end:
    blr

fispos:
    li %r4,0
    cmpd %r3,%r4
    ble @@fispos_neg
    li %r3,1
    jump @@fispos_end
@@fispos_neg:
    li %r3,0
@@fispos_end:
    blr



fiszero:
    li %r4,0
    cmpd %r4,%r3
    beq @@fiszero_true
    fli %r4,-0.0
    cmpd %r4,%r3
    beq @@fiszero_true
    li %r3,0
    jump @@fiszero_end
@@fiszero_true:
    li %r3,1
@@fiszero_end:
    blr

    
    

not:
    li %r4,1
    cmpd %r4,%r3
    beq @@not.true
    li %r3,1
    jump @@not.end
@@not.true:
    li %r3,0
@@not.end:
    blr

eq:
    cmpd %r3,%r4
    beq @@eq.true
    li %r3,0
    jump @@eq.end
@@eq.true:
    li %r3,1
@@eq.end:
    blr
 
leq:
    cmpd %r3,%r4
    ble @@leq.true
    li %r3,0
    jump @@leq.end
@@leq.true:
    li %r3,1
@@leq.end:
    blr
print_char : (* Asm.FunCall Global *)
	outll %r3 (* Global *)
	blr

fless : (* Asm.FunCall Global *)
    jump leq

fless : (* Global Fun  ? somewhere  *)
	addi %fp,%sp,0 (*  ? somewhere  *)
	store %lr,%fp, 1 (*  ? somewhere  *)
	addi %sp,%sp,9 (*  ? somewhere  *)
	store %fp,%sp, 0 (*  ? somewhere  *)
	store %r4,%fp, 2 (*  ? somewhere  *)
	store %r3,%fp, 3 (*  ? somewhere  *)
	bl fisneg (* at line 2 *)
	addi %r28,%r3,0 (*  ? somewhere  *)
	li %r27,0 (* at line 2 *)
	store %r28,%fp, 4 (*  ? somewhere  *)
	store %r27,%fp, 5 (*  ? somewhere  *)
	cmpd %r28,%r27(* at line 2 *)
	beq @@label80.if.true (* at line 2 *)
@@label80.if.false : (* Local Label at line 2 *)
	load %r3, %fp ,2 (*  ? somewhere  *)
	bl fisneg (* at line 2 *)
	addi %r26,%r3,0 (*  ? somewhere  *)
	li %r28,0 (* at line 2 *)
	store %r28,%fp, 6 (*  ? somewhere  *)
	store %r26,%fp, 7 (*  ? somewhere  *)
	cmpd %r26,%r28(* at line 2 *)
	beq @@label84.if.true (* at line 2 *)
@@label84.if.false : (* Local Label at line 2 *)
	load %r3, %fp ,2 (*  ? somewhere  *)
	bl fneg (* at line 2 *)
	addi %r27,%r3,0 (*  ? somewhere  *)
	store %r27,%fp, 8 (*  ? somewhere  *)
	load %r3, %fp ,3 (*  ? somewhere  *)
	bl fneg (* at line 2 *)
	addi %r4,%r3,0 (*  ? somewhere  *)
	load %r3, %fp ,8 (*  ? somewhere  *)
	bl leq (* at line 2 *)
	addi %r3,%r3,0 (*  ? somewhere  *)
	jump @@label85 (* at line 2 *)
@@label84.if.true : (* Local Label at line 2 *)
	li %r3,1 (* at line 2 *)
@@label85 : (* Local Label at line 2 *)
	jump @@label81 (* at line 2 *)
@@label80.if.true : (* Local Label at line 2 *)
	load %r3, %fp ,2 (*  ? somewhere  *)
	bl fisneg (* at line 2 *)
	addi %r26,%r3,0 (*  ? somewhere  *)
	li %r28,0 (* at line 2 *)
	store %r28,%fp, 6 (*  ? somewhere  *)
	store %r26,%fp, 7 (*  ? somewhere  *)
	cmpd %r26,%r28(* at line 2 *)
	beq @@label82.if.true (* at line 2 *)
@@label82.if.false : (* Local Label at line 2 *)
	li %r3,0 (* at line 2 *)
	jump @@label83 (* at line 2 *)
@@label82.if.true : (* Local Label at line 2 *)
	load %r4, %fp ,2 (*  ? somewhere  *)
	load %r3, %fp ,3 (*  ? somewhere  *)
	bl leq (* at line 2 *)
	addi %r3,%r3,0 (*  ? somewhere  *)
@@label83 : (* Local Label at line 2 *)
@@label81 : (* Local Label at line 2 *)
	load %lr, %fp ,1 (*  ? somewhere  *)
	addi %sp,%fp,0 (*  ? somewhere  *)
	load %fp, %fp ,0 (*  ? somewhere  *)
	blr

Obj.magic:
    blr
print_newline:
    li %r3, 10
    outll %r3
    blr

alloc:
    li %r30,0
    load %r29, %r30, 0
    add %r28, %r29, %r3
    store %r28, %r30, 0
    mr %r3, %r29
    blr

create_array:
	addi %fp,%sp,0 (* Global *)
	store %lr,%fp, 1 (* Global *)
	addi %sp,%sp, 2 (* Global *)
    mr %r5,%r3
    bl alloc 
    li %r6, 0
    mr %r7,%r3
    jump @@init_loop 
@@init_loop:
    cmpd %r5,%r6
    beq @@array_create_end
    store %r4,%r7,0
    subi %r5,%r5,1
    addi %r7,%r7,1
    jump @@init_loop
@@array_create_end:
    load %lr, %fp ,1 (* Global *)
	addi %sp,%fp,0 (* Global *)
	load %fp, %fp ,0 (* Global *)
    blr

