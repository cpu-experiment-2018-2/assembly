print_int : (* Asm.FunCall Global *)
	outuh %r3 (* Global *)
	outul %r3 (* Global *)
	outlh %r3 (* Global *)
	outll %r3 (* Global *)
	blr
print_float : (* Asm.FunCall Global *)
	outuh %r3 (* Global *)
	outul %r3 (* Global *)
	outlh %r3 (* Global *)
	outll %r3 (* Global *)
	blr
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
 
fsqr:
    jump sqrt

fhalf:
    fli %r4,0.5
    fmul %r3,%r4,%r3

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
    li %r3,0
    jump @@fisneg_end
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
    addi %r3,%r3, 1
	cmpd %r3,%r4(* Global *)
	ble @@fless.if.true (* Global *)
@@fless.if.false : (* Asm.Other Global *)
	li %r5,0 (* Global *)
	jump @@flesslast (* Global *)
@@fless.if.true : (* Asm.Other Global *)
	li %r5,1 (* Global *)
@@flesslast : (* Asm.Other Global *)
	addi %r3,%r5,0 (* Global *)
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

