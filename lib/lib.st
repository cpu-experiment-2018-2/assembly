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
kernel_atan:
    fmul %r4,%r3,%r3
    li %r5, 0.060035485
    li %r6, 0.08976446

    
