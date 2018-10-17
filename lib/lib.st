    jump main
print_int : (* Asm.FunCall Global *)
	outuh %r3 (* Global *)
	outul %r3 (* Global *)
	outlh %r3 (* Global *)
	outll %r3 (* Global *)
	blr
print_char : (* Asm.FunCall Global *)
	outll %r3 (* Global *)
	blr
fless : (* Asm.FunCall Global *)
	cmpd %r3,%r4(* Global *)
	ble fless.if.true (* Global *)
fless.if.false : (* Asm.Other Global *)
	li %r5,0 (* Global *)
	jump flesslast (* Global *)
fless.if.true : (* Asm.Other Global *)
	li %r5,1 (* Global *)
flesslast : (* Asm.Other Global *)
	addi %r3,%r5,0 (* Global *)
	blr
