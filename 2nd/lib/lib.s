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


print_newline:
    li %r3, 10
    outll %r3
    blr
alloc:
    load %r28,%r0, 1
    add %r3,%r3,%r28
    store %r3, %r0, 1
    addi %r3,%r28,0
    blr

create_array_sub:
	store %sp ,%sp,-2 
	addi %sp ,%sp, -2 
	store %lr ,%sp, 1
    mr %r5,%r3
    bl  alloc
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
	load %lr, %sp ,1 
	load %sp, %sp ,0 
    blr

