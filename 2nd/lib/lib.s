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
    load %r18,%r0, 1
    add %r3,%r3,%r18
    store %r3, %r0, 1
    addi %r3,%r18,0
    blr

joinf:
    join %r3
    blr

fetchf:
    fetch %r3, %r3, %r4
    blr

forkf:
    fork %r3, 0
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
	.globl	print_int               # -- Begin function print_int
	.p2align	2
	.type	print_int,@function
print_char:
    outll %r3
    blr
print_int:                              # @print_int
# %bb.0:                                # %entry
	cmpdi %r3,1
	beq	.LBB1_3
	jump	.LBB1_1
.LBB1_1:                                # %entry
	cmpdi %r3,0
	bne	.LBB1_4
	jump	.LBB1_2
.LBB1_2:                                # %then
	li %r3, 48
	#APP
	outll %r3
	#NO_APP
	blr
.LBB1_3:                                # %then1
	li %r3, 49
	#APP
	outll %r3
	#NO_APP
	blr
.LBB1_4:                                # %else2
	cmpdi %r3,0
	blt	.LBB1_6
	jump	.LBB1_5
.LBB1_5:                                # %then3
	li %r4, 1
	jump V68.V25.print_int_plus # tail call
.LBB1_6:                                # %else4
	li %r4, 45
	#APP
	outll %r4
	#NO_APP
	sub	%r3, %r0, %r3
	li %r4, 1
	jump V68.V25.print_int_plus # tail call
.Lfunc_end1:
	.size	print_int, .Lfunc_end1-print_int
                                        # -- End function
	.globl	V68.V25.print_int_plus  # -- Begin function V68.V25.print_int_plus
	.p2align	2
	.type	V68.V25.print_int_plus,@function
V68.V25.print_int_plus:                 # @V68.V25.print_int_plus
# %bb.0:                                # %entry
	store %sp, %sp, -4
	addi	%sp, %sp, -4
	store %lr, %sp, 3               # 4-byte Folded Spill
	li %r9, 0
	cmpdi %r3,2
	blt	.LBB2_7
	jump	.LBB2_1
.LBB2_1:                                # %else.i.i.preheader
	li %r9, 0
	addi	%r8, %r3, 0
.LBB2_2:                                # %else.i.i
                                        # =>This Inner Loop Header: Depth=1
	add	%r5, %r8, %r9
	srawi	%r5, %r5, 1
	slawi	%r6, %r5, 1
	slawi	%r7, %r5, 3
	add	%r7, %r7, %r6
	cmpd %r7,%r3
	addi	%r6, %r5, 0
	bgt	.LBB2_4
# %bb.3:                                # %else.i.i
                                        #   in Loop: Header=BB2_2 Depth=1
	addi	%r6, %r8, 0
.LBB2_4:                                # %else.i.i
                                        #   in Loop: Header=BB2_2 Depth=1
	cmpd %r7,%r3
	bgt	.LBB2_6
# %bb.5:                                # %else.i.i
                                        #   in Loop: Header=BB2_2 Depth=1
	addi	%r9, %r5, 0
.LBB2_6:                                # %else.i.i
                                        #   in Loop: Header=BB2_2 Depth=1
	addi	%r5, %r9, 1
	cmpd %r5,%r6
	addi	%r8, %r6, 0
	blt	.LBB2_2
	jump	.LBB2_7
.LBB2_7:                                # %V67.V24.div10.exit
	# dummy
	cmpdi %r4,0
	beq	.LBB2_11
	jump	.LBB2_8
.LBB2_8:                                # %else2
	store %r3, %sp, 2               # 4-byte Folded Spill
	li %r4, 1
	cmpdi %r9,0
	bne	.LBB2_10
# %bb.9:                                # %else2
	li %r4, 0
.LBB2_10:                               # %else2
	addi	%r3, %r9, 0
	store %r9, %sp, 1               # 4-byte Folded Spill
	bl V68.V25.print_int_plus
	load %r4, %sp, 1                # 4-byte Folded Reload
	slawi	%r3, %r4, 1
	slawi	%r4, %r4, 3
	add	%r3, %r4, %r3
	load %r4, %sp, 2                # 4-byte Folded Reload
	addi	%r4, %r4, 48
	sub	%r3, %r4, %r3
	#APP
	outll %r3
	#NO_APP
.LBB2_11:                               # %ifcont3
	load %lr, %sp, 3                # 4-byte Folded Reload
	load %sp, %sp, 0
	blr
