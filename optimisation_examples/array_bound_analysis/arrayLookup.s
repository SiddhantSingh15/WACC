/*
WACC Program: 
	# check first element of array

	# Output:
	# 43

	# Program:

	begin
	int[] a = [43, 2, 18, 1] ;
	println a[0]
	end

Optimisation:
	Program does not check for out of bounds array access 
	since 0 is lower than the size of array a.
*/

	.data

	msg_int:
		.word 3
		.ascii "%d\0"
	msg_new_line:
		.word 1
		.ascii "\0"

	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #4
		LDR r0, =20
		BL malloc
		MOV r4, r0
		LDR r5, =43
		STR r5, [r4, #4]
		LDR r5, =2
		STR r5, [r4, #8]
		LDR r5, =18
		STR r5, [r4, #12]
		LDR r5, =1
		STR r5, [r4, #16]
		LDR r5, =4
		STR r5, [r4]
		STR r4, [sp]
		LDR r4, =43
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		ADD sp, sp, #4
		LDR r0, =0
		POP {pc}
		.ltorg
	p_print_int:
		PUSH {lr}
		MOV r1, r0
		LDR r0, =msg_int
		ADD r0, r0, #4
		BL printf
		MOV r0, #0
		BL fflush
		POP {pc}
	p_print_ln:
		PUSH {lr}
		LDR r0, =msg_new_line
		ADD r0, r0, #4
		BL puts
		MOV r0, #0
		BL fflush
		POP {pc}
