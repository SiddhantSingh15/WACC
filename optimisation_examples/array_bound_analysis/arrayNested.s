/*
WACC Program:
	# basic array declaration and assignment

	# Output:
	# 3
	# 3

	# Program:

	begin
	int[] a = [1,2,3];
	int[] b = [3,4];
	int[][] c = [a,b] ;
	println c[0][2] ;
	println c[1][0]
	end

Optimisation:
	Program does not check for out of bounds array access 
	since both instances of array access are within bounds
	of the arrays a, b and c.
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
		SUB sp, sp, #12
		LDR r0, =16
		BL malloc
		MOV r4, r0
		LDR r5, =1
		STR r5, [r4, #4]
		LDR r5, =2
		STR r5, [r4, #8]
		LDR r5, =3
		STR r5, [r4, #12]
		LDR r5, =3
		STR r5, [r4]
		STR r4, [sp, #8]
		LDR r0, =12
		BL malloc
		MOV r4, r0
		LDR r5, =3
		STR r5, [r4, #4]
		LDR r5, =4
		STR r5, [r4, #8]
		LDR r5, =2
		STR r5, [r4]
		STR r4, [sp, #4]
		LDR r0, =12
		BL malloc
		MOV r4, r0
		LDR r5, [sp, #8]
		STR r5, [r4, #4]
		LDR r5, [sp, #4]
		STR r5, [r4, #8]
		LDR r5, =2
		STR r5, [r4]
		STR r4, [sp]
		LDR r4, =3
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		LDR r4, =3
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		ADD sp, sp, #12
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
