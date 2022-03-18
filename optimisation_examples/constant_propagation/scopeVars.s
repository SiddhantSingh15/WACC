/*
WACC Program:
	# simple variable scoping test

	# Output:
	# 2
	# 4
	# 2

	# Program:

	begin
	int x = 2 ;
	println x ;
	begin
		int x = 4 ;
		println x
	end ;
	println x 
	end

Optimisation:
	Program identifies that a new declaration is being made for x 
	and does not reassign the previous value of x. Program then 
	correctly replaces the variables with their constant values.
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
		LDR r4, =2
		STR r4, [sp]
		LDR r4, =2
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		SUB sp, sp, #4
		LDR r4, =4
		STR r4, [sp]
		LDR r4, =4
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		ADD sp, sp, #4
		LDR r4, =2
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
