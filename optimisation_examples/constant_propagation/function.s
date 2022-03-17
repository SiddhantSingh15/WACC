/*
WACC Program:
	# a simple function definition and call

	# Output:
	# 0

	# Program:

	begin
	int f() is
		return 0 
	end
	int x = 100;
	x = call f() ;
	println x 
	end

Optimisation:
	Progam does not replace variable x with a value after the 
	function call since the value is no longer constant.
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
	f_f:
		PUSH {lr}
		LDR r4, =0
		MOV r0, r4
		POP {pc}
		POP {pc}
		.ltorg
	main:
		PUSH {lr}
		SUB sp, sp, #4
		LDR r4, =100
		STR r4, [sp]
		BL f_f
		MOV r4, r0
		STR r4, [sp]
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
