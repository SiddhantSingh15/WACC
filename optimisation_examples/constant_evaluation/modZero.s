/*
WACC Program:
	# division by zero

	# Output:
	# #runtime_error#

	# Exit:
	# 255

	# Program:

	begin
	int x = 10 % 0 ;
	println "should not reach here"
	end

Optimisation:
	Optimisation:
	Program recognizes that there is a division by zero error
	at compile-time and throws that error instead of
	evaluating the expression (10 % 0).
*/
	.data

	msg_string:
		.word 5
		.ascii "%.*s\0"
	msg_division_by_zero:
		.word 49
		.ascii "DivisionByZeroError: division or modulo by zero\n\0"
	msg_0:
		.word 21
		.ascii "should not reach here"
	msg_new_line:
		.word 1
		.ascii "\0"

	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #4
		LDR r4, =10
		LDR r5, =0
		MOV r0, r4
		MOV r1, r5
		BL p_check_division_by_zero
		BL __aeabi_idivmod
		MOV r4, r1
		STR r4, [sp]
		LDR r4, =msg_0
		MOV r0, r4
		BL p_print_string
		BL p_print_ln
		ADD sp, sp, #4
		LDR r0, =0
		POP {pc}
		.ltorg
	p_throw_runtime_error:
		BL p_print_string
		MOV r0, #-1
		BL exit
	p_print_string:
		PUSH {lr}
		LDR r1, [r0]
		ADD r2, r0, #4
		LDR r0, =msg_string
		ADD r0, r0, #4
		BL printf
		MOV r0, #0
		BL fflush
		POP {pc}
	p_check_division_by_zero:
		PUSH {lr}
		CMP r1, #0
		LDREQ r0, =msg_division_by_zero
		BLEQ p_throw_runtime_error
		POP {pc}
	p_print_ln:
		PUSH {lr}
		LDR r0, =msg_new_line
		ADD r0, r0, #4
		BL puts
		MOV r0, #0
		BL fflush
		POP {pc}
