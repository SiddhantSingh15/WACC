/*
WACC Program:
	# integer underflow

	# Output:
	# -2147483647
	# -2147483648
	# #runtime_error#

	# Exit:
	# 255

	# Program:

	begin
	int x = -2147483647 ;
	println x ;
	x = x - 1 ;
	println x ;
	x = x - 1 ; #err here?
	println x
	end

Optimisation:
	Program checks for integer overflow errors at runtime 
	when constant evaluation determines that x <= -2147483649.
*/
	.data

	msg_int:
		.word 3
		.ascii "%d\0"
	msg_new_line:
		.word 1
		.ascii "\0"
	msg_string:
		.word 5
		.ascii "%.*s\0"
	msg_overflow:
		.word 83
		.ascii "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n\0"

	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #4
		LDR r4, =-2147483647
		STR r4, [sp]
		LDR r4, [sp]
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		LDR r4, [sp]
		LDR r5, =1
		SUBS r4, r4, r5
		BLVS p_throw_overflow_error
		STR r4, [sp]
		LDR r4, [sp]
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		LDR r4, [sp]
		LDR r5, =1
		SUBS r4, r4, r5
		BLVS p_throw_overflow_error
		STR r4, [sp]
		LDR r4, [sp]
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
	p_throw_overflow_error:
		LDR r0, =msg_overflow
		BL p_throw_runtime_error