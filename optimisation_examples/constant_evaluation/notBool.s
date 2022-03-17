/*
WACC Program:
	# evaluating not

	# Output:
	# false
	# true

	# Program:

	begin
	println !true ;
	println !false
	end

Optimisation:
	Program evaluates not operators at compile-time.
*/
	.data

	msg_true:
		.word 5
		.ascii "true\0"
	msg_false:
		.word 6
		.ascii "false\0"
	msg_new_line:
		.word 1
		.ascii "\0"

	.text

	.global main
	main:
		PUSH {lr}
		MOV r4, #0
		MOV r0, r4
		BL p_print_bool
		BL p_print_ln
		MOV r4, #1
		MOV r0, r4
		BL p_print_bool
		BL p_print_ln
		LDR r0, =0
		POP {pc}
		.ltorg
	p_print_bool:
		PUSH {lr}
		CMP r0, #0
		LDRNE r0, =msg_true
		LDREQ r0, =msg_false
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
