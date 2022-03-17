/*
WACC Program:
	# Program:

	begin
	println 5 > 3;
	println 5 >= 3;
	println 2 < 3;
	println 2 <= 3;

	println 'z' >= 'a';
	println 'z' > 'a';
	println 'a' <= 'z';
	println 'a' < 'z'
	
	end

Optimisation:
	Program evaluates comparison operators for
	integers and characters at compile-time.
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
		MOV r0, #1
		BL p_print_bool
		BL p_print_ln
		MOV r0, #1
		BL p_print_bool
		BL p_print_ln
		MOV r0, #1
		BL p_print_bool
		BL p_print_ln
		MOV r0, #1
		BL p_print_bool
		BL p_print_ln
		MOV r0, #1
		BL p_print_bool
		BL p_print_ln
		MOV r0, #1
		BL p_print_bool
		BL p_print_ln
		MOV r0, #1
		BL p_print_bool
		BL p_print_ln
		MOV r0, #1
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
