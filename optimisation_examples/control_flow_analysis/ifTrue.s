/*
WACC Program:
	# simple true if statement

	# Output:
	# here

	# Program:

	begin
	if true
	then
		println "here"
	else
		println "not here"
	fi
	end

Optimisation:
	Program evaluates the condition expression at compile-time to be
	true and only executes the 'then' branch of the statement.
*/
	.data

	msg_0:
		.word 4
		.ascii "here"
	msg_string:
		.word 5
		.ascii "%.*s\0"
	msg_new_line:
		.word 1
		.ascii "\0"

	.text

	.global main
	main:
		PUSH {lr}
		LDR r4, =msg_0
		MOV r0, r4
		BL p_print_string
		BL p_print_ln
		LDR r0, =0
		POP {pc}
		.ltorg
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
	p_print_ln:
		PUSH {lr}
		LDR r0, =msg_new_line
		ADD r0, r0, #4
		BL puts
		MOV r0, #0
		BL fflush
		POP {pc}
