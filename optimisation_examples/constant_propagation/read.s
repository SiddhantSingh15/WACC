/*
WACC Program:
	# simple read statement

	# Output:
	# input an integer to continue...
	# #input#

	# Program:

	begin
		int x = 10;
	println "input an integer to continue..." ;
		read x
	end

Optimisation:
	Program does not replace x with any variables since
	it is no longer 10 after read and can be any integer.
*/

	.data

	msg_0:
		.word 31
		.ascii "input an integer to continue..."
	msg_string:
		.word 5
		.ascii "%.*s\0"
	msg_new_line:
		.word 1
		.ascii "\0"
	msg_1:
		.word 3
		.ascii "%d\0"

	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #4
		LDR r4, =10
		STR r4, [sp]
		LDR r4, =msg_0
		MOV r0, r4
		BL p_print_string
		BL p_print_ln
		ADD r4, sp, #0
		MOV r0, r4
		BL p_read_int
		ADD sp, sp, #4
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
	p_read_int:
		PUSH {lr}
		MOV r1, r0
		LDR r0, =msg_1
		ADD r0, r0, #4
		BL scanf
		POP {pc}
