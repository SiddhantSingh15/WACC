/*
WACC Program:
	# evalutaing ord and chr

	# Output:
	# 97
	# c

	# Program:

	begin
	println ord 'a' ;
	println chr 99
	end

Optimisation:
	Program evaluates ord and chr operators at compile-time.
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
		LDR r4, =97
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		MOV r4, #'c'
		MOV r0, r4
		BL putchar
		BL p_print_ln
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