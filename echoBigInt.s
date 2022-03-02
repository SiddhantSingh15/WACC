	.data

	msg_0:
		.word 24
		.ascii "enter an integer to echo"
	msg_string:
		.word 6
		.ascii "%.*s\0"
	msg_new_line:
		.word 2
		.ascii "\0"
	msg_1:
		.word 4
		.ascii "%d\0"
	msg_int:
		.word 4
		.ascii "%d\0"

	.text

	.global main
	p_print_ln:
		PUSH {lr}
		LDR R0, =msg_new_line
		ADD R0, R0, #4
		BL puts
		MOV R0, #0
		BL fflush
		POP {pc}
	p_read_int:
		PUSH {lr}
		MOV R1, R0
		LDR R0, =msg_1
		ADD R0, R0, #4
		BL scanf
		POP {pc}
	main:
		PUSH {lr}
		LDR R4, =1
		STR R4, [sp, #-4]
		LDR R4, =msg_0
		MOV R0, R4
		BL p_print_string
		BL p_print_ln
		ADD R4, sp, #-4
		MOV R0, R4
		BL p_read_int
		LDR R4, [sp, #-4]
		MOV R0, R4
		BL p_print_int
		BL p_print_ln
		LDR R0, =0
		POP {pc}
		.ltorg
