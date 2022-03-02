	.data

	msg_int:
		.word 3
		.ascii "%d\0"
	msg_new_line:
		.word 1
		.ascii "\0"

	.text

	.global main
	p_print_int:
		PUSH {lr}
		MOV R1, R0
		LDR R0, =msg_int
		ADD R0, R0, #4
		BL printf
		MOV R0, #0
		BL fflush
		POP {pc}
	p_print_ln:
		PUSH {lr}
		LDR R0, =msg_new_line
		ADD R0, R0, #4
		BL puts
		MOV R0, #0
		BL fflush
		POP {pc}
	main:
		PUSH {lr}
		LDR R4, =2
		STR R4, [sp, #-4]
		LDR R4, [sp, #-4]
		MOV R0, R4
		BL p_print_int
		BL p_print_ln
		LDR R4, =4
		STR R4, [sp, #-8]
		LDR R4, [sp, #-8]
		MOV R0, R4
		BL p_print_int
		BL p_print_ln
		LDR R4, [sp, #-4]
		MOV R0, R4
		BL p_print_int
		BL p_print_ln
		LDR R0, =0
		POP {pc}
		.ltorg
