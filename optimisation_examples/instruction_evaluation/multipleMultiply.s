/*
Optimisation:
	Removes redundant msg operations. It also uses basic algeraic laws and arithmatic
	to simplify or reorder instructions like, 
	it removes different instructions of loading 5 and 1 and then calculating multiplication
	to one instruction of loading 5 to r4.
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
		LDR r4, =5
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		LDR r4, =0
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		LDR r4, =48
		MOV r0, r4
		BL p_print_int
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
