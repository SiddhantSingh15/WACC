/* 
WACC Program:
	# evaluating and

	# Output:
	# false
	# true
	# false

	# Program:

	begin
	bool a = true ;
	bool b = false ;
	println a && b ;
	println a && true ;
	println b && false
	end

Optimisation:
	Program automatically evaluaes a && true to a (line 52)
	and b && false to false (line 56).
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
		SUB sp, sp, #2
		MOV r4, #1
		STRB r4, [sp, #1]
		MOV r4, #0
		STRB r4, [sp]
		LDRSB r4, [sp, #1]
		LDRSB r5, [sp]
		AND r4, r4, r5
		MOV r0, r4
		BL p_print_bool
		BL p_print_ln
		LDRSB r4, [sp, #1]
		MOV r0, r4
		BL p_print_bool
		BL p_print_ln
		MOV r0, #0
		BL p_print_bool
		BL p_print_ln
		ADD sp, sp, #2
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
