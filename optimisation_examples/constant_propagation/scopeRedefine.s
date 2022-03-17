/*
WACC Program:
	# more complex variable scoping test that redefines a variable

	# Output:
	# true
	# 2

	# Program:

	begin
	int x = 1 ;
	begin
		x = 2 ;
		bool x = true ;
		println x
	end ;
	println x 
	end

Optimisation:
	Program identifies the proper scope of variables during compile-time 
	and redefines them properly. In this case, x in the outer scope is 
	correctly reassigned to 2 and x in the inner scope is correctly
	assigned to true. As these values do not change elsewhere, the program
	replaces the variables with the values at compile-time.
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
	msg_int:
		.word 3
		.ascii "%d\0"

	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #4
		LDR r4, =1
		STR r4, [sp]
		SUB sp, sp, #1
		LDR r4, =2
		STR r4, [sp, #1]
		MOV r4, #1
		STRB r4, [sp]
		MOV r0, #1
		BL p_print_bool
		BL p_print_ln
		ADD sp, sp, #1
		LDR r4, =2
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		ADD sp, sp, #4
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
	p_print_int:
		PUSH {lr}
		MOV r1, r0
		LDR r0, =msg_int
		ADD r0, r0, #4
		BL printf
		MOV r0, #0
		BL fflush
		POP {pc}
