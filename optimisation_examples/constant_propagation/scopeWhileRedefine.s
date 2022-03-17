/*
WACC Program:
	# variable scoping test that redefines a variable within a while-loop

	# Output:
	# counting... 5
	# counting... 4
	# counting... 3
	# counting... 2
	# counting... 1
	# 0 Boom!

	# Program:

	begin
	int x = 5 ;
	string y = " Boom!" ;
	while x > 0 do
		string y = "counting... " ;
		print y;
		println x;
		x = x - 1
	done ;
	print x;
	println y
	end

Optimisation:
	Value of variable x is not constant. Hence, the program does not
	replace the variable with the value at compile-time
*/
	.data

	msg_0:
		.word 6
		.ascii " Boom!"
	msg_1:
		.word 12
		.ascii "counting... "
	msg_2:
		.word 12
		.ascii "counting... "
	msg_string:
		.word 5
		.ascii "%.*s\0"
	msg_int:
		.word 3
		.ascii "%d\0"
	msg_new_line:
		.word 1
		.ascii "\0"
	msg_overflow:
		.word 83
		.ascii "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n\0"
	msg_3:
		.word 6
		.ascii " Boom!"

	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #8
		LDR r4, =5
		STR r4, [sp, #4]
		LDR r4, =msg_0
		STR r4, [sp]
		B L0
	L1:
		SUB sp, sp, #4
		LDR r4, =msg_1
		STR r4, [sp]
		LDR r4, =msg_2
		MOV r0, r4
		BL p_print_string
		LDR r4, [sp, #8]
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		LDR r4, [sp, #8]
		LDR r5, =1
		SUBS r4, r4, r5
		BLVS p_throw_overflow_error
		STR r4, [sp, #8]
		ADD sp, sp, #4
	L0:
		LDR r4, [sp, #4]
		LDR r5, =0
		CMP r4, r5
		MOVGT r4, #1
		MOVLE r4, #0
		CMP r4, #1
		BEQ L1
		LDR r4, [sp, #4]
		MOV r0, r4
		BL p_print_int
		LDR r4, =msg_3
		MOV r0, r4
		BL p_print_string
		BL p_print_ln
		ADD sp, sp, #8
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
	p_throw_runtime_error:
		BL p_print_string
		MOV r0, #-1
		BL exit
	p_throw_overflow_error:
		LDR r0, =msg_overflow
		BL p_throw_runtime_error
