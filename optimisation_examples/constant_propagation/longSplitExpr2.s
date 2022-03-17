/*
WACC Program:
	# tests whether the compiler can handle long expressions with several variables

	# Output:
	# 362880
	# 128

	# Exit:
	# 128

	begin

	int x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 ;
	int y = -1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10 - 11 - 12 - 13 - 14 - 15 - 16 - 17 ;
	int z = 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 ;
	int div = 10 ;
	println x + y + ( z / div ) ;
	println (x + y + ( z / div )) % 256 ;
	exit x + y + ( z / div )

	end

Optimisation:
	Program replaces variables with known constant variables 
	during compile-time and evaluates it with constant evaluation.
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
		SUB sp, sp, #16
		LDR r4, =153
		STR r4, [sp, #12]
		LDR r4, =-153
		STR r4, [sp, #8]
		LDR r4, =3628800
		STR r4, [sp, #4]
		LDR r4, =10
		STR r4, [sp]
		LDR r4, =362880
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		LDR r4, =128
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		LDR r4, =362880
		MOV r0, r4
		BL exit
		ADD sp, sp, #16
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
