/*
WACC Programs:
	# tests whether the compiler can handle long expressions with several variables

	# Output:
	# #empty#

	# Exit:
	# 153

	begin

	int a = 1 + 2 ;
	int b = 3 + 4 ; 
	int c = 5 + 6 ;
	int d = 7 + 8 ;
	int e = 9 + 10 ;
	int f = 11 + 12 ;
	int g = 13 + 14 ;
	int h = 15 + 16 ;
	int i = 17 ;
	exit a + b + c + d + e + f + g + h + i

	end

Optimisation:
	Program replaces variables with known constant variables 
	during compile-time and evaluates it with constant evaluation.
*/
	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #36
		LDR r4, =3
		STR r4, [sp, #32]
		LDR r4, =7
		STR r4, [sp, #28]
		LDR r4, =11
		STR r4, [sp, #24]
		LDR r4, =15
		STR r4, [sp, #20]
		LDR r4, =19
		STR r4, [sp, #16]
		LDR r4, =23
		STR r4, [sp, #12]
		LDR r4, =27
		STR r4, [sp, #8]
		LDR r4, =31
		STR r4, [sp, #4]
		LDR r4, =17
		STR r4, [sp]
		LDR r4, =153
		MOV r0, r4
		BL exit
		ADD sp, sp, #36
		LDR r0, =0
		POP {pc}
		.ltorg
