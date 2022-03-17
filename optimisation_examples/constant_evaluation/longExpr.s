/*
WACC Program:
	# tests whether the compiler can handle long expressions

	# Output:
	# #empty#

	# Exit:
	# 153

	begin

	int x = 1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 + (10 + (11 + (12 + (13 + (14 + (15 + (16 + 17)))))))))))))));
	exit x

	end

Optimisation:
	x is evaluated at compile-time instead of run-time. 153 is assigned 
	to x directly in the program. This can be seen on line 28.
*/
	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #4
		LDR r4, =153
		STR r4, [sp]
		MOV r0, r4
		BL exit
		ADD sp, sp, #4
		LDR r0, =0
		POP {pc}
		.ltorg
