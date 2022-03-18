/*
WACC Program:
	# tests whether the compiler can handle long expressions

	# Output:
	# #empty#

	# Exit:
	# 10

	begin

	int x = (2 + 3 + 2 + 1 + 1 + 1) - (1 + 2) * (3 - 4 / 6) / ( 2 * (18 - 17) + (3 * 4 / 4 + 6));
	exit x

	end

Optimisation:
	x is evaluated at compile-time instead of run-time. 10 is assigned 
	to x directly in the program. This can be seen on line 28.
*/
	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #4
		LDR r4, =10
		STR r4, [sp]
		LDR r4, [sp]
		MOV r0, r4
		BL exit
		ADD sp, sp, #4
		LDR r0, =0
		POP {pc}
		.ltorg