/* 
WACC Program:
	The sample one provided in wacc_examples/valid/scope. 
	Not copy-pasted here since it is too long.

Optimisation:
	Program correctly identifies variables of different types in 
	different scopes and replaces the variables with the constant
	value.
*/
	.data

	msg_0:
		.word 2
		.ascii ", "
	msg_1:
		.word 16
		.ascii "this is a string"
	msg_2:
		.word 5
		.ascii "array"
	msg_3:
		.word 2
		.ascii "of"
	msg_4:
		.word 7
		.ascii "strings"
	msg_5:
		.word 3
		.ascii "( ["
	msg_string:
		.word 5
		.ascii "%.*s\0"
	msg_int:
		.word 3
		.ascii "%d\0"
	msg_6:
		.word 5
		.ascii "] , ["
	msg_7:
		.word 3
		.ascii "] )"
	msg_new_line:
		.word 1
		.ascii "\0"
	msg_8:
		.word 2
		.ascii "[ "
	msg_reference:
		.word 3
		.ascii "%p\0"
	msg_9:
		.word 4
		.ascii " = ("
	msg_true:
		.word 5
		.ascii "true\0"
	msg_false:
		.word 6
		.ascii "false\0"
	msg_10:
		.word 3
		.ascii "), "
	msg_11:
		.word 4
		.ascii " = ("
	msg_12:
		.word 3
		.ascii ") ]"
	msg_13:
		.word 5
		.ascii "array"
	msg_14:
		.word 2
		.ascii "of"
	msg_15:
		.word 7
		.ascii "strings"
	msg_16:
		.word 5
		.ascii "array"
	msg_17:
		.word 2
		.ascii "of"
	msg_18:
		.word 7
		.ascii "strings"
	msg_19:
		.word 16
		.ascii "this is a string"

	.text

	.global main
	main:
		PUSH {lr}
		SUB sp, sp, #8
		LDR r4, =msg_0
		STR r4, [sp, #4]
		LDR r4, =5
		STR r4, [sp]
		SUB sp, sp, #1
		MOV r4, #'x'
		STRB r4, [sp]
		SUB sp, sp, #1
		MOV r4, #1
		STRB r4, [sp]
		SUB sp, sp, #4
		LDR r4, =msg_1
		STR r4, [sp]
		SUB sp, sp, #16
		LDR r0, =16
		BL malloc
		MOV r4, r0
		LDR r5, =1
		STR r5, [r4, #4]
		LDR r5, =2
		STR r5, [r4, #8]
		LDR r5, =3
		STR r5, [r4, #12]
		LDR r5, =3
		STR r5, [r4]
		STR r4, [sp, #12]
		SUB sp, sp, #4
		LDR r0, =7
		BL malloc
		MOV r4, r0
		MOV r5, #'x'
		STRB r5, [r4, #4]
		MOV r5, #'y'
		STRB r5, [r4, #5]
		MOV r5, #'z'
		STRB r5, [r4, #6]
		LDR r5, =3
		STR r5, [r4]
		STR r4, [sp]
		SUB sp, sp, #4
		LDR r0, =7
		BL malloc
		MOV r4, r0
		MOV r5, #1
		STRB r5, [r4, #4]
		MOV r5, #0
		STRB r5, [r4, #5]
		MOV r5, #1
		STRB r5, [r4, #6]
		LDR r5, =3
		STR r5, [r4]
		STR r4, [sp]
		SUB sp, sp, #16
		LDR r0, =16
		BL malloc
		MOV r4, r0
		LDR r5, =msg_2
		STR r5, [r4, #4]
		LDR r5, =msg_3
		STR r5, [r4, #8]
		LDR r5, =msg_4
		STR r5, [r4, #12]
		LDR r5, =3
		STR r5, [r4]
		STR r4, [sp, #12]
		SUB sp, sp, #12
		LDR r0, =8
		BL malloc
		MOV r4, r0
		LDR r5, =1
		LDR r0, =4
		BL malloc
		STR r5, [r0]
		STR r0, [r4]
		LDR r5, =2
		LDR r0, =4
		BL malloc
		STR r5, [r0]
		STR r0, [r4, #4]
		STR r4, [sp, #8]
		SUB sp, sp, #24
		LDR r0, =8
		BL malloc
		MOV r4, r0
		MOV r5, #'a'
		LDR r0, =1
		BL malloc
		STRB r5, [r0]
		STR r0, [r4]
		MOV r5, #1
		LDR r0, =1
		BL malloc
		STRB r5, [r0]
		STR r0, [r4, #4]
		STR r4, [sp, #20]
		LDR r0, =8
		BL malloc
		MOV r4, r0
		MOV r5, #'b'
		LDR r0, =1
		BL malloc
		STRB r5, [r0]
		STR r0, [r4]
		MOV r5, #0
		LDR r0, =1
		BL malloc
		STRB r5, [r0]
		STR r0, [r4, #4]
		STR r4, [sp, #16]
		LDR r0, =12
		BL malloc
		MOV r4, r0
		LDR r5, [sp, #20]
		STR r5, [r4, #4]
		LDR r5, [sp, #16]
		STR r5, [r4, #8]
		LDR r5, =2
		STR r5, [r4]
		STR r4, [sp, #12]
		SUB sp, sp, #20
		LDR r0, =16
		BL malloc
		MOV r4, r0
		LDR r5, =1
		STR r5, [r4, #4]
		LDR r5, =2
		STR r5, [r4, #8]
		LDR r5, =3
		STR r5, [r4, #12]
		LDR r5, =3
		STR r5, [r4]
		STR r4, [sp, #16]
		LDR r0, =7
		BL malloc
		MOV r4, r0
		MOV r5, #'a'
		STRB r5, [r4, #4]
		MOV r5, #'b'
		STRB r5, [r4, #5]
		MOV r5, #'c'
		STRB r5, [r4, #6]
		LDR r5, =3
		STR r5, [r4]
		STR r4, [sp, #12]
		LDR r0, =8
		BL malloc
		MOV r4, r0
		LDR r5, [sp, #16]
		LDR r0, =4
		BL malloc
		STR r5, [r0]
		STR r0, [r4]
		LDR r5, [sp, #12]
		LDR r0, =4
		BL malloc
		STR r5, [r0]
		STR r0, [r4, #4]
		STR r4, [sp, #8]
		LDR r4, [sp, #16]
		STR r4, [sp, #4]
		LDR r4, [sp, #12]
		STR r4, [sp]
		LDR r4, =msg_5
		MOV r0, r4
		BL p_print_string
		LDR r4, =1
		MOV r0, r4
		BL p_print_int
		LDR r4, [sp, #106]
		MOV r0, r4
		BL p_print_string
		LDR r4, =2
		MOV r0, r4
		BL p_print_int
		LDR r4, [sp, #106]
		MOV r0, r4
		BL p_print_string
		LDR r4, =3
		MOV r0, r4
		BL p_print_int
		LDR r4, =msg_6
		MOV r0, r4
		BL p_print_string
		MOV r4, #'a'
		MOV r0, r4
		BL putchar
		LDR r4, [sp, #106]
		MOV r0, r4
		BL p_print_string
		MOV r4, #'b'
		MOV r0, r4
		BL putchar
		LDR r4, [sp, #106]
		MOV r0, r4
		BL p_print_string
		MOV r4, #'c'
		MOV r0, r4
		BL putchar
		LDR r4, =msg_7
		MOV r0, r4
		BL p_print_string
		BL p_print_ln
		ADD sp, sp, #20
		LDR r4, [sp, #20]
		STR r4, [sp, #8]
		MOV r4, #'a'
		STRB r4, [sp, #7]
		MOV r4, #1
		STRB r4, [sp, #6]
		LDR r4, [sp, #16]
		STR r4, [sp, #2]
		MOV r4, #'b'
		STRB r4, [sp, #1]
		MOV r4, #0
		STRB r4, [sp]
		LDR r4, =msg_8
		MOV r0, r4
		BL p_print_string
		LDR r4, [sp, #8]
		MOV r0, r4
		BL p_print_reference
		LDR r4, =msg_9
		MOV r0, r4
		BL p_print_string
		MOV r4, #'a'
		MOV r0, r4
		BL putchar
		LDR r4, [sp, #86]
		MOV r0, r4
		BL p_print_string
		MOV r4, #1
		MOV r0, r4
		BL p_print_bool
		LDR r4, =msg_10
		MOV r0, r4
		BL p_print_string
		LDR r4, [sp, #2]
		MOV r0, r4
		BL p_print_reference
		LDR r4, =msg_11
		MOV r0, r4
		BL p_print_string
		MOV r4, #'b'
		MOV r0, r4
		BL putchar
		LDR r4, [sp, #86]
		MOV r0, r4
		BL p_print_string
		MOV r4, #0
		MOV r0, r4
		BL p_print_bool
		LDR r4, =msg_12
		MOV r0, r4
		BL p_print_string
		BL p_print_ln
		ADD sp, sp, #24
		LDR r4, =1
		STR r4, [sp, #4]
		LDR r4, =2
		STR r4, [sp]
		LDR r4, =1
		MOV r0, r4
		BL p_print_int
		LDR r4, [sp, #62]
		MOV r0, r4
		BL p_print_string
		LDR r4, =2
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		ADD sp, sp, #12
		LDR r4, =msg_13
		STR r4, [sp, #8]
		LDR r4, =msg_14
		STR r4, [sp, #4]
		LDR r4, =msg_15
		STR r4, [sp]
		LDR r4, =msg_16
		MOV r0, r4
		BL p_print_string
		LDR r4, [sp, #50]
		MOV r0, r4
		BL p_print_string
		LDR r4, =msg_17
		MOV r0, r4
		BL p_print_string
		LDR r4, [sp, #50]
		MOV r0, r4
		BL p_print_string
		LDR r4, =msg_18
		MOV r0, r4
		BL p_print_string
		BL p_print_ln
		ADD sp, sp, #16
		MOV r4, #1
		MOV r0, r4
		BL p_print_bool
		LDR r4, [sp, #34]
		MOV r0, r4
		BL p_print_string
		MOV r4, #0
		MOV r0, r4
		BL p_print_bool
		LDR r4, [sp, #34]
		MOV r0, r4
		BL p_print_string
		MOV r4, #1
		MOV r0, r4
		BL p_print_bool
		BL p_print_ln
		ADD sp, sp, #4
		LDR r4, [sp]
		MOV r0, r4
		BL p_print_string
		BL p_print_ln
		ADD sp, sp, #4
		LDR r4, =1
		STR r4, [sp, #8]
		LDR r4, =2
		STR r4, [sp, #4]
		LDR r4, =3
		STR r4, [sp]
		LDR r4, =1
		MOV r0, r4
		BL p_print_int
		LDR r4, [sp, #26]
		MOV r0, r4
		BL p_print_string
		LDR r4, =2
		MOV r0, r4
		BL p_print_int
		LDR r4, [sp, #26]
		MOV r0, r4
		BL p_print_string
		LDR r4, =3
		MOV r0, r4
		BL p_print_int
		BL p_print_ln
		ADD sp, sp, #16
		LDR r4, =msg_19
		MOV r0, r4
		BL p_print_string
		BL p_print_ln
		ADD sp, sp, #4
		MOV r4, #1
		MOV r0, r4
		BL p_print_bool
		BL p_print_ln
		ADD sp, sp, #1
		MOV r4, #'x'
		MOV r0, r4
		BL putchar
		BL p_print_ln
		ADD sp, sp, #1
		LDR r4, =5
		MOV r0, r4
		BL p_print_int
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
	p_print_reference:
		PUSH {lr}
		MOV r1, r0
		LDR r0, =msg_reference
		ADD r0, r0, #4
		BL printf
		MOV r0, #0
		BL fflush
		POP {pc}
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
