.MODEL SMALL
	.387
	.386
	.STACK
	.DATA

	;DECLARE VARIABLES
	ANSWER DD ?
	ASCANSWER DB 22 DUP (0)
	j db 0
	k db 0
	roi db 1
	Count db 0
	pass_again db 0
	H DB 0
	L DB 7FH			;INITIALIZE TO 7Fh AS PER NODEHL MACRO
	M DB ?
	N DB ?
	ND1 DB 0			;NODE NUMBERS FOR TKEVAL
	ND2 DB 0			;NODE NUMBERS FOR TKEVAL
	TEN DD 10
	KILO DD 1000
	MEGA DD 1000000
	TEMP DD 0
	Rows DB 0
	R1 DW	0
	R2 DW	0
	NOOPEN DB  "The file could not be found or opened", 0Ah, 0Dh, "$"
	EMPTY DB "There were no arguments passed to this program", 0Ah, 0Dh, "$"
	NOIGRAB DB "Calculating equivalent resistance please wait ...." , 0Ah, 0Dh, "$"
	OPENSUCCESS DB "Opening file name:",0Ah, "$"
	FileN DB 64 DUP(0)
	ENDIT DB "$"
	A DB 2				;INITIALIZE TO 2 TO OPEN FILE FOR READ
	B DB 0
	D DB 0
	OFF DB 32 DUP(0)
	EOF DB 1			;INITIALIZE TO 1, EOF = 0 MEANS END OF FILE
	MOFF DQ 500 DUP (0)

	.CODE
Fthisshit Macro M , N , MOFF, ROWS, ANSWER, ASCANSWER
	LOCAL L1, L2
	MOVZX EAX, M     	; EAX HAS VALUE OF r
    	SUB   EAX, 1        	; EAX HOLDS VALUE (r-1)
    	MOVZX EDX, Rows        	; EDX HAS VALUE OF R
    	MUL   EDX        	; EAX HOLDS (r-1)R
    	MOVZX EDX, N    	; EDX HAS c
    	SUB   EDX, 1        	; EDX HAS (c-1)
    	ADD   EAX, EDX        	; EAX HAS (r-1)R+(c-1)
    	MOV   EDX, 8        	; EDX HAS THE 8 MULTIPLER TO FIND CORRECT BYTE IN ARRAY
    	MUL   EDX
    	MOV   EDI, EAX
    	FLD MOFF[DI]
	FIST ANSWER
	MOV EAX, ANSWER
	MOV SI, 0
	MOV EDX, 0
	MOV ECX, 10
L1:	DIV ECX
	ADD DX, 30H
	PUSH DX
	MOV EDX, 0
	INC SI
	CMP EAX, 0
	JNE L1

	MOVZX ECX, SI
	MOV SI, 0
L2:	POP DX
	MOV ASCANSWER[SI], DL
	INC SI
	LOOP L2

	MOV ASCANSWER[SI], 20h
	INC SI
	MOV ASCANSWER[SI], "O"
	INC SI
	MOV ASCANSWER[SI], "H"
	INC SI
	MOV ASCANSWER[SI], "M"
	INC SI
	MOV ASCANSWER[SI], "s"
	INC SI
	MOV ASCANSWER[SI], "$"
	INC SI
	MOV DX, OFFSET ASCANSWER
	MOV AH, 9h
	INT 21h
ENDM

;===============================COMMTAIL=MACRO================================
;COMMTAIL MACRO OPENS THE FILE AND READS THE COMMAND TAIL FOR THE NODES OF ;INTEREST.
;THE NODES OF INTEREST ARE PLACED IN M AND N
;NEEDS VARIABLES DEFINED FOR M AND N IN MAIN PROC AS WELL AS FileN WITH THE FILE NAME
;CALL LIKE THIS EXACTLY: COMMTAIL M,N, FileN, EMPTY, NOIGRAB, NOOPEN, OPENSUCCESS
;=============================================================================

COMMTAIL    MACRO M,N, FileN, EMPTY, NOIGRAB, NOOPEN, OPENSUCCESS

	LOCAL L0, L1, NOTAIL, SUCCESS, FAIL, L0b

	MOV AH, 62H
	INT 21h
	MOV FS, BX
	MOV ES,BX
	MOV ECX, 0
	MOV SI, 80H
	MOV CL, ES:[SI]
	CMP ECX, 0
	JE NOTAIL
    	MOV M, 29h                		;INITIALIZE M
    	MOV N, 29h                		;INITIALIZE N
    ;GET FILE NAME OF INPUT FILE
	MOV BP, 0
L0:	INC SI
	CMP BYTE PTR ES:[SI], 20H
   	JE L0


L0b:	MOV DL, ES:[SI]
	MOV FileN[BP], DL
  	INC BP
	INC SI
	CMP BYTE PTR ES:[SI], 20h
  	JNE L0b


	MOV AX, 3D00H
	MOV DX, OFFSET FileN
	INT 21H
	MOV BX,AX
	PUSH BX
	JNC L1

	MOV DX, OFFSET NOOPEN
	MOV Ah, 09h
	INT 21H
	MOV EOF, 0
    	JMP FAIL

L1: 	INC SI                 			;INC SI TO GET TO NEXT CHAR
    	CMP BYTE PTR ES:[SI], 20h 		;COMPARE 1ST CHAR TO SPACE
    	JE L1					;IF CHAR IS A SPACE, CHECK NEXT CHAR
    	MOV EDX, 0               		;CLEAR EDX FOR MUL
    	MOV EAX, 0               		;CLEAR EAX FOR MUL
    	MOV EBX, 0               		;CLEAR EBX FOR MOVE

    ;IF PREV CHAR IS A NUMBER AND M IS EMPTY
    	.IF BYTE PTR ES:[SI+1] >= 30h && BYTE PTR ES:[SI+1] <= 39h && M == 29h
    	MOV EDI, 10                		;DECIMAL IS BASE 10
    	MOV AL, ES:[SI]             		;MOVE CURRNET CHAR IN EAX
    	SUB AL, 30h                		;CONV TO INTEGER
    	MOV BL, ES:[SI+1]          		;MOVE NEXT CHAR INTO EDX
    	SUB BL, 30h                		;CONV TO INT
    	MUL EDI                    		;MULTIPLY PREV CHAR BY 10
    	ADD EAX, EBX               		;ADD CURRENT CHAR
    	MOV M, AL               		;MOVE DOUBLE-DIGIT VALUE IN M
    	INC SI                    		;SKIP SECOND DIGIT

    ;IF PREV LOCA IS A CHAR M IS FULL AND N IS EMPTY
    	.ELSEIF BYTE PTR ES:[SI+1] >= 30h && BYTE PTR ES:[SI+1] <= 39h && N == 29h
    	MOV EDI, 10               		;DECIMAL IS BASE 10
    	MOV AL, ES:[SI]           		;MOVE PREV CHAR IN EAX
    	SUB AL, 30h               		;CONV TO INTEGER
    	MOV BL, ES:[SI+1]         		;MOVE CURRENT CHAR INTO EDX
    	SUB BL, 30h               		;CONV TO INT
    	MUL EDI                   		;MULTPLY PREV CHAR BY 10
    	ADD EAX, EBX              		;ADD CURRENT CHAR
    	MOV N, AL                		;MOVE DOUBLE-DIGIT VALUE IN M
    	INC SI                    		;SKIP SECOND DIGIT
    ;IF NUM IS NOT DOUBLE DIGIT AND M IS EMPTY
    	.ELSEIF M == 29h
    	MOV AL, ES:[SI]           		;MOVE NUMBER INTO AX
    	SUB AL, 30h               		;CONVERT TO INTEGER
    	MOVZX EAX, AL             		;CLEAR UPPER PART OF REG
    	MOV M, AL              			;MOVE INTO M
    	.ELSEIF N == 29h          		;NUM IS NOT DOUBLE DIGIT, M IS FULL AND N IS EMPTY
    	MOV AL, ES:[SI]                     	;MOV NUMBER TO AX
    	SUB AL, 30h                         	;CONV TO INT
    	MOVZX EAX, AL                       	;CLEAR UPPER PART OF REG
    	MOV N, AL                		;MOV TO N
    	.ENDIF

    	CMP BYTE PTR ES:[SI+1], 0Dh		;IF AT THE END OF THE TAIL
    	JNE L1
    	JMP SUCCESS
NOTAIL: MOV DX, OFFSET EMPTY
	MOV Ah, 09h
    	INT 21H
	JMP FAIL
SUCCESS:MOV DX, OFFSET OPENSUCCESS
	MOV Ah, 09h
	INT 21H
	MOV DX, OFFSET FileN
	MOV Ah, 09h
	INT 21H
	MOV DX, OFFSET NOIGRAB
	MOV Ah, 09h
    	INT 21H
FAIL:
	ENDM
;=============================END=MACRO=======================================


;=============================================================================
;THIS MACRO MOVES POINTER TO TOP OR BOTTOM DEPENDING ON WHAT THE USER SPECIFIES
;ACCEPTS A=0 (beginning of file), A=1 (current location), A=2 (end of file)
; B= loads CX for jump (zeros it is unless jump is over 1000 bytes)
; D= loads DX length of jump
;=============================================================================
MOVEPTR MACRO A,B,D
	LOCAL INCORRECTVALUES
	MOVZX CX, B
        MOVZX DX, D
        .IF A == 0
        MOV AX, 4200H
        INT 21H
        .ELSEIF A == 1
        MOV AX, 4201H
        INT 21H
        .ELSEIF A == 2
        MOV AX, 4202H
        INT 21H
        .ELSE
        JMP INCORRECTVALUES
        .ENDIF

INCORRECTVALUES:
	ENDM

;=============================================================================
;=============================================================================
;tested
;Reads from the file one line at a time and saves it in memory
;the int will read 1 byte at a time and test them for 0dh and  0ah
;when 0Dh or 0Ah are found the pointer will move once more to account for second
;enter value. OFF will be the variable the line is stored  DB 32 dup(0)
;EOF will be a variable to denote that the end of file has been reached.
;by loading 0 into the var location.
;=============================================================================
RDLN	MACRO OFF, EOF
	LOCAL RDLN1, RDLN2, TST2, GOUT
	MOV ECX, 1
        MOV DX, OFFSET OFF
RDLN1:	MOV AX, 3F00h
	INT 21h
    	CMP AX, 0
    	JE RDLN2
    	MOV DI, DX
    	CMP BYTE PTR [DI],  0Dh
    	JE TST2
    	CMP BYTE PTR [DI],  0Ah
    	JE TST2
    	INC DX

    	JMP RDLN1


RDLN2:
    	MOV EOF, AL
    	JMP GOUT

TST2:   MOV AX, 4201h
    	MOV DX, 1
    	MOV CX, 0
    	INT 21H
GOUT:
	ENDM
;=============================================================================
;================================================================================
;TESTED
;this Macro will look at a line in memory pull from the file and check if the node
;values in the line are greater or less than the highest and lowest value found before.
;NDH will have the highest node value initialized as  DB 0
;NDL will have the lowest node value initialized as DB 7Fh
;off will be the variable that the line read from the file is saved as.
;================================================================================
NODEHL MACRO NDH, NDL, OFF
LOCAL L1, L2, L3, L4, L5, L6, L7, L8, L9, L10
	MOV CH , 0
    	MOV DH, 0
	MOV DI, 0

L1: 	MOV CL, BYTE PTR [OFF+DI]
    	INC DI
    	CMP CL, 20h
    	JE L1
    	CMP CL, 09h
    	JE L1

L2:     MOV AL, 10                    	;L2 puts into AH the value of the first node
    	MUL DH
    	MOV DH, AL
    	SUB CL, 30H
    	ADD DH, CL
    	MOV CL, BYTE PTR [OFF+DI]
    	INC DI
    	CMP CL, 20h
    	JE L3
      	CMP CL, 09h
      	JNE L2


L3:   	MOV CL,BYTE PTR [OFF+DI]
    	INC DI
    	CMP CL, 20h
    	JE L3
    	CMP CL, 09h
    	JE L3


L4:	MOV AL, 10                    	;L4 puts into CH the value of the second node
    	MUL CH
   	MOV CH, AL
   	SUB CL, 30H
   	ADD CH, CL
   	MOV CL,BYTE PTR [OFF+DI]
   	INC DI
   	CMP CL, 20h
   	JE L5
   	CMP CL, 09h
   	JNE L4
L5:	MOV CL,BYTE PTR [OFF+DI]
   	INC DI
   	CMP CL, 20h
   	JE L6
   	CMP CL, 09h
   	JNE L5
L6:

   	CMP NDH , DH            	;Compares the nodes to the highest node
   	JA  L7
   	MOV NDH , DH
L7:	CMP NDH , CH
   	JA  L8
   	MOV NDH , CH
L8:	CMP NDL , DH            	;Compare the nodes to the lowest node
    	JB  L9
    	MOV NDL , DH
L9: 	CMP NDL , CH
    	JB  L10
    	MOV NDL , CH
L10:

	ENDM
;================================================================================
;=============================================================================
;UNtested
;Macro to Create square matrix of zeroes
;creates a matrix in memory as an array of quad words and initializes them as
;all double word zeroes. Uses the H, L arguments from macro NODEHL and offset
;in memory the array starts. The memory offset used has to be the last value
;set in the data part of the code, if there is anything set after it there
;risks the chance of being over written. chance of error if difference of
;h and l are greater than 127
;=============================================================================
CRTMTRX MACRO H, L, MOFF, Rows
LOCAL CMX1
	MOV DI, 0
	MOV EAX, 0
	MOV AL, H
	SUB AL, L
	INC AL
	MOV Rows, AL
	MUL AL            		;(H-L)^2
	MOV CX, 2
	MUL CX        			;2*(H-L)^2
	MOV ECX, EAX        		;counter set to 2*(H-L)^2
	MOV EAX, 0
CMX1:   MOV DWORD PTR  [MOFF+DI], EAX
    	ADD DI, 4
    	LOOP CMX1
ENDM
;=============================================================================
;=============================================================================
;TkeVal
;This macro will take the line read from the file and find the first node
;the second node and integer value of the resistance. The arguments used are
;OFF = the variable name for the line in Memory the line is saved
;ND(1&2) are the node values that are saved in memory
;TEN =- integer variable value 10
;Mega = INTEGER VARIABLE value 1000000
;Kilo = integer variable value 1000
;TESTED
;=============================================================================
TkeVal	MACRO OFF, ND1, ND2, TEN, MEGA, KILO, TEMP
    LOCAL  L1, L2, L3, L4, L5, L6, L7, L8 , L9, L10,L11, L12,L13,L14 ,DECI, GOUT
	MOV DI, 0
    	MOV ECX, 0
    	MOV EAX, 0
    	MOV EDX, 0
    	MOV ND1, 0
    	MOV ND2, 0
    	FLDZ 	                		;ST(0)= 0

L1: 	MOV CL, BYTE PTR [OFF+DI]
    	MOV [OFF+DI], 0
    	INC DI
    	CMP CL, 20h
    	JE L1
    	CMP CL, 09h
    	JE L1

L2: 	MOV AL, 10                  	;L2 puts into ND1 the value of the first node
    	MUL ND1
    	MOV ND1, AL
    	SUB CL, 30H
    	ADD ND1, CL
    	MOV CL, BYTE PTR [OFF+DI]
	MOV [OFF+DI], 0
	INC DI
	CMP CL, 20h
	JE L3
      	CMP CL, 09h
      	JNE L2


L3:   	MOV CL,BYTE PTR [OFF+DI]
    	MOV [OFF+DI], 0
    	INC DI
      	CMP CL, 20h
      	JE L3
      	CMP CL, 09h
      	JE L3


L4:   	MOV AL, 10                  	;L4 puts into ND2 the value of the second node
    	MUL ND2
    	MOV ND2, AL
    	SUB CL, 30H
    	ADD ND2, CL
    	MOV CL,BYTE PTR [OFF+DI]
    	MOV [OFF+DI], 0
    	INC DI
    	CMP CL, 20h
    	JE L5
    	CMP CL, 09h
    	JNE L4
L5: 	MOV CL,BYTE PTR [OFF+DI]
    	MOV [OFF+DI], 0
    	INC DI
    	CMP CL, 20h
    	JE L12
    	CMP CL, 09h
    	JNE L5
    	CMP CL, "."
    	JZ DECI

L12: 	MOV CL,BYTE PTR [OFF+DI]
    	MOV [OFF+DI], 0
    	INC DI
    	CMP CL, "."
    	JZ DECI
    	CMP CL, 20h
    	JE L12
    	CMP CL, 09h
    	JE L12


L8: 	FIMUL TEN                		;ST(0)=RESIST*10
    	SUB CL, 30H              		;If there is no decimal then the value will be in st(0)
    	MOV TEMP, ECX
    	FILD TEMP                		;ST(0)=TEMP,ST(1)=RESIST
    	FADDP ST(1), ST          		;ST(0)=RESIST+TEMP
    	MOV CL, BYTE PTR[OFF+DI]
    	MOV [OFF+DI], 0
    	INC DI
    	CMP CL, "."
    	JZ DECI
    	CMP CL, "M"
    	JNE  L6
    	FILD MEGA                		;ST(0)=1000000, st(1)=resist
    	FMULP  ST(1), ST         		;ST(0)= 1000000*resist
    	JMP GOUT
L6: 	CMP CL, "k"
    	JNE  L7
    	FILD KILO                		;ST(0)=1000, st(1)=resist
    	FMULP  ST(1), ST			;ST(0)= 1000*resist
	JMP GOUT
L7:    	CMP CL, 0Dh
    	JE GOUT
    	CMP CL, 0
    	JE GOUT


    	CMP CL, 20h
    	JE GOUT
    	CMP CL, 09h
    	JE GOUT
    	JMP L8
DECI:
    	MOV CL, BYTE PTR [OFF+DI]
    	MOV [OFF+DI], 0
    	INC DI
    	SUB CL, 30h

    	FLD1                    		;ST(0)=1, st(1)=resist

L11:    FIDIV TEN                		;ST(0)=.1, ST(1)=RESIST
    	MOV TEMP, ECX
    	FILD  TEMP                		;ST(0)=CL ,ST(1)=.1, ST(1) = RESIST
    	FMUL ST(0), ST(1)         		;ST(0)=CL*.1,ST(1)=.1, ST(2)=RESIST
    	FADDP ST(2), ST(0)        		;ST(1)=.1,  ST(2)=RESIST +(CL*1)
    	MOV CL, BYTE PTR [OFF+DI]
    	MOV [OFF+DI], 0
    	INC DI
    	CMP CL, "M"
    	JNE  L10
    	FSTP ST(0)
    	FILD MEGA                		;ST(0)=1000000, st(1)=resist
    	FMULP  ST(1), ST         		;ST(0)= 1000000*resist
    	JMP GOUT
L10:	CMP CL, "k"
    	JNE  L9
    	FSTP ST(0)               		;ST(0)=RESIST
    	FILD KILO                		;ST(0)=1000, st(1)=resist
    	FMULP  ST(1), ST         		;ST(0)= 1000*resist
    	JMP GOUT
L9: 	CMP CL, 20h
    	JE GOUT
    	CMP CL, 09h
    	JE GOUT
    	CMP CL , 0Dh
    	JNE L14
    	FSTP ST(0)
    	JMP GOUT
    	CMP CL , 0
	JNE L14
	FSTP ST(0)
    	JMP GOUT
L14:   	SUB CL, 30h
    	JMP L11

GOUT:

	MOV CX, 32
	SUB CX, DI
L13:	MOV [OFF+DI], 0
	LOOP L13
ENDM
;=============================================================================
;====================================
;Assumptions:     the zero matrix (array) is already created with correct size
;          which is 2*(H-L)^2 and consists of values that are double precision i.e. quadwords
;Takes:     Node1, Node2, and Rows (number of rows), M (offset of array)
;Does:      loads array at locations Node1,Node2 and Node2, Node1 with the value of Rlow and Rhigh
;        also checks to see if a value is already there, if it is this macro also
;        calls on "Parallel" and computes the parallel value before loading that location
;        in the array
;        arrayM(x) = (r-1)R + (c-1)
;Returns:    Nothing
;<<<( TESTED )>>>
;====================================


LDMAT MACRO ND1, ND2, Rows, MOFF

    	MOVZX EAX, ND1     	; EAX HAS VALUE OF r
    	SUB   EAX, 1        	; EAX HOLDS VALUE (r-1)
    	MOVZX EDX, Rows        	; EDX HAS VALUE OF R
    	MUL   EDX        	; EAX HOLDS (r-1)R
    	MOVZX EDX, ND2    	; EDX HAS c
    	SUB   EDX, 1        	; EDX HAS (c-1)
    	ADD   EAX, EDX        	; EAX HAS (r-1)R+(c-1)
    	MOV   EDX, 8        	; EDX HAS THE 8 MULTIPLER TO FIND CORRECT BYTE IN ARRAY
    	MUL   EDX
    	MOV   EDI, EAX        	; EDI HOLDS X1*8
; This section is meant to calculate arrayM(x) = (c-1)R + (r-1)
; for the purpose of populating the array at both locations: (r,c) and (c,r)
; to setup the array for the manipulations to come in later macros
    	MOVZX EAX, ND2
    	SUB   EAX, 1
    	MOVZX EDX, Rows
    	MUL   EDX
    	MOVZX EDX, ND1
    	SUB   EDX, 1
    	ADD   EAX, EDX
    	MOV   EDX, 8
    	MUL   EDX
    	MOV   ESI, EAX        	; ESI HOLDS X2*8

    	FLD QWORD PTR [MOFF+EDI]; ST(0) = WHATEVER IS AT THE OFFSET
    	FLDZ            	; ST(0) = 0, ST(1) = ^^^, ST(2) = RESISTOR VALUE IN QUE
    	FCOMP            	; COMPARES ST(0), AND ST(1)
    	FSTSW AX
    	SAHF
    	JNE CMPTPARA        	; IF IT'S NOT EQUAL THERE IS A VALUE THERE ALREADY, AND
                		; THE PARALLEL RESISTANCE NEEDS TO BE CALCULATED
                		; ELSE IF ZF = 1 THEN WE CAN JUST STORE A VALUE THERE
   	FADDP ST(1),ST(0)    	; IF IT IS HERE THEN arrayM(x) = 0 SO IT ADDS ZERO TO ST(1)
                		; WHICH HOLDS THE RESISTOR VALUE THIS MACRO STARTED WITH AND
                		; POPS THE ZERO OUT
	FST [MOFF+EDI]        	; STORING ST(0) INTO OFFSET + EDI
	FSTP [MOFF+ESI]    	; STORING ST(0) INTO OFFSET + ESI AND POPS ST(0)
	JMP AWAY        	; EXIT MACRO
CMPTPARA:            		; THERE WAS AN R VALUE AT THAT POSITION IN ARRAY
    	Parallel      		; IF JMP WENT HERE ST(0) HOLDS R VALUE AT THAT OFFSET AND
                		; ST(1) SHOULD HOLD ORIGINAL R VALUE TO BE STORED IN ARRAY
                		; ST(0) NOW HAS PARALLEL VALUE
	FST [MOFF+EDI]        	; STORING ST(0) INTO OFFSET + EDI
	FSTP [MOFF+ESI]     	; STORING ST(0) INTO OFFSET + ESI AND POPS ST(0)


AWAY:
    	ENDM
;=============================================================================
;==============================================================================

Parallel	MACRO
	FLD1           		;ST(0) => ST(1) = r1 & ST(2) = R2

    	FDIVR ST(1),ST(0)  	;ST(1) = ST(0)/ST(1)
    	FDIVRP ST(2),ST(0) 	;ST(2) = ST(0)/ST(2)
    	FADDP ST(1), ST(0) 	;ST(1) = ST(1) + ST(0)
    	FLD1        		;ST(0) = 1
    	FDIVRP ST(1), ST(0) 	; ST(0) = ST(1)^-1
ENDM


;=============================================================================
;======================================================
;ARRLOCS takes two node values Node1, Node2, and Rows which is the number of rows in the matrix
;or the sqrt of the array size.
;Returns: x1(EDI) and x2 (ESI)which are array locations for the matrix entries A(m,n) and A(n,m)
;======================================================
ARRLOCS MACRO Node1, Node2, Rows
PUSH EBX
PUSH EAX
PUSH ECX
PUSH EDX

	MOVZX EAX, Node1 	; EAX HAS VALUE OF r
	SUB   EAX, 1		; EAX HOLDS VALUE (r-1)
	MOVZX EDX, Rows		; EDX HAS VALUE OF R
	MUL   EDX		; EAX HOLDS (r-1)R
	MOVZX EDX, Node2	; EDX HAS c
	SUB   EDX, 1		; EDX HAS (c-1)
	ADD   EAX, EDX		; EAX HAS (r-1)R+(c-1)
	MOV   EDX, 8		; EDX HAS THE 8 MULTIPLER TO FIND CORRECT BYTE IN ARRAY
	MUL   EDX		;
	MOV   EDI, EAX		; EDI HOLDS X1*8
; This section is meant to calculate arrayM(x) = (c-1)R + (r-1)
; for the purpose of populating the array at both locations: (r,c) and (c,r)
; to setup the array for the manipulations to come in later macros
	MOVZX EAX, Node2
	SUB   EAX, 1
	MOVZX EDX, Rows
	MUL   EDX
	MOVZX EDX, Node1
	SUB   EDX, 1
	ADD   EAX, EDX
	MOV   EDX, 8
	MUL   EDX
	MOV   ESI, EAX		; ESI HOLDS X2*8
POP EBX
POP EAX
POP ECX
POP EDX
ENDM
;=============================================================================
;This macro will take the series resistor values R1 and R2 as REAL values and compute series ;equivalent
;Tested
;=============================================================================
Series MACRO

	FADDP ST(1), ST(0)          ;ST(1) = ST(1) + ST(0)
ENDM
;=============================================================================
;Macro to detect how many values are in a row of the matrix.
;Assumes that each element is 8bytes
;Assumes count = 0
;After running macro comparing Count to 2 will determine series
;Tested Works. Needs to test some extraneous values
;OFF needs to actually be the variable that represents the array
;=============================================================================
CNTVAL	MACRO roi, Rows, M, Count
		LOCAL J1, FINISHED
		PUSHAD
		MOV Count, 0
		MOVZX ECX, Rows		; Length of rows
		MOVZX EAX, roi		; row of interest
		SUB   EAX, 1		; EAX HOLDS VALUE (r-1)
		MOVZX EDX, Rows		; EDX HAS VALUE OF R
		MUL   EDX		; EAX HOLDS (r-1)R
		MOV   EDX, 8		; EDX HAS THE 8 MULTIPLER TO FIND CORRECT BYTE IN ARRAY
		MUL   EDX		;
		MOV   EDI, EAX		; EDI HOLDS (X1-1)*8 (now at first spot in roi-row of intrest)

J1:		CMP ECX, 0
		JZ FINISHED
		FLD QWORD PTR[M+EDI]	; ST(0) = WHATEVER IS AT THE OFFSET
		FLDZ			; ST(0) = 0, ST(1) = ^^^
		FCOMPP			; COMPARES ST(0), AND ST(1)		DEC ECX			; dec ecx incase jump occurs
		ADD EDI, 8
		DEC ECX
		FSTSW AX
		SAHF
		JE  J1			; if spot holds one move to next and check
		INC ECX			; to return ecx incase Jump was made
		INC Count		; count number of values in a row
		LOOP J1			; loop to check all values in a row
		FINISHED:
		POPAD
ENDM

;=====================================
;This macro will scan a single row of a matrix and sum the two series resistor values on that row
;it also replaces the resistor values with zeroes to setup for the next part of the matrix collapsing.
;Finally it saves which columns the two values were in in order to assist with the rest of the matrix collapsing.
;Passes back values of j and k (columns with the values), and ST(0) holds the sum
;=====================================
SUM_CLR_ROW	MACRO roi, Rows, M, j, k
		LOCAL J1, FINISHED
		PUSHAD
		MOVZX ECX, Rows		; Length of rows
		MOVZX EAX, roi		; row of interest
		SUB   EAX, 1		; EAX HOLDS VALUE (r-1)
		MOVZX EDX, Rows		; EDX HAS VALUE OF R
		MUL   EDX		; EAX HOLDS (r-1)R
		MOV   EDX, 8		; EDX HAS THE 8 MULTIPLER TO FIND CORRECT BYTE IN ARRAY
		MUL   EDX		;
		MOV   EDI, EAX		; EDI HOLDS (X1-1)*8 (now at first spot in roi-row of intrest)
		MOV   EBX, 0		; Holds value of column for later macros
K1:		CMP ECX, 0
		JZ FINISHED
		FLD QWORD PTR[M+EDI]	; ST(0) = WHATEVER IS AT THE OFFSET
		FLDZ			; ST(0) = 0, ST(1) = ^^^
		FCOMPP			; COMPARES ST(0), AND ST(1)
		ADD EDI, 8		; Moves to the next entry in array
		DEC ECX			; incase the compare was equal decrements ECX
		INC EBX			; tracking which column/array value we're at
		FSTSW AX		; loads the status register in flags for appropriate jumps
		SAHF			; ^^
		JE  K1			; if thing in location was equal to zero go back to start of loop

		INC ECX			; if they were NOT equal, increment ecx to keep accruate loop count
					; because the loop command will decrement automatically
		SUB EDI, 8		; back up to position with the value=/=0
		FLD QWORD PTR[M+EDI]	; ST(0) = WHATEVER IS AT THE OFFSET
		FLDZ			; ST(0) = 0, ST(1) = WHATEVER IS AT THE OFFSET
		FSTP QWORD PTR[M+EDI]	; plugs zero into the array where the value was
					; and pops ST(0), ST(1) = WHATEVER IS AT THE OFFSET
		ADD EDI, 8		; onto the next value in the rest
		.IF j == 0		; if this is the first series value store it in k
		MOV j, BL
		.ELSE			; else this is the 2nd value in the row and put the colomn value in k
		MOV k , BL
		.ENDIF
		LOOP K1			; loop to check all values in a row
		FINISHED:
		Series
		POPAD
ENDM

;=======================================
;This value clears the correct values along the columns after a series addition and row clear has been made
;roi: the row that was summed and cleared
;j: is the first column that held a value
;k: is the second column that held a value
;M: is the array
;Rows: is the number of rows in the array/matrix
;This macro basically clears the values in the array at values arrayM(X_j,roi), arrayM(X_k,roi)
;and places 0.0 in the place of the values that were there.
;=======================================
CLEAR_COLUMN	MACRO roi, j ,k, M, Rows
	PUSHAD
	ARRLOCS j, roi, Rows
	FLDZ
	FLDZ
	FSTP [M+EDI]
	ARRLOCS k, roi, Rows
	FSTP [M+EDI]
	POPAD
ENDM
;=============================================================================
;This macro will take the parallel resistor values R1 and R2 as REAL values and compute parallel ;equivalent
;This macro requires that the resistor values R1 and R2 already be in the FPU in any order
;=============================================================================
Parallel	MACRO

	FLD1		  	; ST(0) => ST(1) = r1 & ST(2) = R2
	FDIVR ST(1),ST(0)  	; ST(1) = ST(0)/ST(1)
	FDIVRP ST(2),ST(0)	; ST(2) = ST(0)/ST(2)
	FADDP ST(1), ST(0)	; ST(1) = ST(1) + ST(0)\
	FLD1			; ST(0) = 1
	FDIVRP ST(1), ST(0) 	; ST(0) = ST(1)^-1
ENDM
;================================
;This macro takes a value that is assumed to be in the FPU (that was left from the SUM_CLR_ROW macro)
;and places it in the correct space in the matrix/array in location arrayM(X_j,k) and arrayM(X_k,j)
;if there is already a value there then it will calculate the parallel resistance value of the two values:
;the ones already there and the one passed from SUM_CLR_ROW
;================================

PLUG_VALUE	MACRO j, k, M, Rows
	LOCAL A1, toEND, A2
	PUSHAD
	ARRLOCS j, k, Rows
	FLD [M+EDI]
	FLDZ
	FCOMPP
	FSTSW AX
	SAHF
	JE A1
	JNE A2

A1:	FST [M+EDI]
	FSTP [M+ESI]
	JMP toEND
A2: 	FLD [M+EDI]
	Parallel
	FST [M+EDI]
	FSTP [M+ESI]
toEND:	POPAD
ENDM

;=============================================================================
MAIN 	PROC FAR
	.STARTUP

;OPENF
	;GET COMMAND TAIL ARGUMENTS
COMMTAIL M,N, FileN, EMPTY, NOIGRAB, NOOPEN, OPENSUCCESS
	POP BX					; PRESERVING THE FILE HANDLE



	;SET A TO 0 FOR MOVE PTR TO BEGINNING OF FILE
L4:	MOV A, 0
	MOVEPTR A, B, D
	;READ A LINE FROM FILE
L1:	RDLN OFF, EOF
	;FIND HIGH AND LOW NODE VALUES
	NODEHL H, L, OFF
	CMP EOF, 1
	JE L1
	;CREATE A MATRIX OF ZEROES
	CRTMTRX H, L, MOFF, Rows
	MOV CX, 0
	MOVEPTR cx, Cl, Cl
	;READ LINE
L2:	RDLN OFF, EOF
	;CHECK MOFF FOR ZEROES
	CMP OFF, 0
	JE ENDOFFILE
	;TAKE VAL
	TKEVAL OFF, ND1, ND2, TEN, MEGA, KILO, TEMP

	;LOAD INTO MATRIX
	LDMAT ND1, ND2, Rows, MOFF
	CMP EOF, 1
	JNE L2

ENDOFFILE:

	MOV DX, offset FileN
	MOV Ah, 09h
	int 21H


.REPEAT					; sets up a do-while loops
	MOV pass_again, 0		; resets trigger value to reinitialize the loop
	MOVZX ECX, Rows			; #rows = number of columns to scan through
J1:	CNTVAL roi, Rows, MOFF, Count	; checks the number of values in that row
		.IF Count == 2		; if finds a row with 2values => a series connection
		MOV pass_again, 1	; ^^ will alter the array so we'll have to scan again from top
					; after these manipulations are executed
		JMP SSUMCLR		; jump to the SUM_CLR_ROW macro
		.ENDIF
	INC roi				; will increment to check next row for a series connection
	LOOP J1				; loops back to j1 to scan row
	CMP ECX, 0			; if  ECX=0 => no series connections were found, matrix is maximally simplified
	JE TTPrint			; Time to print answer and jumps out of algorithms

SSUMCLR:SUM_CLR_ROW roi, Rows, MOFF, j,k	; already decided there is a series connection i.e. only 2 vals on this row
					; so it sums those values, holds the sum in ST(0) and replaces their position in the array
					; with 0.0
	CLEAR_COLUMN roi, j, k, MOFF, Rows	; clears values of arrayM(X_j,roi) and arrayM(k,roi)
	PLUG_VALUE j, k, MOFF, Rows	; plugs value from SUM_CLR_ROWS (stored in ST(0)) into arrayM(X_j,k) and arrayM(X_k,j)
					; and if a val already exists there calculates the || resistance and place the ||
					; value in that postion
	MOV roi, 1			; since alterations were made to the matrix it requires that the collapsing starts again
					; the top so resets the row_of_interest back to the top of the matrix
TTPrint:				; if another pass is not needed wil go through the .UNTIL condition
	.UNTIL pass_again == 0		; repeats if a series connection was found because matrix chances completely after
					; the array/matrix operations are completed
	moV CX, OFFSET MOFF


	MOVZX EAX, M     	; EAX HAS VALUE OF r
    	SUB   EAX, 1        	; EAX HOLDS VALUE (r-1)
    	MOVZX EDX, Rows        	; EDX HAS VALUE OF R
    	MUL   EDX        	; EAX HOLDS (r-1)R
    	MOVZX EDX, N    	; EDX HAS c
    	SUB   EDX, 1        	; EDX HAS (c-1)
    	ADD   EAX, EDX        	; EAX HAS (r-1)R+(c-1)
    	MOV   EDX, 8        	; EDX HAS THE 8 MULTIPLER TO FIND CORRECT BYTE IN ARRAY
    	MUL   EDX
    	MOV   EDI, EAX
    	FLD MOFF[DI]
	FIST ANSWER
	MOV EAX, ANSWER
	MOV SI, 0
	MOV EDX, 0
	MOV ECX, 10
M1:	DIV ECX
	ADD DX, 30H
	PUSH DX
	MOV EDX, 0
	INC SI
	CMP EAX, 0
	JNE M1

	MOVZX ECX, SI
	MOV SI, 0
M2:	POP DX
	MOV ASCANSWER[SI], DL
	INC SI
	LOOP M2

	MOV ASCANSWER[SI], 20h
	INC SI
	MOV ASCANSWER[SI], "O"
	INC SI
	MOV ASCANSWER[SI], "H"
	INC SI
	MOV ASCANSWER[SI], "M"
	INC SI
	MOV ASCANSWER[SI], "s"
	INC SI
	MOV ASCANSWER[SI], "$"
	INC SI
	MOV DX, OFFSET ASCANSWER
	MOV AH, 9h
	INT 21h


	.EXIT
MAIN 	ENDP

	END



