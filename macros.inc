ClnSearchMazes	MACRO	
	LOCAL CleaningLoop	
	MOV SI, OFFSET predecessorMatrix
	MOV DI, OFFSEt distanceMatrix
	MOV BX, OFFSET visitedMatrix
	MOV CX, 2048

	CleaningLoop:
		MOV DS:[SI], 0FFh
		MOV DS:[SI + 1], 0FFh
		MOV DS:[DI}, 0FFh
		MOV DS:[DI + 1], 0FFh
		MOV DS:[BX], 0
		ADD SI, 2
		ADD DI, 2
		INC BX
		DEC CX
	JNZ CleaningLoop
	
	MOV CX, 310
	MOV SI, OFFSET Queue
	CleanQueu:
		MOV [SI], '$'
		MOV [SI + 1], '$'
		ADD SI, 2
		DEC CX
	JNZ CleanQueu
	
	MOV tempIndex, 0	

ENDM ClnSearchMazes

; Changes AX, BX.
PositionToIndex MACRO XPos, YPos, outputIndex
	MOV AL, YPos
	MOV BL, MazeWidth
	MOV AH,0
	MUL BL
	MOV BL, XPos
	MOV BH, 0
	ADD AX, BX
	MOV outputIndex, AX
ENDM PositionToIndex