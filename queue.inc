; Initializes the queue.
InitQueue	MACRO
	MOV QueueHead, 0
	MOV QueueTail, 0
	MOV QueueNumElements, 0
ENDM InitQueue


; Put target to enqueue in "Target" variable.
; Changes AX and BX.
Enqueue MACRO
	LOCAL EndEnqeue
	MOV AX, Target
	MOV BX, QueueTail
	MOV [BX + Queue], AX
	ADD QueueTail, 2
	INC QueueNumElements
	CMP QueueTail, 310
	JBE EndEnqeue
	MOV QueueTail, 0
EndEnqeue:
	NOP
ENDM Enqueue

; Returns dequeue to "Target" variable
; Changes AX and BX
Dequeue MACRO
	LOCAL EndDeqeue
	MOV BX, QueueHead
	MOV AX, [BX+Queue]
	MOV Target, AX
	ADD QueueHead, 2
	DEC QueueNumElements
	CMP QueueHead, 310
	JBE EndDeqeue
	MOV QueueHead, 0
EndDeqeue:
	NOP
ENDM	Dequeue