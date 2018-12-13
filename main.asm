include "macros.inc"
include "queue.inc"
include "data.inc"
.CODE
MAIN PROC   FAR
	MOV AX, @DATA
	MOV DS, AX    
	
    Call FillData    
	
	MOV AH, 00h
    INT 1Ah 
    MOV LastUpdateTime, DX
    
    ;----Graphical mode 800x600
    MOV AX, 4F02H
    MOV BX, 103H
    INT 10h
    
    CALL DisplayMainMenu  
           
    ;---- Graphical mode 800x600 to clear screen
    MOV AX, 4F02H
    MOV BX, 103H
    INT 10h
    
	; Draw the maze
	MOV AX, VMazeWidth
    MOV [DMPW + 2], AX
    MOV AX, VMazeHeight
    MOV [DMPW + 4], AX
    MOV AL, VMazeUnit
    MOV UNIT, AL
    MOV AL, VMazeColor
    MOV [DMPB + 3], AL
    MOV AX, OFFSET VMaze
    MOV DMPW, AX
    MOV CX, ScreenX
    MOV DX, ScreenY
    CALL DrawObject
    
    GameLoop:
        ; First we listen for input
        CALL    GetInput
        ; Gandalf and Triss act upon it               	    
	    MOV     AX, OFFSET Gandalf
	    MOV     DMPW, AX
	    CALL    MakeMove
	    MOV     AX, OFFSET Triss
	    MOV     DMPW, AX
	    CALL    MakeMove
	    ; We update the status of ghosts and gems
	    CALL    UpdateGhosts
	    CALL    UpdateGems
	    ; Check gandalf and triss for if damaged
	    MOV     AX, OFFSET Gandalf
	    MOV     DMPW, AX
	    CALL    WasIShot
	    MOV     AX, OFFSET Triss
	    MOV     DMPW, AX
	    CALL    WasIShot
	    ; Update death spots (traps and fire)
	    CALL    UpdateDeathSpots
	    ; Update the screen
	    CALL    DisplayScreen
	    CALL    SystemPause
		; Check winning condition
		CMP GandalfHealth, 0
		JE GameEndGandalfLoses
		CMP TrissHealth, 0
		JE GameEndTrissLoses
	JMP GameLoop  
	
	GameEndTrissLoses:
	    MOV DMPW, OFFSET GandalfScreen
	    JMP ShowWinningScreen:
	GameEndGandalfLoses:
	    MOV DMPW, OFFSET TrissScreen
    
    ShowWinningScreen:
        CALL SystemPause
        CALL SystemPause
        CALL SystemPause
        CALL SystemPause
        CALL SystemPause
        ;---- Graphical mode 800x600 to clear screen
        MOV AX, 4F02H
        MOV BX, 103H
        INT 10h
        ;---- Show Winning Screen
    	MOV AL, EndScreenWidth
    	MOV AH, 0
    	MOV [DMPW + 2], AX
    	MOV AL, EndScreenHeight	
    	MOV AH, 0
    	MOV [DMPW + 4], AX
    	MOV UNIT, 18
    	MOV CX, 112
    	MOV DX, 12
    	MOV [DMPB + 3], 5
    	CALL DrawObject             
    	; Wait for a key.
    	MOV AH, 1
    	INT 21h
    	; Exit game.
    	MOV AH,4CH
    	INT 21H    
	HLT
MAIN ENDP   

GetInput PROC     
    ; UP -> AH = 72
    ; RIGHT -> AH = 77 
    ; DOWN -> AH = 80
    ; LEFT -> AH = 75 
    MOV     SI, OFFSET Gandalf
    MOV     DI, OFFSET Triss
    ADD     SI, 7
    ADD     DI, 7
    GetInputLoop:
    ; Check the buffer for pressed key
    MOV     AH, 1
    INT     16h     
    JZ      GetInputEnd
    ; Get the keystroke from the buffer
    MOV     AH, 00h 
    INT     16h
    
    CMP     AL, 'w'
    JE UPGANDALF
    CMP     AH, 72
    JE UPTRISS 
    JMP UPEND
    
    UPGANDALF:
        MOV [SI + 6], 1
        JMP GetInputLoop
    UPTRISS:
        MOV [DI + 6], 1
        JMP GetInputLoop 
    UPEND:              
    
    CMP     AL, 'a'
    JE LEFTGANDALF
    CMP     AH, 75
    JE LEFTTRISS
    JMP LEFTEND
    
    LEFTGANDALF:
        MOV [SI + 6], 2
        JMP GetInputLoop
    LEFTTRISS:
        MOV [DI + 6], 2
        JMP GetInputLoop       
    LEFTEND:                 
    
    CMP     AL, 's'
    JE DOWNGANDALF
    CMP     AH, 80
    JE DOWNTRISS
    JMP DOWNEND
    
    DOWNGANDALF:
        MOV [SI + 6], 3
        JMP GetInputLoop
    DOWNTRISS:
        MOV [DI + 6], 3
        JMP GetInputLoop    
    DOWNEND:                
    
    CMP     AL, 'd'
    JE RIGHTGANDALF
    CMP     AH, 77
    JE RIGHTTRISS
    JMP RIGHTEND
    
    RIGHTGANDALF:
        MOV [SI + 6], 4
        JMP GetInputLoop
    RIGHTTRISS:
        MOV [DI + 6], 4 
        JMP GetInputLoop           
    RIGHTEND:              
    
    CMP     AL, 't'
    JE  TRAPGANDALF
    CMP     AL, ','
    JE  TRAPTRISS
    JMP TRAPEND

    TRAPGANDALF:
        MOV [SI + 1], 1 ; 1 = TRAP
        JMP GetInputLoop
    TRAPTRISS:
        MOV [DI + 1], 1 ; 1 = TRAP
        JMP GetInputLoop          
    TRAPEND:                     
    
    CMP     AL, 'f'
    JE      FIREGANDALF
    CMP     AL, '/'
    JE      FIRETRISS
    JMP     FIREEND
    
    FIREGANDALF:
        MOV [SI + 1], 2 ; 2 = FIRE
        JMP GetInputLoop
    FIRETRISS:
        MOV [DI + 1], 2 ; 2 = TRAP
        JMP GetInputLoop    
    FIREEND:
    
    CMP     AL, 'q'
    JE      SUMMONGANDALF
    CMP     AL, '.'
    JE      SUMMONTRISS
    JMP     SUMMONEND
    
    SUMMONGANDALF:
        MOV [SI + 1], 3 ; 3 = Summon
        JMP GetInputLoop
    SUMMONTRISS:
        MOV [DI + 1], 3 ; 3 = Summon
        JMP GetInputLoop
    SUMMONEND:        
        
    JMP GetInputLoop
    GetInputEnd:
    RET
GetInput ENDP

; Input: X(DMPB[0]), Y(DMPB[1])
; Flags a certain spot as dirty to wipe it when redrawing
FlagDirty   PROC
    PUSHA
    MOV SI, OFFSET DMPB
    MOV DI, OFFSET Gems
    MOV AX, 0
    MOV AL, [SI + 1] ; AL = Y
    MUL MazeWidth ; AX = AL * MazeWidth
    ADD AL, [SI]
    ADC AH, 0
    ; AX = Y * Width + X
    ADD DI, AX
    MOV DL, [DI]
    OR DL, 10000000b
    MOV [DI], DL
    MOV DX, 0
    MOV BX, 8
    DIV BX
    ; AX = number of byte, DX = number of bit
    MOV DI, OFFSET VWipeMatrix
    MOV BX, AX
    ADD DI, BX
    MOV BL, [DI]
    MOV BH, 0
    ; BL has our byte
    MOV SI, OFFSET someConstant
    ADD SI, DX
    OR BL, [SI]
    MOV [DI], BL
    POPA
    RET          
FlagDirty   ENDP    
     
; Get X-Y bit from maze
; Input: X(DMPB[0]), Y(DMPB[1])
; Output: 0/1 in DMPB[5]
; Changes: CX, DX, AX, SI, DI   
GetXYFromMaze PROC     
    MOV SI, OFFSET DMPB
    MOV AX, 0
    MOV AL, [SI + 1] ; AL = Y
    MUL MazeWidth ; AX = AL * MazeWidth
    ADD AL, [SI]
    ADC AH, 0
    ; AX = Y * Width + X
    MOV DX, 0
    MOV BX, 8
    DIV BX
    ; AX = number of byte, DX = number of bit
    MOV DI, OFFSET Maze
    MOV BX, AX
    ADD DI, BX
    MOV BL, [DI]
    MOV BH, 0
    ; BL has our byte
    MOV CL, DL
    ADD CL, 1
    SHL BL, CL
    ; DX will have our bit
    MOV DX, 0
    ADC DX, 0
    MOV [SI + 5], DX  
    RET
GetXYFromMaze ENDP
         
; Input: DMPB[0] = x, DMPB[1] = y
TrapTheArea PROC
    MOV SI, OFFSET DMPB
    MOV DI, OFFSET TrapsData
    TrapTheAreaLoop:
        CMP [DI + 2], 0
        JE TrapItNow
        ADD DI, 3
        JMP TrapTheAreaLoop
    TrapItNow:
        MOV AL, [SI]
        MOV [DI], AL
        MOV AL, [SI + 1]
        MOV [DI + 1], AL
        MOV AL, TrapActiveTime
        MOV [DI + 2], AL
        RET    
TrapTheArea ENDP  

; Input: DMPB[0] = x, DMPB[1] = y, DMPB[4] = dir, DMPB[12] = owner 
Burn        PROC
    MOV SI, OFFSET DMPB
    MOV DI, OFFSET FireData
    MOV AL, 40
    MUL [SI + 12]
    ADD DI, AX
    MOV BL, [SI]
    MOV BH, [SI + 1]
    MOV CL, 0  
    BurnLoop:
        PUSHA
        CALL GetChildNode
        POPA
        CMP [SI + 5], 1
        JE BurnEnd
        MOV AL, [SI]
        MOV [DI], AL
        MOV AL, [SI + 1]
        MOV [DI + 1], AL
        MOV [DI + 2], CL
        MOV [DI + 3], CL
        ADD DI, 4  
        INC CL
        CMP CL, FireLength
        JB  BurnLoop
    BurnEnd:
    MOV [SI], BL
    MOV [SI + 1], BH    
    RET                
Burn        ENDP 

UpdateGhosts    PROC     
    MOV DI, OFFSET Ghosts
    MOV CL, MaxGhostNum
    MOV CH, 0  
    MOV SI, OFFSET DMPB
    UpdateGhostLoop:
        CMP [DI + 3], 0
        JE UpdateGhostNxt
        CMP [DI + 3], 1
        JE CheckGhostDamage
        DEC [DI + 3]
        JMP UpdateGhostNxt
        CheckGhostDamage:
            ; Set dmpb[0] = x and dmpb[1] = y
            MOV AL, [DI]
            MOV [SI], AL
            MOV AL, [DI + 1]
            MOV [SI + 1], AL
            
            CMP GhostCurSpeed, 0
            JNE DontFlagGhostDirty
                CALL FlagDirty
            DontFlagGhostDirty:
            ; check for damage
            PUSHA
            CALL WasITrapped
            POPA
            CMP [SI + 5], 1
            JE DieGhost
            PUSHA
            CALL WasIBurned
            POPA
            CMP [SI + 5], 1
            JNE UpdateGhostNxt 
        DieGhost:
            MOV [DI + 3], 0
            DEC CurrentGhostNum
		UpdateGhostNxt:
		    ADD DI, 5
            DEC CX
    JNZ UpdateGhostLoop
    
    CMP GhostCurSpeed, 0
    JNZ UpdateGhostCurSpeed:
    MOV AL, GhostSpeed
    MOV GhostCurSpeed, AL
   
    ADD SI, 12
    MOV AL, TrissX
    MOV [SI], AL
    MOV AL, TrissY
    MOV [SI + 1], AL
    CALL BFS
    MOV DL, GandalfID
    MOV DH, 2
       
    DoItAgain:
    MOV DI, offset Ghosts
	MOV CL, MaxGhostNum
	MOV CH, 0
	MageGhostLoop:
	    ; Check that the ghost is alive and well
		CMP [DI + 3], 1
		JNE NextMageGhost
		; Check that the ghosts belongs to the proper player
		CMP [DI + 2], DL ;
		JNE NextMageGhost
		MOV AL, [DI]
		MOV AH, [DI + 1]
		MOV Target, AX
		PUSHA   
		CALL ShortestPath
		POPA
		MOV AX, adjacentIndex
		MOV [DI], AL
		MOV [DI + 1], AH
		NextMageGhost:
			ADD DI, 5
			DEC CX
	JNZ MageGhostLoop  
	MOV SI, OFFSET DMPB
	ADD SI, 12
	MOV AL, GandalfX
	MOV [SI], AL
	MOV AL, GandalfY
	MOV [SI + 1], AL
	PUSH DX
	CALL BFS
	POP DX
	MOV DL, TrissID
	DEC DH
	JNZ DoItAgain             
    ENDIT:         
	RET	
	UpdateGhostCurSpeed:
	   DEC GhostCurSpeed 
	RET
UpdateGhosts    ENDP    

; X[0], Y[1], Owner[2] (0 = gandalf, 1 = triss),
; Time to spawn[3] (0 = not allocated, 1 = active, else = time), dir[4]  
; Input: Owner DMPB[12]
Summon      PROC
    MOV DI, OFFSET Ghosts
    MOV SI, OFFSET  DMPB
    ADD SI, 12 
    MOV CL, MaxGhostNum
    MOV CH, 0
    GetGhostLoop:
        CMP [DI + 3], 0
        JE GetGhostEnd
        ADD DI, 5
        DEC CX
        JZ NoSummoningToday
        JMP GetGhostLoop
    GetGhostEnd:
    MOV AL, SummonCircleX
    MOV [DI], AL
    MOV AL, SummonCircleY
    MOV [DI + 1], AL
    MOV AL, [SI]
    MOV [DI + 2], AL
    MOV AL, GhostTimeToSpawn
    MOV [DI + 3], AL
    MOV [DI + 4], 0
    INC CurrentGhostNum
    NoSummoningToday:
    RET
Summon      ENDP    
     
; Input: Creature Offset (DMPW[0])
MakeMove    PROC
    MOV DI, DMPW
    
    MOV SI, OFFSET DMPB  
    MOV AL, [DI]
    MOV [SI], AL
    MOV AL, [DI + 1]
    MOV [SI + 1], AL
    MOV AL, [DI + 2]
    MOV [SI + 2], AL
    MOV AL, [DI + 3]
    MOV [SI + 3], AL
    MOV AL, [DI + 12]
    MOV [SI + 12], AL 
    MOV AL, [DI + 7]
    MOV [SI + 4], AL         
    MOV AL, [DI + 12]
    MOV [SI + 12], AL
    
    CMP [DI + 10], 0        ; Check if cur speed = 0
    JNE MakeMoveUpdateSp     
    MOV AL, [DI + 9]
    MOV [DI + 10], AL       ; Update cur speed
       
    CMP [DI + 8], 1
    JE  PutTrap
    CMP [DI + 8], 2
    JE  BurnThemAll
    CMP [DI + 8], 3
    JE  SummonGhost
    JMP SkillEnd
    
    PutTrap:
        MOV AX, [DI + 5]
        MOV BL, TrapCost
        MOV BH, 0
        CMP AX, BX
        JA PutItAlready
        JMP SkillEnd
        PutItAlready:
            MOV AL, MaxNumTraps
            CMP AL, CurrentNumTraps
            JBE SkillEnd
            INC CurrentNumTraps
            MOV BL, TrapCost
            MOV BH, 0
            SUB [DI + 5], BX
            PUSHA
            CALL TrapTheArea
            POPA
    JMP SkillEnd
    
    BurnThemAll:
        MOV AX, [DI + 5]
        MOV BL, FireCost
        MOV BH, 0
        CMP AX, BX
        JA BurnThemAlready
        JMP SkillEnd
        BurnThemAlready:
            SUB [DI + 5], BX
            PUSHA
            CALL Burn
            POPA
    JMP SkillEnd
    
    SummonGhost:
        MOV AX, [DI + 5]
        MOV BL, GhostCost
        MOV BH, 0
        CMP AX, BX
        JA SummonAlready
        JMP SkillEnd
        SummonAlready:
            MOV AL, MaxGhostNum
            CMP AL, CurrentGhostNum
            JBE SkillEnd
            INC CurrentGhostNum
            SUB [DI + 5], BX
            PUSHA
            CALL Summon
            POPA
    JMP SkillEnd                
    
    SkillEnd:
    MOV [DI + 8], 0
    CMP [DI + 13], 0
    JE TryOldDirection
    MOV AL, [DI + 13]
    MOV [DMPB + 4], AL  
    CALL GetChildNode
    MOV DI, DMPW
    CMP [DMPB + 5], 1
    JE TryOldDirection
    MOV AL, [DI + 13]
    MOV [DI + 13], 0
    MOV [DI + 7], AL
    
    CheckDirty: 

    MOV BL, DMPB
    MOV BH, [DMPB + 1]
    MOV AL, [DI]
    MOV AH, [DI + 1]
    MOV DMPB, AL
    MOV [DMPB + 1], AH
    CALL FlagDirty
    MOV DMPB, BL
    MOV [DMPB + 1], BH
    MOV AL, DMPB
    MOV [DI], AL 
    MOV AL, [DMPB + 1]
    MOV [DI + 1], AL    
    CALL IsGemTaken
    RET  
    
    TryOldDirection:
        MOV AL, [DI]
        MOV DMPB, AL
        MOV AL, [DI + 1]
        MOV [DMPB + 1], AL
        MOV AL, [DI + 7]
        MOV [DMPB + 4], AL
        CALL GetChildNode
        MOV DI, DMPW
        CMP [DMPB + 5], 1
        JE THEEND
        JMP CheckDirty    
    THEEND:
        RET
    MakeMoveUpdateSp:
        DEC [DI + 10]
        RET
MakeMove    ENDP  

; Get the address of gem at X-Y
; Input: X(DMPB[0]), Y(DMPB[1])
; Output: Address of the gem at DMPW[2] and at DI
; Changes SI, DI, AX, DX
GetXYFromGems   PROC
    MOV SI, OFFSET DMPB
    MOV DI, OFFSET Gems
    ; Set AX to Y
    MOV AH, 0
    MOV AL, [SI + 1] 
    MOV AH, MazeWidth
    ; AX = Y * MazeWidth
    MUL AH
    MOV DL, [SI]
    MOV DH, 0
    ; AX = Y * MazeWidth + X
    ADD AX, DX
    ; Move DI to the proper location of the gem 
    ADD DI, AX
    MOV SI, OFFSET DMPW
    MOV [SI + 2], DI 
    RET
GetXYFromGems   ENDP    
      
; Input: Pointer to summoner info(DMPW[0])
; Checks if summoner took any gems
IsGemTaken  PROC
    ; CL = X, CH = Y
    MOV SI, DMPW
    MOV CL, [SI]
    MOV CH, [SI + 1]
    ; DL = X + Summoner Width, DH = Y + Summoner Height
    MOV BL, CL
    ADD BL, [SI + 2]
    MOV BH, CH
    ADD BH, [SI + 3]
    IsGemTakenYLoop:
        PUSH CX
        IsGemTakenXLoop:
            MOV DI, OFFSET DMPB
            ; Set DMPB[0] to X and DMPB[1] to Y 
            MOV [DI], CL
            MOV [DI + 1], CH 
            ; Get gems address in DI
            CALL GetXYFromGems
            MOV DL, 01111111b
            AND DL, [DI]
            CMP DL, 1
            JE IsGemTakenTaken
            JMP IsGemTakenNxt
            IsGemTakenTaken:
                MOV AL, GemsRespawnTime
                MOV [DI], AL
                MOV SI, DMPW
                MOV AX, [SI + 5]
                INC AX
                MOV [SI + 5], AX
            IsGemTakenNxt:
            INC CL
            CMP CL, BL
            JAE IsGemTakenXEnd
            JMP IsGemTakenXLoop
        IsGemTakenXEnd:
        POP AX
        MOV CL, AL
        INC CH
        CMP CH, BH
        JB IsGemTakenYLoop
    RET
IsGemTaken  ENDP

; Updates the remaining time for gems to respawn
UpdateGems  PROC
    CMP Latency, 0
    JNE UpdateLatency
    MOV SI, OFFSET Gems
    MOV AL, MazeWidth
    MUL MazeHeight
    UpdateGemsLoop:
        CMP [SI], 10000001b
        JA UpdateGem
        JMP UpdateGemEnd
        UpdateGem:
            DEC [SI]
        UpdateGemEnd:
        INC SI
        DEC AX
        JNZ UpdateGemsLoop
    MOV CL, LatencyPeriod    
    MOV Latency, CL    
    RET
    UpdateLatency:
    DEC Latency
    RET
UpdateGems  ENDP 

; Decrease the lifetime of each death spot
UpdateDeathSpots    PROC
    MOV SI, OFFSET TrapsData    
    MOV CX, 25
    UpdateTrapsLoop:
        CMP [SI + 2], 0
        JA UpdateTheDS
        JMP UpdateTheDSEnd
        UpdateTheDS:
            DEC [SI + 2]
            CMP [SI + 2], 0
            JE UpdateTheNUM
            JMP UpdateTheDSEnd
            UpdateTheNUM:
                DEC CurrentNumTraps
                MOV AL, [SI]
                MOV AH, [SI + 1]
                MOV DMPB, AL
                MOV [DMPB + 1], AH
                CALL FlagDirty  
        UpdateTheDSEnd:
        ADD SI, 3
        DEC CX
        JNZ UpdateTrapsLoop
    
    MOV SI, OFFSET FireData     
    MOV CX, 20
    UpdateFireLoop:
        CMP [SI + 3], 0
        JNE DecreaseTimeTillSpawn
        JMP TimeTillSpawnEnd
        DecreaseTimeTillSpawn:
            DEC [SI + 3]
            JMP FireLoopNxt
        TimeTillSpawnEnd:
        CMP [SI + 2], 0
        JE FireLoopNxt
        DEC [SI + 2]
        CMP [SI + 2], 0
        JNE FireLoopNxt
            MOV AL, [SI]
            MOV AH, [SI + 1]
            MOV DMPB, AL
            MOV [DMPB + 1], AH
            CALL FlagDirty
       FireLoopNxt:
       ADD SI, 4
       DEC CX
       JNZ UpdateFireLoop     
    RET   
UpdateDeathSpots    ENDP

; Input: X at DMPB[0], Y at DMPB[1]
; Output: WasITrapped at DMPB[5] (1 = yes, 0 = no)
; Also removes the trap if you were trapped
WasITrapped         PROC
    MOV SI, OFFSET TrapsData
    MOV DI, OFFSET DMPB              
    MOV CX, 25
    WasITrappedLoop:
        CMP [SI + 2], 0
        JE IWasTrappedEnd
        MOV AL, [SI]
        MOV AH, [SI + 1]
        MOV BL, [DI]
        MOV BH, [DI + 1]
        CMP AX, BX
        JE IWasTrapped
        JMP IWasTrappedEnd
        IWasTrapped:
            MOV [DI + 5], 1
            MOV [SI + 2], 0
            RET
        IWasTrappedEnd:
        ADD SI, 3
        DEC CX
    JNZ WasITrappedLoop
    MOV [DI + 5], 0
    RET    
WasITrapped         ENDP

; Input: X at DMPB[0], Y at DMPB[1]
; Output: WasITrapped at DMPB[5] (1 = yes, 0 = no) 
WasIBurned          PROC
    MOV SI, OFFSET FireData
    MOV DI, OFFSET DMPB
    MOV CX, 30
    WasIBurnedLoop:
        MOV AL, [SI]
        MOV AH, [SI + 1]
        MOV BL, [DI]
        MOV BH, [DI + 1]
        CMP AX, BX
        JNE IWasBurnedNxt
        CMP [SI + 3], 0
        JNE IWasBurnedNxt
        CMP [SI + 2], 0
        JE IWasBurnedNxt
            MOV [DI + 5],  1
            RET
        IWasBurnedNxt:
        ADD SI, 4
        DEC CX
    JNZ WasIBurnedLoop
    MOV [DI + 5], 0
    RET
WasIBurned          ENDP

; Input: Pointer to the creature at DMPW[0]
; Checks if the creature is standing on a death spot
; If so, decreases health
WasIShot            PROC
    MOV DI, DMPW 
    CMP [DI + 11], 0 ; Check if vulnerable
    JE CheckDamage
        DEC [DI + 11]
        RET 
    CheckDamage: 
     
    MOV SI, OFFSET DMPB
    MOV AL, [DI]
    MOV [SI], AL
    MOV AL, [DI + 1]
    MOV [SI + 1], AL
    ; First we check traps
    PUSHA
    CALL WasITrapped
    POPA
    CMP [SI + 5], 1
    JE IWasShot    
    ; Now we check fire
    PUSHA
    CALL WasIBurned
    POPA                        
    CMP [SI + 5], 1
    JE IWasShot
            
    ;Now we check ghosts
     MOV SI, OFFSET Ghosts
     MOV CL, MaxGhostNum
     MOV CH, 0
     WasIGhostedLoop:
        CMP [SI + 3], 0
        JE IWasGhostedNxt
        MOV AL, [DI + 12]
        CMP [SI + 2], AL
        JE  IWasGhostedNxt
        MOV AL, [SI]
        MOV AH, [SI + 1]
        MOV BL, [DI]
        MOV BH, [DI + 1]
        CMP AX, BX
        JE IWasGhosted
        JMP IWasGhostedNxt
        IWasGhosted:
            MOV [SI + 3], 0
            JMP IWasShot
        IWasGhostedNxt:
        ADD SI, 5
        DEC CX
        JZ WasIGhostedEnd
        JMP WasIGhostedLoop
     WasIGhostedEnd:
    RET
    IWasShot:
        DEC [DI + 4]
        MOV AL, InvulnerabilityTime
        MOV [DI + 11], AL 
        RET   
WasIShot            ENDP    

; Pauses the game for a clock tick (about fifth of a second)
SystemPause PROC  
    MOV AH, 00h
    SystemPauseLoop:
        INT 1Ah
        MOV BX, DX
        SUB BX, LastUpdateTime
        CMP BX, 2
        JB SystemPauseLoop
    MOV LastUpdateTime, DX
    RET   
SystemPause ENDP

; --------------------------------------------------------------- ;
; -----------------------------------------------------------------
; AI Code
; -----------------------------------------------------------------
; -----------------------------------------------------------------

; Requires BFS to have been done on Target! (i.e. the distanceMatrix should be properly filled!)
; Input: Target Position (Target), Current Position (CX)
; Output: New Position (adjacentIndex)
ShortestPath PROC
    MOV AX, Target
	MOV adjacentIndex, 0FFFFH
	MOV DL, 0
	LeastDistanceChildLoop:
		MOV BX, offset DMPB
		MOV CX, Target
		MOV [BX], CL
		MOV [BX+1], CH
		MOV [BX+4], DL
		PUSHA
		CALL GetChildNode
		POPA
		CMP [BX+5], 1
		JE ChildDistanceTest
    		MOV BX, offset DMPB   ; from child node
    		MOV CL, [BX]
    		MOV CH, [BX+1]
    		MOV word ptr tempIndex, CX
    		MOV AX, tempIndex
    		CMP adjacentIndex, 0FFFFH
		JE NoComparisonNeeded
    		PositionToIndex CL, CH, SI
    		MOV CX, word ptr adjacentIndex
    		PositionToIndex CL, CH, DI
    		MOV BX, offset distanceMatrix
    		ADD BX, SI
    		ADD BX, SI
    		MOV AX, [BX]
    		MOV BX, offset distanceMatrix
    		ADD BX, DI
    		ADD BX, DI
    		CMP AX, [BX] ; dM[tI] < dM[aI] ?
		JA ChildDistanceTest
		NoComparisonNeeded:
			MOV AX, tempIndex
			MOV adjacentIndex, AX
		ChildDistanceTest:
			INC DL
			CMP DL, 5
		JB LeastDistanceChildLoop
	RET
ShortestPath ENDP


; Gets the child Node
; Input: X(DMPB[0]), Y(DMPB[1]), Direction(DMPB[4])
; Direction: NOMOVE = 0, UP = 1, LEFT = 2, DOWN = 3, RIGHT = 4
; Output: IsLegal(DMPB[5]) ILLEGAL = 1, LEGAL = 0
; Changes DMPB[0] and DMPB[1] to the new coordinates
GetChildNode PROC
	MOV BP, offset DMPB
	MOV CL, DS:[BP] ; Put X in CL
	MOV CH, DS:[BP + 1] ; Put Y in CH
	; This procedure assumes Width = Height = 1.
	MOV DL, DS:[BP + 4]
	ChangingPositionCN:
		CMP DL, 1
		JNE NotUpCN
		CMP CH, 0
		JE NotALegalMoveCN
		SUB CH, 1
		JMP CheckNotBlockedCN
	NotUpCN:
		CMP DL,2
		JNE NotLeftCN
		CMP CL, 0
		JE NotALegalMoveCN
		SUB CL, 1
		JMP CheckNotBlockedCN
	NotLeftCN:
		CMP DL,3
		JNE NotDownCN
		MOV BL, MazeHeight
		SUB BL, 1
		CMP CH, BL
		JE NotALegalMoveCN
		ADD CH, 1
		JMP CheckNotBlockedCN
	NotDownCN:
		CMP DL, 4
		JNE CheckNotBlockedCN
		MOV BL, MazeWidth
		SUB BL, 1
		CMP CL, BL
		JE NotALegalMoveCN
		ADD CL, 1
	CheckNotBlockedCN:
		MOV DS:[BP], CL
		MOV DS:[BP + 1], CH
		CALL GetXYFromMaze	
		RET
	NotALegalMoveCN:
		MOV DS:[BP + 5], 1
		RET
GetChildNode ENDP 

; Does a Breadth-first Search.
; Input: Source Vertex X (DMPB[12]), Source Vertex Y (DMPB[13])
; Output: distanceMatrix containing the shortest distances from the source.
; Changes: all of the search mazes. BP. AX. BX.
BFS PROC
	; Initialize the queue.
	InitQueue
	; Clean the search mazes (we don't want it cluttering the way).
	ClnSearchMazes
	MOV BP, offset DMPB
	ADD BP,12
	MOV CL, DS:[BP] ; Store X coordinate in CL.
	MOV CH, DS:[BP+1] ; Store Y coordinate in CH.
	PositionToIndex CL, CH, SI
	
	MOV BP, offset visitedMatrix
	MOV DS:[BP + SI], 1H

	MOV BP, offset distanceMatrix
	ADD BP,SI
	MOV word ptr DS:[BP + SI], 0H
	
	MOV BP, offset predecessorMatrix
	ADD BP,SI
	MOV word ptr DS:[BP + SI], 0FFFFH
	
	MOV Target, CX
	Enqueue
	
	SearchLoop:
		CMP QueueNumElements,0
		JE EndBFS
		Dequeue ; dequeues node to "Target" variable.
		MOV DL,1
		AdjacentCheckLoop:				
				MOV CX, Target
				; Move data for get child node.
				; To check if the motion is legal.
				MOV BP, offset DMPB
				MOV DS:[BP], CL
				MOV DS:[BP + 1], CH
				MOV DS:[BP + 4], DL
				PUSHA
				CALL GetChildNode
				POPA
				CMP DS:[BP + 5], 1
			JE IllegalMove
				; Check if the node has been visited before.
				MOV CL,DS:[BP]
				MOV CH,DS:[BP+1]
				PositionToIndex CL, CH, SI
				MOV BP, offset visitedMatrix
				ADD BP, SI
				CMP DS:[BP], 0
			JNE IllegalMove
				MOV DS:[BP], 1
				PUSH CX
				PUSH SI
				MOV CX, Target
				PositionToIndex CL, CH, SI
				; distanceMatrix[child Node] = distanceMatrix[Target] + 1;
				MOV BP, offset distanceMatrix
				ADD BP, SI
				ADD BP, SI
				MOV AX, DS:[BP]
				ADD AX, 1
				POP SI
				POP CX
				MOV BP, offset distanceMatrix
				ADD BP, SI
				ADD BP, SI
				MOV DS:[BP], AX
				; predecessorMatrix[child Node] = Target;
				MOV BP, offset predecessorMatrix
				ADD BP, SI
				ADD BP, SI
				MOV AX, Target
				MOV DS:[BP], AX
				MOV Target, CX
				PUSH AX
				; Enqueue the child node (to process its children).
				Enqueue	
				POP AX           
				MOV Target, AX
			IllegalMove:
				INC DL
				CMP DL, 5
			JB AdjacentCheckLoop
		MOV CX, Target
		PositionToIndex CL, CH, SI
		MOV BP, offset visitedMatrix
		ADD BP, SI
		MOV DS:[BP], 2
		JMP SearchLoop
EndBFS:	
	RET
BFS ENDP

; Get which direction to walk in to go from source(X, Y) to target(X, Y)
; target(X, Y) MUST be at most ONE move away from Source.
; returns direction in DMPB[5].
GetDirection MACRO sourceX, sourceY, targetX, targetY
	LOCAL NotRightDir, NotLeftDir, NotUpDir, NotDownDir
	MOV SI, offset DMPB
	ADD SI, 5
	MOV [SI], 0
	MOV AL, sourceX
	MOV AH, sourceY
	MOV BL, targetX
	MOV BH, targetY
		CMP BL, AL
		JAE  NotLeftDir
		MOV [SI], 2 
	NotLeftDir:
		JBE NotRightDir
		MOV [SI], 4
	NotRightDir:
		CMP BH, AH
		JBE NotDownDir
		MOV [SI], 3
	NotDownDir:
		JAE NotUpDir
		MOV [SI], 1
	NotUpDir:
		NOP
ENDM GetDirection  
; -----------------------------------------------------------------
; -----------------------------------------------------------------
; Graphics Code
; -----------------------------------------------------------------
; -----------------------------------------------------------------

DisplayScreen       PROC
    CMP SpriteCounter, 3
    JNE DontResetCounter
    MOV SpriteCounter, 0
    DontResetCounter: 
    
    ; Clear Dirty stuff
    MOV SpecialByte, 1
    MOV SI, OFFSET VWipeMatrix
    MOV DMPW, SI
    MOV AL, MazeWidth
    MOV AH, 0
    MOV [DMPW + 2], AX
    MOV AL, MazeHeight
    MOV [DMPW + 4], AX
    MOV AL, VMazeFactor
    MOV UNIT, AL
    MOV [DMPB + 3], 0ffh
    MOV CX, ScreenX
    MOV DX, ScreenY
    CALL DrawObject
    MOV SpecialByte, 0
    
    MOV SI, OFFSET VWipeMatrix
    MOV AL, MazeWidth
    MOV AH, MazeHeight
    MUL AH
    MOV CL, 3
    SHR AX, CL
    
    ; Reset wipematrix
    ClearWipeMatrix:
        MOV [SI], 0
        INC SI
        DEC AX
    JNZ ClearWipeMatrix
    
    ; all the other shapes has a width of 24 and a height of 24
    MOV [DMPW + 2],  24
    MOV [DMPW + 4],  24     
    MOV UNIT, 1        
    
    ; Draw Gandalf
    ; Transform from Model to View   
    MOV AL, VGandalfColor
    MOV [DMPB + 3], AL
    MOV AL, GandalfX
    MOV AH, GandalfY
    MOV [DMPB + 8], AL
    MOV [DMPB + 9], AH
    MOV AL, VGandalfUnit
    MOV [DMPB + 10], AL
    CALL ModelToPixel
    MOV CX, [DMPW + 6]
    MOV DX, [DMPW + 8]
    SUB CX, 3
    SUB DX, 3
    CMP SpriteCounter, 1
    JA GandalfSecondSprite
    MOV BP, OFFSET VGandalfUp1
    JMP DrawGandalf
    GandalfSecondSprite:
    MOV BP, OFFSET VGandalfUp2
    DrawGandalf:
    MOV AL, GandalfDir
    SUB AL, 1
    MOV AH, 72
    MUL AH
    ADD BP, AX
    MOV DMPW, BP  
    CALL DrawObject
    
    ; Draw Triss
    ; Transform from Model to View
    MOV AL, VTrissColor
    MOV [DMPB + 3], AL
    MOV AL, TrissX
    MOV AH, TrissY
    MOV [DMPB + 8], AL
    MOV [DMPB + 9], AH
    MOV AL, VTrissUnit
    MOV [DMPB + 10], AL
    CALL ModelToPixel
    MOV CX, [DMPW + 6]
    MOV DX, [DMPW + 8]
    SUB CX, 3
    SUB DX, 3
    CMP SpriteCounter, 1
    JA TrissSecondSprite
    MOV BP, OFFSET VTrissUp1
    JMP DrawTriss
    TrissSecondSprite:
    MOV BP, OFFSET VTrissUp2
    DrawTriss:
    MOV AL, TrissDir
    SUB AL, 1
    MOV AH, 72
    MUL AH
    ADD BP, AX
    MOV DMPW, BP
    CALL DrawObject
    
    ; Draw Ghosts 
    MOV DI, OFFSET Ghosts
    MOV DMPW, OFFSET VGhost 
    MOV counter3, 20 
    DrawGhostLoop:
        CMP [DI + 3], 0
        JE DrawGhostNxt
        CMP [DI + 3], 1
        JNE DrawGhostNxt:
            ; Set dmpb[0] = x and dmpb[1] = y
            MOV AL, [DI]
            MOV AH, [DI + 1]
            MOV [DMPB + 8], AL
            MOV [DMPB + 9], AH
            CALL ModelToPixel
            MOV CX, [DMPW + 6]
            MOV DX, [DMPW + 8]
            CMP [DI + 2], 0
            JE GandalfColor:
                MOV AL, VTrissColor
                MOV [DMPB + 3], AL
                JMP ColorEnd
            GandalfColor:
                MOV AL, VGandalfColor
                MOV [DMPB + 3], AL
            ColorEnd:       
            CALL DrawObject
		DrawGhostNxt:
		    ADD DI, 5
            DEC counter3
    JNZ DrawGhostLoop 
    CALL UpdateScore 
     
    ; Draw Gems
    MOV AL, VGemColor
    MOV [DMPB + 3], AL
    MOV CX, ScreenX
    MOV DX, ScreenY 
    MOV AL, MazeWidth
    MOV AH, MazeHeight
    ; [DMPB] is counter x, [DMPB + 1] is counter y
    MOV [DMPB + 8], 0  
    MOV [DMPB + 9], 0
    MOV DMPW, OFFSET VGem
    MOV SI, OFFSET Gems
    DrawGemYLoop:
    PUSH CX
    DrawGemXLoop:
        CMP [SI], 10000001b
        JNE DontDrawGem
            CALL DrawObject
            XOR [SI], 10000000b    
        DontDrawGem:
        ADD CL, VMazeFactor
        ADC CH, 0
        INC SI
        INC [DMPB + 8]
        CMP [DMPB + 8], AL
        JB DrawGemXLoop
    POP CX
    MOV [DMPB + 8], 0
    INC [DMPB + 9]
    ADD DL, VMazeFactor
    ADC DH, 0
    CMP [DMPB + 9], AH
    JB DrawGemYLoop

    ; Draw Traps
    MOV AL, VTrapColor
    MOV [DMPB + 3], AL
    MOV DMPW, OFFSET VTrap 
    MOV SI, OFFSET TrapsData    
    MOV [DMPB + 14], 0
    DrawTrapsLoop:
        CMP [SI + 2], 0
        JE DrawNxtTrap
            MOV AL, [SI]
            MOV AH, [SI + 1]
            MOV [DMPB + 8], AL
            MOV [DMPB + 9], AH
            MOV [DMPB + 10], 1
            CALL ModelToPixel
            MOV CX, [DMPW + 6]
            MOV DX, [DMPW + 8]
            CALL DrawObject      
        DrawNxtTrap:
        ADD SI, 3
        INC [DMPB + 14]
        CMP [DMPB + 14], 20
    JB DrawTrapsLoop
    
    ; Draw Fire
    MOV AL, VFireColor
    MOV [DMPB + 3], AL
    MOV DMPW, OFFSET VFire
    MOV SI, OFFSET FireData
    MOV [DMPB + 8], 0
    DrawFireLoop:
        CMP [SI + 2], 0
        JE DrawNxtFire
        CMP [SI + 3], 0
        JNE DrawNxtFire
            MOV AL, [SI]
            MOV AH, [SI + 1]
            MOV [DMPB + 8], AL
            MOV [DMPB + 9], AH
            CALL ModelToPixel
            MOV CX, [DMPW + 6]
            MOV DX, [DMPW + 8]
            CALL DrawObject
        DrawNxtFire:
        ADD SI, 4
        INC [DMPB + 8]
        CMP [DMPB + 8], 20
    JB DrawFireLoop
    INC SpriteCounter
    RET    
DisplayScreen ENDP

;------Draw String
; Draws a string at (CX, DX)
; Input: X(CX), Y(DX), Data(DMPW[0]), Width(DMPW[2]), Height(DMPW[4]), Factor(UNIT), Color(DMPB[3])
DrawObject PROC Near  
    PUSHA
    PUSH CX
    PUSH DX    
    ; Set Width counter
    MOV AX, [DMPW + 2]
    MOV CL, 3
    SHR AX, CL
    MOV AH, 0
    MOV counter2, AX
    ; Set height counter
    MOV AX, [DMPW + 4]
    MOV BH, 00
    POP DX
    POP CX
    MOV [DMPW + 14], CX
    MOV BP, DMPW   
    MazeLoop:
        PUSH AX
        MOV AX, counter2 
        DrawXLoop:
            MOV BH,DS:[BP]
            CALL DrawByte
            INC BP
            DEC AX
            CMP AX, 0
        JNZ DrawXLoop
        POP AX 
        MOV CX, [DMPW + 14]
        ADD DL, UNIT
        ADC DH, 0  
        DEC AX
        CMP AX,0
    JA MazeLoop         
    POPA
    RET         
DrawObject ENDP

; Input: X at DMPB[8], Y at DMPB[9]
; Output: Pixel X at DMPW[6], Pixel Y at DMPW[8]
ModelToPixel        PROC
    MOV AL, [DMPB + 8]
    MOV AH, 0
    MUL VMazeFactor
    ADD AX, ScreenX
    MOV [DMPW + 6], AX    
    MOV AL, [DMPB + 9]
    MOV AH, 0
    MUL VMazeFactor
    ADD AX, ScreenY
    MOV [DMPW + 8], AX    
    RET
ModelToPixel        ENDP    

; Draws a square with side length of UNIT at (CX, DY)
; using a color of DMPB[3]     
DrawRectangle PROC NEAR 
    PUSHA
    MOV AL, [DMPB + 3]
    MOV AH, 0CH
    MOV BL, UNIT
    MOV BH, 0
    THICKLOOP:
        PUSH CX 
        PUSH BX
        MOV BL, UNIT
        MOV BH, 0
        BACKH:
            INT 10H
            INC CX
            DEC BX
            CMP BX,0
            JNE YH
            JE M
        YH: 
            CMP CX,689
            JNE BACKH
        M:
        POP BX
        POP CX  
        INC DX
        DEC BX
        CMP BX,0
    JNZ THICKLOOP
    POPA     
    RET
DrawRectangle ENDP

;---- This procedure draws a byte by shifting and checking on carry flag
DrawByte Proc NEAR
    CMP SpecialByte, 1
    JE UseSpecialByte   
    PUSH AX
    PUSH BX
    MOV AL,0   
    LENGTHLOOP:
        SHL BH, 1
        ADC AL, 0
        CMP AL, 0
        JNE DRAW 
        JE NO
        DRAW:
            CALL DrawRectangle 
            JMP NO
        NO:
        ADD CL,UNIT
        ADC CH, 0
        MOV AL,0
        DEC LENGTH  
    JNZ LENGTHLOOP
    MOV LENGTH,8 
    POP BX
    POP AX
    RET
    UseSpecialByte:
        CALL DrawSpecialByte
        RET
DrawByte ENDP

; Draws a byte in special way designed for the WipeMatrix
DrawSpecialByte PROC 
    PUSH AX
    PUSH BX
    MOV AL,0 
    MOV UNIT, 24  
    LENGTHSPECIALLOOP:
        SHL BH, 1
        ADC AL, 0
        CMP AL, 0
        JNE DRAWSPECIAL
        JE NOSPECIAL
        DRAWSPECIAL:
            SUB CX, 3
            SUB DX, 3
            CALL DrawRectangle
            ADD CX, 3
            ADD DX, 3 
            JMP NOSPECIAL
        NOSPECIAL:
        ADD CL, 18
        ADC CH, 0
        MOV AL,0
        DEC LENGTH  
    JNZ LENGTHSPECIALLOOP
    MOV LENGTH,8
    MOV UNIT, 18  
    POP BX
    POP AX
    RET
DrawSpecialByte ENDP

UpdateScore PROC NEAR
    ; set Cursor
    MOV AH, 02
    MOV BH, 00
    MOV DL, 1
    MOV DH, 2
    INT 10h   
    MOV AH, 9
    
    mov dx,offset Player1 
    int 21h  
    
    ; set cursor
    mov ah,02
    mov bh,00
    mov dl,1
    mov dh,4
    int 10h
    
    ; print player1's "score"
    mov ah, 9
    
    mov dx,offset score 
    int 21h   
    
    mov ah,02
    mov bh,00
    mov dl,1
    mov dh,6
    int 10h 
    
    ;division for printing
    mov ax,GandalfScore
    mov bl,10
    div bl 
 
    mov si,offset Result
    mov [si+2],ah
    add [si + 2], '0'
    mov ah, 0
    div bl
    mov [si+1], ah
    add [si + 1], '0'
    mov [si], al
    add [si], '0'    
    
    mov ah, 9
    mov DX,OFFSET Result
    int 21H 
    ;---- same goes for player 2
    
    mov ah,02
    mov bh,00
    mov dl,91
    mov dh,2
    int 10h 
    
    mov ah, 9
    mov dx,offset Player2 
    int 21h
    
    mov ah,02
    mov bh,00
    mov dl,91
    mov dh,4
    int 10h
    
    mov ah, 9
    mov dx,offset Score 
    int 21h
    
    mov ah,02
    mov bh,00
    mov dl,91
    mov dh,6
    int 10h
    
    ;division for printing
    mov ax,TrissScore
    mov bl,10
    div bl 
 
    mov si,offset Result
    mov [si+2],ah
    add [si + 2], '0'
    mov ah, 0
    div bl
    mov [si+1], ah
    add [si + 1], '0'
    mov [si], al
    add [si], '0'
    mov ah, 9
    mov DX,OFFSET Result
    int 21H    
    RET    
UpdateScore endp  

DisplayMainMenu     PROC
    MOV AL, MenuWidth
    MOV AH, 0
    MOV [DMPW + 2], AX
    MOV AL, MenuHeight
    MOV AH, 0
    MOV [DMPW + 4], AX
    MOV DMPW, OFFSET MainMenu
    MOV UNIT, 18
    
    LABEL:
    MOV CX, 112
    MOV DX, 12
    CALL DrawObject
    INC [DMPB + 3]
    
    MOV AX, 3
    INT 33h
    CMP CX, 130
    JAE INSIDE2
    JB LABEL
    INSIDE2:
    cmp CX, 670
    jB INSIDE3
    jAE LABEL
    INSIDE3:
    cmp DX, 20
    jAE INSIDE4
    jB LABEL
    INSIDE4:
    cmp DX ,190
    JBE PLAY2
    jA LABEL
    PLAY2:
    cmp bx,1
    jne LABEL
    RET    
DisplayMainMenu     ENDP    

; -----------------------------------------------------------------
; -----------------------------------------------------------------
; IO Code
; -----------------------------------------------------------------
; -----------------------------------------------------------------
; Input: DX = Offset FilePath, SI = Offset destination
ReadTheWholeFile    PROC
    MOV AH,3DH
	MOV AL,0
	INT 21H
	MOV BX,AX ; The File handle is now in DI
	; Load the data
	MOV AH, 3FH
	MOV CX, 0FFFFh
	MOV DX, SI
	INT 21H
	; Closes the file
	MOV AH,3EH
	INT 21H
	RET    
ReadTheWholeFile    ENDP

FillData            PROC
    ; Fill Data
    MOV DX, OFFSET VMazePath
    MOV SI, OFFSET VMazeEmpty
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET VGandalfPath1
    MOV SI, OFFSET VGandalfWidth
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET VTrissPath1
    MOV SI, OFFSET VTrissWidth
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET VGandalfPath2
    MOV SI, OFFSET VGandalfUp2
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET VTrissPath2
    MOV SI, OFFSET VTrissUp2
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET GemsPath
    MOV SI, OFFSET Gems
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET ColorsPath
    MOV SI, OFFSET Colors
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET VGhostPath
    MOV SI, OFFSET VGhostWidth
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET VFirePath
    MOV SI, OFFSET VFireWidth
    CALL ReadTheWholeFile
                                                      
    MOV DX, OFFSET VTrapPath
    MOV SI, OFFSET VTrapWidth
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET VGemPath
    MOV SI, OFFSET VGemWidth
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET MazePath
    MOV SI, OFFSET MazeWidth
    CALL ReadTheWholeFile
    
    MOV DX, OFFSET MenuPath
    MOV SI, OFFSET MenuWidth
    CALL ReadTheWholeFile
    
    MOV DX,OFFSET GandalfScreenPath
    MOV SI,OFFSET EndScreenWidth
    CALL ReadTheWholeFile 
    
    MOV DX,OFFSET TrissScreenPath
    MOV SI,OFFSET EndScreenWidth2
    CALL ReadTheWholeFile
    RET    
FillData            ENDP  

END