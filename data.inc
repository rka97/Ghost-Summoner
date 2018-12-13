.MODEL HUGE     

.STACK 128

.DATA   
; Maze Info          
MazePath        DB "C:\media\maze.mp",00
MazeOutPath     DB "maze_out.mp",00
MazeWidth 		DB 	8D
MazeHeight 		DB 	8D
Maze 			DB	512 dup(0)

; First Summoner Info
Gandalf     LABEL    BYTE
GandalfX            DB  1 
GandalfY            DB  1
GandalfWidth        DB  1
GandalfHeight	    DB	1
GandalfHealth       DB  1
GandalfScore        DW  0
GandalfDir          DB  3
; Use no skill = 0, trap = 1, fire = 2
GandalfSkill        DB  0
GandalfSpeed        DB  1  ; The less the faster
GandalfCurSp        DB  0
GandalfVulnerable   DB  0
GandalfID           DB  0
GandalfNewDir       DB  0      

; Second Summoner Info
Triss       LABEL   BYTE
TrissX          DB  26
TrissY          DB  29
TrissWidth      DB  1
TrissHeight     DB  1
TrissHealth     DB  1
TrissScore      DW  0
TrissDir        DB  3
; Use no skill = 0, trap = 1, fire = 2, hell fire =3
TrissSkill      DB  0
TrissSpeed      DB  1   ; The less the faster  
TrissCurSp      DB  0
TrissVulernable DB  0
TrissID         DB  1
TrissNewDir     DB  0  

InvulnerabilityTime DB  10

; Gems Data
GemsRespawnTime DB  255
NumberOfGems    DW  0
; Byte for each block
; No gem = 0, Gem exists = 1, else time till respawning 
; The most significant bit signifies whether the gems is dirty or not
; A gem is dirty if it needs to be redrawn
GemsPath        DB "C:\media\gemsmap.mp",00
Gems            DB 4096 dup(0)

; To delay the respawn time of gems by a multiple of LatencyPeriod
Latency         DB  0
LatencyPeriod   DB  12

; Skills
; Traps
TrapCost        DB  15
MaxNumTraps     DB  25
CurrentNumTraps DB  0
TrapActiveTime  DB  255
TrapsData       DB  75 dup(0) 

; Fire                                
;(X, Y, active time, time till activation)                                
FireData        DB  120 dup(0)   
; Constants for fire
FireCost        DB  30
FireLength      DB  8

LastUpdateTime  DW  0  

; Ghosts Data  
MaxGhostNum         DB  20
CurrentGhostNum     DB  0
GhostTimeToSpawn    DB  2
SummonCircleX       DB  13
SummonCircleY       DB  15
GhostCost           DB  50
; X[0], Y[1], Owner[2] (0 = gandalf, 1 = triss),
; Time to spawn[3] (0 = not allocated, 1 = active, else = time), dir[4] 
Ghosts          DB  100 dup(0)   
 
GhostSpeed          DB  2
GhostCurSpeed       DB  0 
                           
                           
; ----------- Graphics Data -----------------
;---- Maze DATA
VMazePath        DB "C:\media\viewmaze.mp",00
VMazeEmpty      DB   ?
VMazeWidth 		DW 	8D
VMazeHeight 		DW 	8D
VMaze 			DB	20480 dup(0)
VMazeUnit       DB  2
VMazeFactor     DB  18
VWipeMatrix     DB  512 dup(0)  

;---- Triss DATA
VTrissPath1      DB  'C:\media\gandalf.mp',00
VTrissWidth     DB  19
VTrissHeight    DB  5 
VTrissUp1      DB   72 dup(0)
VTrissLeft1    DB   72 dup(0)
VTrissDown1    DB   72 dup(0)
VTrissRight1   DB   72 dup(0)
VTrissPath2      DB  'C:\media\gandalf2.mp',00
VTrissUp2      DB   72 dup(0)
VTrissLeft2    DB   72 dup(0)
VTrissDown2    DB   72 dup(0)
VTrissRight2   DB   72 dup(0)
VTrissPOSITION dw 130,19,9h
VTrissUnit    DB  1


;---- Gandalf DATA
VGandalfPath1     DB  'C:\media\triss.mp',00 
VGandalfWidth     DB  19
VGandalfHeight    DB  5 
VGandalfUp1      DB   72 dup(0)
VGandalfLeft1    DB   72 dup(0)
VGandalfDown1    DB   72 dup(0)
VGandalfRight1   DB   72 dup(0)
VGandalfPath2     DB  'C:\media\triss2.mp',00
VGandalfUp2      DB   72 dup(0)
VGandalfLeft2    DB   72 dup(0)
VGandalfDown2    DB   72 dup(0)
VGandalfRight2   DB   72 dup(0)
VGandalfPOSITION dw 636,525,5h 
VGandalfUnit      DB 1  

;---- Ghost DATA 
VGhostWidth    db ?
VGhostHeight   db ?
VGhost         db 150 dup('$')
VGhostPath     db 'C:\media\ghost.mp',00 

;---- Fire DATA
VFireWidth    db ?
VFireHeight   db ?
VFire         db 150 dup('$')
VFirePath     db 'C:\media\fire.mp',00  

;---- Trap DATA
VTrapWidth    db ?
VTrapHeight   db ?
VTrap         db 150 dup('$')
VTrapPath     db 'C:\media\trap.mp',00    
                            
;---- Gem DATA
VGemWidth     db ?
VGemHeight    db ?
VGem          db 100 dup ('$')
VGemPath      db 'C:\media\gem.mp',00  

;---- Colors
ColorsPath      DB "C:\media\colors.mp",00
Colors          LABEL   BYTE    
VMazeColor      DB  00000011b
VTrissColor     DB  ?
VGandalfColor   DB  ?
VGhostColor     DB  ?
VFireColor      DB  ?
VTrapColor      DB  ?
VGemColor       DB  11000000b 

;---- MainMenu DATA
MenuPath     DB 'C:\media\mainmenu.mp',00
MenuWidth    DB    ?
MenuHeight   DB    ?
MainMenu    DB    150 dup('$')
Result      DB  4 dup('$')
Player1     DB "TRISS $"
Player2     DB "GANDALF $"
Score       DB "TALLY: $"
Health      DB  "Health: $"

;---- EndScreen DATA

GandalfScreenPath DB 'C:\media\esc2.mp',00
EndScreenWidth 	  DB ?
EndScreenHeight   DB ?
GandalfScreen   DB 150 dup (1)

TrissScreenPath   DB 'C:\media\esc1.mp',00
EndScreenWidth2 DB ?
EndScreenHeight2 DB ?
TrissScreen     DB 150 dup (1)


;---- Screen DATA
ScreenX     DW 140
ScreenY     DW 20

SpriteCounter   DB  0

UNIT    DB   2 ; value of unit changes depending on the size of object to screen size. 
position2    dw ? ; to save the position of the first pixel of the object
LENGTH      DB  8
counter		DW	0
counter2    DW  0
counter3    DW  0 
Width       DW  0
Height      DW  0
                            
; Allocated Data for Procedures
DMPB                DB  15 dup(0)  
DMPW                DW  15 dup(0)  
Queue				DW	310D dup('$')
QueueHead			DW	0
QueueTail			DW	0
QueueNumElements	DW	0
Target				DW 	0
predecessorMatrix	DW	2048 dup(0FFFFh)
distanceMatrix		DW	2048 dup(0FFFFh)
visitedMatrix		DB	2048 dup(0)
adjacentIndex		DW	0101h
tempIndex			DW	0  
SpecialByte         DB  0
someConstant        DB  10000000b, 01000000b, 00100000b, 00010000b, 00001000b, 00000100b, 00000010b, 00000001b