TITLE Snake 			(Snake.asm) 

; Program Description: Basic Snake Video Game simulator
; Snake will be controlled with keys J and K. 
; Author: Ishaq Sloan   
; Date: 5/15/2017

INCLUDE Irvine32.inc
INCLUDE Macros.inc

.data
playStartX BYTE 15  ;starting x coordinate 
playStartY BYTE 17  ;starting y coordinate
playerLocationX BYTE ? ;dynamic x coordinate  
playerLocationY BYTE ?	;dynamic y coordinate
foodTuple BYTE ?,?
snakeTail DWORD 1225 DUP(?) ;array of coordinates: (x,y),(x,y),(x,y)...
;there is enough space for tail to fill snake play area. 

Snake BYTE 'S'     	;snake's head 
Wall1 BYTE '_'
Wall2 BYTE '|'
 
clockwise BYTE 107  ; rotate clockwise 'k'
counterclock BYTE 106 ;rotate counter clockwise 'j'
outOfBoundsMax BYTE 35
outOfBoundsMin BYTE 0
GAMEOVERFLAG BYTE 0

GOstring BYTE "GAME OVER.",0
livesleft BYTE "LIVES LEFT: ",0
yourScore BYTE "YOUR SCORE: ",0

gameClock DWORD 250
Lives BYTE 1
Score BYTE 0 
   
.code
main PROC
  START:
	call  WaitMsg  ;push to start 
	call  Clrscr  ;clearscreen and start game
	;----------start boundary creation----------------- 
	mov   dl,outOfBoundsMin
	mov   dh,outOfBoundsMin 
  BOUNDSTop:
	cmp   dl,outOfBoundsMax
	je    BOUNDSLeftRight
	inc   dl
	call  gotoXY
	mov   eax,10
	call  delay	  
	mov   al,Wall1
	call  WriteChar
	jmp    BOUNDSTop
  BOUNDSLeftRight:
	mov   dl,outOfBoundsMin
	cmp   dh,outOfBoundsMax
	je 	  BOUNDSBottom
	inc   dh
	call  gotoXY
	mov   eax,10
	call  delay
	mov   al,Wall2
	call  WriteChar
	mov   bl,outOfBoundsMax
	xchg  dl,bl
	call  gotoXY
	mov   al,Wall2
	call  WriteChar
	xchg  dl,bl
	jmp   BOUNDSLeftRight
  BOUNDSBottom:
	cmp   dl,outOfBoundsMax
	je    PLAY
	inc   dl
	call  gotoXY
	mov   eax,10
	call  delay
	mov   al,Wall1
	call  WriteChar
	jmp   BOUNDSBottom 
	;--------end boundary creation------------
  PLAY:
	;ESI Register is the front of snakeTail Queue
	;EDI REgister is the back of snakeTail Queue 
	mov   esi,OFFSET snakeTail ;top of queue  (for TAIL CREATOR)
	mov   edi,OFFSET snakeTail ;back of queue (for TAIL CREATOR)
	 
	call  food				;generate food
	mov   eax,500
	call  delay 			
	mov   dl,playStartX 	;set x coordinate for start
	mov   dh,playStartY		;set y coordinate for start
	call  gotoXY 			;call cursor to coordinate 
	mov   al,Snake 			
	call  WriteChar			;draw snake character
	mov	  playerLocationX,dl
	mov   playerLocationY,dh
      UP:
	;--------------ASSERT SNAKE HEAD-----------------
	call  gotoXY				;mov back to previous coordinate 
	mov   al,32					;place empty space 
	call  WriteChar
	;--------------ASSERT SNAKE TAIL-----------------
	push  edx
	call  TailUpdater        ;moves space char where tail was 
	pop   edx
	;---------------Tail data Prep-------------------
	mov   bl,dl
	mov   bh,dh		
	mov   [esi],ebx ;push coordinates in snakeTail Queue
	;---------------Move Snake Head----------------------
	dec   playerLocationY		;decrease y coordinate 
	mov   dh,playerLocationY
	call  gotoXY			;place cursor at new coordinate 
	mov   al,Snake
	call  WriteChar			;write character to window
	;----------------Create Tail-------------------------
	push  edx				;save current coordinates
	call  TailCreator
	pop   edx				;restore current coordinates
	;---------------Check Collision Flag----------------
	cmp   GAMEOVERFLAG,0
	jne   GAMEOVER
	;---------------End Collision Flag Check-------------
	push  edx 			
	call  TailMapper  ;data arrangement for snakeTail     
	pop   edx 
	;Check if snake is out of bounds here OR making collision 
	;If no, continue, if yes, jump to GAMEOVER.
	cmp   dh,outOfBoundsMax
	jge   GAMEOVER
	cmp   dh,outOfBoundsMin
	je	  GAMEOVER
	cmp	  dl,outOfBoundsMax
	jge   GAMEOVER
	cmp   dl, outOfBoundsMin
	je    GAMEOVER
	;-------end out of bounds check-----------------
	push  edx 
	call  CheckFood		;ActionListener for Food 
	pop   edx 
	mov   eax,gameClock
	call  delay
	push  edx		;push player coordinates 
	call  ReadKey	;check for key press 
	pop   edx 		;pop player coordinates 
	jz    UP		;if no key pressed, keep moving straight 
	cmp   al,counterclock
	je    LEFT		
	cmp   al,clockwise
	je    RIGHT
	cmp   al,0 
      LEFT:
	;--------------ASSERT SNAKE HEAD-----------------
	call  gotoXY				;mov back to previous coordinate 
	mov   al,32					;place empty space 
	call  WriteChar
	;--------------ASSERT SNAKE TAIL-----------------
	push  edx
	call  TailUpdater
	pop   edx
	;---------------Tail data Prep-------------------
	mov   bl,dl
	mov   bh,dh		;ebx holds head address before movement of snakehead
	mov   [esi],ebx
	;---------------Move Snake Head----------------------
	dec   playerLocationX
	mov   dl,playerLocationX
	call  gotoXY
	mov   al,Snake
	call  WriteChar
	;----------------Create Tail-------------------------
	;call  DumpRegs			;DEBUG
	push  edx				;save current coordinates
	call  TailCreator
	pop   edx				;restore current coordinates
	;---------------Check Collision Flag----------------
	cmp   GAMEOVERFLAG,0
	jne   GAMEOVER
	;---------------End Collision Flag Check-------------
	push  edx 
	call  TailMapper
	pop   edx 
	;Check if snake is out of bounds here OR making collision 
	;If no, continue, if yes, jump to GAMEOVER.
	cmp   dh,outOfBoundsMax
	jge   GAMEOVER
	cmp   dh,outOfBoundsMin
	je	  GAMEOVER
	cmp	  dl,outOfBoundsMax
	jge   GAMEOVER
	cmp   dl, outOfBoundsMin
	je    GAMEOVER
	;--------------end out of bounds check------------------------
	push  edx 
	call  CheckFood
	pop   edx
	mov   eax,gameClock
	call  delay
	push  edx
	call  ReadKey
	pop   edx 
	jz    LEFT
	cmp   al,counterclock 
	je    DOWN
	cmp   al,clockwise
	je    UP 
     DOWN:
	;--------------ASSERT SNAKE HEAD-----------------
	call  gotoXY				;mov back to previous coordinate 
	mov   al,32					;place empty space 
	call  WriteChar
	;--------------ASSERT SNAKE TAIL-----------------
	push  edx
	call  TailUpdater
	pop   edx
	;---------------Tail data Prep-------------------
	mov   bl,dl
	mov   bh,dh		;ebx holds head address before movement of snakehead
	mov   [esi],ebx
	;---------------Move Snake Head----------------------
	inc   playerLocationY
	mov   dh,playerLocationY
	call  gotoXY
	mov   al,Snake
	call  WriteChar
	;----------------Create Tail-------------------------
	;call  DumpRegs			;DEBUG
	push  edx				;save current coordinates
	call  TailCreator
	pop   edx				;restore current coordinates
	;---------------Check Collision Flag----------------
	cmp   GAMEOVERFLAG,0
	jne   GAMEOVER
	;---------------End Collision Flag Check-------------
	push  edx 
	call  TailMapper
	pop   edx 
	;Check if snake is out of bounds here OR making collision 
	;If no, continue, if yes, jump to GAMEOVER.
	cmp   dh,outOfBoundsMax
	jge   GAMEOVER
	cmp   dh,outOfBoundsMin
	je	  GAMEOVER
	cmp	  dl,outOfBoundsMax
	jge   GAMEOVER
	cmp   dl, outOfBoundsMin
	je    GAMEOVER
	;------------------end out of bounds check-----------------------
	push  edx 
	call  CheckFood
	pop   edx 
	mov   eax,gameClock
	call  delay
	push  edx 
	call  ReadKey
	pop   edx 
	jz 	  DOWN 
	cmp   al,counterclock
	je    RIGHT
	cmp   al,clockwise
	je    LEFT
     RIGHT:
	;--------------ASSERT SNAKE HEAD-----------------
	call  gotoXY				;mov back to previous coordinate 
	mov   al,32					;place empty space 
	call  WriteChar
	;--------------ASSERT SNAKE TAIL-----------------
	push  edx
	call  TailUpdater
	pop   edx
	;---------------Tail data Prep-------------------
	mov   bl,dl
	mov   bh,dh		;ebx holds head address before movement of snakehead
	mov   [esi],ebx
	;---------------Move Snake Head----------------------
	inc   playerLocationX
	mov   dl,playerLocationX
	call  gotoXY
	mov   al,Snake
	call  WriteChar
	;----------------Create Tail-------------------------
	;call  DumpRegs			;DEBUG
	push  edx				;save current coordinates
	call  TailCreator
	pop   edx				;restore current coordinates
	;---------------Check Collision Flag----------------
	cmp   GAMEOVERFLAG,0
	jne   GAMEOVER
	;---------------End Collision Flag Check-------------
	push  edx 
	call  TailMapper
	pop   edx 
	;Check if snake is out of bounds here OR making collision 
	;If no, continue, if yes, jump to GAMEOVER.
	cmp   dh,outOfBoundsMax
	jge   GAMEOVER
	cmp   dh,outOfBoundsMin
	je	  GAMEOVER
	cmp	  dl,outOfBoundsMax
	jge   GAMEOVER
	cmp   dl, outOfBoundsMin
	je    GAMEOVER
	;----------------end out of bounds check-------------------
	push  edx 
	call  CheckFood
	pop   edx 
	mov   eax,gameClock
	call  delay
	push  edx
	call  ReadKey
	pop   edx 
	jz    RIGHT
	cmp  al,counterclock
	je   UP
	cmp  al,clockwise
	je   DOWN
GAMEOVER: ;Prompt GAMEOVER Messages 
	dec   lives
	mov   edx, OFFSET livesleft
	movzx eax, lives 
	call  WriteString ;prep lives 
	call  WriteDec	;show lives left
	call  Crlf
	movzx eax,Score
	mov   edx,OFFSET yourScore
	call  WriteString
	call  WriteDec
	mov   eax,5000
	call  delay     ;give user time to read 
	cmp   lives,0 
	jg    START
	call  Clrscr
	mov   dh,15
	mov   dl,15
	call  gotoXY
	mov   edx,OFFSET GOstring
	call  WriteString
	mov   eax,3000
	call  delay 
	exit 
main ENDP
;------------creating RandomXY Procedure------------
;RandomXY procedure will create 2 random numbers
;This Procedure will use eax registers for RandomRange PROC
;This Procedure will also use the lower half of edx register 
; to designate X and Y axis for gotoXY to use.  
Food PROC
	push eax
	push edx
	push ebx 
	mov  ebx,OFFSET foodTuple
	call  Randomize
	movzx eax,outOfBoundsMax
	sub   eax,3
	call  RandomRange
	add   al,1
	mov   [ebx],al 
	mov   dl,al
	add   ebx, TYPE foodTuple
	movzx eax,outOfBoundsMax
	sub   eax,3
	call  RandomRange
	add   al,1
	mov   [ebx],al
	mov   dh,al
	call  gotoXY
	mov  al,42			;'*' is food 
	call WriteChar
	pop eax
	pop ebx
	pop edx 
	ret
Food ENDP
;----------Food Action Listener aka CHECKFOOD ---------------
;will compare current coordinates with stored food coordinates 
;courtesy of the FOOD Procedure. If coordinates match this proc
;will add 1 to score, decrease gameClock(increase game speed),
;add extra memory for allocation in snakeTail, and call the 
;Food procedure again to generate more food for snake. 
CheckFood PROC
	push  ebx
	push  eax
	mov   eax,0 
	mov   ebx,OFFSET foodTuple
	mov   ah,[ebx]
	cmp   dl,ah
	jne   DONE
	add   ebx,TYPE foodTuple
	mov   al,[ebx]
	cmp   dh,al
	jne   DONE
	inc   Score				;increase score 
	mov   eax,gameClock
	cmp   eax,25				;see if gameClock can be reduced further
	je    TopSpeed
	sub   eax,25			;increase game speed
  TopSpeed:
	mov   gameClock,eax
	add   edi, TYPE snakeTail ;add extra space for address keeping
	call  food				;regenerate food 
  DONE:
	pop   eax 
	pop   ebx
	ret
CheckFood ENDP
;---------------------TAILMAPPER PROC--------------------
;This Procedure will push coordinates down into the snakeTail
;Queue and release the oldest coordinate. 
TailMapper PROC
	push  ecx 
	push  ebx
	push  eax 
	;------------------------------------------------
	;------------------------------------------------
	cmp   Score,0		
	je    DONE2		;when score = 0 there is no need to move memory
	jg    PUSHMEM	;score > 0, use push memory loop 
	cmp   edi,esi 
	je    DONE2     ;if head and tail of queue are equal,reset tail 
  PUSHMEM:
	movzx ecx,Score 			;set loop for mem push
  MEMPUSH:
	mov   ebx,[edi-4]		;ebx = edi->back 
	mov   [edi],ebx			;edi = ebx, which overwrites old coordinate 
	sub   edi,TYPE snakeTail;edi = edi->back 
	loop  MEMPUSH			;loop in relationship with score
  DONE2:
	mov   ebx,OFFSET snakeTail ;prep comparison for start of the array
	movzx eax,Score 		   ;move score to eax 
	imul  eax,TYPE snakeTail   ;eax holds product of (score*4)
	add   ebx,eax ;move product to ebx which holds hex of last queue
	mov   edi,ebx ;restore EDI to last  
	;------------------------------------------------
	;------------------------------------------------
	pop eax 
	pop ebx 
	pop ecx 
	ret 
TailMapper ENDP
;---------------TAILCREATOR PROC--------------------------------
;This procedure will print the snakeTail queue as long as 
;the queue "has next" element. While printing, the proc will
;also check to see if the snake head has the same coordinate as 
;some of the items it is printing, which indicates a collision.
;A proof of collision will set the GAMEOVERFLAG and the game will 
;end once this procedure is complete.
;YOU MUST PUSH EDX BEFORE AND POP EDX AFTER YOU CALL THIS PROC
;OR ELSE YOU WILL CORRUPT SNAKE HEAD COORDINATES  
TailCreator PROC  
	push  ecx
	push  eax
	push  ebx
	cmp   Score,0
	je    NOTAIL   ;if score is 0 there should be no tail
  HASNEXT1:
	cmp   esi,edi		;check if first == last of snakeTail queue 
	je    NOTAIL		;if true, then there is no Tail 
	movzx ecx,Score 		;prep loop printing 
  PRINTLOOP:
	mov   ebx,[esi]			;try printing forward now!
	;---------insert a quick collision check here---------
	;Note: Snake cannot hit itself unless it has 3 tails or more
	cmp   Score,3		
	jng   CANTCOLLIDE   ;if score >3 check for possible collision 
	pop   edx 			;restore current head coordinates
	push  eax
	push  ecx
	mov   eax,[esi+12];check for collision from snakeTail[3] or more
	movzx ecx,Score 
	sub   ecx,3    ;set loop for checking
	;compare current head coordinates to coordinates in queue 
  CalcCollision:	;(dh == ah && dl == al) for this to be true
	cmp   dh,ah
	jne   NOTOUCH
	cmp   dl,al
	jne   NOTOUCH 
	inc   GAMEOVERFLAG
	loop  CalcCollision
  NOTOUCH:
	pop   ecx 
	pop   eax
	push  edx 
  CANTCOLLIDE:
	;---------end collision check-------------------------
	mov   dh,bh 
	mov   dl,bl
	call  gotoXY			;go to coordinate  
	mov   al,'o'
	call  WriteChar			;print tail piece 
	add   esi,TYPE snakeTail;move cursor back
	jmp   HASNEXT1
  NOTAIL:
	mov    esi,OFFSET snakeTail ;set esi back to first  
	pop   ebx 
	pop   eax
	pop   ecx
TailCreator ENDP

;TailUpdater will place an empty space where the tail used to be
TailUpdater PROC 
	push  eax
	push  ecx 
	mov   edx,[edi] ;should place an empty space in the oldest location
	call  gotoXY
	mov   al,32
	call  WriteChar
	pop   ecx 
	pop   eax
	ret
TailUpdater ENDP 
END main	 