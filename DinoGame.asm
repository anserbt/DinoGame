[org 0x0100]
jmp start
spacecheck: dw 0
msg2: db 'Game Over'
msg: db 'Score: '
oldisr2: dw 0,0
oldisr: dw 0,0
factor: dw 0
dconst: dw 20
dinotime: dw 0
dinopos: dw 0 ;jump = 1
score: dw 0 ;Initial = 0
cactuspos: dw 3024 ;Max Vals = 2880+148, 1920+148
type: dw 0 ;Obstacle Type bird = 1
dtime: dw 20 ;Delay for Obstacle
gameon: dw 1;If operable=1

bird:
;push location of top left corner
;make sure to keep index above 2 rows for 2x5 sprite

;  /\ /\ 
;    ^

push bp
mov bp,sp
pusha

mov di,[bp+4]
push 0xb800
pop es

mov ah,0x03
mov bh,0x03
mov ch,0x03


mov al,'\'
mov bl,'/'
mov cl,'^'


mov si,0
add si,di
mov [es:si],bx
add si,2
mov [es:si],ax
add si,4
mov [es:si],bx
add si,2
mov [es:si],ax

mov si,160
add si,di
add si,4
mov [es:si],cx

popa
pop bp
ret 2

cactus:
;insert index for top left corner by push
;make sure to keep four rows below 6x8 sprite
; _  _
;| || |
;| || | _
; \_  || |
;   |  _/
;   |_| 

push bp
mov bp,sp
pusha

mov di,[bp+4]

push 0xb800
pop es

mov ah,0x02
mov bh,0x02
mov ch,0x02
mov dh,0x02
mov si,0
add si,di

mov al,'|'
mov bl,'_'
mov cl,'\'
mov dl,'/'

;line1
add si,2
mov [es:si],bx
add si,6
mov [es:si],bx

;line2
mov si,160
add si,di
mov [es:si],ax
add si,4
mov [es:si],ax
add si,2
mov [es:si],ax
add si,4
mov [es:si],ax

;line3
mov si,320
add si,di
mov [es:si],ax
add si,4
mov [es:si],ax
add si,2
mov [es:si],ax
add si,4
mov [es:si],ax
add si,4
mov [es:si],bx

;line4
mov si,480
add si,di
add si,2
mov [es:si],cx
add si,2
mov [es:si],bx
add si,6
mov [es:si],ax
add si,2
mov [es:si],ax
add si,4
mov [es:si],ax

;line5
mov si,640
add si,di
add si,6
mov [es:si],ax
add si,6
mov [es:si],bx
add si,2
mov [es:si],dx

;line6
mov si,800
add si,di
add si,6
mov [es:si],ax
add si,2
mov [es:si],bx
add si,2
mov [es:si],ax

popa
pop bp
ret 2

ground:
;no need for any input will print ground at bottom of the screen
push bp
mov bp,sp

pusha

push 0xb800
pop es

mov si,3840
mov di,0
mov ah,0x06
mov al,'-'
mov bh,0x06
mov bl,'.'
mov cx,0

loop1:
cmp di,160
je end

cmp cx,4
je print2

print1:
mov [es:si],ax
add si,2
add di,2
add cx,1
jmp loop1

print2:
mov [es:si],bx
add si,2
add di,2
mov cx,0
jmp loop1


end:
popa
pop bp
ret

horseman:
;insert index for top left corner
;make sure to keep a space of 5 rows below for 6x12 sprite
push bp
mov bp,sp
pusha

mov di,[bp+4]
push 0xb800
pop es

mov si,0
add si,di
mov ah,0x07
mov bh,0x03
mov ch,0x05
mov dh,0x02

add si,10
mov dl,'p'
mov [es:si],dx
add si,6
mov bl,','
mov [es:si],bx
add si,2
mov bl,'.'
mov [es:si],bx
add si,2
mov al,'_'
mov [es:si],ax

mov si,160
add si,di
add si,8
mov dl,'['
mov [es:si],dx
add si,2
mov dl,'#'
mov [es:si],dx
add si,2
mov dl,'\'
mov [es:si],dx
add si,2
mov al,'('
mov bl,'('
mov [es:si],bx
add si,2
mov [es:si],bx
add si,2
mov [es:si],ax
add si,2
mov cl,'*'
mov [es:si],cx
add si,2
mov al,'\'
mov [es:si],ax

mov si,320
add si,di
add si,2
mov bl,'_'
mov [es:si],bx
add si,2
mov al,','
mov [es:si],ax
add si,2
mov al,'-'
mov dl,'\'
mov [es:si],ax
add si,2
mov [es:si],ax
add si,2
mov [es:si],dx
add si,2
mov [es:si],ax
add si,2
mov bl,')'
mov [es:si],bx
add si,4
mov al,')'
mov [es:si],ax
add si,2
mov al,'}'
mov [es:si],ax
add si,2
mov al,96
mov [es:si],ax

mov si,480
add si,di
mov bl,'('
mov [es:si],bx
add si,4
mov al,'('
mov [es:si],ax
add si,4
mov al,')'
mov [es:si],ax
add si,2
mov dl,'i'
mov [es:si],dx
add si,2
mov al,'_'
mov [es:si],ax
add si,2
mov [es:si],ax
add si,2
mov al,')'
mov [es:si],ax
add si,2
mov [es:si],ax

mov si,640
add si,di
mov bl,'('
mov [es:si],bx
add si,2
mov bl,'/'
mov [es:si],bx
add si,2
mov al,'/'
mov [es:si],ax
add si,2
mov [es:si],ax
add si,2
mov [es:si],ax
add si,6
mov al,'\'
mov [es:si],ax
add si,2
mov [es:si],ax
add si,2
mov [es:si],ax

mov si,800
add si,di
add si,4
mov al,'\'
mov [es:si],ax
add si,2
mov [es:si],ax
add si,2
mov [es:si],ax
add si,8
mov [es:si],ax
add si,2
mov [es:si],ax
add si,2
mov [es:si],ax

popa
pop bp
ret 2

clrscr: ;Clear Entire Screen
	push es
	push ax
	push di
	mov ax, 0xb800
	mov es, ax
	mov ax,0x0720
	mov cx,2000
	mov di, 0
	rep stosw
	pop di
	pop ax	
	pop es
	ret

printnum: ;bp+6=Number bp+4=Location
	push bp
	mov bp,sp
	pusha
	push 0xb800
	pop es
	mov bx,10
	mov dx,0
	mov cx,0
	mov ax,[bp+6]
	mov di,[bp+4]
lpn:
	div bx
	push dx
	mov dx,0
	inc cx
	cmp ax,0
	jne lpn
lpn2:
	pop ax
	mov ah,0x07
	add al,0x30
	stosw 
	loop lpn2
	popa
	pop bp
	ret 4

pscore: ;Modify and print score
	add word [score],1
	push word [score]
	push 120
	call printnum
	ret

clearev: ;Clears for Cactus and Horseman 
	push bp
	mov bp,sp 
	push di
	push si
	push ax
	push es
	push cx
	push bx
	push 0xb800
	pop es
	mov cx,6  ;6 rows
	mov ax,0x0720
	mov di,[bp+4] ;Store top left corner in DI
clear:
	mov si,di
	mov bx,di
	add bx,24 ;12 Columns
cl1:
	stosw 
	cmp di,bx
	jne cl1
	mov di,si
	add di,160 ;Go To Next Rows Start
	loop clear
	pop bx
	pop cx
	pop es
	pop ax
	pop si
	pop di
	pop bp
	ret 2

dino:
	cmp word [dinopos],1 ;Determine if Dino Jumpig
	je jumping 
	push 1920 ;Clear Space above Dino since Dino not jumping
	call clearev
	push 2880
	jmp dend
jumping:
	push 2880 ;Clear Space below Dino if it Jumping
	call clearev
	push 1920
dend:
	call horseman ;Draw Dino or equivelant
	ret

newop: ;Get Data of New Hurdle when Prev goes off screen
	push bp
	mov bp,sp
	push ax
	mov al,0
	out 0x70,al
	in al,0x71 ;get Current Seconds of system time
	and al,1 ;See if seconds odd
	cmp al,0
	jne newopbird ;if Odd draw a bird
	mov word [type],0
	mov word [cactuspos],3018
	jmp newopend
newopbird:
	mov word [type],1
	mov word [cactuspos],2070
newopend:
	pop ax
	pop bp
	ret

clecac:
	push ax
	mov ax,[cactuspos] 
	add ax,2 ;Get Cactus Pos
	push ax
	call clearev
	pop ax 
	ret

clebird:
	push cx
	push ax
	push di
	mov di,[cactuspos] ;Get Bird Pos
	add di,2 ;Since have to clear Prev Pos
	mov cx,5
	mov ax,0x0720
	rep stosw
	mov di,[cactuspos] ;Get Bird Second Row Start Pos
	add di,160
	mov cx,5
	rep stosw
	pop di
	pop ax
	pop cx
	ret

sound:
	push cx
	push dx
	push bx
	push ax
	push di
	mov     dx,2000
	mov  	bx,1000
	mov 	di,100
	cmp word [gameon],1         
	je else
	mov 	di,1000
	mov     bx,1 ;Sound for game off   
else:
	mov     al, 10110110b   ;Magic Number  
	out     0x43, al        ;Send to initializing port 43H Timer 2.
nextfreq:       
	mov     ax, bx 			;Move frequency value into ax        
	out     0x42, al 		; Send LSB to port 42H.         
	mov     al, ah   		; Move MSB into AL         
	out     0x42, al 		; Send MSB to port 42H.        
	in      al, 0x61 		; Get current value of port 61H.        
	or      al, 00000011b   ; OR AL to this value, forcing first two bits high.
	out     0x61, al   		; Copy it to port 61H of the PPI Chip to turn ON the speaker.  	                        
	mov      cx, di          
dloop:              
	loop    dloop      
	inc     bx     			; Incrementing the value of BX lowers the frequency     	          
	dec     dx           
	cmp     dx, 0           
	jnz     nextfreq   
	in      al,0x61 		; Get current value of port 61H.       
	and     al,11111100b 	; AND AL to this value, forcing first two bits low.
	out     0x61,al  		; Copy it to port 61H of the PPI Chip to turn OFF the speaker.
	pop di
	pop ax
	pop bx
	pop dx 
	pop cx
	ret

obstacle:
	push ax
	cmp word [cactuspos],2882 ;See if row end reached for Cactus
	je onew
	cmp word [cactuspos],1922 ;See if row end Reached for Bird
	je onew
	sub word [cactuspos],2
	cmp word [type],0 ;Determine What to Draw
	je drawcac
	call clebird ;Clear form prev Pos
	push word [cactuspos]
	call bird ;Draw to New Pos
	jmp oend
drawcac:
	call clecac ;Clear form prev Pos
	push word [cactuspos]
	call cactus ;Draw to New Pos
	jmp oend
onew:
	call sound
	;sub sp,2
	call pscore ;Update Score. Obstacle is cleared
	add word [factor],1 ;Increase obstacle production Speed after 5 jumps
	cmp word [factor],1
	jne otemp
	mov word [factor],0
	cmp word [dconst],0 ;If Speed is maxed let it stay maxed
	je otemp
	sub word [dconst],1 ;Speed Factor 1
otemp:
	mov word ax,[dconst]
	mov word [dtime],ax
	call newop
	;pop word[cactuspos] ;Reset Data
oend:
	pop ax
	ret

hook:  ;Hook Int 8h
	sub word [dtime],1
	sub word [dinotime],1
	cmp word [dinotime],0 ;Make Dino Fall after 20 int 8h calls
	jne hnext
	mov word [spacecheck],0
	mov word [dinopos],0
hnext:
	call collision ;Check for collision
	call dino ;Update Dino
	cmp word [gameon],0 ;See if Game still playable
	je hnext2
	cmp word [dtime],0
	jne hnext2
	add word [dtime],1
	call obstacle ;Update Obstacle
hnext2:
	mov al, 0x20
	out 0x20, al
	iret

collision:
	push ax
	push bx
	cmp word [dinopos],1 ;Calculate Top Right Corner of Dino
	je jumpin
	mov bx,2904
	jmp nextcheck
jumpin:
	mov bx,1944
nextcheck:
	mov ax,[cactuspos] ;Top Left Obstacle
	sub ax,2
	cmp word [type],1 ;Determine Type of obstacle
	je cbird
	sub ax,bx ;Cactus Pos - Dino Pos
	jnl cend
	mov word [gameon],0
	jmp cend
cbird:
	cmp bx,2000 ;See if Dino on ground
	jg cend
	sub ax,bx
	jnl cend
	mov word [gameon],0
cend:
	pop bx
	pop ax
	ret

hook2: ;Int 9h Hook
	in al,0x60
	cmp al,0x01 ;Check if Escape Pressed
	jne h2next
	mov word [gameon],0
	jmp temp
h2next:
	cmp word [spacecheck],1
	je temp
	cmp al,0x39 ;Check if Space checked
	jne temp
	mov word [spacecheck],1
	mov word [dinotime],20 
	mov word [dinopos],1
temp:
	mov al, 0x20
	out 0x20, al
	iret

start: 
	call newop
	call clrscr
	;Hook ISR
	mov ax,0
	mov es,ax
	mov word ax,[es:8*4]
	mov [oldisr],ax
	mov word ax,[es:8*4+2]
	mov [oldisr+2],ax
	mov word [es:8*4],hook
	mov word [es:8*4+2],cs
	mov word ax,[es:9*4]
	mov word [oldisr2],ax
	mov word ax,[es:9*4+2]
	mov word [oldisr2+2],ax
	cli
	mov word [es:9*4],hook2
	mov word [es:9*4+2],cs
	sti
	call ground
	push 0xb800
	pop es
	;Print 'Score: '
	mov cx,6
	mov si,msg
	mov di,106
	mov ah,0x07
sloop:
	lodsb 
	stosw
	loop sloop
	push word [score]
	push 120
	call printnum
	mov word [gameon],1
lop:
	cmp word [gameon],0
	je term
	jmp lop
	;Print 'Game Over. Score: '
term:
	call sound
	push 0xb800
	pop es
	mov cx,9
	mov ah,0x07
	mov si,msg2
	mov di,1820
endloop1:
	lodsb 
	stosw
	loop endloop1
	mov cx,6
	mov si,msg
	mov di,1980	
endloop2:
	lodsb 
	stosw
	loop endloop2
	push word [score]
	push 2000
	call printnum
	;UnHook ISR
	mov ax,0
	mov es,ax
	mov word ax,[oldisr]
	mov word [es:8*4],ax
	mov word ax,[oldisr+2]
	mov word [es:8*4+2],ax
	cli
	mov word ax,[oldisr2]
	mov word [es:9*4],ax
	mov word ax,[oldisr2+2]
	mov word [es:9*4+2],ax
	sti
	mov  ax, 0x4c00                      
	int  0x21 