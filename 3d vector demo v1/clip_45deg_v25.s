
CUSTOM	equ	$dff000

SCREEN_HOR_MIDDLE EQU SCN_WIDTH/2
SCREEN_VER_MIDDLE EQU SCN_HEIGHT/2

CC_TOP		EQU 1
CC_BOTTOM	EQU 2
CC_LEFT 	EQU 4
CC_RIGHT	EQU 8
CC_BEHIND 	EQU 16
CC_ON		EQU	0
CC_OFF		EQU	31

CC_TOP_BIT		EQU 0
CC_BOTTOM_BIT	EQU 1
CC_LEFT_BIT		EQU 2
CC_RIGHT_BIT	EQU 3
CC_BEHIND_BIT 	EQU 4

DMACON  equ	$096
DMACONR	equ	$002

INTENA 	equ	$09a
INTENAR	equ	$01c

INTREQ	equ	$09c
INTREQR	equ	$01e

ADKCON	equ	$09e
ADKCONR	equ	$010

COP1LCH	equ	$080
COP2LCH equ	$084
COPJMP1	equ	$088
VPOSR	equ	$004

BLTCON0	equ	$040
BLTCON1 equ	$042
BLTAFWM	equ	$044
BLTALWM	equ	$046
BLTCPTH equ	$048
BLTAPTH	equ	$050
BLTDPTH	equ	$054
BLTSIZE	equ	$058
BLTCON0L equ	$05A	; Blitter control 0, lower 8 bits (minterms)
BLTCMOD equ	$060
BLTBMOD equ	$062
BLTAMOD equ	$064
BLTDMOD equ	$066
BLTBDAT equ	$072
BLTADAT	equ 	$074

Exec_Forbid		equ	-132
Exec_Permit		equ	-138
Exec_OpenLibrary	equ	-552
Exec_CloseLibrary	equ	-414
Exec_FindTask		equ	-294
Exec_SetTaskPri		equ	-300

Gfx_LoadView		equ	-222
Gfx_WaitTOF		equ	-270
Gfx_Field_ActiView	equ	34
Gfx_Field_copinit	equ	38
Gfx_Field_LOFlist	equ	50

SCN_HEIGHT = 200
SCN_WIDTH = 320
NXT_LIN = SCN_WIDTH / 8 ; next line
SCN_WIDTH_CHAR = 40

STORE_USP_ON_STACK	macro
			MOVE.L 	USP,A0
			MOVE.L 	A0,-(A7) 		; store USP
 			endm

RESTORE_USP_FROM_STACK	macro
			MOVE.L 	(A7)+,A0
			MOVE.L 	A0,USP		 	; restore USP
			endm

STORE_STACK_POINTER		macro
			MOVE.L 	A7,STACK_POINTER	; store sp
			ENDM

RESTORE_STACK_POINTER	macro
			MOVE.L 	STACK_POINTER,A7 	; restore sp
			endm
	
BLITTER_WAIT	macro
		inline
.B_WAIT	BTST	#14,DMACONR(A6)		; wait for blitter ready
		BNE.S	.B_WAIT
		einline
		endm

VBL_WAIT		macro
		inline
.V_WAIT	MOVE.L	VPOSR(A1),D0				; wait for vertical blank
		AND.L	#$0001ff00,D0
		CMP.L	#$00012700,D0
		BNE.S	.V_WAIT
		einline
		endm

ADD_OFFSET	MACRO
		MOVE.L 	XOFFSET,D3
		ADD.L	D0,D3
		MOVE.L 	D3,D0
		MOVE.L 	YOFFSET,D3
		ADD.L	D1,D3
		MOVE.L 	D3,D1
		MOVE.L 	ZOFFSET,D3
		ADD.L	D2,D3
		MOVE.L 	D3,D2
	ENDM

SIN_COS:	macro
		LEA		sintab,a4
		LEA		costab,a5

		move.w	ophi,d1
		add.w	d1,d1
		move.w	(a4,d1.w),sphi
		move.w	(a5,d1.w),cphi

	endm

; rotate around y-axis:
;	x'' = x'*cos() + z*sin()
;	z'' = x'*sin() - z*cos()

ROTATE_Y:	macro

		move.w	d0,d3
		muls.w	cphi,d0 ; x*cos()
		add.l	d0,d0
		swap	d0

		move.w	d2,d4
		muls.w	sphi,d4 ; z*sin()
		add.l	d4,d4
		swap	d4

		add.w	d4,d0 ; x = x*cos() + z*sin()

		muls.w 	sphi,d3	 ; x*sin()
		add.l	d3,d3
		swap	d3

		muls.w	cphi,d2 ; z*cos()
		add.l	d2,d2
		swap	d2

		sub.w	d2,d3
		move.w	d3,d2 ; z = x*sin() - z*cos()

	endm

start:	MOVE.L 	$4.w,A6			; Switch to supervisor mode, because
		LEA 	.get(PC),A5		; movec is a privileged instruction.
		JSR 	-$1e(A6)  		; Supervisor()
;   	BRA.S 	.store			;
.get
; 	MOVEC	vbr,d0			; (DC.L $4e7A0801)
;   	RTE				;
.store
; 	MOVE.L 	d0,_vbr			; Store VBR

;	MOVE.L	$4,A6

		SUB.L	a1,a1				; zero - find current task
		JSR		Exec_FindTask(A6)
		MOVE.L	d0,a1				; set current task to high pri
		MOVEQ	#127,d0
		JSR		Exec_SetTaskPri(A6)

		LEA		gfxname,a1
		MOVEQ	#0,d0
		JSR		Exec_OpenLibrary(A6)
		TST.L	d0
		BEQ		error
		MOVE.L	d0,gfxbase

		MOVE.L	d0,A6
		MOVE.L	Gfx_Field_ActiView(A6),-(A7)	;	 store active view

		SUB.L	a1,a1					; load zero view so we get default zero state 
		JSR		Gfx_LoadView(A6)
		JSR		Gfx_WaitTOF(A6)				; Wait for both long and short frame to finish
		JSR		Gfx_WaitTOF(A6)

		MOVE.L	$4,A6
		JSR		Exec_Forbid(A6)

		LEA		CUSTOM,a1
		MOVE.W	DMACONR(a1),-(A7)		; save enabled dma channels
		MOVE.W	INTENAR(a1),-(A7)		; save enabled interrupts
		MOVE.W	#%0111111111111111,INTENA(A7)	; disable all interupts
		MOVE.W	INTREQR(a1),-(A7)		; store current interrupt request bits
		MOVE.W	ADKCONR(a1),-(A7)		; store current disk and sound control
	
		LEA		CUSTOM,A1
		VBL_WAIT
		
		MOVE.L	#copperlist,COP1LCH(a1)
		MOVE.W	#%0111111111111111,DMACON(a1)	; turn off all dma 
		MOVE.W	#%1000011111000000,DMACON(a1)	; enable bitplane + copper + blitter dma
		
;		MOVE.W	#0,ophi

.LOOP:		LEA		CUSTOM,a1
			VBL_WAIT
			BSR		SWAP_SCN
			
			BSR		KEYBOARD

			MOVE.W	ophi,D0
			ADDQ.B	#1,D0
			MOVE.W	D0,ophi
	
			LEA		CUSTOM+2,A6
			BSR		DL_Init

	moveq.L	#8-1,d7;#2-1,D7; ,#8-1,d7			; points
	lea	objectpointlist,a0	; point list
	moveq.L	#12-1,d1;#1-1,D1;#12-1,d1 ; #12-1,d1			;  #12-1
	lea	objectlinelist,a1		; line list

			BSR		DRAW_OBJECT

			MOVE.L	XOFFSET,D0
			moveq.l	#0,d1
			BSR		HEX32
			MOVE.L	YOFFSET,D0
			moveq.l	#9,d1
			BSR		HEX32
			MOVE.L	ZOFFSET,D0
			moveq.l	#18,d1
			BSR		HEX32

			MOVE.w	ophi,D0
			ext.l 	d0
			moveq.l	#27,d1
			BSR		HEX32

;			MOVE.L	TEST_CC,D0
;			move.l	#(6*40)+0,d1
;			BSR		HEX32
						
			BTST	#6,$bfe001                          ; left mouse button
			BNE		.LOOP

		LEA		CUSTOM,a1
		VBL_WAIT

		MOVE.L	gfxbase,A6
		MOVE.L	Gfx_Field_copinit(A6),COP1LCH(a1)	; restore system copper list
		MOVE.L	Gfx_Field_LOFlist(A6),COP2LCH(a1)

		MOVE.W	#$8000,d1			; enable bit

		MOVE.W	(A7)+,d0			; restore disk and sound control
		OR.W	d1,d0
		MOVE.W	d0,ADKCON(a1)

		MOVE.W	(A7)+,d0			; restore interrupt request bits
		OR.W	d1,d0
		MOVE.W	d0,INTREQ(a1)

		MOVE.W	(A7)+,d0			; restore enabled interrupts
		OR.W	d1,d0
		MOVE.W	d0,INTENA(a1)

		MOVE.W	(A7)+,d0			; restore enabled dma channels
		OR.W	d1,d0
		MOVE.W	d0,DMACON(a1)

		MOVE.L	$4,A6
		JSR		Exec_Permit(A6)

		MOVE.L	gfxbase,A6
		MOVE.L	(A7)+,a1				; load stored active view
		JSR		Gfx_LoadView(A6)
		JSR		Gfx_WaitTOF(A6)
		JSR		Gfx_WaitTOF(A6)

		MOVE.L	$4,A6
		MOVE.L	gfxbase,a1
		JSR		Exec_CloseLibrary(A6)

error:	MOVEQ.l		#0,d0
		RTS

gfxname:	DC.B	'graphics.library',0
		even
gfxbase:	DC.L	0

; display the current one
; flip to the next one
; cLEAr the next one
; draw on it
	
SWAP_SCN:	MOVE.L	mainscreenp,a1			; get current screen just drawn on and update copper list for next frame
			MOVE.L	(a1),d0
			LEA		mainbitp,A0
			MOVE.W	d0,6(A0) 	; set bit plane 1
			SWAP	d0
			MOVE.W	d0,2(A0)
			SWAP	d0
	
			MOVE.L	mainscreenp,d0	; next to be displayed		; shift the screens to the last drawn is at the end
			MOVE.L	mainscreenp+4,a1  ; next to be drawn
			MOVE.L	mainscreenp+8,d1  ; following after next
	
			MOVE.L	a1,mainscreenp 	; to be drawn on
			MOVE.L	d1,mainscreenp+4 	; shuffle up
			MOVE.L	d0,mainscreenp+8 	; next to be display - last to be updated

			LEA		CUSTOM,A6 	; cLEAr next screen to be drawn
			MOVE.L	mainscreenp+4,A0 	; +4,A0	; cLEAr the next screen not this screen
			MOVE.L	(A0),A0		; address to cLEAr
;			BSR		SCN_CLR

;			RTS

SCN_CLR:	MOVE.W	#$030,$180(A6)
			BLITTER_WAIT
			MOVE.W	#$300,$180(A6)

			MOVE.L	A0,BLTDPTH(A6)	; screen address
			MOVE.W	#0,BLTDMOD(A6)	; no modulo
			MOVE.L	#%1000000000000000000000000,BLTCON0(A6)
			MOVE.W	#((200*1)*64)+20,BLTSIZE(A6)	; screen height *64 + screen width in words
	
			BLITTER_WAIT
			MOVE.W	#$000,$180(A6)
			RTS

KEYPRESS:	MOVE.B  $BFEC01,D0      ; Keypress
			NOT.B   D0
			ROR.B   #1,D0           ; d0 now contains the raw key
			RTS

KEY_7	EQU	$07
KEY_8	EQU	$08

KEY_Q	EQU	$10
KEY_W	EQU	$11
KEY_T	EQU	$14
KEY_I	EQU	$17

KEY_A	EQU	$20
KEY_S	EQU $21
KEY_D	EQU	$22

KEY_F	EQU	$23
KEY_G	EQU	$24
KEY_H	EQU	$25

KEY_J	EQU	$26
KEY_K	EQU	$27
KEY_L	EQU	$28

KEY_Z	EQU	$31

KEYBOARD:	BSR		KEYPRESS
			CMP.B	#KEY_A,D0
			BEQ		.KEYA
			CMP.B	#KEY_D,D0
			BEQ		.KEYD
			CMP.B	#KEY_W,D0
			BEQ		.KEYW
			CMP.B	#KEY_S,D0
			BEQ		.KEYS
			CMP.B	#KEY_Q,D0
			BEQ		.KEYQ
			CMP.B	#KEY_Z,D0
			BEQ		.KEYZ

			CMP.B	#KEY_7,D0
			BEQ		.KEY7
			CMP.B	#KEY_8,D0
			BEQ		.KEY8

			CMP.B	#KEY_T,D0
			BEQ		.KEYT
			CMP.B	#KEY_G,D0
			BEQ		.KEYG
			CMP.B	#KEY_F,D0
			BEQ		.KEYF
			CMP.B	#KEY_H,D0
			BEQ		.KEYH

			CMP.B	#KEY_I,D0
			BEQ		.KEYI
			CMP.B	#KEY_K,D0
			BEQ		.KEYK
			CMP.B	#KEY_J,D0
			BEQ		.KEYJ
			CMP.B	#KEY_L,D0
			BEQ		.KEYL
			
			RTS

.KEYA		MOVE.L	XOFFSET,D0
			SUB.L	#111,D0
			MOVE.L	D0,XOFFSET
			RTS
.KEYD		MOVE.L	XOFFSET,D0
			ADD.L	#111,D0
			MOVE.L	D0,XOFFSET
			RTS
.KEYW		MOVE.L	YOFFSET,D0
			SUB.L	#111,D0
			MOVE.L	D0,YOFFSET
			RTS
.KEYS		MOVE.L	YOFFSET,D0
			ADD.L	#111,D0
			MOVE.L	D0,YOFFSET
			RTS
.KEYQ		MOVE.L	ZOFFSET,D0
			ADD.L	#111,D0
			MOVE.L	D0,ZOFFSET
			RTS
.KEYZ		MOVE.L	ZOFFSET,D0
			SUB.L	#111,D0
			MOVE.L	D0,ZOFFSET
			RTS
.KEY7		MOVE.W	ophi,D0
			SUBQ.B	#1,D0
			MOVE.W	D0,ophi
			RTS
.KEY8		MOVE.W	ophi,D0
			ADDQ.B	#1,D0
			MOVE.W	D0,ophi
			RTS

.KEYT
.KEYG
.KEYF
.KEYH
.KEYI
.KEYK
.KEYJ
.KEYL

CLIP_RIGHT_3D	MACRO
	SUB.L 	D0,D3	; dx = Bx - Ax
	SUB.L 	D1,D4	; dy = By - Ay
	SUB.L 	D2,D5	; dz = Bz - Az

	MOVE.L 	D0,D6
	SUB.L 	D2,D6	; (Ax - Az)

	MOVE.L 	D5,D7
	NEG.L 	D7
	ADD.L 	D3,D7	; (-dz + dx)

	MULS 	D6,D3
	DIVS	D7,D3
	EXT.L 	D3
	SUB.L 	D3,D0

	MULS 	D6,D4
	DIVS	D7,D4
	EXT.L 	D4
	SUB.L 	D4,D1

;	MULS 	D6,D5
;	DIVS	D7,D5
;	EXT.L 	D5
;	SUB.L 	D5,D2

	MOVE.L	D0,D2
	ADDQ.L 	#1,D2

	ENDM

CLIP_LEFT_3D	MACRO
	SUB.L 	D0,D3	; dx = Bx - Ax
	SUB.L 	D1,D4	; dy = By - Ay
	SUB.L 	D2,D5	; dz = Bz - Az

	MOVE.L 	D0,D6
	ADD.L 	D2,D6	; (Ax + Az)

	MOVE.L 	D3,D7
	NEG.L 	D7
	SUB.L 	D5,D7	; (-dx - dz)

	MULS 	D6,D3
	DIVS	D7,D3
	EXT.L 	D3
	ADD.L 	D3,D0

	MULS 	D6,D4
	DIVS	D7,D4
	EXT.L 	D4
	ADD.L 	D4,D1

;	MULS 	D6,D5
;	DIVS	D7,D5
;	EXT.L 	D5
;	ADD.L 	D5,D2

	MOVE.L	D0,D2
	NEG.L 	D2
	ADDQ.L 	#1,D2

	ENDM

CLIP_TOP_3D	MACRO
	SUB.L 	D0,D3	; dx = Bx - Ax
	SUB.L 	D1,D4	; dy = By - Ay
	SUB.L 	D2,D5	; dz = Bz - Az

	MOVE.L 	D1,D6
	ADD.L 	D2,D6	; (Ay + Az)

	MOVE.L 	D4,D7
	NEG.L 	D7
	SUB.L 	D5,D7	; (-dy - dz)

	MULS 	D6,D3
	DIVS	D7,D3
	EXT.L 	D3
	ADD.L 	D3,D0

	MULS 	D6,D4
	DIVS	D7,D4
	EXT.L 	D4
	ADD.L 	D4,D1

;	MULS 	D6,D5
;	DIVS	D7,D5
;	EXT.L 	D5
;	ADD.L 	D5,D2

	MOVE.L	D1,D2
	NEG.L 	D2
	ADDQ.L 	#1,D2

	ENDM

CLIP_BOTTOM_3D	MACRO
	SUB.L 	D0,D3	; dx = Bx - Ax
	SUB.L 	D1,D4	; dy = By - Ay
	SUB.L 	D2,D5	; dz = Bz - Az

	MOVE.L 	D1,D6
	SUB.L 	D2,D6	; (Ay - Az)

	MOVE.L 	D5,D7
	NEG.L 	D7
	ADD.L 	D4,D7	; (-dz + dx)

	MULS 	D6,D3
	DIVS	D7,D3
	EXT.L 	D3
	SUB.L 	D3,D0

	MULS 	D6,D4
	DIVS	D7,D4
	EXT.L 	D4
	SUB.L 	D4,D1

;	MULS 	D6,D5
;	DIVS	D7,D5
;	EXT.L 	D5
;	SUB.L 	D5,D2

	MOVE.L	D1,D2
	ADDQ.L 	#1,D2

	ENDM

CLIP_BEHIND_3D	MACRO
	SUB.L 	D0,D3	; dx = xon - xoff
	SUB.L 	D1,D4	; dy = yon - yoff
	move.l 	d5,d6
	neg.l d5
	ADD.L 	D2,D5	; dz = zon - -z0ff

	MULS 	D2,D3	; DX*DZ
	DIVS	D5,D3	; DX/ZON
	EXT.L 	D3
	add.L 	D3,D0

	MULS 	D2,D4
	DIVS	D5,D4
	EXT.L 	D4
	ADD.L 	D4,D1

	MOVEQ.L #1,D2
	ENDM

CLIP_BEHIND_3D_V1	MACRO
	SUB.L 	D0,D3	; dx = xon - xoff
	SUB.L 	D1,D4	; dy = yon - yoff
	ADD.L 	D2,D5	; dz = zon - -z0ff

	MULS 	D2,D3	; DX*DZ
	DIVS	D5,D3	; DX/ZON
	EXT.L 	D3
	ADD.L 	D3,D0

	MULS 	D1,D4
	DIVS	D6,D4
	EXT.L 	D4
	ADD.L 	D4,D1

	MOVEQ.L #1,D2

	ENDM

DRAW_OBJECT	MOVEM.L	D1/A1,-(SP)				; put line details on stack

			SIN_COS

			LEA		ROTATED_POINT,A1		; store rotated point
			LEA		PERSPECTIVE_POINT,A2	; store perspective point
			LEA		CLIPCODE_POINT,A3		; store point clip code

			MOVEQ.L	#CC_ON,D6 			; and clip code
			MOVEQ.L	#CC_OFF,D5 			; or clip code
	
.POINT_LOOP:	MOVEM.W (A0)+,D0/D1/D2		; get point
				ROTATE_Y				; do rotation
				EXT.L D0
				EXT.L D1
				EXT.L D2

				ADD_OFFSET					; add offsets
				MOVEM.L	D0/D1/D2,(A1)		; store rotated point

				TST.L	D2					; if z is positive
				BPL.S	.PNT_FRONT
.PNT_BEHIND			MOVEQ.L	#CC_BEHIND,D4	;  set clip code to behind
					MOVE.B	D4,(A3)					; store clipcode
					BRA.S	.PNT_OFF
.PNT_FRONT:		
;	BEQ.S .PNT_BEHIND
				
				BSR		CLIPCODE			; calc clip code
				MOVE.B	D4,(A3)					; store clipcode
				AND.B	D4,D5					; object and clip code
				OR.B	D4,D6					; object or clip code

				TST.W	D4
				BNE.S	.PNT_OFF
					BSR		PERSPECTIVE	; do perspective
					MOVEM.W	D0/D1,(A2)				; store perspective

.PNT_OFF:		LEA		16(A1),A1
				LEA		4(A2),A2
				LEA		1(A3),A3				
				DBF		D7,.POINT_LOOP

			MOVEM.L	(SP)+,D7/A4 				; get line details off stack

			TST.B	D5							; if and_object_clip_code ne 0 all off one side so exit
			BNE.S	.OFF_SCREEN

			LEA		PERSPECTIVE_POINT,A3
			MOVE.L	mainscreenp,A0
			LEA		CUSTOM+2,A6
 
			TST.B	D6							; if or_object_clip_code eq 0 all on screen so no clip needed
			BNE.S	.CLIP_OBJECT
 
.WHOLE_OBJECT:
.WHOLE_OBJECT_LOOP:	MOVEM.W	(A4)+,D5/D6
					ADD.W	D5,D5
					ADD.W	D5,D5
					MOVEM.W	(A3,D5.W),D0/D1
					ADD.W	D6,D6
					ADD.W	D6,D6
					MOVEM.W	(A3,D6.W),D2/D3
					BSR		LINE_DRAW
					DBF		D7,.WHOLE_OBJECT_LOOP
.OFF_SCREEN:	RTS
	
.CLIP_OBJECT:	
			LEA		CLIPCODE_POINT,A5		; store point clip code
;	lea	rotated_point,a5		; store rotated point

.CLIP_OBJECT_LOOP:	MOVEM.W	(A4)+,D5/D6
					MOVE.B	(A5,D5.W),D0	; start clip code
					MOVE.B	(A5,D6.W),D1	; end clip code
					MOVE.B	D0,D2			; backup start clip code
					AND.B	D1,D2			; both points off same side?
					BNE.S	.DRAW_LINE_OFF	; yes so next line
						MOVEM.L D7/A0/A3/A4/A5/A6,-(SP)	; put on stack		
						MOVE.B	D0,D2					; backup start clip code
						OR.B	D1,D2					; either point off?
						BNE.S	.CLIP_LINE				; yes so clip line
							ADD.W	D5,D5
							ADD.W	D5,D5
							MOVEM.W	(A3,D5.W),D0/D1
							ADD.W	D6,D6
							ADD.W	D6,D6
							MOVEM.W	(A3,D6.W),D2/D3
.DRAW_CLIP_LINE:			BSR		LINE_DRAW
.DRAW_LINE_NEXT:		MOVEM.L 	(SP)+,D7/A0/A3/A4/A5/A6 	; get off stack
.DRAW_LINE_OFF:		DBF	D7,.CLIP_OBJECT_LOOP
					RTS

.CLIP_LINE:						; D0/D1 = start/end clip code D2=OR clip codes

					MOVE.B 	D0,(CLIP_START_CC)
					MOVE.B 	D1,(CLIP_END_CC)

					MOVE.B 	D5,(CLIP_START_POINT)
					MOVE.B 	D6,(CLIP_END_POINT)

					ADD.W	D5,D5
					ADD.W	D5,D5
					MOVEM.W	(A3,D5.W),D3/D4
					MOVE.W	D3,(CLIP_START_PERS_H)
					MOVE.W 	D4,(CLIP_START_PERS_V)
					
					ADD.W	D6,D6
					ADD.W	D6,D6
					MOVEM.W	(A3,D6.W),D3/D4
					MOVE.W	D3,(CLIP_END_PERS_H)
					MOVE.W 	D4,(CLIP_END_PERS_V)

					LEA		ROTATED_POINT,A1		; store rotated point

					ADD.W	D5,D5
					ADD.W	D5,D5
					MOVEM.L	(A1,D5.W),D3/D4/D5
					MOVE.L	D3,(CLIP_START_ROT_X)
					MOVE.L 	D4,(CLIP_START_ROT_Y)
					MOVE.L 	D5,(CLIP_START_ROT_Z)

					ADD.W	D6,D6
					ADD.W	D6,D6
					MOVEM.L	(A1,D6.W),D3/D4/D5
					MOVE.L	D3,(CLIP_END_ROT_X)
					MOVE.L 	D4,(CLIP_END_ROT_Y)
					MOVE.L 	D5,(CLIP_END_ROT_Z)

	MOVE.L D2,TEST_CC
					BTST	#CC_BEHIND_BIT,D2    ; Check if either off right
					BEQ 	.CLIP_RIGHT_TST		; no - bit is clear

;	BRA		.DRAW_LINE_NEXT

						BTST 	#CC_BEHIND_BIT,D0  	; Check if start off right
						BEQ		.CLIP_BEHIND_START	; no so end is off right

.CLIP_BEHIND_END:
					MOVE.L	(CLIP_START_ROT_X),D0	; yes so start is off right
					MOVE.L 	(CLIP_START_ROT_Y),D1
					MOVE.L 	(CLIP_START_ROT_Z),D2
					MOVE.L	(CLIP_END_ROT_X),D3
					MOVE.L 	(CLIP_END_ROT_Y),D4
					MOVE.L 	(CLIP_END_ROT_Z),D5
					CLIP_BEHIND_3D
					MOVE.L 	D0,(CLIP_START_ROT_X)
					MOVE.L 	D1,(CLIP_START_ROT_Y)
					MOVE.L 	D2,(CLIP_START_ROT_Z)
	
					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_START_CC)

					AND.B 	(CLIP_END_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_START_CC),D4
					TST.B 	D4
					BNE 	.CLIP_RIGHT_TST

						MOVE.L 	(CLIP_START_ROT_X),D0
						MOVE.L 	(CLIP_START_ROT_Y),D1
						MOVE.L 	(CLIP_START_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_START_PERS_H)
						MOVE.W	D1,(CLIP_START_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.CLIP_RIGHT_TST

							MOVE.W	(CLIP_END_PERS_H),D2
							MOVE.W 	(CLIP_END_PERS_V),D3
							BSR		LINE_DRAW
							BRA		.DRAW_LINE_NEXT

.CLIP_BEHIND_START:
					MOVE.L	(CLIP_START_ROT_X),D3	; start point is off right
					MOVE.L 	(CLIP_START_ROT_Y),D4
					MOVE.L 	(CLIP_START_ROT_Z),D5
					MOVE.L	(CLIP_END_ROT_X),D0
					MOVE.L 	(CLIP_END_ROT_Y),D1
					MOVE.L 	(CLIP_END_ROT_Z),D2
					CLIP_BEHIND_3D
					MOVE.L 	D0,(CLIP_END_ROT_X)
					MOVE.L 	D1,(CLIP_END_ROT_Y)
					MOVE.L 	D2,(CLIP_END_ROT_Z)

					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_END_CC)

					AND.B 	(CLIP_START_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_END_CC),D4
					TST.B 	D4
					BNE 	.CLIP_RIGHT_TST

						MOVE.L 	(CLIP_END_ROT_X),D0
						MOVE.L 	(CLIP_END_ROT_Y),D1
						MOVE.L 	(CLIP_END_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_END_PERS_H)
						MOVE.W	D1,(CLIP_END_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.CLIP_RIGHT_TST

							MOVE.W	(CLIP_START_PERS_H),D2
							MOVE.W 	(CLIP_START_PERS_V),D3
							BSR		LINE_DRAW
							BRA		.DRAW_LINE_NEXT

.CLIP_RIGHT_TST:
					MOVE.B	(CLIP_START_CC),D0	; start clip code
					MOVE.B	(CLIP_END_CC),D1	; end clip code
					MOVE.B	D0,D2				; backup start clip code
					OR.B	D1,D2				; both points off same side?

					BTST	#CC_RIGHT_BIT,D2    ; Check if either off right
					BEQ 	.CLIP_LEFT_TST		; no - bit is clear

						BTST 	#CC_RIGHT_BIT,D0  	; Check if start off right
						BEQ		.CLIP_RIGHT_START	; no so end is off right

.CLIP_RIGHT_END:
					MOVE.L	(CLIP_START_ROT_X),D0	; yes so start is off right
					MOVE.L 	(CLIP_START_ROT_Y),D1
					MOVE.L 	(CLIP_START_ROT_Z),D2
					MOVE.L	(CLIP_END_ROT_X),D3
					MOVE.L 	(CLIP_END_ROT_Y),D4
					MOVE.L 	(CLIP_END_ROT_Z),D5
					CLIP_RIGHT_3D
					MOVE.L 	D0,(CLIP_START_ROT_X)
					MOVE.L 	D1,(CLIP_START_ROT_Y)
					MOVE.L 	D2,(CLIP_START_ROT_Z)
	
					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_START_CC)

					AND.B 	(CLIP_END_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_START_CC),D4
					TST.B 	D4
					BNE 	.CLIP_LEFT_TST

						MOVE.L 	(CLIP_START_ROT_X),D0
						MOVE.L 	(CLIP_START_ROT_Y),D1
						MOVE.L 	(CLIP_START_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_START_PERS_H)
						MOVE.W	D1,(CLIP_START_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.CLIP_LEFT_TST

							MOVE.W	(CLIP_END_PERS_H),D2
							MOVE.W 	(CLIP_END_PERS_V),D3
							BSR		LINE_DRAW
							BRA		.DRAW_LINE_NEXT

.CLIP_RIGHT_START:
					MOVE.L	(CLIP_START_ROT_X),D3	; start point is off right
					MOVE.L 	(CLIP_START_ROT_Y),D4
					MOVE.L 	(CLIP_START_ROT_Z),D5
					MOVE.L	(CLIP_END_ROT_X),D0
					MOVE.L 	(CLIP_END_ROT_Y),D1
					MOVE.L 	(CLIP_END_ROT_Z),D2
					CLIP_RIGHT_3D
					MOVE.L 	D0,(CLIP_END_ROT_X)
					MOVE.L 	D1,(CLIP_END_ROT_Y)
					MOVE.L 	D2,(CLIP_END_ROT_Z)

					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_END_CC)

					AND.B 	(CLIP_START_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_END_CC),D4
					TST.B 	D4
					BNE 	.CLIP_LEFT_TST

						MOVE.L 	(CLIP_END_ROT_X),D0
						MOVE.L 	(CLIP_END_ROT_Y),D1
						MOVE.L 	(CLIP_END_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_END_PERS_H)
						MOVE.W	D1,(CLIP_END_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.CLIP_LEFT_TST

							MOVE.W	(CLIP_START_PERS_H),D2
							MOVE.W 	(CLIP_START_PERS_V),D3
							BSR		LINE_DRAW
							BRA		.DRAW_LINE_NEXT

.CLIP_LEFT_TST:		MOVE.B	(CLIP_START_CC),D0	; start clip code
					MOVE.B	(CLIP_END_CC),D1	; end clip code
					MOVE.B	D0,D2				; backup start clip code
					OR.B	D1,D2				; both points off same side?

					BTST	#CC_LEFT_BIT,D2    ; Check if either off right
					BEQ 	.CLIP_BOTTOM_TST		; no - bit is clear

						BTST 	#CC_LEFT_BIT,D0  	; Check if start off right
						BEQ	.CLIP_LEFT_START		; no - bit is clear

.CLIP_LEFT_END:		MOVE.L	(CLIP_START_ROT_X),D0	; end point is off right
					MOVE.L 	(CLIP_START_ROT_Y),D1
					MOVE.L 	(CLIP_START_ROT_Z),D2
					MOVE.L	(CLIP_END_ROT_X),D3
					MOVE.L 	(CLIP_END_ROT_Y),D4
					MOVE.L 	(CLIP_END_ROT_Z),D5
					CLIP_LEFT_3D
					MOVE.L 	D0,(CLIP_START_ROT_X)
					MOVE.L 	D1,(CLIP_START_ROT_Y)
					MOVE.L 	D2,(CLIP_START_ROT_Z)
	
					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_START_CC)

					AND.B 	(CLIP_END_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_START_CC),D4
					TST.B 	D4
					BNE 	.CLIP_BOTTOM_TST

						MOVE.L 	(CLIP_START_ROT_X),D0
						MOVE.L 	(CLIP_START_ROT_Y),D1
						MOVE.L 	(CLIP_START_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_START_PERS_H)
						MOVE.W	D1,(CLIP_START_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.CLIP_BOTTOM_TST

							MOVE.W	(CLIP_END_PERS_H),D2
							MOVE.W 	(CLIP_END_PERS_V),D3
							BSR		LINE_DRAW
							BRA		.DRAW_LINE_NEXT

.CLIP_LEFT_START:	MOVE.L	(CLIP_START_ROT_X),D3	; start point is off right
					MOVE.L 	(CLIP_START_ROT_Y),D4
					MOVE.L 	(CLIP_START_ROT_Z),D5
					MOVE.L	(CLIP_END_ROT_X),D0
					MOVE.L 	(CLIP_END_ROT_Y),D1
					MOVE.L 	(CLIP_END_ROT_Z),D2
					CLIP_LEFT_3D
					MOVE.L 	D0,(CLIP_END_ROT_X)
					MOVE.L 	D1,(CLIP_END_ROT_Y)
					MOVE.L 	D2,(CLIP_END_ROT_Z)

					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_END_CC)

					AND.B 	(CLIP_START_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_END_CC),D4
					TST.B 	D4
					BNE 	.CLIP_BOTTOM_TST

						MOVE.L 	(CLIP_END_ROT_X),D0
						MOVE.L 	(CLIP_END_ROT_Y),D1
						MOVE.L 	(CLIP_END_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_END_PERS_H)
						MOVE.W	D1,(CLIP_END_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.CLIP_BOTTOM_TST

							MOVE.W	(CLIP_START_PERS_H),D2
							MOVE.W 	(CLIP_START_PERS_V),D3
							BSR		LINE_DRAW
							BRA		.DRAW_LINE_NEXT

.CLIP_BOTTOM_TST:	MOVE.B	(CLIP_START_CC),D0	; start clip code
					MOVE.B	(CLIP_END_CC),D1	; end clip code
					MOVE.B	D0,D2				; backup start clip code
					OR.B	D1,D2				; both points off same side?

					BTST	#CC_BOTTOM_BIT,D2    ; Check if either off right
					BEQ 	.CLIP_TOP_TST		; no - bit is clear

						BTST 	#CC_BOTTOM_BIT,D0  	; Check if start off right
						BEQ	.CLIP_BOTTOM_START		; no - bit is clear

.CLIP_BOTTOM_END:	MOVE.L	(CLIP_START_ROT_X),D3	; end point is off right
					MOVE.L 	(CLIP_START_ROT_Y),D4
					MOVE.L 	(CLIP_START_ROT_Z),D5
					MOVE.L	(CLIP_END_ROT_X),D0
					MOVE.L 	(CLIP_END_ROT_Y),D1
					MOVE.L 	(CLIP_END_ROT_Z),D2
					CLIP_BOTTOM_3D
					MOVE.L 	D0,(CLIP_START_ROT_X)
					MOVE.L 	D1,(CLIP_START_ROT_Y)
					MOVE.L 	D2,(CLIP_START_ROT_Z)

					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_START_CC)

					AND.B 	(CLIP_END_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_START_CC),D4
					TST.B 	D4
					BNE 	.CLIP_TOP_TST

						MOVE.L 	(CLIP_START_ROT_X),D0
						MOVE.L 	(CLIP_START_ROT_Y),D1
						MOVE.L 	(CLIP_START_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_START_PERS_H)
						MOVE.W	D1,(CLIP_START_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.CLIP_TOP_TST

							MOVE.W	(CLIP_END_PERS_H),D2
							MOVE.W 	(CLIP_END_PERS_V),D3
							BSR		LINE_DRAW
							BRA		.DRAW_LINE_NEXT
					
.CLIP_BOTTOM_START:	MOVE.L	(CLIP_START_ROT_X),D0	; start point is off right
					MOVE.L 	(CLIP_START_ROT_Y),D1
					MOVE.L 	(CLIP_START_ROT_Z),D2
					MOVE.L	(CLIP_END_ROT_X),D3
					MOVE.L 	(CLIP_END_ROT_Y),D4
					MOVE.L 	(CLIP_END_ROT_Z),D5
					CLIP_BOTTOM_3D
					MOVE.L 	D0,(CLIP_END_ROT_X)
					MOVE.L 	D1,(CLIP_END_ROT_Y)
					MOVE.L 	D2,(CLIP_END_ROT_Z)

					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_END_CC)

					AND.B 	(CLIP_START_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_END_CC),D4
					TST.B 	D4
					BNE 	.CLIP_TOP_TST

						MOVE.L 	(CLIP_END_ROT_X),D0
						MOVE.L 	(CLIP_END_ROT_Y),D1
						MOVE.L 	(CLIP_END_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_END_PERS_H)
						MOVE.W	D1,(CLIP_END_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.CLIP_TOP_TST

							MOVE.W	(CLIP_START_PERS_H),D2
							MOVE.W 	(CLIP_START_PERS_V),D3
							BSR		LINE_DRAW
							BRA		.DRAW_LINE_NEXT

.CLIP_TOP_TST:		MOVE.B	(CLIP_START_CC),D0	; start clip code
					MOVE.B	(CLIP_END_CC),D1	; end clip code
					MOVE.B	D0,D2				; backup start clip code
					OR.B	D1,D2				; both points off same side?

					BTST	#CC_TOP_BIT,D2    ; Check if either off right
					BEQ 	.DRAW_LINE_NEXT ; .CLIP_EXIT			; no - bit is clear

						BTST 	#CC_TOP_BIT,D0  	; Check if start off right
						BEQ	.CLIP_TOP_START		; no - bit is clear

.CLIP_TOP_END:		MOVE.L	(CLIP_START_ROT_X),D3	; end point is off right
					MOVE.L 	(CLIP_START_ROT_Y),D4
					MOVE.L 	(CLIP_START_ROT_Z),D5
					MOVE.L	(CLIP_END_ROT_X),D0
					MOVE.L 	(CLIP_END_ROT_Y),D1
					MOVE.L 	(CLIP_END_ROT_Z),D2
					CLIP_TOP_3D
					MOVE.L 	D0,(CLIP_START_ROT_X)
					MOVE.L 	D1,(CLIP_START_ROT_Y)
					MOVE.L 	D2,(CLIP_START_ROT_Z)
	
					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_START_CC)

					AND.B 	(CLIP_END_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_START_CC),D4
					TST.B 	D4
					BNE 	.DRAW_LINE_NEXT ; .CLIP_EXIT

						MOVE.L 	(CLIP_START_ROT_X),D0
						MOVE.L 	(CLIP_START_ROT_Y),D1
						MOVE.L 	(CLIP_START_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_START_PERS_H)
						MOVE.W	D1,(CLIP_START_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.DRAW_LINE_NEXT ; .CLIP_EXIT

							MOVE.W	(CLIP_END_PERS_H),D2
							MOVE.W 	(CLIP_END_PERS_V),D3
							BSR		LINE_DRAW
							BRA		.DRAW_LINE_NEXT

.CLIP_TOP_START:	MOVE.L	(CLIP_START_ROT_X),D0	; start point is off right
					MOVE.L 	(CLIP_START_ROT_Y),D1
					MOVE.L 	(CLIP_START_ROT_Z),D2
					MOVE.L	(CLIP_END_ROT_X),D3
					MOVE.L 	(CLIP_END_ROT_Y),D4
					MOVE.L 	(CLIP_END_ROT_Z),D5
					CLIP_TOP_3D
					MOVE.L 	D0,(CLIP_END_ROT_X)
					MOVE.L 	D1,(CLIP_END_ROT_Y)
					MOVE.L 	D2,(CLIP_END_ROT_Z)

					BSR 	CLIPCODE
					MOVE.B 	D4,(CLIP_END_CC)

					AND.B 	(CLIP_START_CC),D4
					BNE		.DRAW_LINE_NEXT

					MOVE.B 	(CLIP_END_CC),D4
					TST.B 	D4
					BNE 	.DRAW_LINE_NEXT ; .CLIP_EXIT

						MOVE.L 	(CLIP_END_ROT_X),D0
						MOVE.L 	(CLIP_END_ROT_Y),D1
						MOVE.L 	(CLIP_END_ROT_Z),D2
						BSR		PERSPECTIVE
						MOVE.W	D0,(CLIP_END_PERS_H)
						MOVE.W	D1,(CLIP_END_PERS_V)

						MOVE.B 	(CLIP_END_CC),D4
						OR.B 	(CLIP_START_CC),D4
						BNE		.DRAW_LINE_NEXT ; .CLIP_EXIT

							MOVE.W	(CLIP_START_PERS_H),D2
							MOVE.W 	(CLIP_START_PERS_V),D3
							BSR		LINE_DRAW

.CLIP_EXIT:	BRA		.DRAW_LINE_NEXT	

CLIPCODE	TST.L	D2
			BMI.S	.BEHIND
			MOVEQ.L	#CC_ON,D4
			MOVE.L	D2,D3
			NEG.L	D3
			
.XTEST		TST.L	D0						; X IS +VE
			BPL.S	.RIGHT					; YES SO CHECK RIGHT

.LEFT		CMP.L	D3,D0				; COMPARE LEFT AND -Z VAL
			BGE.S	.YTEST				; ON
				MOVEQ.L	#CC_LEFT,D4			; OFF
				BRA.S	.YTEST

.RIGHT		CMP.L	D2,D0				; COMPARE RIGHT AND +Z VAL
			BLE.S	.YTEST				; ON
				MOVEQ.L	#CC_RIGHT,D4	; OFF
			
.YTEST		TST.L	D1						; Y IS +VE
			BPL.S	.BOTTOM					; YES SO CHECK BOTTOM

.TOP		CMP.L	D3,D1				; COMPARE TOP AND -Z VAL
			BGE.S	.EXIT				; ON
				OR.B	#CC_TOP,D4			; OFF
				RTS							;	BRA.S	.EXIT

.BOTTOM		CMP.L	D2,D1				; COMPARE BOTTOM AND +Z VAL
			BLE.S	.EXIT				; ON
				OR.B	#CC_BOTTOM,D4		; OFF
.EXIT		RTS

.BEHIND		MOVEQ.L	#CC_BEHIND,D4
			RTS
	
PLOT		ADD.W	D1,D1
			ADD.W	D1,D1
			MOVE.L	(A0,D1.W),A2
			MOVE.W 	D0,D1
			LSR.W 	#3,D1
			AND.W 	#7,D0
			MOVE.B 	BIT_TAB(PC,D0.W),D0
			OR.B	(A2,D1.W),D0
			MOVE.B	D0,(A2,D1.W)
			RTS
BIT_TAB:	DC.B 128,64,32,16,8,4,2,1

PLOT_LINE	BSR		PLOT
			MOVE.W 	D2,D0
			MOVE.W 	D3,D1
			BSR		PLOT
			RTS

LINE_DRAW_PB:	MOVE.W 	D2,D4
				MOVE.W 	D3,D5
				JSR 	PLOT_BIG
				MOVE.W 	D4,D0
				MOVE.W 	D5,D1
				JSR 	PLOT_BIG				
			RTS

PLOT_BIG	MOVE.W	D0,D2
			MOVE.W 	D1,D3
			BSR		PLOT
			SUBQ.L 	#1,D2
			MOVE.W 	D2,D0
			MOVE.W 	D3,D1
			BSR		PLOT
			ADDQ.L 	#2,D2
			MOVE.W 	D2,D0
			MOVE.W 	D3,D1
			BSR		PLOT

			ADDQ.L 	#1,D3

			MOVE.W 	D2,D0
			MOVE.W 	D3,D1
			BSR		PLOT
			SUBQ.L 	#1,D2
			MOVE.W 	D2,D0
			MOVE.W 	D3,D1
			BSR		PLOT
			SUBQ.L 	#1,D2
			MOVE.W 	D2,D0
			MOVE.W 	D3,D1
			BSR		PLOT

			ADDQ.L 	#1,D3

			MOVE.W 	D2,D0
			MOVE.W 	D3,D1
			BSR		PLOT
			ADDQ.L 	#1,D2
			MOVE.W 	D2,D0
			MOVE.W 	D3,D1
			BSR		PLOT
			ADDQ.L 	#1,D2
			MOVE.W 	D2,D0
			MOVE.W 	D3,D1
			BSR		PLOT
			RTS

LINE_DRAW	
;			LEA		CUSTOM+2,A6
;			MOVE.L	mainscreenp,A0

DRAW_LINE:	CMP.W	D1,D3				;	A0 = PlanePtr, A6 = $DFF002, D0/D1 = X0/Y0, D2/D3 = X1/Y1, D4 = PlaneWidth > Kills: D0-D4/A0-A1 (+D5 in Fill Mode)
			BGE.S	.y1ly2				; Drawing only from Top to Bottom is necessary for:
				EXG	D0,D2				; 1) Up-down Differences (same coords)
				EXG	D1,D3				; 2) Blitter Invert Bit (only at top of line)
.y1ly2:		SUB.W	D1,D3				; D3 = yd

;			LSL.W	#2,D1
			ADD.W	D1,D1
			ADD.W	D1,D1
			MOVE.L	(A0,D1.W),A2

			MOVEQ.l	#0,D1				; D1 = Quant-Counter
			SUB.W	D0,D2				; D2 = xd
			BGE.S	.xdpos
				ADDQ.W	#2,D1			; Set Bit 1 of Quant-Counter (here it could be a MOVEQ)
				NEG.W	D2
.xdpos:		MOVEQ.l	#$f,D4				; D4 full cLEAned (for later oktants MOVE.B)
			AND.W	D0,D4

			LSR.W	#3,D0				; Yeah, on byte (necessary for bchg)...
			ADD.W	D0,A2				; ...Blitter ands automagically
			ROR.W	#4,D4				; D4 = Shift
			OR.W	#$B00+$CA,D4		; BLTCON0-codes / DL_MInterns = $CA
			SWAP	D4
			CMP.W	D2,D3				; Which Delta is the Biggest ?
			BGE.S	.dygdx
				ADDQ.W	#1,D1			; Set Bit 0 of Quant-Counter
				EXG		D2,D3			; Exchange xd with yd
.dygdx:		ADD.W	D2,D2				; D2 = xd*2
			MOVE.W	D2,D0				; D0 = Save for $52(A6)
			SUB.W	D3,D0				; D0 = xd*2-yd
			ADDX.W	D1,D1				; Bit0 = Sign-Bit
			MOVE.B	Oktants(PC,d1.w),D4	; In Low Byte of d4 (upper byte cLEAned above)
			SWAP	D2
			MOVE.W	D0,D2
			SUB.W	D3,D2				; D2 = 2*(xd-yd)
			MOVEQ.l	#6,D1				; D1 = ShiftVal (not necessary) + TestVal for the Blitter
			LSL.W	D1,D3				; D3 = BLTSIZE
			ADD.W	#$42,D3
			LEA	$52-2(A6),A1			; A1 = CUSTOM+$52

.wb:		BTST	D1,(A6)				; WARNING : If you use FastMem and an extreme DMA-Access (e.g. 6 Planes and Copper), you should Insert a tst.b (A6) here (for the shitty AGNUS-BUG)
			BNE.S	.wb					; Waiting for the Blitter...

			MOVE.L	D4,BLTCON0-2(A6)	; Writing to the Blitter Regs as fast
			MOVE.L	D2,BLTBMOD-2(A6)	; as possible
			MOVE.L	A2,BLTCPTH-2(A6)
			MOVE.W	D0,(A1)+
			MOVE.L	A2,(A1)+			; Shit-Word Buffer Ptr...
			MOVE.W	D3,(A1)
	RTS

DL_Init:	MOVEQ.l	#-1,d1	; Optimized Init Part... A6 = $DFF000 > Kills : D0-D2
			MOVEQ	#SCN_WIDTH_CHAR,d0
			MOVEQ.l	#6,d2

.wb:		BTST	d2,(A6)
			BNE.S	.wb

		MOVE.W	d1,BLTAFWM-2(A6)
		MOVE.W	d1,BLTBDAT-2(A6)
		MOVE.W	#$8000,BLTADAT-2(A6)
		MOVE.W	d0,BLTCMOD-2(A6)
		MOVE.W	d0,BLTDMOD-2(A6)
		RTS

Oktants:	DC.B	1,1+$40
			DC.B	17,17+$40
			DC.B	9,9+$40
			DC.B	21,21+$40
						
HEX32:		;d0=number
			;d1=chars across
			;a0 = screen start
			MOVE.L	mainscreenp,A0
			move.l	(a0),a1
			lea		8(a1,d1),a1
			lea		hexhcharlist,a2
			
			BSR		HEX_CHAR_SETUP
			BSR		HEX_CHAR_SETUP
			BSR		HEX_CHAR_SETUP
			BSR		HEX_CHAR_SETUP
			BSR		HEX_CHAR_SETUP
			BSR		HEX_CHAR_SETUP
			BSR		HEX_CHAR_SETUP
			BSR		HEX_CHAR_SETUP
			RTS

HEX_CHAR_SETUP:		move.w	d0,D1
					lsr.l	#4,d0
					and.w	#15,D1
					move.w	d1,d2
					add.w	d2,D2
					add.w	d2,d2
					add.w	d2,d1

HEX_CHAR:	move.b	(a2,d1.w),d2
			move.B	d2,SCN_WIDTH_CHAR*0(a1)
			move.b	1(a2,d1.w),d2
			move.B	d2,SCN_WIDTH_CHAR*1(a1)
			move.b	2(a2,d1.w),d2
			move.B	d2,SCN_WIDTH_CHAR*2(a1)
			move.b	3(a2,d1.w),d2
			move.B	d2,SCN_WIDTH_CHAR*3(a1)
			move.b	4(a2,d1.w),d2
			move.B	d2,SCN_WIDTH_CHAR*4(a1)
			subq.l	#1,a1
			RTS
			
;PROJ_END_V1	MACRO
;	ext.l D0
;	ext.l D1
			
;			MOVE.L D0,TEST_X2
;			MOVE.L D1,TEST_Y2
;			MOVE.L D2,TEST_Z2

;			DIVS.W	D2,D1
;			DIVS.W 	D2,D0

;	move.l #\1,TEST_NUMBER_Z
;	move.l d0,TEST_NUMBER_X
;	move.l d1,TEST_NUMBER_Y

;			ASR.W	#1,d1			;2
;			ADD.W	#SCREEN_VER_MIDDLE,D1
;			ADD.W	#SCREEN_HOR_MIDDLE,D0
;			RTS
;			ENDM

PROJ_END	MACRO
	LEA	PERS_MUL_TAB,A4
	ADD.W D2,D2
	MULS (A4,D2.W),D0
	MULS (A4,D2.W),D1

	ADD.L D0,D0
	SWAP D0
	SWAP D1
			ADD.W	#SCREEN_VER_MIDDLE,D1
			ADD.W	#SCREEN_HOR_MIDDLE,D0
			RTS
			ENDM


PROJ_31
;		MOVEQ.L #29,D3
;			LSL.L 	#d3,D2
;			LSL.L 	#d3,D1
;			LSL.L 	#d3,D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $31

PROJ_30
;		MOVEQ.L #30,D3
;			LSL.L 	#d3,D2
;			LSL.L 	#d3,D1
;			LSL.L 	#d3,D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $30

PROJ_29	MOVEQ.L #01,D3
			LSL.L 	D2
			LSL.L 	D1
			LSL.L 	D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $29

PROJ_28	MOVEQ.L #02,D3
			LSL.L 	#2,D2
			LSL.L 	#2,D1
			LSL.L 	#2,D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $28

PROJ_27	MOVEQ.L #03,D3
			LSL.L 	#3,D2
			LSL.L 	#3,D1
			LSL.L 	#3,D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $27

PROJ_26	MOVEQ.L #04,D3
			LSL.L 	#4,D2
			LSL.L 	#4,D1
			LSL.L 	#4,D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $26

PROJ_25	MOVEQ.L #05,D3
			LSL.L 	#5,D2
			LSL.L 	#5,D1
			LSL.L 	#5,D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $25

PROJ_24	MOVEQ.L #06,D3
			LSL.L 	#6,D2
			LSL.L 	#6,D1
			LSL.L 	#6,D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $24

PROJ_23	MOVEQ.L #07,D3
			LSL.L 	#7,D2
			LSL.L 	#7,D1
			LSL.L 	#7,D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $23

PROJ_22	MOVEQ.L #08,D3
			LSL.L 	#8,D2
			LSL.L 	#8,D1
			LSL.L 	#8,D0
			SWAP D0
			SWAP D1
			SWAP D2
			LSR.L #7,D2
			PROJ_END $22

PROJ_21	MOVEQ.L #09,D3
			LSL.L 	#8,D2
			LSL.L 	#8,D1
			LSL.L 	#8,D0
			LSR.w #7,D2
			PROJ_END $21

PROJ_20		aSr.L 	#7,D2
			aSr.L 	#7,D1
			LSr.L 	#7,D0
			LSR.w #7,D2
			PROJ_END $20

PROJ_19		aSr.L 	#6,D2
			aSr.L 	#6,D1
			LSr.L 	#6,D0
			LSR.w #7,D2
			PROJ_END $19

PROJ_18		aSr.L 	#5,D2
			aSr.L 	#5,D1
			LSr.L 	#5,D0
			LSR.w #7,D2
			PROJ_END $18

PROJ_17		aSr.L 	#4,D2
			aSr.L 	#4,D1
			LSr.L 	#4,D0
			LSR.w #7,D2
			PROJ_END $17

PROJ_16		aSr.L 	#3,D2
			aSr.L 	#3,D1
			LSr.L 	#3,D0
			LSR.w #7,D2
			PROJ_END $16

PROJ_15		ASr.L 	#2,D2
			ASr.L 	#2,D1
			LSr.L 	#2,D0
			LSR.w #7,D2
			PROJ_END $15

PROJ_14	;		MOVEQ.L #16-16,D3
;			LSL.L 	d3,D0
;			LSL.L 	d3,D1
;			LSL.L 	d3,D2
			LSR.L #7,D2
			PROJ_END $14

PROJ_13	;		MOVEQ.L #17-16,D3
			LSL.w 	D0
			LSL.w 	D1
;			LSL.w 	D2
			LSR.w #7-1,D2
			PROJ_END $13

PROJ_12	;		MOVEQ.L #18-16,D3
			LSL.w 	#2,D0
			LSL.w 	#2,D1
;			LSL.w 	#2,D2
			LSR.w #7-2,D2
			PROJ_END $12

PROJ_11	;	MOVEQ.L #19-16,D3
			LSL.w 	#3,D0
			LSL.w 	#3,D1
;			LSL.w 	#3,D2
			LSR.w #7-3,D2
			PROJ_END $11

PROJ_10	;	MOVEQ.L #20-16,D3
			LSL.w 	#4,D0
			LSL.w 	#4,D1
;			LSL.w 	#4,D2
			LSR.w #7-4,D2
			PROJ_END $10

PROJ_09	;	MOVEQ.L #21-16,D3
			LSL.w 	#5,D0
			LSL.w 	#5,D1
;			LSL.w 	#5,D2
			LSR.w #7-5,D2
			PROJ_END $09

PROJ_08	;	MOVEQ.L #22-16,D3
			LSL.w 	#6,D0
			LSL.w 	#6,D1
;			LSL.w 	#6,D2
			LSR.w D2
			PROJ_END $08

PROJ_07	;	MOVEQ.L #23-16,D3
			LSL.w 	#7,D0
			LSL.w 	#7,D1
;			LSL.w 	#7,D2
;			LSR.w #7,D2
			PROJ_END $07

PROJ_06	;	MOVEQ.L #24-16,D3
			LSL.w 	#8,D0
			LSL.w 	#8,D1
;			LSL.w 	#8,D2
;			LSR.w #7,D2
			LSL.w 	D2
			PROJ_END $06

PROJ_05		MOVEQ.L #25-16,D3
			LSL.w 	d3,D0
			LSL.w 	d3,D1
;			LSL.w 	d3,D2
;			LSR.w #7,D2
			LSL.w 	#2,D2
			PROJ_END $05

PROJ_04		MOVEQ.L #26-16,D3
			LSL.w 	d3,D0
			LSL.w 	d3,D1
			LSL.w 	d3,D2
			LSR.w #7,D2
			PROJ_END $04

PROJ_03		MOVEQ.L #27-16,D3
			LSL.w 	d3,D0
			LSL.w 	d3,D1
;			LSL.w 	d3,D2
;			LSR.w #7,D2
			LSL.w 	#3,D2
			PROJ_END $03

PROJ_02		MOVEQ.L #28-16,D3
			LSL.w 	d3,D0
			LSL.w 	d3,D1
;			LSL.w 	d3,D2
;			LSR.w #7,D2
			LSL.w 	#4,D2
			PROJ_END $02

PROJ_01		MOVEQ.L #29-16,D3
			LSL.W 	d3,D0
			LSL.W 	d3,D1
;			LSL.W 	d3,D2
;			LSR.W 	#7,D2
			LSL.w 	#5,D2
			PROJ_END $01

PROJ_00		MOVEQ.L #30-16,D3
			LSL.W 	d3,D0
			LSL.W 	d3,D1
;			LSL.W 	d3,D2
;			LSR.w #7,D2
			LSL.w 	#6,D2
			PROJ_END $00

PERS_TAB_3224
			DCB.L 1,PROJ_24		;24*1
			DCB.L 1,PROJ_24		;24*1
			DCB.L 2,PROJ_25		;25*1
			DCB.L 4,PROJ_26		;26*1
			DCB.L 8,PROJ_27		;27*1
			DCB.L 16,PROJ_28	;28*1
			DCB.L 32,PROJ_29	;29*1
			DCB.L 64,PROJ_30	;30*1
			DCB.L 128,PROJ_31	;31*1

PERS_TAB_2416
			DCB.L 1,PROJ_16		;16*1
			DCB.L 1,PROJ_16		;16*1
			DCB.L 2,PROJ_17		;17*1
			DCB.L 4,PROJ_18		;18*1
			DCB.L 8,PROJ_19		;19*1
			DCB.L 16,PROJ_20	;20*1
			DCB.L 32,PROJ_21	;21*1
			DCB.L 64,PROJ_22	;22*1
			DCB.L 128,PROJ_23	;23*1

PERS_TAB_1608
			DCB.L 1,PROJ_08		;8*1
			DCB.L 1,PROJ_08		;8*1
			DCB.L 2,PROJ_09		;9*1
			DCB.L 4,PROJ_10		;10*1
			DCB.L 8,PROJ_11		;11*1
			DCB.L 16,PROJ_12	;12*1
			DCB.L 32,PROJ_13	;13*1
			DCB.L 64,PROJ_14	;14*1
			DCB.L 128,PROJ_15	;15*1

PERS_TAB_0800
			DCB.L 1,PROJ_00		;0*1
			DCB.L 1,PROJ_00		;0*1
			DCB.L 2,PROJ_01		;1*1
			DCB.L 4,PROJ_02		;2*1
			DCB.L 8,PROJ_03		;3*1
			DCB.L 16,PROJ_04	;4*1
			DCB.L 32,PROJ_05	;5*1
			DCB.L 64,PROJ_06	;6*1
			DCB.L 128,PROJ_07	;7*1

PERSPECTIVE	
			ADDQ.L 	#1,D2
			CMP.L #$00FFFFFF,D2
			BLS.S .CONT2416
.CONT3224		MOVE.L D2,D3
				SWAP D3
				LSR.W #8,D3
				ADD.W D3,D3
				ADD.W D3,D3
				LEA PERS_TAB_3224,A4
				MOVE.L (A4,D3.W),A4
				JMP (A4)

.CONT2416	CMP.L #$0000FFFF,D2
			BLS.S .CONT1608
				MOVE.L D2,D3
				SWAP D3
				AND.W #255,D3
				ADD.W D3,D3
				ADD.W D3,D3
				LEA PERS_TAB_2416,A4
				MOVE.L (A4,D3.W),A4
				JMP (A4)

.CONT1608	CMP.W #$00FF,D2
			BLS.S .CONT0800
				MOVE.W D2,D3
				LSR.W #8,D3
				ADD.W D3,D3
				ADD.W D3,D3
				LEA PERS_TAB_1608,A4
				MOVE.L (A4,D3.W),A4
				JMP (A4)

.CONT0800	MOVE.W D2,D3
			AND.W #255,D3
				ADD.W D3,D3
				ADD.W D3,D3
				LEA PERS_TAB_0800,A4
				MOVE.L (A4,D3.W),A4
				JMP (A4)


;PERS_OLD	LEA 	BIT_SET_TAB,A4
;			MOVEQ.L	#0,D3			; clear long word of result
;			CMP.L 	#$0000FFFF,D2
;			BLS.S 	.PROJECT_LO
;.PROJECT_HI	SWAP	D2
;			MOVE.B	(A4,D2.W),D3			; get high word
;			SWAP	D2
;			MOVE.L	-128(A4,D3.W),A4		; PROJECT_TAB_HI
;			JMP		(A4)
;.PROJECT_LO	MOVE.B	(A4,D2.W),D3		; get low word
;			MOVE.L	-64(A4,D3.W),A4		; PROJECT_TAB_LO
;			JMP		(A4)
;PROJECT_TAB_HI	DC.L 	PROJ_16,PROJ_17,PROJ_18,PROJ_19,PROJ_20,PROJ_21,PROJ_22,PROJ_23,PROJ_24,PROJ_25,PROJ_26,PROJ_27,PROJ_28,PROJ_29,PROJ_30,PROJ_31
;PROJECT_TAB_LO	DC.L 	PROJ_00,PROJ_01,PROJ_02,PROJ_03,PROJ_04,PROJ_05,PROJ_06,PROJ_07,PROJ_08,PROJ_09,PROJ_10,PROJ_11,PROJ_12,PROJ_13,PROJ_14,PROJ_15
;BIT_SET_TAB
;	DCB.B	1,0*4
;	DCB.B	1,0*4
;	DCB.B	2,1*4
;	DCB.B	4,2*4
;	DCB.B	8,3*4
;	DCB.B	16,4*4
;	DCB.B	32,5*4
;	DCB.B	64,6*4
;	DCB.B	128,7*4
;	DCB.B	256,8*4
;	DCB.B	512,9*4
;	DCB.B	1024,10*4
;	DCB.B	2048,11*4
;	DCB.B	4096,12*4
;	DCB.B	8192,13*4
;	DCB.B	16384,14*4
;	DCB.B	32768,15*4

;	LEA	PERS_MUL_TAB,A4
;	ADD.W D0,D0
;	MULS (A$,D0.W),D1
;	MULS (A$,D0.W),D2

PERS_MUL_TAB
 DC.W 0 ; 0
 DC.W 32767 ; 1
 DC.W 16384 ; 2
 DC.W 10922 ; 3
 DC.W 8192 ; 4
 DC.W 6553 ; 5
 DC.W 6553 ; 5
 DC.W 5461 ; 6
 DC.W 4681 ; 7
 DC.W 4096 ; 8
 DC.W 3641 ; 9
 DC.W 3277 ; 10
 DC.W 2979 ; 11
 DC.W 2731 ; 12
 DC.W 2521 ; 13
 DC.W 2341 ; 14
 DC.W 2184 ; 15
 DC.W 2048 ; 16
 DC.W 1927 ; 17
 DC.W 1820 ; 18
 DC.W 1725 ; 19
 DC.W 1638 ; 20
 DC.W 1560 ; 21
 DC.W 1489 ; 22
 DC.W 1425 ; 23
 DC.W 1365 ; 24
 DC.W 1311 ; 25
 DC.W 1260 ; 26
 DC.W 1214 ; 27
 DC.W 1170 ; 28
 DC.W 1130 ; 29
 DC.W 1092 ; 30
 DC.W 1057 ; 31
 DC.W 1024 ; 32
 DC.W 993 ; 33
 DC.W 964 ; 34
 DC.W 936 ; 35
 DC.W 910 ; 36
 DC.W 886 ; 37
 DC.W 862 ; 38
 DC.W 840 ; 39
 DC.W 819 ; 40
 DC.W 799 ; 41
 DC.W 780 ; 42
 DC.W 762 ; 43
 DC.W 745 ; 44
 DC.W 728 ; 45
 DC.W 712 ; 46
 DC.W 697 ; 47
 DC.W 683 ; 48
 DC.W 669 ; 49
 DC.W 655 ; 50
 DC.W 642 ; 51
 DC.W 630 ; 52
 DC.W 618 ; 53
 DC.W 607 ; 54
 DC.W 596 ; 55
 DC.W 585 ; 56
 DC.W 575 ; 57
 DC.W 565 ; 58
 DC.W 555 ; 59
 DC.W 546 ; 60
 DC.W 537 ; 61
 DC.W 529 ; 62
 DC.W 520 ; 63
 DC.W 512 ; 64
 DC.W 504 ; 65
 DC.W 496 ; 66
 DC.W 489 ; 67
 DC.W 482 ; 68
 DC.W 475 ; 69
 DC.W 468 ; 70
 DC.W 462 ; 71
 DC.W 455 ; 72
 DC.W 449 ; 73
 DC.W 443 ; 74
 DC.W 437 ; 75
 DC.W 431 ; 76
 DC.W 426 ; 77
 DC.W 420 ; 78
 DC.W 415 ; 79
 DC.W 410 ; 80
 DC.W 405 ; 81
 DC.W 400 ; 82
 DC.W 395 ; 83
 DC.W 390 ; 84
 DC.W 385 ; 85
 DC.W 381 ; 86
 DC.W 377 ; 87
 DC.W 372 ; 88
 DC.W 368 ; 89
 DC.W 364 ; 90
 DC.W 360 ; 91
 DC.W 356 ; 92
 DC.W 352 ; 93
 DC.W 349 ; 94
 DC.W 345 ; 95
 DC.W 341 ; 96
 DC.W 338 ; 97
 DC.W 334 ; 98
 DC.W 331 ; 99
 DC.W 328 ; 100
 DC.W 324 ; 101
 DC.W 321 ; 102
 DC.W 318 ; 103
 DC.W 315 ; 104
 DC.W 312 ; 105
 DC.W 309 ; 106
 DC.W 306 ; 107
 DC.W 303 ; 108
 DC.W 301 ; 109
 DC.W 298 ; 110
 DC.W 295 ; 111
 DC.W 293 ; 112
 DC.W 290 ; 113
 DC.W 287 ; 114
 DC.W 285 ; 115
 DC.W 282 ; 116
 DC.W 280 ; 117
 DC.W 278 ; 118
 DC.W 275 ; 119
 DC.W 273 ; 120
 DC.W 271 ; 121
 DC.W 269 ; 122
 DC.W 266 ; 123
 DC.W 264 ; 124
 DC.W 262 ; 125
 DC.W 260 ; 126
 DC.W 258 ; 127
 DC.W 256 ; 128
 DC.W 254 ; 129
 DC.W 252 ; 130
 DC.W 250 ; 131
 DC.W 248 ; 132
 DC.W 246 ; 133
 DC.W 245 ; 134
 DC.W 243 ; 135
 DC.W 241 ; 136
 DC.W 239 ; 137
 DC.W 237 ; 138
 DC.W 236 ; 139
 DC.W 234 ; 140
 DC.W 232 ; 141
 DC.W 231 ; 142
 DC.W 229 ; 143
 DC.W 228 ; 144
 DC.W 226 ; 145
 DC.W 224 ; 146
 DC.W 223 ; 147
 DC.W 221 ; 148
 DC.W 220 ; 149
 DC.W 218 ; 150
 DC.W 217 ; 151
 DC.W 216 ; 152
 DC.W 214 ; 153
 DC.W 213 ; 154
 DC.W 211 ; 155
 DC.W 210 ; 156
 DC.W 209 ; 157
 DC.W 207 ; 158
 DC.W 206 ; 159
 DC.W 205 ; 160
 DC.W 204 ; 161
 DC.W 202 ; 162
 DC.W 201 ; 163
 DC.W 200 ; 164
 DC.W 199 ; 165
 DC.W 197 ; 166
 DC.W 196 ; 167
 DC.W 195 ; 168
 DC.W 194 ; 169
 DC.W 193 ; 170
 DC.W 192 ; 171
 DC.W 191 ; 172
 DC.W 189 ; 173
 DC.W 188 ; 174
 DC.W 187 ; 175
 DC.W 186 ; 176
 DC.W 185 ; 177
 DC.W 184 ; 178
 DC.W 183 ; 179
 DC.W 182 ; 180
 DC.W 181 ; 181
 DC.W 180 ; 182
 DC.W 179 ; 183
 DC.W 178 ; 184
 DC.W 177 ; 185
 DC.W 176 ; 186
 DC.W 175 ; 187
 DC.W 174 ; 188
 DC.W 173 ; 189
 DC.W 172 ; 190
 DC.W 172 ; 191
 DC.W 171 ; 192
 DC.W 170 ; 193
 DC.W 169 ; 194
 DC.W 168 ; 195
 DC.W 167 ; 196
 DC.W 166 ; 197
 DC.W 165 ; 198
 DC.W 165 ; 199
 DC.W 164 ; 200
 DC.W 163 ; 201
 DC.W 162 ; 202
 DC.W 161 ; 203
 DC.W 161 ; 204
 DC.W 160 ; 205
 DC.W 159 ; 206
 DC.W 158 ; 207
 DC.W 158 ; 208
 DC.W 157 ; 209
 DC.W 156 ; 210
 DC.W 155 ; 211
 DC.W 155 ; 212
 DC.W 154 ; 213
 DC.W 153 ; 214
 DC.W 152 ; 215
 DC.W 152 ; 216
 DC.W 151 ; 217
 DC.W 150 ; 218
 DC.W 150 ; 219
 DC.W 149 ; 220
 DC.W 148 ; 221
 DC.W 148 ; 222
 DC.W 147 ; 223
 DC.W 146 ; 224
 DC.W 146 ; 225
 DC.W 145 ; 226
 DC.W 144 ; 227
 DC.W 144 ; 228
 DC.W 143 ; 229
 DC.W 142 ; 230
 DC.W 142 ; 231
 DC.W 141 ; 232
 DC.W 141 ; 233
 DC.W 140 ; 234
 DC.W 139 ; 235
 DC.W 139 ; 236
 DC.W 138 ; 237
 DC.W 138 ; 238
 DC.W 137 ; 239
 DC.W 137 ; 240
 DC.W 136 ; 241
 DC.W 135 ; 242
 DC.W 135 ; 243
 DC.W 134 ; 244
 DC.W 134 ; 245
 DC.W 133 ; 246
 DC.W 133 ; 247
 DC.W 132 ; 248
 DC.W 132 ; 249
 DC.W 131 ; 250
 DC.W 131 ; 251
 DC.W 130 ; 252
 DC.W 130 ; 253
 DC.W 129 ; 254
 DC.W 128 ; 255
 DC.W 128 ; 256
 DC.W 127 ; 257
 DC.W 127 ; 258
 DC.W 127 ; 259
 DC.W 126 ; 260
 DC.W 126 ; 261
 DC.W 125 ; 262
 DC.W 125 ; 263
 DC.W 124 ; 264
 DC.W 124 ; 265
 DC.W 123 ; 266
 DC.W 123 ; 267
 DC.W 122 ; 268
 DC.W 122 ; 269
 DC.W 121 ; 270
 DC.W 121 ; 271
 DC.W 120 ; 272
 DC.W 120 ; 273
 DC.W 120 ; 274
 DC.W 119 ; 275
 DC.W 119 ; 276
 DC.W 118 ; 277
 DC.W 118 ; 278
 DC.W 117 ; 279
 DC.W 117 ; 280
 DC.W 117 ; 281
 DC.W 116 ; 282
 DC.W 116 ; 283
 DC.W 115 ; 284
 DC.W 115 ; 285
 DC.W 115 ; 286
 DC.W 114 ; 287
 DC.W 114 ; 288
 DC.W 113 ; 289
 DC.W 113 ; 290
 DC.W 113 ; 291
 DC.W 112 ; 292
 DC.W 112 ; 293
 DC.W 111 ; 294
 DC.W 111 ; 295
 DC.W 111 ; 296
 DC.W 110 ; 297
 DC.W 110 ; 298
 DC.W 110 ; 299
 DC.W 109 ; 300
 DC.W 109 ; 301
 DC.W 109 ; 302
 DC.W 108 ; 303
 DC.W 108 ; 304
 DC.W 107 ; 305
 DC.W 107 ; 306
 DC.W 107 ; 307
 DC.W 106 ; 308
 DC.W 106 ; 309
 DC.W 106 ; 310
 DC.W 105 ; 311
 DC.W 105 ; 312
 DC.W 105 ; 313
 DC.W 104 ; 314
 DC.W 104 ; 315
 DC.W 104 ; 316
 DC.W 103 ; 317
 DC.W 103 ; 318
 DC.W 103 ; 319
 DC.W 102 ; 320
 DC.W 102 ; 321
 DC.W 102 ; 322
 DC.W 101 ; 323
 DC.W 101 ; 324
 DC.W 101 ; 325
 DC.W 101 ; 326
 DC.W 100 ; 327
 DC.W 100 ; 328
 DC.W 100 ; 329
 DC.W 99 ; 330
 DC.W 99 ; 331
 DC.W 99 ; 332
 DC.W 98 ; 333
 DC.W 98 ; 334
 DC.W 98 ; 335
 DC.W 98 ; 336
 DC.W 97 ; 337
 DC.W 97 ; 338
 DC.W 97 ; 339
 DC.W 96 ; 340
 DC.W 96 ; 341
 DC.W 96 ; 342
 DC.W 96 ; 343
 DC.W 95 ; 344
 DC.W 95 ; 345
 DC.W 95 ; 346
 DC.W 94 ; 347
 DC.W 94 ; 348
 DC.W 94 ; 349
 DC.W 94 ; 350
 DC.W 93 ; 351
 DC.W 93 ; 352
 DC.W 93 ; 353
 DC.W 93 ; 354
 DC.W 92 ; 355
 DC.W 92 ; 356
 DC.W 92 ; 357
 DC.W 92 ; 358
 DC.W 91 ; 359
 DC.W 91 ; 360
 DC.W 91 ; 361
 DC.W 91 ; 362
 DC.W 90 ; 363
 DC.W 90 ; 364
 DC.W 90 ; 365
 DC.W 90 ; 366
 DC.W 89 ; 367
 DC.W 89 ; 368
 DC.W 89 ; 369
 DC.W 89 ; 370
 DC.W 88 ; 371
 DC.W 88 ; 372
 DC.W 88 ; 373
 DC.W 88 ; 374
 DC.W 87 ; 375
 DC.W 87 ; 376
 DC.W 87 ; 377
 DC.W 87 ; 378
 DC.W 86 ; 379
 DC.W 86 ; 380
 DC.W 86 ; 381
 DC.W 86 ; 382
 DC.W 86 ; 383
 DC.W 85 ; 384
 DC.W 85 ; 385
 DC.W 85 ; 386
 DC.W 85 ; 387
 DC.W 84 ; 388
 DC.W 84 ; 389
 DC.W 84 ; 390
 DC.W 84 ; 391
 DC.W 84 ; 392
 DC.W 83 ; 393
 DC.W 83 ; 394
 DC.W 83 ; 395
 DC.W 83 ; 396
 DC.W 83 ; 397
 DC.W 82 ; 398
 DC.W 82 ; 399
 DC.W 82 ; 400
 DC.W 82 ; 401
 DC.W 82 ; 402
 DC.W 81 ; 403
 DC.W 81 ; 404
 DC.W 81 ; 405
 DC.W 81 ; 406
 DC.W 81 ; 407
 DC.W 80 ; 408
 DC.W 80 ; 409
 DC.W 80 ; 410
 DC.W 80 ; 411
 DC.W 80 ; 412
 DC.W 79 ; 413
 DC.W 79 ; 414
 DC.W 79 ; 415
 DC.W 79 ; 416
 DC.W 79 ; 417
 DC.W 78 ; 418
 DC.W 78 ; 419
 DC.W 78 ; 420
 DC.W 78 ; 421
 DC.W 78 ; 422
 DC.W 77 ; 423
 DC.W 77 ; 424
 DC.W 77 ; 425
 DC.W 77 ; 426
 DC.W 77 ; 427
 DC.W 77 ; 428
 DC.W 76 ; 429
 DC.W 76 ; 430
 DC.W 76 ; 431
 DC.W 76 ; 432
 DC.W 76 ; 433
 DC.W 76 ; 434
 DC.W 75 ; 435
 DC.W 75 ; 436
 DC.W 75 ; 437
 DC.W 75 ; 438
 DC.W 75 ; 439
 DC.W 74 ; 440
 DC.W 74 ; 441
 DC.W 74 ; 442
 DC.W 74 ; 443
 DC.W 74 ; 444
 DC.W 74 ; 445
 DC.W 73 ; 446
 DC.W 73 ; 447
 DC.W 73 ; 448
 DC.W 73 ; 449
 DC.W 73 ; 450
 DC.W 73 ; 451
 DC.W 72 ; 452
 DC.W 72 ; 453
 DC.W 72 ; 454
 DC.W 72 ; 455
 DC.W 72 ; 456
 DC.W 72 ; 457
 DC.W 72 ; 458
 DC.W 71 ; 459
 DC.W 71 ; 460
 DC.W 71 ; 461
 DC.W 71 ; 462
 DC.W 71 ; 463
 DC.W 71 ; 464
 DC.W 70 ; 465
 DC.W 70 ; 466
 DC.W 70 ; 467
 DC.W 70 ; 468
 DC.W 70 ; 469
 DC.W 70 ; 470
 DC.W 70 ; 471
 DC.W 69 ; 472
 DC.W 69 ; 473
 DC.W 69 ; 474
 DC.W 69 ; 475
 DC.W 69 ; 476
 DC.W 69 ; 477
 DC.W 69 ; 478
 DC.W 68 ; 479
 DC.W 68 ; 480
 DC.W 68 ; 481
 DC.W 68 ; 482
 DC.W 68 ; 483
 DC.W 68 ; 484
 DC.W 68 ; 485
 DC.W 67 ; 486
 DC.W 67 ; 487
 DC.W 67 ; 488
 DC.W 67 ; 489
 DC.W 67 ; 490
 DC.W 67 ; 491
 DC.W 67 ; 492
 DC.W 66 ; 493
 DC.W 66 ; 494
 DC.W 66 ; 495
 DC.W 66 ; 496
 DC.W 66 ; 497
 DC.W 66 ; 498
 DC.W 66 ; 499
 DC.W 66 ; 500
 DC.W 65 ; 501
 DC.W 65 ; 502
 DC.W 65 ; 503
 DC.W 65 ; 504
 DC.W 65 ; 505
 DC.W 65 ; 506
 DC.W 65 ; 507
 DC.W 65 ; 508
 DC.W 64 ; 509
 DC.W 64 ; 510
 DC.W 64 ; 511
 DC.W 64 ; 512
 DC.W 64 ; 513

hexhcharlist:	dc.B	%01111111,%01100011,%01100011,%01100011,%01111111
				dc.B	%00001100,%00111100,%00001100,%00001100,%01111111
				dc.B	%01111111,%00000011,%01111111,%01100000,%01111111
				dc.B	%01111111,%00000011,%01111111,%00000011,%01111111
				dc.B	%01100011,%01100011,%01111111,%00000011,%00000011
				dc.B	%01111111,%01100000,%01111111,%00000011,%01111111
				dc.B	%01111111,%01100000,%01111111,%01100011,%01111111
				dc.B	%01111111,%00000011,%00000011,%00000011,%00000011
				dc.B	%01111111,%01100011,%01111111,%01100011,%01111111
				dc.B	%01111111,%01100011,%01111111,%00000011,%01111111
				dc.B	%01111111,%01100011,%01111111,%01100011,%01100011
				dc.B	%01111110,%01100011,%01111110,%01100011,%01111110
				dc.B	%01111111,%01100000,%01100000,%01100000,%01111111
				dc.B	%01111110,%01100011,%01100011,%01100011,%01111110
				dc.B	%01111111,%01100000,%01111111,%01100000,%01111111
				dc.B	%01111111,%01100000,%01111111,%01100000,%01100000

;HEX_LINE0:	DC.B	%01111111,%00001100,%01111111,%01111111,%01100011,%01111111,%01111111,%01111111,%01111111,%01111111,%01111111,%01111110,%01111111,%01111110,%01111111,%01111111
;HEX_LINE1:	DC.B	%01100011,%00111100,%00000011,%00000011,%01100011,%01100000,%01100000,%00000011,%01100011,%01100011,%01100011,%01100011,%01100000,%01100011,%01100000,%01100000
;HEX_LINE2:	DC.B	%01100011,%00001100,%01111111,%01111111,%01111111,%01111111,%01111111,%00000011,%01111111,%01111111,%01111111,%01111110,%01100000,%01100011,%01111111,%01111111
;HEX_LINE3:	DC.B	%01100011,%00001100,%01100000,%00000011,%00000011,%00000011,%01100011,%00000011,%01100011,%00000011,%01100011,%01100011,%01100000,%01100011,%01100000,%01100000
;HEX_LINE4:	DC.B	%01111111,%01111111,%01111111,%01111111,%00000011,%01111111,%01111111,%00000011,%01111111,%01111111,%01100011,%01111110,%01111111,%01111110,%01111111,%01100000

mainscreenp:	DC.L	scrptr1,scrptr2,scrptr3

scrptr1	DC.L	scr1+(SCN_WIDTH_CHAR*000),scr1+(SCN_WIDTH_CHAR*001),scr1+(SCN_WIDTH_CHAR*002),scr1+(SCN_WIDTH_CHAR*003),scr1+(SCN_WIDTH_CHAR*004),scr1+(SCN_WIDTH_CHAR*005),scr1+(SCN_WIDTH_CHAR*006),scr1+(SCN_WIDTH_CHAR*007),scr1+(SCN_WIDTH_CHAR*008),scr1+(SCN_WIDTH_CHAR*009)
	DC.L	scr1+(SCN_WIDTH_CHAR*010),scr1+(SCN_WIDTH_CHAR*011),scr1+(SCN_WIDTH_CHAR*012),scr1+(SCN_WIDTH_CHAR*013),scr1+(SCN_WIDTH_CHAR*014),scr1+(SCN_WIDTH_CHAR*015),scr1+(SCN_WIDTH_CHAR*016),scr1+(SCN_WIDTH_CHAR*017),scr1+(SCN_WIDTH_CHAR*018),scr1+(SCN_WIDTH_CHAR*019)
	DC.L	scr1+(SCN_WIDTH_CHAR*020),scr1+(SCN_WIDTH_CHAR*021),scr1+(SCN_WIDTH_CHAR*022),scr1+(SCN_WIDTH_CHAR*023),scr1+(SCN_WIDTH_CHAR*024),scr1+(SCN_WIDTH_CHAR*025),scr1+(SCN_WIDTH_CHAR*026),scr1+(SCN_WIDTH_CHAR*027),scr1+(SCN_WIDTH_CHAR*028),scr1+(SCN_WIDTH_CHAR*029)
	DC.L	scr1+(SCN_WIDTH_CHAR*030),scr1+(SCN_WIDTH_CHAR*031),scr1+(SCN_WIDTH_CHAR*032),scr1+(SCN_WIDTH_CHAR*033),scr1+(SCN_WIDTH_CHAR*034),scr1+(SCN_WIDTH_CHAR*035),scr1+(SCN_WIDTH_CHAR*036),scr1+(SCN_WIDTH_CHAR*037),scr1+(SCN_WIDTH_CHAR*038),scr1+(SCN_WIDTH_CHAR*039)
	DC.L	scr1+(SCN_WIDTH_CHAR*040),scr1+(SCN_WIDTH_CHAR*041),scr1+(SCN_WIDTH_CHAR*042),scr1+(SCN_WIDTH_CHAR*043),scr1+(SCN_WIDTH_CHAR*044),scr1+(SCN_WIDTH_CHAR*045),scr1+(SCN_WIDTH_CHAR*046),scr1+(SCN_WIDTH_CHAR*047),scr1+(SCN_WIDTH_CHAR*048),scr1+(SCN_WIDTH_CHAR*049)
	DC.L	scr1+(SCN_WIDTH_CHAR*050),scr1+(SCN_WIDTH_CHAR*051),scr1+(SCN_WIDTH_CHAR*052),scr1+(SCN_WIDTH_CHAR*053),scr1+(SCN_WIDTH_CHAR*054),scr1+(SCN_WIDTH_CHAR*055),scr1+(SCN_WIDTH_CHAR*056),scr1+(SCN_WIDTH_CHAR*057),scr1+(SCN_WIDTH_CHAR*058),scr1+(SCN_WIDTH_CHAR*059)
	DC.L	scr1+(SCN_WIDTH_CHAR*060),scr1+(SCN_WIDTH_CHAR*061),scr1+(SCN_WIDTH_CHAR*062),scr1+(SCN_WIDTH_CHAR*063),scr1+(SCN_WIDTH_CHAR*064),scr1+(SCN_WIDTH_CHAR*065),scr1+(SCN_WIDTH_CHAR*066),scr1+(SCN_WIDTH_CHAR*067),scr1+(SCN_WIDTH_CHAR*068),scr1+(SCN_WIDTH_CHAR*069)
	DC.L	scr1+(SCN_WIDTH_CHAR*070),scr1+(SCN_WIDTH_CHAR*071),scr1+(SCN_WIDTH_CHAR*072),scr1+(SCN_WIDTH_CHAR*073),scr1+(SCN_WIDTH_CHAR*074),scr1+(SCN_WIDTH_CHAR*075),scr1+(SCN_WIDTH_CHAR*076),scr1+(SCN_WIDTH_CHAR*077),scr1+(SCN_WIDTH_CHAR*078),scr1+(SCN_WIDTH_CHAR*079)
	DC.L	scr1+(SCN_WIDTH_CHAR*080),scr1+(SCN_WIDTH_CHAR*081),scr1+(SCN_WIDTH_CHAR*082),scr1+(SCN_WIDTH_CHAR*083),scr1+(SCN_WIDTH_CHAR*084),scr1+(SCN_WIDTH_CHAR*085),scr1+(SCN_WIDTH_CHAR*086),scr1+(SCN_WIDTH_CHAR*087),scr1+(SCN_WIDTH_CHAR*088),scr1+(SCN_WIDTH_CHAR*089)
	DC.L	scr1+(SCN_WIDTH_CHAR*090),scr1+(SCN_WIDTH_CHAR*091),scr1+(SCN_WIDTH_CHAR*092),scr1+(SCN_WIDTH_CHAR*093),scr1+(SCN_WIDTH_CHAR*094),scr1+(SCN_WIDTH_CHAR*095),scr1+(SCN_WIDTH_CHAR*096),scr1+(SCN_WIDTH_CHAR*097),scr1+(SCN_WIDTH_CHAR*098),scr1+(SCN_WIDTH_CHAR*099)
	DC.L	scr1+(SCN_WIDTH_CHAR*100),scr1+(SCN_WIDTH_CHAR*101),scr1+(SCN_WIDTH_CHAR*102),scr1+(SCN_WIDTH_CHAR*103),scr1+(SCN_WIDTH_CHAR*104),scr1+(SCN_WIDTH_CHAR*105),scr1+(SCN_WIDTH_CHAR*106),scr1+(SCN_WIDTH_CHAR*107),scr1+(SCN_WIDTH_CHAR*108),scr1+(SCN_WIDTH_CHAR*109)
	DC.L	scr1+(SCN_WIDTH_CHAR*110),scr1+(SCN_WIDTH_CHAR*111),scr1+(SCN_WIDTH_CHAR*112),scr1+(SCN_WIDTH_CHAR*113),scr1+(SCN_WIDTH_CHAR*114),scr1+(SCN_WIDTH_CHAR*115),scr1+(SCN_WIDTH_CHAR*116),scr1+(SCN_WIDTH_CHAR*117),scr1+(SCN_WIDTH_CHAR*118),scr1+(SCN_WIDTH_CHAR*119)
	DC.L	scr1+(SCN_WIDTH_CHAR*120),scr1+(SCN_WIDTH_CHAR*121),scr1+(SCN_WIDTH_CHAR*122),scr1+(SCN_WIDTH_CHAR*123),scr1+(SCN_WIDTH_CHAR*124),scr1+(SCN_WIDTH_CHAR*125),scr1+(SCN_WIDTH_CHAR*126),scr1+(SCN_WIDTH_CHAR*127),scr1+(SCN_WIDTH_CHAR*128),scr1+(SCN_WIDTH_CHAR*129)
	DC.L	scr1+(SCN_WIDTH_CHAR*130),scr1+(SCN_WIDTH_CHAR*131),scr1+(SCN_WIDTH_CHAR*132),scr1+(SCN_WIDTH_CHAR*133),scr1+(SCN_WIDTH_CHAR*134),scr1+(SCN_WIDTH_CHAR*135),scr1+(SCN_WIDTH_CHAR*136),scr1+(SCN_WIDTH_CHAR*137),scr1+(SCN_WIDTH_CHAR*138),scr1+(SCN_WIDTH_CHAR*139)
	DC.L	scr1+(SCN_WIDTH_CHAR*140),scr1+(SCN_WIDTH_CHAR*141),scr1+(SCN_WIDTH_CHAR*142),scr1+(SCN_WIDTH_CHAR*143),scr1+(SCN_WIDTH_CHAR*144),scr1+(SCN_WIDTH_CHAR*145),scr1+(SCN_WIDTH_CHAR*146),scr1+(SCN_WIDTH_CHAR*147),scr1+(SCN_WIDTH_CHAR*148),scr1+(SCN_WIDTH_CHAR*149)
	DC.L	scr1+(SCN_WIDTH_CHAR*150),scr1+(SCN_WIDTH_CHAR*151),scr1+(SCN_WIDTH_CHAR*152),scr1+(SCN_WIDTH_CHAR*153),scr1+(SCN_WIDTH_CHAR*154),scr1+(SCN_WIDTH_CHAR*155),scr1+(SCN_WIDTH_CHAR*156),scr1+(SCN_WIDTH_CHAR*157),scr1+(SCN_WIDTH_CHAR*158),scr1+(SCN_WIDTH_CHAR*159)
	DC.L	scr1+(SCN_WIDTH_CHAR*160),scr1+(SCN_WIDTH_CHAR*161),scr1+(SCN_WIDTH_CHAR*162),scr1+(SCN_WIDTH_CHAR*163),scr1+(SCN_WIDTH_CHAR*164),scr1+(SCN_WIDTH_CHAR*165),scr1+(SCN_WIDTH_CHAR*166),scr1+(SCN_WIDTH_CHAR*167),scr1+(SCN_WIDTH_CHAR*168),scr1+(SCN_WIDTH_CHAR*169)
	DC.L	scr1+(SCN_WIDTH_CHAR*170),scr1+(SCN_WIDTH_CHAR*171),scr1+(SCN_WIDTH_CHAR*172),scr1+(SCN_WIDTH_CHAR*173),scr1+(SCN_WIDTH_CHAR*174),scr1+(SCN_WIDTH_CHAR*175),scr1+(SCN_WIDTH_CHAR*176),scr1+(SCN_WIDTH_CHAR*177),scr1+(SCN_WIDTH_CHAR*178),scr1+(SCN_WIDTH_CHAR*179)
	DC.L	scr1+(SCN_WIDTH_CHAR*180),scr1+(SCN_WIDTH_CHAR*181),scr1+(SCN_WIDTH_CHAR*182),scr1+(SCN_WIDTH_CHAR*183),scr1+(SCN_WIDTH_CHAR*184),scr1+(SCN_WIDTH_CHAR*185),scr1+(SCN_WIDTH_CHAR*186),scr1+(SCN_WIDTH_CHAR*187),scr1+(SCN_WIDTH_CHAR*188),scr1+(SCN_WIDTH_CHAR*189)
	DC.L	scr1+(SCN_WIDTH_CHAR*190),scr1+(SCN_WIDTH_CHAR*191),scr1+(SCN_WIDTH_CHAR*192),scr1+(SCN_WIDTH_CHAR*193),scr1+(SCN_WIDTH_CHAR*194),scr1+(SCN_WIDTH_CHAR*195),scr1+(SCN_WIDTH_CHAR*196),scr1+(SCN_WIDTH_CHAR*197),scr1+(SCN_WIDTH_CHAR*198),scr1+(SCN_WIDTH_CHAR*199)

scrptr2	DC.L	scr2+(SCN_WIDTH_CHAR*000),scr2+(SCN_WIDTH_CHAR*001),scr2+(SCN_WIDTH_CHAR*002),scr2+(SCN_WIDTH_CHAR*003),scr2+(SCN_WIDTH_CHAR*004),scr2+(SCN_WIDTH_CHAR*005),scr2+(SCN_WIDTH_CHAR*006),scr2+(SCN_WIDTH_CHAR*007),scr2+(SCN_WIDTH_CHAR*008),scr2+(SCN_WIDTH_CHAR*009)
	DC.L	scr2+(SCN_WIDTH_CHAR*010),scr2+(SCN_WIDTH_CHAR*011),scr2+(SCN_WIDTH_CHAR*012),scr2+(SCN_WIDTH_CHAR*013),scr2+(SCN_WIDTH_CHAR*014),scr2+(SCN_WIDTH_CHAR*015),scr2+(SCN_WIDTH_CHAR*016),scr2+(SCN_WIDTH_CHAR*017),scr2+(SCN_WIDTH_CHAR*018),scr2+(SCN_WIDTH_CHAR*019)
	DC.L	scr2+(SCN_WIDTH_CHAR*020),scr2+(SCN_WIDTH_CHAR*021),scr2+(SCN_WIDTH_CHAR*022),scr2+(SCN_WIDTH_CHAR*023),scr2+(SCN_WIDTH_CHAR*024),scr2+(SCN_WIDTH_CHAR*025),scr2+(SCN_WIDTH_CHAR*026),scr2+(SCN_WIDTH_CHAR*027),scr2+(SCN_WIDTH_CHAR*028),scr2+(SCN_WIDTH_CHAR*029)
	DC.L	scr2+(SCN_WIDTH_CHAR*030),scr2+(SCN_WIDTH_CHAR*031),scr2+(SCN_WIDTH_CHAR*032),scr2+(SCN_WIDTH_CHAR*033),scr2+(SCN_WIDTH_CHAR*034),scr2+(SCN_WIDTH_CHAR*035),scr2+(SCN_WIDTH_CHAR*036),scr2+(SCN_WIDTH_CHAR*037),scr2+(SCN_WIDTH_CHAR*038),scr2+(SCN_WIDTH_CHAR*039)
	DC.L	scr2+(SCN_WIDTH_CHAR*040),scr2+(SCN_WIDTH_CHAR*041),scr2+(SCN_WIDTH_CHAR*042),scr2+(SCN_WIDTH_CHAR*043),scr2+(SCN_WIDTH_CHAR*044),scr2+(SCN_WIDTH_CHAR*045),scr2+(SCN_WIDTH_CHAR*046),scr2+(SCN_WIDTH_CHAR*047),scr2+(SCN_WIDTH_CHAR*048),scr2+(SCN_WIDTH_CHAR*049)
	DC.L	scr2+(SCN_WIDTH_CHAR*050),scr2+(SCN_WIDTH_CHAR*051),scr2+(SCN_WIDTH_CHAR*052),scr2+(SCN_WIDTH_CHAR*053),scr2+(SCN_WIDTH_CHAR*054),scr2+(SCN_WIDTH_CHAR*055),scr2+(SCN_WIDTH_CHAR*056),scr2+(SCN_WIDTH_CHAR*057),scr2+(SCN_WIDTH_CHAR*058),scr2+(SCN_WIDTH_CHAR*059)
	DC.L	scr2+(SCN_WIDTH_CHAR*060),scr2+(SCN_WIDTH_CHAR*061),scr2+(SCN_WIDTH_CHAR*062),scr2+(SCN_WIDTH_CHAR*063),scr2+(SCN_WIDTH_CHAR*064),scr2+(SCN_WIDTH_CHAR*065),scr2+(SCN_WIDTH_CHAR*066),scr2+(SCN_WIDTH_CHAR*067),scr2+(SCN_WIDTH_CHAR*068),scr2+(SCN_WIDTH_CHAR*069)
	DC.L	scr2+(SCN_WIDTH_CHAR*070),scr2+(SCN_WIDTH_CHAR*071),scr2+(SCN_WIDTH_CHAR*072),scr2+(SCN_WIDTH_CHAR*073),scr2+(SCN_WIDTH_CHAR*074),scr2+(SCN_WIDTH_CHAR*075),scr2+(SCN_WIDTH_CHAR*076),scr2+(SCN_WIDTH_CHAR*077),scr2+(SCN_WIDTH_CHAR*078),scr2+(SCN_WIDTH_CHAR*079)
	DC.L	scr2+(SCN_WIDTH_CHAR*080),scr2+(SCN_WIDTH_CHAR*081),scr2+(SCN_WIDTH_CHAR*082),scr2+(SCN_WIDTH_CHAR*083),scr2+(SCN_WIDTH_CHAR*084),scr2+(SCN_WIDTH_CHAR*085),scr2+(SCN_WIDTH_CHAR*086),scr2+(SCN_WIDTH_CHAR*087),scr2+(SCN_WIDTH_CHAR*088),scr2+(SCN_WIDTH_CHAR*089)
	DC.L	scr2+(SCN_WIDTH_CHAR*090),scr2+(SCN_WIDTH_CHAR*091),scr2+(SCN_WIDTH_CHAR*092),scr2+(SCN_WIDTH_CHAR*093),scr2+(SCN_WIDTH_CHAR*094),scr2+(SCN_WIDTH_CHAR*095),scr2+(SCN_WIDTH_CHAR*096),scr2+(SCN_WIDTH_CHAR*097),scr2+(SCN_WIDTH_CHAR*098),scr2+(SCN_WIDTH_CHAR*099)
	DC.L	scr2+(SCN_WIDTH_CHAR*100),scr2+(SCN_WIDTH_CHAR*101),scr2+(SCN_WIDTH_CHAR*102),scr2+(SCN_WIDTH_CHAR*103),scr2+(SCN_WIDTH_CHAR*104),scr2+(SCN_WIDTH_CHAR*105),scr2+(SCN_WIDTH_CHAR*106),scr2+(SCN_WIDTH_CHAR*107),scr2+(SCN_WIDTH_CHAR*108),scr2+(SCN_WIDTH_CHAR*109)
	DC.L	scr2+(SCN_WIDTH_CHAR*110),scr2+(SCN_WIDTH_CHAR*111),scr2+(SCN_WIDTH_CHAR*112),scr2+(SCN_WIDTH_CHAR*113),scr2+(SCN_WIDTH_CHAR*114),scr2+(SCN_WIDTH_CHAR*115),scr2+(SCN_WIDTH_CHAR*116),scr2+(SCN_WIDTH_CHAR*117),scr2+(SCN_WIDTH_CHAR*118),scr2+(SCN_WIDTH_CHAR*119)
	DC.L	scr2+(SCN_WIDTH_CHAR*120),scr2+(SCN_WIDTH_CHAR*121),scr2+(SCN_WIDTH_CHAR*122),scr2+(SCN_WIDTH_CHAR*123),scr2+(SCN_WIDTH_CHAR*124),scr2+(SCN_WIDTH_CHAR*125),scr2+(SCN_WIDTH_CHAR*126),scr2+(SCN_WIDTH_CHAR*127),scr2+(SCN_WIDTH_CHAR*128),scr2+(SCN_WIDTH_CHAR*129)
	DC.L	scr2+(SCN_WIDTH_CHAR*130),scr2+(SCN_WIDTH_CHAR*131),scr2+(SCN_WIDTH_CHAR*132),scr2+(SCN_WIDTH_CHAR*133),scr2+(SCN_WIDTH_CHAR*134),scr2+(SCN_WIDTH_CHAR*135),scr2+(SCN_WIDTH_CHAR*136),scr2+(SCN_WIDTH_CHAR*137),scr2+(SCN_WIDTH_CHAR*138),scr2+(SCN_WIDTH_CHAR*139)
	DC.L	scr2+(SCN_WIDTH_CHAR*140),scr2+(SCN_WIDTH_CHAR*141),scr2+(SCN_WIDTH_CHAR*142),scr2+(SCN_WIDTH_CHAR*143),scr2+(SCN_WIDTH_CHAR*144),scr2+(SCN_WIDTH_CHAR*145),scr2+(SCN_WIDTH_CHAR*146),scr2+(SCN_WIDTH_CHAR*147),scr2+(SCN_WIDTH_CHAR*148),scr2+(SCN_WIDTH_CHAR*149)
	DC.L	scr2+(SCN_WIDTH_CHAR*150),scr2+(SCN_WIDTH_CHAR*151),scr2+(SCN_WIDTH_CHAR*152),scr2+(SCN_WIDTH_CHAR*153),scr2+(SCN_WIDTH_CHAR*154),scr2+(SCN_WIDTH_CHAR*155),scr2+(SCN_WIDTH_CHAR*156),scr2+(SCN_WIDTH_CHAR*157),scr2+(SCN_WIDTH_CHAR*158),scr2+(SCN_WIDTH_CHAR*159)
	DC.L	scr2+(SCN_WIDTH_CHAR*160),scr2+(SCN_WIDTH_CHAR*161),scr2+(SCN_WIDTH_CHAR*162),scr2+(SCN_WIDTH_CHAR*163),scr2+(SCN_WIDTH_CHAR*164),scr2+(SCN_WIDTH_CHAR*165),scr2+(SCN_WIDTH_CHAR*166),scr2+(SCN_WIDTH_CHAR*167),scr2+(SCN_WIDTH_CHAR*168),scr2+(SCN_WIDTH_CHAR*169)
	DC.L	scr2+(SCN_WIDTH_CHAR*170),scr2+(SCN_WIDTH_CHAR*171),scr2+(SCN_WIDTH_CHAR*172),scr2+(SCN_WIDTH_CHAR*173),scr2+(SCN_WIDTH_CHAR*174),scr2+(SCN_WIDTH_CHAR*175),scr2+(SCN_WIDTH_CHAR*176),scr2+(SCN_WIDTH_CHAR*177),scr2+(SCN_WIDTH_CHAR*178),scr2+(SCN_WIDTH_CHAR*179)
	DC.L	scr2+(SCN_WIDTH_CHAR*180),scr2+(SCN_WIDTH_CHAR*181),scr2+(SCN_WIDTH_CHAR*182),scr2+(SCN_WIDTH_CHAR*183),scr2+(SCN_WIDTH_CHAR*184),scr2+(SCN_WIDTH_CHAR*185),scr2+(SCN_WIDTH_CHAR*186),scr2+(SCN_WIDTH_CHAR*187),scr2+(SCN_WIDTH_CHAR*188),scr2+(SCN_WIDTH_CHAR*189)
	DC.L	scr2+(SCN_WIDTH_CHAR*190),scr2+(SCN_WIDTH_CHAR*191),scr2+(SCN_WIDTH_CHAR*192),scr2+(SCN_WIDTH_CHAR*193),scr2+(SCN_WIDTH_CHAR*194),scr2+(SCN_WIDTH_CHAR*195),scr2+(SCN_WIDTH_CHAR*196),scr2+(SCN_WIDTH_CHAR*197),scr2+(SCN_WIDTH_CHAR*198),scr2+(SCN_WIDTH_CHAR*199)

scrptr3	DC.L	scr3+(SCN_WIDTH_CHAR*000),scr3+(SCN_WIDTH_CHAR*001),scr3+(SCN_WIDTH_CHAR*002),scr3+(SCN_WIDTH_CHAR*003),scr3+(SCN_WIDTH_CHAR*004),scr3+(SCN_WIDTH_CHAR*005),scr3+(SCN_WIDTH_CHAR*006),scr3+(SCN_WIDTH_CHAR*007),scr3+(SCN_WIDTH_CHAR*008),scr3+(SCN_WIDTH_CHAR*009)
	DC.L 	scr3+(SCN_WIDTH_CHAR*010),scr3+(SCN_WIDTH_CHAR*011),scr3+(SCN_WIDTH_CHAR*012),scr3+(SCN_WIDTH_CHAR*013),scr3+(SCN_WIDTH_CHAR*014),scr3+(SCN_WIDTH_CHAR*015),scr3+(SCN_WIDTH_CHAR*016),scr3+(SCN_WIDTH_CHAR*017),scr3+(SCN_WIDTH_CHAR*018),scr3+(SCN_WIDTH_CHAR*019)
	DC.L	scr3+(SCN_WIDTH_CHAR*020),scr3+(SCN_WIDTH_CHAR*021),scr3+(SCN_WIDTH_CHAR*022),scr3+(SCN_WIDTH_CHAR*023),scr3+(SCN_WIDTH_CHAR*024),scr3+(SCN_WIDTH_CHAR*025),scr3+(SCN_WIDTH_CHAR*026),scr3+(SCN_WIDTH_CHAR*027),scr3+(SCN_WIDTH_CHAR*028),scr3+(SCN_WIDTH_CHAR*029)
	DC.L	scr3+(SCN_WIDTH_CHAR*030),scr3+(SCN_WIDTH_CHAR*031),scr3+(SCN_WIDTH_CHAR*032),scr3+(SCN_WIDTH_CHAR*033),scr3+(SCN_WIDTH_CHAR*034),scr3+(SCN_WIDTH_CHAR*035),scr3+(SCN_WIDTH_CHAR*036),scr3+(SCN_WIDTH_CHAR*037),scr3+(SCN_WIDTH_CHAR*038),scr3+(SCN_WIDTH_CHAR*039)
	DC.L	scr3+(SCN_WIDTH_CHAR*040),scr3+(SCN_WIDTH_CHAR*041),scr3+(SCN_WIDTH_CHAR*042),scr3+(SCN_WIDTH_CHAR*043),scr3+(SCN_WIDTH_CHAR*044),scr3+(SCN_WIDTH_CHAR*045),scr3+(SCN_WIDTH_CHAR*046),scr3+(SCN_WIDTH_CHAR*047),scr3+(SCN_WIDTH_CHAR*048),scr3+(SCN_WIDTH_CHAR*049)
	DC.L	scr3+(SCN_WIDTH_CHAR*050),scr3+(SCN_WIDTH_CHAR*051),scr3+(SCN_WIDTH_CHAR*052),scr3+(SCN_WIDTH_CHAR*053),scr3+(SCN_WIDTH_CHAR*054),scr3+(SCN_WIDTH_CHAR*055),scr3+(SCN_WIDTH_CHAR*056),scr3+(SCN_WIDTH_CHAR*057),scr3+(SCN_WIDTH_CHAR*058),scr3+(SCN_WIDTH_CHAR*059)
	DC.L	scr3+(SCN_WIDTH_CHAR*060),scr3+(SCN_WIDTH_CHAR*061),scr3+(SCN_WIDTH_CHAR*062),scr3+(SCN_WIDTH_CHAR*063),scr3+(SCN_WIDTH_CHAR*064),scr3+(SCN_WIDTH_CHAR*065),scr3+(SCN_WIDTH_CHAR*066),scr3+(SCN_WIDTH_CHAR*067),scr3+(SCN_WIDTH_CHAR*068),scr3+(SCN_WIDTH_CHAR*069)
	DC.L	scr3+(SCN_WIDTH_CHAR*070),scr3+(SCN_WIDTH_CHAR*071),scr3+(SCN_WIDTH_CHAR*072),scr3+(SCN_WIDTH_CHAR*073),scr3+(SCN_WIDTH_CHAR*074),scr3+(SCN_WIDTH_CHAR*075),scr3+(SCN_WIDTH_CHAR*076),scr3+(SCN_WIDTH_CHAR*077),scr3+(SCN_WIDTH_CHAR*078),scr3+(SCN_WIDTH_CHAR*079)
	DC.L	scr3+(SCN_WIDTH_CHAR*080),scr3+(SCN_WIDTH_CHAR*081),scr3+(SCN_WIDTH_CHAR*082),scr3+(SCN_WIDTH_CHAR*083),scr3+(SCN_WIDTH_CHAR*084),scr3+(SCN_WIDTH_CHAR*085),scr3+(SCN_WIDTH_CHAR*086),scr3+(SCN_WIDTH_CHAR*087),scr3+(SCN_WIDTH_CHAR*088),scr3+(SCN_WIDTH_CHAR*089)
	DC.L	scr3+(SCN_WIDTH_CHAR*090),scr3+(SCN_WIDTH_CHAR*091),scr3+(SCN_WIDTH_CHAR*092),scr3+(SCN_WIDTH_CHAR*093),scr3+(SCN_WIDTH_CHAR*094),scr3+(SCN_WIDTH_CHAR*095),scr3+(SCN_WIDTH_CHAR*096),scr3+(SCN_WIDTH_CHAR*097),scr3+(SCN_WIDTH_CHAR*098),scr3+(SCN_WIDTH_CHAR*099)
	DC.L	scr3+(SCN_WIDTH_CHAR*100),scr3+(SCN_WIDTH_CHAR*101),scr3+(SCN_WIDTH_CHAR*102),scr3+(SCN_WIDTH_CHAR*103),scr3+(SCN_WIDTH_CHAR*104),scr3+(SCN_WIDTH_CHAR*105),scr3+(SCN_WIDTH_CHAR*106),scr3+(SCN_WIDTH_CHAR*107),scr3+(SCN_WIDTH_CHAR*108),scr3+(SCN_WIDTH_CHAR*109)
	DC.L	scr3+(SCN_WIDTH_CHAR*110),scr3+(SCN_WIDTH_CHAR*111),scr3+(SCN_WIDTH_CHAR*112),scr3+(SCN_WIDTH_CHAR*113),scr3+(SCN_WIDTH_CHAR*114),scr3+(SCN_WIDTH_CHAR*115),scr3+(SCN_WIDTH_CHAR*116),scr3+(SCN_WIDTH_CHAR*117),scr3+(SCN_WIDTH_CHAR*118),scr3+(SCN_WIDTH_CHAR*119)
	DC.L	scr3+(SCN_WIDTH_CHAR*120),scr3+(SCN_WIDTH_CHAR*121),scr3+(SCN_WIDTH_CHAR*122),scr3+(SCN_WIDTH_CHAR*123),scr3+(SCN_WIDTH_CHAR*124),scr3+(SCN_WIDTH_CHAR*125),scr3+(SCN_WIDTH_CHAR*126),scr3+(SCN_WIDTH_CHAR*127),scr3+(SCN_WIDTH_CHAR*128),scr3+(SCN_WIDTH_CHAR*129)
	DC.L	scr3+(SCN_WIDTH_CHAR*130),scr3+(SCN_WIDTH_CHAR*131),scr3+(SCN_WIDTH_CHAR*132),scr3+(SCN_WIDTH_CHAR*133),scr3+(SCN_WIDTH_CHAR*134),scr3+(SCN_WIDTH_CHAR*135),scr3+(SCN_WIDTH_CHAR*136),scr3+(SCN_WIDTH_CHAR*137),scr3+(SCN_WIDTH_CHAR*138),scr3+(SCN_WIDTH_CHAR*139)
	DC.L	scr3+(SCN_WIDTH_CHAR*140),scr3+(SCN_WIDTH_CHAR*141),scr3+(SCN_WIDTH_CHAR*142),scr3+(SCN_WIDTH_CHAR*143),scr3+(SCN_WIDTH_CHAR*144),scr3+(SCN_WIDTH_CHAR*145),scr3+(SCN_WIDTH_CHAR*146),scr3+(SCN_WIDTH_CHAR*147),scr3+(SCN_WIDTH_CHAR*148),scr3+(SCN_WIDTH_CHAR*149)
	DC.L	scr3+(SCN_WIDTH_CHAR*150),scr3+(SCN_WIDTH_CHAR*151),scr3+(SCN_WIDTH_CHAR*152),scr3+(SCN_WIDTH_CHAR*153),scr3+(SCN_WIDTH_CHAR*154),scr3+(SCN_WIDTH_CHAR*155),scr3+(SCN_WIDTH_CHAR*156),scr3+(SCN_WIDTH_CHAR*157),scr3+(SCN_WIDTH_CHAR*158),scr3+(SCN_WIDTH_CHAR*159)
	DC.L	scr3+(SCN_WIDTH_CHAR*160),scr3+(SCN_WIDTH_CHAR*161),scr3+(SCN_WIDTH_CHAR*162),scr3+(SCN_WIDTH_CHAR*163),scr3+(SCN_WIDTH_CHAR*164),scr3+(SCN_WIDTH_CHAR*165),scr3+(SCN_WIDTH_CHAR*166),scr3+(SCN_WIDTH_CHAR*167),scr3+(SCN_WIDTH_CHAR*168),scr3+(SCN_WIDTH_CHAR*169)
	DC.L	scr3+(SCN_WIDTH_CHAR*170),scr3+(SCN_WIDTH_CHAR*171),scr3+(SCN_WIDTH_CHAR*172),scr3+(SCN_WIDTH_CHAR*173),scr3+(SCN_WIDTH_CHAR*174),scr3+(SCN_WIDTH_CHAR*175),scr3+(SCN_WIDTH_CHAR*176),scr3+(SCN_WIDTH_CHAR*177),scr3+(SCN_WIDTH_CHAR*178),scr3+(SCN_WIDTH_CHAR*179)
	DC.L	scr3+(SCN_WIDTH_CHAR*180),scr3+(SCN_WIDTH_CHAR*181),scr3+(SCN_WIDTH_CHAR*182),scr3+(SCN_WIDTH_CHAR*183),scr3+(SCN_WIDTH_CHAR*184),scr3+(SCN_WIDTH_CHAR*185),scr3+(SCN_WIDTH_CHAR*186),scr3+(SCN_WIDTH_CHAR*187),scr3+(SCN_WIDTH_CHAR*188),scr3+(SCN_WIDTH_CHAR*189)
	DC.L	scr3+(SCN_WIDTH_CHAR*190),scr3+(SCN_WIDTH_CHAR*191),scr3+(SCN_WIDTH_CHAR*192),scr3+(SCN_WIDTH_CHAR*193),scr3+(SCN_WIDTH_CHAR*194),scr3+(SCN_WIDTH_CHAR*195),scr3+(SCN_WIDTH_CHAR*196),scr3+(SCN_WIDTH_CHAR*197),scr3+(SCN_WIDTH_CHAR*198),scr3+(SCN_WIDTH_CHAR*199)

XOFFSET	DC.L 	$0000A851 ; v
YOFFSET	DC.L 	$FFFF71B0 ; h
ZOFFSET	DC.L 	$000099A4

ophi	dc.w	$cb

;TST_ST_X	DC.L	10
;TST_ST_Y	DC.L	0
;TST_ST_Z	DC.L	20
;TST_EN_X	DC.L	0
;TST_EN_Y	DC.L	0
;TST_EN_Z	DC.L	60
;TST_ST_CC	DC.B	0
;TST_EN_CC	DC.B	0
;TST_ST_PNT	DC.B	0
;TST_EN_PNT	DC.B	0

objectlinelist:
	dc.w	0,1
	dc.w	1,3
	dc.w	3,2
	dc.w	2,0	
	dc.w	4,5
	dc.w	5,7
	dc.w	7,6
	dc.w	6,4
	dc.w	0,4
	dc.w	1,5
	dc.w	2,6
	dc.w	3,7

objectpointlist:
	dc.w -5000,-8000,-5000
	dc.w -5000,-8000,+5000
	dc.w -8000,+8000,-8000
	dc.w -8000,+8000,+8000
	dc.w +5000,-8000,-5000
	dc.w +5000,-8000,+5000
	dc.w +8000,+8000,-8000
	dc.w +8000,+8000,+8000

	include "SINCOS2.H"
	
	section ChipRAM,Data_c

copperlist:	dc.l	$01800000 ; COLOR00
			dc.l	$01820FFF ; COLOR01
			dc.l	$008e2c81,$0090f4c1 ; 2cc1		; 08e=DIWSTRT and 090=DIWSTOP ; $F4C1 320x200
			dc.l	$00920038,$009400d0		; 092=DDFSTRT and 094=DDFSTOP
			dc.l	$01080000;+40	; BPL1MOD ; 40 byte = 1 line / 80 = 2 lines down
			dc.l	$010a0000;+40	; BPL2MOD ; 40 byte = 1 line / 80 = 2 lines down	
mainbitp:	dc.l	$00e00000	; BPL1PTH
			dc.l	$00e20000	; BPL1PTL
			dc.l	$01001200	; BPLCON0
			dc.l	$fffffffe

;copperlist:	DC.L	$01800000 				; COLOR00
;			DC.L	$01820FFF 				; COLOR01
;			DC.L	$008e4881,$009010c1		; 08e=DIWSTRT and 090=DIWSTOP ; $F4C1 320x200
;			DC.L	$0092003c,$009400d4		; 092=DDFSTRT and 094=DDFSTOP
;			DC.L	$01080000				; BPL1MOD ; 40 byte = 1 line / 80 = 2 lines down
;			DC.L	$010A0000				; BPL1MOD ; 40 byte = 1 line / 80 = 2 lines down
;mainbitp:	DC.L	$00e00000				; BPL1PTH
;			DC.L	$00e20000				; BPL1PTL
;			DC.L	$01009200				; BPLCON0 - Hires
;			DC.L	$fffffffe

scr1:	DS.B	40*200 ;,$AA
scr2:	DS.B	40*200 ;,$AA
scr3:	DS.B	40*200 ;,$AA

	section bss

STACK_POINTER:		DC.L	0

ROTATED_POINT		DS.L 256*3 	; 256 rx-ry-rz points
PERSPECTIVE_POINT	DS.W 256*2 	; 256 px-py points
CLIPCODE_POINT		DS.B 256	; clip codes for points list

CLIP_START_ROT_X	DS.L	1
CLIP_START_ROT_Y	DS.L	1
CLIP_START_ROT_Z	DS.L	1

CLIP_END_ROT_X		DS.L	1
CLIP_END_ROT_Y		DS.L	1
CLIP_END_ROT_Z		DS.L	1

CLIPPED_ROT_X		DS.L 	1
CLIPPED_ROT_Y		DS.L 	1
CLIPPED_ROT_Z		DS.L 	1

CLIP_START_PERS_H	DS.W 	1
CLIP_START_PERS_V	DS.W	1
					
CLIP_END_PERS_H		DS.W 	1
CLIP_END_PERS_V		DS.W 	1

sphi	ds.w	1
cphi	ds.w	1

CLIP_START_POINT	DS.B	1
CLIP_END_POINT		DS.B	1

CLIP_START_CC 		DS.B 	1
CLIP_END_CC			DS.B 	1

TEST_CC			DS.l 	1


