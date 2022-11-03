
CUSTOM	equ	$dff000

SCREEN_HOR_MIDDLE EQU 320
SCREEN_VER_MIDDLE EQU 100

CC_TOP		EQU 1
CC_BOTTOM	EQU 2
CC_LEFT 	EQU 4
CC_RIGHT	EQU 8
CC_BEHIND 	EQU 16
CC_ON		EQU	0
CC_OFF		EQU	31
PERS_WIDTH	EQU	319

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

SCN_HEIGHT = 400
SCN_WIDTH = 640
NXT_LIN = SCN_WIDTH / 8 ; next line
SCN_WIDTH_CHAR = 80

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
		ADD.W	XOFFSET,D0
		ADD.W	YOFFSET,D1	
		ADD.W	ZOFFSET,D2
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

;	MOVE.W	D0,D3 ; d4 = spare
;	MOVE.W	D1,D4
;	MOVE.W	D2,D5

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
		
		MOVE.W	#0,ophi

.LOOP:		LEA		CUSTOM,a1
			VBL_WAIT
			BSR		SWAP_SCN
			
			BSR		KEYBOARD

	movem.w		(POINT),d0/d1/d2/d3
	BSR 	movepoint
	movem.w		d0/d1/d2/d3,(POINT)
	
			LEA		CUSTOM+2,A6
			BSR		DL_Init

	moveq.L	#8-1,d7			; points
	lea	objectpointlist,a0	; point list
	moveq.L	#12-1,d1 ; #12-1,d1			;  #12-1
	lea	objectlinelist,a1		; line list

			BSR		DRAW_OBJECT
			
			LEA		CUSTOM+2,A6
			MOVE.L	mainscreenp,A0
			MOVE.w	#0,D0
			MOVE.w	#0,D1
			MOVEM.W	(POINT),D2/D3
			BSR		DRAW_LINE

			LEA		CUSTOM+2,A6
			MOVE.L	mainscreenp,A0
			MOVE.w	#0,D0
			MOVE.w	#159,D1
			MOVEM.W	(POINT),D2/D3
			BSR		DRAW_LINE

			LEA		CUSTOM+2,A6
			MOVE.L	mainscreenp,A0
			MOVE.w	#639,D0
			MOVE.w	#0,D1
			MOVEM.W	(POINT),D2/D3
			BSR		DRAW_LINE

			LEA		CUSTOM+2,A6
			MOVE.L	mainscreenp,A0
			MOVE.w	#639,D0
			MOVE.w	#159,D1
			MOVEM.W	(POINT),D2/D3
			BSR		DRAW_LINE
		
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
			MOVE.W	#((200*1)*64)+40,BLTSIZE(A6)	; screen height *64 + screen width in words
	
			BLITTER_WAIT
			MOVE.W	#$000,$180(A6)
			RTS

KEYPRESS:	MOVE.B  $BFEC01,D0      ; Keypress
			NOT.B   D0
			ROR.B   #1,D0           ; d0 now contains the raw key
			RTS

KEYBOARD:	BSR		KEYPRESS
			CMP.B	#$20,D0
			BEQ.S	.KEY1
			CMP.B	#$22,D0
			BEQ.S	.KEY2
			CMP.B	#$11,D0
			BEQ.S	.KEY3
			CMP.B	#$21,D0
			BEQ.S	.KEY4
			CMP.B	#$10,D0
			BEQ.S	.KEY5
			CMP.B	#$31,D0
			BEQ.S	.KEY6
			CMP.B	#7,D0
			BEQ.S	.KEY7
			CMP.B	#8,D0
			BEQ.S	.KEY8
			RTS
.KEY1		MOVE.W	XOFFSET,D0
			SUBQ.W	#1,D0
			MOVE.W	D0,XOFFSET
			RTS
.KEY2		MOVE.W	XOFFSET,D0
			ADDQ.W	#1,D0
			MOVE.W	D0,XOFFSET
			RTS
.KEY3		MOVE.W	YOFFSET,D0
			SUBQ.W	#1,D0
			MOVE.W	D0,YOFFSET
			RTS
.KEY4		MOVE.W	YOFFSET,D0
			ADDQ.W	#1,D0
			MOVE.W	D0,YOFFSET
			RTS
.KEY5		MOVE.W	ZOFFSET,D0
			ADDQ.W	#1,D0
			MOVE.W	D0,ZOFFSET
			RTS
.KEY6		MOVE.W	ZOFFSET,D0
			SUBQ.W	#1,D0
			MOVE.W	D0,ZOFFSET
			RTS
.KEY7		MOVE.W	ophi,D0
			SUBQ.B	#1,D0
			MOVE.W	D0,ophi
			RTS
.KEY8		MOVE.W	ophi,D0
			ADDQ.B	#1,D0
			MOVE.W	D0,ophi
			RTS

DRAW_OBJECT	MOVEM.L	D1/A1,-(SP)				; put line details on stack

			SIN_COS

			LEA		ROTATED_POINT,A1		; store rotated point
			LEA		PERSPECTIVE_POINT,A2	; store perspective point
			LEA		CLIPCODE_POINT,A3		; store point clip code
			LEA		PERS_TAB,A4				; perspective table lookup

			MOVEQ.L	#CC_ON,D6 			; and clip code
			MOVEQ.L	#CC_OFF,D5 			; or clip code
	
.POINT_LOOP:	MOVEM.W (A0)+,D0/D1/D2		; get point
				ROTATE_Y				; do rotation

				ADD_OFFSET					; add offsets
				MOVEM.W	D0/D1/D2,(A1)		; store rotated point

				TST.W	D2					; if z is positive
				BPL.S	.PNT_FRONT
					MOVEQ.L	#CC_BEHIND,D4	;  set clip code to behind
					BRA.S	.PNT_OFF
.PNT_FRONT:		BSR		CLIPCODE			; calc clip code	
				TST.W	D4
				BNE.S	.PNT_OFF
					BSR		PERSPECTIVE				; do perspective
					MOVEM.W	D0/D1,(A2)				; store perspective

.PNT_OFF:		MOVE.B	D4,(A3)+				; store clipcode

				AND.B	D4,D5					; object and clip code
				OR.B	D4,D6					; object or clip code

				LEA		8(A1),A1
				LEA		4(A2),A2
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

.CLIP_LINE:	BRA		.DRAW_LINE_NEXT	; TEMP

ROTATE:
		MOVE.W	D0,D3
			MOVE.W	D1,D4
			MOVE.W	D2,D5
			RTS

PERSPECTIVE	AND.W	#%0111111111111110,D2
			MOVE.W	(A4,D2.W),D2
			MULS.W	D2,D0
			ADD.L	D0,D0
			SWAP	D0
			ADD.W	#SCREEN_HOR_MIDDLE,D0 ; screen middle 
			MULS.W	D2,D1
			ADD.L	D1,D1	; remove ?
			SWAP	D1
			ASR.W 	#2,D1	; change to #1 ?
			ADD.W	#SCREEN_VER_MIDDLE,D1 ; screen middle 
			RTS
			
CLIPCODE	MOVEQ.L	#CC_ON,D4
			MOVE.W	D2,A5					; Z
			ADD.W	#PERS_WIDTH,A5			; + PERS OFFSET
			MOVE.W	A5,D3
			NEG.W	D3						; NEGATE IF NEEDED FURTHER DOWN

.XTEST		TST.W	D0						; X IS +VE
			BPL.S	.RIGHT					; YES SO CHECK RIGHT

.LEFT		CMP.W	D3,D0				; COMPARE LEFT AND -Z VAL
			BGE.S	.YTEST				; ON
				MOVEQ.L	#CC_LEFT,D4			; OFF
				BRA.S	.YTEST

.RIGHT		CMP.W	A5,D0				; COMPARE RIGHT AND +Z VAL
			BLE.S	.YTEST				; ON
				MOVEQ.L	#CC_RIGHT,D4	; OFF

.YTEST		TST.W	D1						; Y IS +VE
			BPL.S	.BOTTOM					; YES SO CHECK BOTTOM

.TOP		CMP.W	D3,D1				; COMPARE TOP AND -Z VAL
			BGE.S	.EXIT				; ON
				OR.W	#CC_TOP,D4			; OFF
				RTS							;	BRA.S	.EXIT

.BOTTOM		CMP.W	A5,D1				; COMPARE BOTTOM AND +Z VAL
			BLE.S	.EXIT				; ON
				OR.W	#CC_BOTTOM,D4		; OFF
.EXIT		RTS

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
			
movepoint	ADD.W 	d2,d0
			BNE.S 	.testl
				NEG.W 	d2
.testl		CMP.W 	#639,d0
			BNE.S 	.test2
				NEG.W 	d2
.test2		ADD.W 	d3,d1
			BNE.S	.test3
				NEG.W 	d3
.test3		CMP.W 	#159,d1
			BNE.S 	.test4
				NEG.W 	d3
.test4		RTS

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

POINT	DC.W	10,150,+1,-1

XOFFSET	DC.W 	10 ; v
YOFFSET	DC.W 	80 ; h
ZOFFSET	DC.W 	400

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
	dc.w -320,-320,-320
	dc.w -320,-320,+320
	dc.w -320,+320,-320
	dc.w -320,+320,+320
	dc.w +320,-320,-320
	dc.w +320,-320,+320
	dc.w +320,+320,-320
	dc.w +320,+320,+320

	include "PERSTAB.H"
	include "SINCOS2.H"
	
	section ChipRAM,Data_c

copperlist:	DC.L	$01800000 				; COLOR00
			DC.L	$01820FFF 				; COLOR01
			DC.L	$008e4881,$009010c1		; 08e=DIWSTRT and 090=DIWSTOP ; $F4C1 320x200
			DC.L	$0092003c,$009400d4		; 092=DDFSTRT and 094=DDFSTOP
			DC.L	$01080000				; BPL1MOD ; 40 byte = 1 line / 80 = 2 lines down
			DC.L	$010A0000				; BPL1MOD ; 40 byte = 1 line / 80 = 2 lines down
mainbitp:	DC.L	$00e00000				; BPL1PTH
			DC.L	$00e20000				; BPL1PTL
			DC.L	$01009200				; BPLCON0 - Hires
			DC.L	$fffffffe

scr1:	DS.B	80*200 ;,$AA
scr2:	DS.B	80*200 ;,$AA
scr3:	DS.B	80*200 ;,$AA

	section bss

STACK_POINTER:		DC.L	0

ROTATED_POINT		DS.W 256*3 ; 256 rx-ry-rz points
PERSPECTIVE_POINT	DS.W 256*2 ; 256 px-py points
CLIPCODE_POINT		DS.W 256		; clip codes for points list

ophi	ds.w	1
sphi	ds.w	1
cphi	ds.w	1
