
CUSTOM	equ	$dff000

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

STORE_USP_ON_STACK	macro
			move.l 	usp,a0
			move.l 	a0,-(a7) 		; store usp
 			endm

RESTORE_USP_FROM_STACK	macro
			move.l 	(a7)+,a0
			move.l 	a0,usp		 	; restore usp
			endm

STORE_STACK_POINTER		macro
			move.l 	a7,STACK_POINTER	; store sp
			ENDM

RESTORE_STACK_POINTER	macro
			move.l 	STACK_POINTER,a7 	; restore sp
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

start:	move.l 	$4.w,a6			; Switch to supervisor mode, because
		lea 	.get(pc),a5		; movec is a privileged instruction.
		jsr 	-$1e(a6)  		; Supervisor()
;   	bra.b 	.store			;
.get
; 	movec	vbr,d0			; (dc.l $4e7a0801)
;   	rte				;
.store
; 	move.l 	d0,_vbr			; Store VBR

;	move.l	$4,a6

		sub.l	a1,a1				; zero - find current task
		jsr		Exec_FindTask(a6)
		move.l	d0,a1				; set current task to high pri
		moveq	#127,d0
		jsr		Exec_SetTaskPri(a6)

		lea		gfxname,a1
		moveq	#0,d0
		jsr		Exec_OpenLibrary(a6)
		tst.l	d0
		beq		error
		move.l	d0,gfxbase

		move.l	d0,a6
		move.l	Gfx_Field_ActiView(a6),-(a7)	;	 store active view

		sub.l	a1,a1					; load zero view so we get default zero state 
		jsr		Gfx_LoadView(a6)
		jsr		Gfx_WaitTOF(a6)				; Wait for both long and short frame to finish
		jsr		Gfx_WaitTOF(a6)

		move.l	$4,a6
		jsr		Exec_Forbid(a6)

		lea		CUSTOM,a1
		move.w	DMACONR(a1),-(a7)		; save enabled dma channels
		move.w	INTENAR(a1),-(a7)		; save enabled interrupts
		move.w	#%0111111111111111,INTENA(a7)	; disable all interupts
		move.w	INTREQR(a1),-(a7)		; store current interrupt request bits
		move.w	ADKCONR(a1),-(a7)		; store current disk and sound control
	
		lea		CUSTOM,A1
		VBL_WAIT
		
		move.l	#copperlist,COP1LCH(a1)
		move.w	#%0111111111111111,DMACON(a1)	; turn off all dma 
		move.w	#%1000011111000000,DMACON(a1)	; enable bitplane + copper + blitter dma

main_loop:	lea		CUSTOM,a1
			VBL_WAIT
			BSR		SWAP_SCN

	moveq.l #2,d7
	bsr movement
	
			LEA		CUSTOM,A6
			BSR		DL_Init
;	A0 = PlanePtr, A6 = $DFF002, D0/D1 = X0/Y0, D2/D3 = X1/Y1 ;	D4 = PlaneWidth > Kills: D0-D4/A0-A1 (+D5 in Fill Mode)

			LEA		CUSTOM,A6
			add.l	#2,A6
			MOVE.L	mainscreenp,A0			; get current screen just drawn on and update copper list for next frame
			MOVE.L	(A0),A0
			MOVE.L	#0,D0
			MOVE.L	#0,D1
	LEA pointlist,A5
	move.l	(a5),d3
	move.l	4(a5),d2
			BSR		DrawLine

			LEA		CUSTOM,A6
			add.l	#2,A6
			MOVE.L	mainscreenp,A0
			MOVE.L	(A0),A0
			MOVE.L	#0,D0
			MOVE.L	#255,D1
	LEA pointlist,A5
	move.l	(a5),d3
	move.l	4(a5),d2
			BSR		DrawLine

			LEA		CUSTOM,A6
			add.l	#2,A6
			MOVE.L	mainscreenp,A0
			MOVE.L	(A0),A0
			MOVE.L	#511,D0
			MOVE.L	#0,D1
	LEA pointlist,A5
	move.l	(a5),d3
	move.l	4(a5),d2
			BSR		DrawLine

			LEA		CUSTOM,A6
			add.l	#2,A6
			MOVE.L	mainscreenp,A0
			MOVE.L	(A0),A0
			MOVE.L	#511,D0
			MOVE.L	#255,D1
	LEA pointlist,A5
	move.l	(a5),d3
	move.l	4(a5),d2
			BSR		DrawLine

			MOVE.L	mainscreenp,A0
			MOVE.L	(A0),A0
			move.l	#$ffffffff,d0
			move.l	d0,80*0(a0)
;			move.l	d0,80*1(a0)
			move.l	d0,80*2(a0)
;			move.l	d0,80*3(a0)
			move.l	d0,80*4(a0)
;			move.l	d0,80*5(a0)
			move.l	d0,80*6(a0)
;			move.l	d0,80*7(a0)
			move.l	d0,80*8(a0)
			move.l	d0,80*190(a0)
			move.l	d0,80*250(a0)


;	move.l	mainscreenp,a0	; address of screenybuffer
;	move.l 	(a0),a0 	; screen = ver pos

			
		
			btst	#6,$bfe001                          ; left mouse button
			bne		main_loop

		lea		CUSTOM,a1
		VBL_WAIT

		move.l	gfxbase,a6
		move.l	Gfx_Field_copinit(a6),COP1LCH(a1)	; restore system copper list
		move.l	Gfx_Field_LOFlist(a6),COP2LCH(a1)

		move.w	#$8000,d1			; enable bit

		move.w	(a7)+,d0			; restore disk and sound control
		or.w	d1,d0
		move.w	d0,ADKCON(a1)

		move.w	(a7)+,d0			; restore interrupt request bits
		or.w	d1,d0
		move.w	d0,INTREQ(a1)

		move.w	(a7)+,d0			; restore enabled interrupts
		or.w	d1,d0
		move.w	d0,INTENA(a1)

		move.w	(a7)+,d0			; restore enabled dma channels
		or.w	d1,d0
		move.w	d0,DMACON(a1)

		move.l	$4,a6
		jsr		Exec_Permit(a6)

		move.l	gfxbase,a6
		move.l	(a7)+,a1				; load stored active view
		jsr		Gfx_LoadView(a6)
		jsr		Gfx_WaitTOF(a6)
		jsr		Gfx_WaitTOF(a6)

		move.l	$4,a6
		move.l	gfxbase,a1
		jsr		Exec_CloseLibrary(a6)

error:	moveq.l		#0,d0
		rts

gfxname:	dc.b	'graphics.library',0
		even
gfxbase:	dc.l	0

octant:
    dc.w	$0051,$0055,$0059,$005d
    dc.w	$0041,$0049,$0045,$004d
    dc.w	$0011,$0015,$0019,$001d
    dc.w	$0001,$0009,$0005,$000d	
; display the current one
; flip to the next one
; clear the next one
; draw on it
	
SWAP_SCN:	move.l	mainscreenp,a1			; get current screen just drawn on and update copper list for next frame
			move.l	(a1),d0
			lea		mainbitp,a0
			move.w	d0,6(a0) 	; set bit plane 1
			swap	d0
			move.w	d0,2(a0)
			swap	d0
	
			move.l	mainscreenp,d0	; next to be displayed		; shift the screens to the last drawn is at the end
			move.l	mainscreenp+4,a1  ; next to be drawn
			move.l	mainscreenp+8,d1  ; following after next
	
			move.l	a1,mainscreenp 	; to be drawn on
			move.l	d1,mainscreenp+4 	; shuffle up
			move.l	d0,mainscreenp+8 	; next to be display - last to be updated

			lea		CUSTOM,a6 	; clear next screen to be drawn
			move.l	mainscreenp+4,a0 	; +4,a0	; clear the next screen not this screen
			move.l	(a0),a0		; address to clear
;			bsr		SCN_CLR

;			RTS

SCN_CLR:	move.w	#$030,$180(a6)
			BLITTER_WAIT
			move.w	#$300,$180(a6)

			move.l	a0,BLTDPTH(a6)	; screen address
			move.w	#16,BLTDMOD(a6)	; no modulo
			move.l	#%1000000000000000000000000,BLTCON0(a6)
			move.w	#((256*1)*64)+32,BLTSIZE(a6)	; screen height *64 + screen width in words
	
			BLITTER_WAIT
			move.w	#$000,$180(a6)
			rts

STACK_POINTER:	DC.L	0


*******************************************************************************
*									      *
*			'DrawLine V1.01' By TIP/SPREADPOINT		      *
*			ннннннннннннннннннннннннннннннннннн		      *
*									      *
*******************************************************************************

DL_Width	= 80
DL_MInterns	= $CA

;нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
;	A0 = PlanePtr, A6 = $DFF002, D0/D1 = X0/Y0, D2/D3 = X1/Y1, D4 = PlaneWidth > Kills: D0-D4/A0-A1 (+D5 in Fill Mode)

DrawLine:	cmp.w	d1,d3		; Drawing only from Top to Bottom is
			bge.s	.y1ly2		; necessary for:
				exg	d0,d2		; 1) Up-down Differences (same coords)
				exg	d1,d3		; 2) Blitter Invert Bit (only at top of line)
.y1ly2:		sub.w	d1,d3		; D3 = yd

; Here we could do an Optimization with Special Shifts depending on the DL_Width value... I know it, but please, let it be.

		mulu	#DL_Width,d1		; Use muls for neg Y-Vals
		add.l	d1,a0		; Please don't use add.w here !!!
		moveq.l	#0,d1		; D1 = Quant-Counter
		sub.w	d0,d2		; D2 = xd
		bge.s	.xdpos
			addq.w	#2,d1		; Set Bit 1 of Quant-Counter (here it could be a moveq)
			neg.w	d2
.xdpos:	moveq.l	#$f,d4		; D4 full cleaned (for later oktants move.b)
		and.w	d0,d4

		lsr.w	#3,d0		; Yeah, on byte (necessary for bchg)...
		add.w	d0,a0		; ...Blitter ands automagically
		ror.w	#4,d4		; D4 = Shift
		or.w	#$B00+DL_MInterns,d4	; BLTCON0-codes
		swap	d4
		cmp.w	d2,d3		; Which Delta is the Biggest ?
		bge.s	.dygdx
			addq.w	#1,d1		; Set Bit 0 of Quant-Counter
			exg	d2,d3		; Exchange xd with yd
.dygdx:	add.w	d2,d2		; D2 = xd*2
		move.w	d2,d0		; D0 = Save for $52(a6)
		sub.w	d3,d0		; D0 = xd*2-yd
		addx.w	d1,d1		; Bit0 = Sign-Bit
		move.b	Oktants(PC,d1.w),d4	; In Low Byte of d4 (upper byte cleaned above)
		swap	d2
		move.w	d0,d2
		sub.w	d3,d2		; D2 = 2*(xd-yd)
		moveq.l	#6,d1		; D1 = ShiftVal (not necessary) + TestVal for the Blitter
		lsl.w	d1,d3		; D3 = BLTSIZE
		add.w	#$42,d3
		lea	$52-2(a6),a1	; A1 = CUSTOM+$52

; WARNING : If you use FastMem and an extreme DMA-Access (e.g. 6 Planes and Copper), you should Insert a tst.b (a6) here (for the shitty AGNUS-BUG)

.wb:	btst	d1,(a6)		; Waiting for the Blitter...
		bne.s	.wb

;-------------- Not necessary, only for testing purposes ! (Cf Init Part...)
;		move.w	#$8000,BLTADAT-2(a6)
;		move.w	#-1,BLTAFWM-2(a6)
;		move.w	#-1,BLTBDAT-2(a6)
;		move.w	#DL_Width,BLTCMOD-2(a6)
;		move.w	#DL_Width,BLTDMOD-2(a6)
;--------------

		move.l	d4,BLTCON0-2(a6)	; Writing to the Blitter Regs as fast
		move.l	d2,BLTBMOD-2(a6)	; as possible
		move.l	a0,BLTCPTH-2(a6)
		move.w	d0,(a1)+
		move.l	a0,(a1)+	; Shit-Word Buffer Ptr...
		move.w	d3,(a1)
		rts
;нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
SML = 0

Oktants:	dc.b	SML+1,SML+1+$40
			dc.b	SML+17,SML+17+$40
			dc.b	SML+9,SML+9+$40
			dc.b	SML+21,SML+21+$40
;нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
;		Optimized Init Part... A6 = $DFF000 > Kills : D0-D2
DL_Init:	addq.w	#2,a6		; A6 = $DFF002 for DrawLine !
			moveq.l	#-1,d1
			moveq	#DL_Width,d0
			moveq.l	#6,d2
.wb:		btst	d2,(a6)
			bne.s	.wb
		move.w	d1,BLTAFWM-2(a6)
		move.w	d1,BLTBDAT-2(a6)
		move.w	#$8000,BLTADAT-2(a6)
		move.w	d0,BLTCMOD-2(a6)
		move.w	d0,BLTDMOD-2(a6)
		rts

DL_Exit:	subq.w	#2,a6		; A6 = $DFF000
		rts

movement:	lea pointlist,a0
		lea dirlist,a1
 
.moveloop	movem.l (a0),d0-d1
		movem.l (a1),d2-d3
		add.l d2,d0
		add.l d3,d1
		movem.l d0-d1,(a0)

		cmp.w #0,d0
		bne .testxl
			neg.l d2
.testxl
		cmp.w #0,d1
		bne .testyl
			neg.l d3
.testyl
 		cmp #255,d0
 		bne .testxr
			neg.l d2
.testxr
 		cmp #511,d1
		bne .testyr
			neg.l d3
.testyr
		movem.l d2-d3,(a1)
 
		lea 8(a0),a0
 		lea 8(a1),a1
 	dbra d7,.moveloop
	rts
	
mainscreenp:	dc.l	scrptr1,scrptr2,scrptr3
	
scrptr1	dc.l	scr1+(000*NXT_LIN),scr1+(001*NXT_LIN),scr1+(002*NXT_LIN),scr1+(003*NXT_LIN),scr1+(004*NXT_LIN),scr1+(005*NXT_LIN),scr1+(006*NXT_LIN),scr1+(007*NXT_LIN),scr1+(008*NXT_LIN),scr1+(009*NXT_LIN)
	dc.l	scr1+(010*NXT_LIN),scr1+(011*NXT_LIN),scr1+(012*NXT_LIN),scr1+(013*NXT_LIN),scr1+(014*NXT_LIN),scr1+(015*NXT_LIN),scr1+(016*NXT_LIN),scr1+(017*NXT_LIN),scr1+(018*NXT_LIN),scr1+(019*NXT_LIN)
	dc.l	scr1+(020*NXT_LIN),scr1+(021*NXT_LIN),scr1+(022*NXT_LIN),scr1+(023*NXT_LIN),scr1+(024*NXT_LIN),scr1+(025*NXT_LIN),scr1+(026*NXT_LIN),scr1+(027*NXT_LIN),scr1+(028*NXT_LIN),scr1+(029*NXT_LIN)
	dc.l	scr1+(030*NXT_LIN),scr1+(031*NXT_LIN),scr1+(032*NXT_LIN),scr1+(033*NXT_LIN),scr1+(034*NXT_LIN),scr1+(035*NXT_LIN),scr1+(036*NXT_LIN),scr1+(037*NXT_LIN),scr1+(038*NXT_LIN),scr1+(039*NXT_LIN)
	dc.l	scr1+(040*NXT_LIN),scr1+(041*NXT_LIN),scr1+(042*NXT_LIN),scr1+(043*NXT_LIN),scr1+(044*NXT_LIN),scr1+(045*NXT_LIN),scr1+(046*NXT_LIN),scr1+(047*NXT_LIN),scr1+(048*NXT_LIN),scr1+(049*NXT_LIN)
	dc.l	scr1+(050*NXT_LIN),scr1+(051*NXT_LIN),scr1+(052*NXT_LIN),scr1+(053*NXT_LIN),scr1+(054*NXT_LIN),scr1+(055*NXT_LIN),scr1+(056*NXT_LIN),scr1+(057*NXT_LIN),scr1+(058*NXT_LIN),scr1+(059*NXT_LIN)
	dc.l	scr1+(060*NXT_LIN),scr1+(061*NXT_LIN),scr1+(062*NXT_LIN),scr1+(063*NXT_LIN),scr1+(064*NXT_LIN),scr1+(065*NXT_LIN),scr1+(066*NXT_LIN),scr1+(067*NXT_LIN),scr1+(068*NXT_LIN),scr1+(069*NXT_LIN)
	dc.l	scr1+(070*NXT_LIN),scr1+(071*NXT_LIN),scr1+(072*NXT_LIN),scr1+(073*NXT_LIN),scr1+(074*NXT_LIN),scr1+(075*NXT_LIN),scr1+(076*NXT_LIN),scr1+(077*NXT_LIN),scr1+(078*NXT_LIN),scr1+(079*NXT_LIN)
	dc.l	scr1+(080*NXT_LIN),scr1+(081*NXT_LIN),scr1+(082*NXT_LIN),scr1+(083*NXT_LIN),scr1+(084*NXT_LIN),scr1+(085*NXT_LIN),scr1+(086*NXT_LIN),scr1+(087*NXT_LIN),scr1+(088*NXT_LIN),scr1+(089*NXT_LIN)
	dc.l	scr1+(090*NXT_LIN),scr1+(091*NXT_LIN),scr1+(092*NXT_LIN),scr1+(093*NXT_LIN),scr1+(094*NXT_LIN),scr1+(095*NXT_LIN),scr1+(096*NXT_LIN),scr1+(097*NXT_LIN),scr1+(098*NXT_LIN),scr1+(099*NXT_LIN)
	dc.l	scr1+(100*NXT_LIN),scr1+(101*NXT_LIN),scr1+(102*NXT_LIN),scr1+(103*NXT_LIN),scr1+(104*NXT_LIN),scr1+(105*NXT_LIN),scr1+(106*NXT_LIN),scr1+(107*NXT_LIN),scr1+(108*NXT_LIN),scr1+(109*NXT_LIN)
	dc.l	scr1+(110*NXT_LIN),scr1+(111*NXT_LIN),scr1+(112*NXT_LIN),scr1+(113*NXT_LIN),scr1+(114*NXT_LIN),scr1+(115*NXT_LIN),scr1+(116*NXT_LIN),scr1+(117*NXT_LIN),scr1+(118*NXT_LIN),scr1+(119*NXT_LIN)
	dc.l	scr1+(120*NXT_LIN),scr1+(121*NXT_LIN),scr1+(122*NXT_LIN),scr1+(123*NXT_LIN),scr1+(124*NXT_LIN),scr1+(125*NXT_LIN),scr1+(126*NXT_LIN),scr1+(127*NXT_LIN),scr1+(128*NXT_LIN),scr1+(129*NXT_LIN)
	dc.l	scr1+(130*NXT_LIN),scr1+(131*NXT_LIN),scr1+(132*NXT_LIN),scr1+(133*NXT_LIN),scr1+(134*NXT_LIN),scr1+(135*NXT_LIN),scr1+(136*NXT_LIN),scr1+(137*NXT_LIN),scr1+(138*NXT_LIN),scr1+(139*NXT_LIN)
	dc.l	scr1+(140*NXT_LIN),scr1+(141*NXT_LIN),scr1+(142*NXT_LIN),scr1+(143*NXT_LIN),scr1+(144*NXT_LIN),scr1+(145*NXT_LIN),scr1+(146*NXT_LIN),scr1+(147*NXT_LIN),scr1+(148*NXT_LIN),scr1+(149*NXT_LIN)
	dc.l	scr1+(150*NXT_LIN),scr1+(151*NXT_LIN),scr1+(152*NXT_LIN),scr1+(153*NXT_LIN),scr1+(154*NXT_LIN),scr1+(155*NXT_LIN),scr1+(156*NXT_LIN),scr1+(157*NXT_LIN),scr1+(158*NXT_LIN),scr1+(159*NXT_LIN)
	dc.l	scr1+(160*NXT_LIN),scr1+(161*NXT_LIN),scr1+(162*NXT_LIN),scr1+(163*NXT_LIN),scr1+(164*NXT_LIN),scr1+(165*NXT_LIN),scr1+(166*NXT_LIN),scr1+(167*NXT_LIN),scr1+(168*NXT_LIN),scr1+(169*NXT_LIN)
	dc.l	scr1+(170*NXT_LIN),scr1+(171*NXT_LIN),scr1+(172*NXT_LIN),scr1+(173*NXT_LIN),scr1+(174*NXT_LIN),scr1+(175*NXT_LIN),scr1+(176*NXT_LIN),scr1+(177*NXT_LIN),scr1+(178*NXT_LIN),scr1+(179*NXT_LIN)
	dc.l	scr1+(180*NXT_LIN),scr1+(181*NXT_LIN),scr1+(182*NXT_LIN),scr1+(183*NXT_LIN),scr1+(184*NXT_LIN),scr1+(185*NXT_LIN),scr1+(186*NXT_LIN),scr1+(187*NXT_LIN),scr1+(188*NXT_LIN),scr1+(189*NXT_LIN)
	dc.l	scr1+(190*NXT_LIN),scr1+(191*NXT_LIN),scr1+(192*NXT_LIN),scr1+(193*NXT_LIN),scr1+(194*NXT_LIN),scr1+(195*NXT_LIN),scr1+(196*NXT_LIN),scr1+(197*NXT_LIN),scr1+(198*NXT_LIN),scr1+(199*NXT_LIN)

scrptr2	dc.l	scr2+(000*NXT_LIN),scr2+(001*NXT_LIN),scr2+(002*NXT_LIN),scr2+(003*NXT_LIN),scr2+(004*NXT_LIN),scr2+(005*NXT_LIN),scr2+(006*NXT_LIN),scr2+(007*NXT_LIN),scr2+(008*NXT_LIN),scr2+(009*NXT_LIN)
	dc.l	scr2+(010*NXT_LIN),scr2+(011*NXT_LIN),scr2+(012*NXT_LIN),scr2+(013*NXT_LIN),scr2+(014*NXT_LIN),scr2+(015*NXT_LIN),scr2+(016*NXT_LIN),scr2+(017*NXT_LIN),scr2+(018*NXT_LIN),scr2+(019*NXT_LIN)
	dc.l	scr2+(020*NXT_LIN),scr2+(021*NXT_LIN),scr2+(022*NXT_LIN),scr2+(023*NXT_LIN),scr2+(024*NXT_LIN),scr2+(025*NXT_LIN),scr2+(026*NXT_LIN),scr2+(027*NXT_LIN),scr2+(028*NXT_LIN),scr2+(029*NXT_LIN)
	dc.l	scr2+(030*NXT_LIN),scr2+(031*NXT_LIN),scr2+(032*NXT_LIN),scr2+(033*NXT_LIN),scr2+(034*NXT_LIN),scr2+(035*NXT_LIN),scr2+(036*NXT_LIN),scr2+(037*NXT_LIN),scr2+(038*NXT_LIN),scr2+(039*NXT_LIN)
	dc.l	scr2+(040*NXT_LIN),scr2+(041*NXT_LIN),scr2+(042*NXT_LIN),scr2+(043*NXT_LIN),scr2+(044*NXT_LIN),scr2+(045*NXT_LIN),scr2+(046*NXT_LIN),scr2+(047*NXT_LIN),scr2+(048*NXT_LIN),scr2+(049*NXT_LIN)
	dc.l	scr2+(050*NXT_LIN),scr2+(051*NXT_LIN),scr2+(052*NXT_LIN),scr2+(053*NXT_LIN),scr2+(054*NXT_LIN),scr2+(055*NXT_LIN),scr2+(056*NXT_LIN),scr2+(057*NXT_LIN),scr2+(058*NXT_LIN),scr2+(059*NXT_LIN)
	dc.l	scr2+(060*NXT_LIN),scr2+(061*NXT_LIN),scr2+(062*NXT_LIN),scr2+(063*NXT_LIN),scr2+(064*NXT_LIN),scr2+(065*NXT_LIN),scr2+(066*NXT_LIN),scr2+(067*NXT_LIN),scr2+(068*NXT_LIN),scr2+(069*NXT_LIN)
	dc.l	scr2+(070*NXT_LIN),scr2+(071*NXT_LIN),scr2+(072*NXT_LIN),scr2+(073*NXT_LIN),scr2+(074*NXT_LIN),scr2+(075*NXT_LIN),scr2+(076*NXT_LIN),scr2+(077*NXT_LIN),scr2+(078*NXT_LIN),scr2+(079*NXT_LIN)
	dc.l	scr2+(080*NXT_LIN),scr2+(081*NXT_LIN),scr2+(082*NXT_LIN),scr2+(083*NXT_LIN),scr2+(084*NXT_LIN),scr2+(085*NXT_LIN),scr2+(086*NXT_LIN),scr2+(087*NXT_LIN),scr2+(088*NXT_LIN),scr2+(089*NXT_LIN)
	dc.l	scr2+(090*NXT_LIN),scr2+(091*NXT_LIN),scr2+(092*NXT_LIN),scr2+(093*NXT_LIN),scr2+(094*NXT_LIN),scr2+(095*NXT_LIN),scr2+(096*NXT_LIN),scr2+(097*NXT_LIN),scr2+(098*NXT_LIN),scr2+(099*NXT_LIN)
	dc.l	scr2+(100*NXT_LIN),scr2+(101*NXT_LIN),scr2+(102*NXT_LIN),scr2+(103*NXT_LIN),scr2+(104*NXT_LIN),scr2+(105*NXT_LIN),scr2+(106*NXT_LIN),scr2+(107*NXT_LIN),scr2+(108*NXT_LIN),scr2+(109*NXT_LIN)
	dc.l	scr2+(110*NXT_LIN),scr2+(111*NXT_LIN),scr2+(112*NXT_LIN),scr2+(113*NXT_LIN),scr2+(114*NXT_LIN),scr2+(115*NXT_LIN),scr2+(116*NXT_LIN),scr2+(117*NXT_LIN),scr2+(118*NXT_LIN),scr2+(119*NXT_LIN)
	dc.l	scr2+(120*NXT_LIN),scr2+(121*NXT_LIN),scr2+(122*NXT_LIN),scr2+(123*NXT_LIN),scr2+(124*NXT_LIN),scr2+(125*NXT_LIN),scr2+(126*NXT_LIN),scr2+(127*NXT_LIN),scr2+(128*NXT_LIN),scr2+(129*NXT_LIN)
	dc.l	scr2+(130*NXT_LIN),scr2+(131*NXT_LIN),scr2+(132*NXT_LIN),scr2+(133*NXT_LIN),scr2+(134*NXT_LIN),scr2+(135*NXT_LIN),scr2+(136*NXT_LIN),scr2+(137*NXT_LIN),scr2+(138*NXT_LIN),scr2+(139*NXT_LIN)
	dc.l	scr2+(140*NXT_LIN),scr2+(141*NXT_LIN),scr2+(142*NXT_LIN),scr2+(143*NXT_LIN),scr2+(144*NXT_LIN),scr2+(145*NXT_LIN),scr2+(146*NXT_LIN),scr2+(147*NXT_LIN),scr2+(148*NXT_LIN),scr2+(149*NXT_LIN)
	dc.l	scr2+(150*NXT_LIN),scr2+(151*NXT_LIN),scr2+(152*NXT_LIN),scr2+(153*NXT_LIN),scr2+(154*NXT_LIN),scr2+(155*NXT_LIN),scr2+(156*NXT_LIN),scr2+(157*NXT_LIN),scr2+(158*NXT_LIN),scr2+(159*NXT_LIN)
	dc.l	scr2+(160*NXT_LIN),scr2+(161*NXT_LIN),scr2+(162*NXT_LIN),scr2+(163*NXT_LIN),scr2+(164*NXT_LIN),scr2+(165*NXT_LIN),scr2+(166*NXT_LIN),scr2+(167*NXT_LIN),scr2+(168*NXT_LIN),scr2+(169*NXT_LIN)
	dc.l	scr2+(170*NXT_LIN),scr2+(171*NXT_LIN),scr2+(172*NXT_LIN),scr2+(173*NXT_LIN),scr2+(174*NXT_LIN),scr2+(175*NXT_LIN),scr2+(176*NXT_LIN),scr2+(177*NXT_LIN),scr2+(178*NXT_LIN),scr2+(179*NXT_LIN)
	dc.l	scr2+(180*NXT_LIN),scr2+(181*NXT_LIN),scr2+(182*NXT_LIN),scr2+(183*NXT_LIN),scr2+(184*NXT_LIN),scr2+(185*NXT_LIN),scr2+(186*NXT_LIN),scr2+(187*NXT_LIN),scr2+(188*NXT_LIN),scr2+(189*NXT_LIN)
	dc.l	scr2+(190*NXT_LIN),scr2+(191*NXT_LIN),scr2+(192*NXT_LIN),scr2+(193*NXT_LIN),scr2+(194*NXT_LIN),scr2+(195*NXT_LIN),scr2+(196*NXT_LIN),scr2+(197*NXT_LIN),scr2+(198*NXT_LIN),scr2+(199*NXT_LIN)

scrptr3	dc.l	scr3+(000*NXT_LIN),scr3+(001*NXT_LIN),scr3+(002*NXT_LIN),scr3+(003*NXT_LIN),scr3+(004*NXT_LIN),scr3+(005*NXT_LIN),scr3+(006*NXT_LIN),scr3+(007*NXT_LIN),scr3+(008*NXT_LIN),scr3+(009*NXT_LIN)
	dc.l 	scr3+(010*NXT_LIN),scr3+(011*NXT_LIN),scr3+(012*NXT_LIN),scr3+(013*NXT_LIN),scr3+(014*NXT_LIN),scr3+(015*NXT_LIN),scr3+(016*NXT_LIN),scr3+(017*NXT_LIN),scr3+(018*NXT_LIN),scr3+(019*NXT_LIN)
	dc.l	scr3+(020*NXT_LIN),scr3+(021*NXT_LIN),scr3+(022*NXT_LIN),scr3+(023*NXT_LIN),scr3+(024*NXT_LIN),scr3+(025*NXT_LIN),scr3+(026*NXT_LIN),scr3+(027*NXT_LIN),scr3+(028*NXT_LIN),scr3+(029*NXT_LIN)
	dc.l	scr3+(030*NXT_LIN),scr3+(031*NXT_LIN),scr3+(032*NXT_LIN),scr3+(033*NXT_LIN),scr3+(034*NXT_LIN),scr3+(035*NXT_LIN),scr3+(036*NXT_LIN),scr3+(037*NXT_LIN),scr3+(038*NXT_LIN),scr3+(039*NXT_LIN)
	dc.l	scr3+(040*NXT_LIN),scr3+(041*NXT_LIN),scr3+(042*NXT_LIN),scr3+(043*NXT_LIN),scr3+(044*NXT_LIN),scr3+(045*NXT_LIN),scr3+(046*NXT_LIN),scr3+(047*NXT_LIN),scr3+(048*NXT_LIN),scr3+(049*NXT_LIN)
	dc.l	scr3+(050*NXT_LIN),scr3+(051*NXT_LIN),scr3+(052*NXT_LIN),scr3+(053*NXT_LIN),scr3+(054*NXT_LIN),scr3+(055*NXT_LIN),scr3+(056*NXT_LIN),scr3+(057*NXT_LIN),scr3+(058*NXT_LIN),scr3+(059*NXT_LIN)
	dc.l	scr3+(060*NXT_LIN),scr3+(061*NXT_LIN),scr3+(062*NXT_LIN),scr3+(063*NXT_LIN),scr3+(064*NXT_LIN),scr3+(065*NXT_LIN),scr3+(066*NXT_LIN),scr3+(067*NXT_LIN),scr3+(068*NXT_LIN),scr3+(069*NXT_LIN)
	dc.l	scr3+(070*NXT_LIN),scr3+(071*NXT_LIN),scr3+(072*NXT_LIN),scr3+(073*NXT_LIN),scr3+(074*NXT_LIN),scr3+(075*NXT_LIN),scr3+(076*NXT_LIN),scr3+(077*NXT_LIN),scr3+(078*NXT_LIN),scr3+(079*NXT_LIN)
	dc.l	scr3+(080*NXT_LIN),scr3+(081*NXT_LIN),scr3+(082*NXT_LIN),scr3+(083*NXT_LIN),scr3+(084*NXT_LIN),scr3+(085*NXT_LIN),scr3+(086*NXT_LIN),scr3+(087*NXT_LIN),scr3+(088*NXT_LIN),scr3+(089*NXT_LIN)
	dc.l	scr3+(090*NXT_LIN),scr3+(091*NXT_LIN),scr3+(092*NXT_LIN),scr3+(093*NXT_LIN),scr3+(094*NXT_LIN),scr3+(095*NXT_LIN),scr3+(096*NXT_LIN),scr3+(097*NXT_LIN),scr3+(098*NXT_LIN),scr3+(099*NXT_LIN)
	dc.l	scr3+(100*NXT_LIN),scr3+(101*NXT_LIN),scr3+(102*NXT_LIN),scr3+(103*NXT_LIN),scr3+(104*NXT_LIN),scr3+(105*NXT_LIN),scr3+(106*NXT_LIN),scr3+(107*NXT_LIN),scr3+(108*NXT_LIN),scr3+(109*NXT_LIN)
	dc.l	scr3+(110*NXT_LIN),scr3+(111*NXT_LIN),scr3+(112*NXT_LIN),scr3+(113*NXT_LIN),scr3+(114*NXT_LIN),scr3+(115*NXT_LIN),scr3+(116*NXT_LIN),scr3+(117*NXT_LIN),scr3+(118*NXT_LIN),scr3+(119*NXT_LIN)
	dc.l	scr3+(120*NXT_LIN),scr3+(121*NXT_LIN),scr3+(122*NXT_LIN),scr3+(123*NXT_LIN),scr3+(124*NXT_LIN),scr3+(125*NXT_LIN),scr3+(126*NXT_LIN),scr3+(127*NXT_LIN),scr3+(128*NXT_LIN),scr3+(129*NXT_LIN)
	dc.l	scr3+(130*NXT_LIN),scr3+(131*NXT_LIN),scr3+(132*NXT_LIN),scr3+(133*NXT_LIN),scr3+(134*NXT_LIN),scr3+(135*NXT_LIN),scr3+(136*NXT_LIN),scr3+(137*NXT_LIN),scr3+(138*NXT_LIN),scr3+(139*NXT_LIN)
	dc.l	scr3+(140*NXT_LIN),scr3+(141*NXT_LIN),scr3+(142*NXT_LIN),scr3+(143*NXT_LIN),scr3+(144*NXT_LIN),scr3+(145*NXT_LIN),scr3+(146*NXT_LIN),scr3+(147*NXT_LIN),scr3+(148*NXT_LIN),scr3+(149*NXT_LIN)
	dc.l	scr3+(150*NXT_LIN),scr3+(151*NXT_LIN),scr3+(152*NXT_LIN),scr3+(153*NXT_LIN),scr3+(154*NXT_LIN),scr3+(155*NXT_LIN),scr3+(156*NXT_LIN),scr3+(157*NXT_LIN),scr3+(158*NXT_LIN),scr3+(159*NXT_LIN)
	dc.l	scr3+(160*NXT_LIN),scr3+(161*NXT_LIN),scr3+(162*NXT_LIN),scr3+(163*NXT_LIN),scr3+(164*NXT_LIN),scr3+(165*NXT_LIN),scr3+(166*NXT_LIN),scr3+(167*NXT_LIN),scr3+(168*NXT_LIN),scr3+(169*NXT_LIN)
	dc.l	scr3+(170*NXT_LIN),scr3+(171*NXT_LIN),scr3+(172*NXT_LIN),scr3+(173*NXT_LIN),scr3+(174*NXT_LIN),scr3+(175*NXT_LIN),scr3+(176*NXT_LIN),scr3+(177*NXT_LIN),scr3+(178*NXT_LIN),scr3+(179*NXT_LIN)
	dc.l	scr3+(180*NXT_LIN),scr3+(181*NXT_LIN),scr3+(182*NXT_LIN),scr3+(183*NXT_LIN),scr3+(184*NXT_LIN),scr3+(185*NXT_LIN),scr3+(186*NXT_LIN),scr3+(187*NXT_LIN),scr3+(188*NXT_LIN),scr3+(189*NXT_LIN)
	dc.l	scr3+(190*NXT_LIN),scr3+(191*NXT_LIN),scr3+(192*NXT_LIN),scr3+(193*NXT_LIN),scr3+(194*NXT_LIN),scr3+(195*NXT_LIN),scr3+(196*NXT_LIN),scr3+(197*NXT_LIN),scr3+(198*NXT_LIN),scr3+(199*NXT_LIN)

pointlist: 
 dc.l 10,150,190,318,189,2,0,0,0,0

dirlist: 
 dc.l +1,-1,+1,-1,+1,-1,+1,-1,+1,-1,+1,-1
 
; blitter address (long)

; left - and allbitplanes word, or bitplane1 word , or bitplane2 word, or bitplane3 word ; 4 words
; right - and allbitplanes word, or bitplane1 word , or bitplane2 word, or bitplane3 word ; 4 words
; = 8 words per bitplane * 16 bits * 20 words across = 5k per colour?

; $8e=$2c81
; $90=$1bc1
; $92=$0038
; $94=$00d8
; $100=$c204 (Hires Laced)

		section ChipRAM,Data_c

copperlist:	dc.l	$01800000 				; COLOR00
			dc.l	$01820FFF 				; COLOR01
			dc.l	$008e2c81,$00902cc1		; 08e=DIWSTRT and 090=DIWSTOP ; $F4C1 320x200
			dc.l	$0092003c,$009400d4		; 092=DDFSTRT and 094=DDFSTOP
			dc.l	$01080000				; BPL1MOD ; 40 byte = 1 line / 80 = 2 lines down
			dc.l	$010A0000				; BPL1MOD ; 40 byte = 1 line / 80 = 2 lines down
mainbitp:	dc.l	$00e00000				; BPL1PTH
			dc.l	$00e20000				; BPL1PTL
			dc.l	$01009200				; BPLCON0 - Hires
			dc.l	$fffffffe

scr1:	blk.b	80*512,$AA
scr2:	blk.b	80*512,$AA
scr3:	blk.b	80*512,$AA
