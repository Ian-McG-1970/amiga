
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

BP1	= 40
BP2	= 80
NXT_LIN = 120 ; next line

BLITTER_WAIT	macro
		inline
.B_WAIT	BTST	#14,DMACONR(A6)		; wait for blitter ready
	BNE.S	.B_WAIT
		einline
		endm

VBL_WAIT		macro
		inline
.V_WAIT		MOVE.L	VPOSR(A1),D0				; wait for vertical blank
		AND.L	#$0001ff00,D0
		CMP.L	#$00012700,D0
		BNE.S	.V_WAIT
		einline
		endm

FILL_SLOPE	macro
		MOVE.L	D1,\1(A2)	; store ypos at xpos
		ADD.L 	D3,D1 			; add slope
		endm

FILL_NEXT_LINE	macro
		LEA 8(a0),a0 		; next points
		LEA NXT_LIN(a1),a1 	; next screen line
		DBEQ D7,LINE_NEXT		; dec line count
		endm

FILL_LEFT_RIGHT	macro
		lsl.w 	#4,d0 					; *32
		MOVEM.L	(a4,d0.w),D0/D2/D3/D4
	
		move.l	(a3),D5
		AND.l	D0,D5		; d0
		OR.l	D2,D5		; d2
		move.l	D5,(a3)
		move.l	BP1(a3),D5
		AND.l	D0,D5 		; d3
		OR.l	D3,D5		; d4
		move.l	D5,BP1(a3)
		move.l	BP2(a3),D5
		AND.l	D0,D5		; d5
		OR.l	D4,D5		; d6 /a3?
		move.l	D5,BP2(a3)
		
		MOVE.W	(a5,d1.w),d2
		LEA 	(A1,D2.W),A2
		lsl.w 	#4,d1 					; *16 *2
		MOVEM.l	16(a4,d1.w),D0/D2/d3/d4
	
		move.l	(a2),D5
		AND.l	D0,D5
		OR.l	D2,D5
		move.l	D5,(a2)
		move.l	BP1(a2),D5
		AND.l	D0,D5
		OR.l	D3,D5
		move.l	D5,BP1(a2)
		move.l	BP2(a2),D5
		AND.l	D0,D5
		OR.l	D4,D5
		move.l	D5,BP2(a2)
		endm

FILL_SAME	macro
		lsl.w 	#4,d0 					; *16 *2
		MOVE.l	16(a4,d0.w),D0

		lsl.w 	#4,d1 					; *16 *2
		MOVE.l	(a4,d1.w),D1

		MOVEM.l	4(a4),D2/d3/d4
		
		AND.l	D0,D1
		and.l	d1,d2
		and.l	d1,d3
		and.l	d1,d4

		NOT.l	D1
		
		move.l	(a3),D5
		AND.l	D1,D5
		OR.l	D2,D5
		move.l	D5,(a3)
		move.l	BP1(a3),D5
		AND.l	D1,D5
		OR.l	D3,D5
		move.l	D5,BP1(a3)
		move.l	BP2(a3),D5
		AND.l	D1,D5
		OR.l	D4,D5
		move.l	D5,BP2(a3)
		endm

start:
	move.l	$4,a6

	sub.l	a1,a1				; zero - find current task
	jsr	Exec_FindTask(a6)
	move.l	d0,a1				; set current task to high pri
	moveq	#127,d0
	jsr	Exec_SetTaskPri(a6)

	lea	gfxname,a1
	moveq	#0,d0
	jsr	Exec_OpenLibrary(a6)
	tst.l	d0
	beq	.error
	move.l	d0,gfxbase

	move.l	d0,a6
	move.l	Gfx_Field_ActiView(a6),-(a7)	;	 store active view

	sub.l	a1,a1					; load zero view so we get default zero state 
	jsr	Gfx_LoadView(a6)
	jsr	Gfx_WaitTOF(a6)				; Wait for both long and short frame to finish
	jsr	Gfx_WaitTOF(a6)

	move.l	$4,a6
	jsr	Exec_Forbid(a6)

	bsr	program

	move.l	$4,a6
	jsr	Exec_Permit(a6)

	move.l	gfxbase,a6
	move.l	(a7)+,a1				; load stored active view
	jsr	Gfx_LoadView(a6)
	jsr	Gfx_WaitTOF(a6)
	jsr	Gfx_WaitTOF(a6)

	move.l	$4,a6
	move.l	gfxbase,a1
	jsr	Exec_CloseLibrary(a6)

	moveq	#0,d0
	rts

.error:	moveq	#-1,d0
	rts

gfxname:	dc.b	'graphics.library',0
		even
gfxbase:	dc.l	0

program:

	lea	CUSTOM,a1
	move.w	DMACONR(a1),-(a7)		; save enabled dma channels
	move.w	INTENAR(a1),-(a7)		; save enabled interrupts
	move.w	#%0111111111111111,INTENA(a7)	; disable all interupts
	move.w	INTREQR(a1),-(a7)		; store current interrupt request bits
	move.w	ADKCONR(a1),-(a7)		; store current disk and sound control
		
	
	lea	CUSTOM,a1
	VBL_WAIT
		
	move.l	#copperlist,COP1LCH(a1)
	move.w	#%0111111111111111,DMACON(a1)	; turn off all dma 
	move.w	#%1000011111000000,DMACON(a1)	; enable bitplane + copper + blitter dma

main_loop:

	lea	CUSTOM,a1
	VBL_WAIT

		bsr	main_frame
		
		btst	#6,$bfe001                          ; left mouse button
		bne.b	main_loop

	lea	CUSTOM,a1
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

	rts

	
	; display the current one
	; flip to the next one
	; clear the next one
	; draw on it
	
main_frame:
	move.l	mainscreenp,a1			; get current screen just drawn on and update copper list for next frame
	move.l	(a1),d0
	lea	mainbitp,a0
	move.w	d0,6(a0) 	; set bit plane 1
	swap	d0
	move.w	d0,2(a0)
	swap	d0
	add.l	#40,d0
	move.w	d0,8+6(a0) 	; set bit plane 2
	swap	d0
	move.w	d0,8+2(a0)
	swap	d0
	add.l	#40,d0
	move.w	d0,16+6(a0) 	; set bit plane 3
	swap	d0
	move.w	d0,16+2(a0)

	
	move.l	mainscreenp,d0	; next to be displayed		; shift the screens to the last drawn is at the end
	move.l	mainscreenp+4,a1  ; next to be drawn
	move.l	mainscreenp+8,d1  ; following after next
	
	move.l	a1,mainscreenp 	; to be drawn on
	move.l	d1,mainscreenp+4 	; shuffle up
	move.l	d0,mainscreenp+8 	; next to be display - last to be updated

	
	lea	CUSTOM,a6 	; clear next screen to be drawn
	move.l	mainscreenp+4,a0 	; +4,a0	; clear the next screen not this screen
	move.l	(a0),a0		; address to clear
	move.w	#20,d4		; 20 words across
	move.w	#200*3,d5	; 200*3 lines down
	bsr	SCREEN_CLEAR

;	lea	CUSTOM,a6
;	move.l	mainscreenp,a0
;	move.w	#20,d4
;	move.w	#200*3,d5
;	bsr	fillmem

;	BLITTER_WAIT

	moveq.l #2,d7
	bsr movement
	
	LEA pointlist,A5	
	MOVEQ.L	#2,D7
	BSR	FILL
	rts

SCREEN_CLEAR:
	; a0 - screen address
	; d4 - screen width in words
	; d5 - screen height
	; a6 - custom base
	
	move.w	#$030,$180(a6)
	BLITTER_WAIT
	move.w	#$300,$180(a6)

	move.l	a0,BLTDPTH(a6)
	moveq.l	#0,d0
	move.w	d0,BLTDMOD(a6)
	move.w	#%100000000,d0
	swap d0
	move.l	d0,BLTCON0(a6)

	move.w	d5,d0
	lsl.w	#6,d0
	or.w	d4,d0
	move.w	d0,BLTSIZE(a6)

;	add.l	#(200*3)-15600,a0 
;	bsr CLEAR
	
;	BLITTER_WAIT
	move.w	#$000,$180(a6)

	rts

fillmem:
	; a0 - screen address
	; d4 - screen width in words
	; d5 - screen height
	; a6 - custom base

	BLITTER_WAIT

	move.w	d5,d0
	muls.w	d4,d0
	add.w	d0,d0
	lea	-2(a0,d0),a2

	move.l	a2,BLTAPTH(a6)
	move.l	a2,BLTDPTH(a6)
	move.l	#-1,BLTAFWM(a6)
	move.w	#00,BLTAMOD(a6)
	move.w	#00,BLTDMOD(a6)
	move.l	#$09f00012,BLTCON0(a6)		; A shift, USEA&D, LF4..7

	move.w	d5,d0
	lsl.w	#6,d0
	or.w	d4,d0
	move.w	d0,BLTSIZE(a6)
	rts

	
FILL 	; d7=points d6=colour
	lea	pointlist,a0 		; point list
	move.w 	d7,d5 			; points
	lsl.w 	#3,d5 			; last point address
	movem.l 	(a0),d0-d1		; copy first point
	movem.l 	d0-d1,8(a0,d5.w)		; to last point   

	lea 	ColourTab,a5 					;
	lsl.w 	#2,d6 							;
	move.l 	(a5,d6.w),a5 		; get colour

	lea 	slopejumptable,a4		; pointer to slope jump table

	moveq.l #0,d5 			; ends up holding biggest
	moveq.l #-1,d4 			; ends up holding smallest

point_loop:	movem.l	(a0),d0-d3	; get xstart, ystart, xend, yend

		lea	linetable,a3 		; fill points table left edge
		cmp.w	d0,d2 		; compare xstart, xend
		beq	point_next 			; if same ignore
		bcc 	.point_noswap		; if xstart lt xend
			exg 	d0,d2 		; swap xstart, xend
			exg 	d1,d3 		; swap ystart, yend
			lea 	4(a3),a3 		; right edge

.point_noswap:
		cmp.w 	d0,d4 	; smallest xpos
		bcs 	.point_low
			move.w 	d0,d4 	; yes

.point_low:
		cmp.w	d2,d5	; biggest xpos
		bcc 	.point_high
			move.w	d2,d5 	; yes

.point_high:
		move 	d2,d6
		lsl.w 	#3,d6 	; xend *8
		lea 	(a3,d6.w),a2 	; end position of fill points

		sub.w 	d0,d2 	; xdiff
		moveq.l 	#0,d6 	; reverse clear 
		sub.w 	d1,d3 	; ydiff
 		bcc 	.y_positive 		; not negative
 			neg.w	d3 	; negate ydiff
			moveq.l	#1,d6 	; reverse set 

.y_positive:
		add.w	d3,d3 	; ydiff
		lsl.l	#8,d3 	; ydiff
		divu.w	d2,d3 	; divide ydiff by xdiff ; source / dest

		swap	d3 		; swap whole and remainder 
		lsr.l 	#8,d3
		lsr.l 	#1,d3
		tst.w 	d6 	; is reverse set
		beq 	.point_line_pos
			neg.l	d3 	; yes

.point_line_pos:
		swap	d1 			; swap xstart
		lsl.w	#2,d2 		; xdiff *4
		move.l	(a4,d2.w),a3	; get jump pos
		jmp	(a3)			; do jump

fillslope200: FILL_SLOPE -200*8
fillslope199: FILL_SLOPE -199*8
fillslope198: FILL_SLOPE -198*8
fillslope197: FILL_SLOPE -197*8
fillslope196: FILL_SLOPE -196*8
fillslope195: FILL_SLOPE -195*8
fillslope194: FILL_SLOPE -194*8
fillslope193: FILL_SLOPE -193*8
fillslope192: FILL_SLOPE -192*8
fillslope191: FILL_SLOPE -191*8
fillslope190: FILL_SLOPE -190*8
fillslope189: FILL_SLOPE -189*8
fillslope188: FILL_SLOPE -188*8
fillslope187: FILL_SLOPE -187*8
fillslope186: FILL_SLOPE -186*8
fillslope185: FILL_SLOPE -185*8
fillslope184: FILL_SLOPE -184*8
fillslope183: FILL_SLOPE -183*8
fillslope182: FILL_SLOPE -182*8
fillslope181: FILL_SLOPE -181*8
fillslope180: FILL_SLOPE -180*8
fillslope179: FILL_SLOPE -179*8
fillslope178: FILL_SLOPE -178*8
fillslope177: FILL_SLOPE -177*8
fillslope176: FILL_SLOPE -176*8
fillslope175: FILL_SLOPE -175*8
fillslope174: FILL_SLOPE -174*8
fillslope173: FILL_SLOPE -173*8
fillslope172: FILL_SLOPE -172*8
fillslope171: FILL_SLOPE -171*8
fillslope170: FILL_SLOPE -170*8
fillslope169: FILL_SLOPE -169*8
fillslope168: FILL_SLOPE -168*8
fillslope167: FILL_SLOPE -167*8
fillslope166: FILL_SLOPE -166*8
fillslope165: FILL_SLOPE -165*8
fillslope164: FILL_SLOPE -164*8
fillslope163: FILL_SLOPE -163*8
fillslope162: FILL_SLOPE -162*8
fillslope161: FILL_SLOPE -161*8
fillslope160: FILL_SLOPE -160*8
fillslope159: FILL_SLOPE -159*8
fillslope158: FILL_SLOPE -158*8
fillslope157: FILL_SLOPE -157*8
fillslope156: FILL_SLOPE -156*8
fillslope155: FILL_SLOPE -155*8
fillslope154: FILL_SLOPE -154*8
fillslope153: FILL_SLOPE -153*8
fillslope152: FILL_SLOPE -152*8
fillslope151: FILL_SLOPE -151*8
fillslope150: FILL_SLOPE -150*8
fillslope149: FILL_SLOPE -149*8
fillslope148: FILL_SLOPE -148*8
fillslope147: FILL_SLOPE -147*8
fillslope146: FILL_SLOPE -146*8
fillslope145: FILL_SLOPE -145*8
fillslope144: FILL_SLOPE -144*8
fillslope143: FILL_SLOPE -143*8
fillslope142: FILL_SLOPE -142*8
fillslope141: FILL_SLOPE -141*8
fillslope140: FILL_SLOPE -140*8
fillslope139: FILL_SLOPE -139*8
fillslope138: FILL_SLOPE -138*8
fillslope137: FILL_SLOPE -137*8
fillslope136: FILL_SLOPE -136*8
fillslope135: FILL_SLOPE -135*8
fillslope134: FILL_SLOPE -134*8
fillslope133: FILL_SLOPE -133*8
fillslope132: FILL_SLOPE -132*8
fillslope131: FILL_SLOPE -131*8
fillslope130: FILL_SLOPE -130*8
fillslope129: FILL_SLOPE -129*8
fillslope128: FILL_SLOPE -128*8
fillslope127: FILL_SLOPE -127*8
fillslope126: FILL_SLOPE -126*8
fillslope125: FILL_SLOPE -125*8
fillslope124: FILL_SLOPE -124*8
fillslope123: FILL_SLOPE -123*8
fillslope122: FILL_SLOPE -122*8
fillslope121: FILL_SLOPE -121*8
fillslope120: FILL_SLOPE -120*8
fillslope119: FILL_SLOPE -119*8
fillslope118: FILL_SLOPE -118*8
fillslope117: FILL_SLOPE -117*8
fillslope116: FILL_SLOPE -116*8
fillslope115: FILL_SLOPE -115*8
fillslope114: FILL_SLOPE -114*8
fillslope113: FILL_SLOPE -113*8
fillslope112: FILL_SLOPE -112*8
fillslope111: FILL_SLOPE -111*8
fillslope110: FILL_SLOPE -110*8
fillslope109: FILL_SLOPE -109*8
fillslope108: FILL_SLOPE -108*8
fillslope107: FILL_SLOPE -107*8
fillslope106: FILL_SLOPE -106*8
fillslope105: FILL_SLOPE -105*8
fillslope104: FILL_SLOPE -104*8
fillslope103: FILL_SLOPE -103*8
fillslope102: FILL_SLOPE -102*8
fillslope101: FILL_SLOPE -101*8
fillslope100: FILL_SLOPE -100*8
fillslope099: FILL_SLOPE -99*8
fillslope098: FILL_SLOPE -98*8
fillslope097: FILL_SLOPE -97*8
fillslope096: FILL_SLOPE -96*8
fillslope095: FILL_SLOPE -95*8
fillslope094: FILL_SLOPE -94*8
fillslope093: FILL_SLOPE -93*8
fillslope092: FILL_SLOPE -92*8
fillslope091: FILL_SLOPE -91*8
fillslope090: FILL_SLOPE -90*8
fillslope089: FILL_SLOPE -89*8
fillslope088: FILL_SLOPE -88*8
fillslope087: FILL_SLOPE -87*8
fillslope086: FILL_SLOPE -86*8
fillslope085: FILL_SLOPE -85*8
fillslope084: FILL_SLOPE -84*8
fillslope083: FILL_SLOPE -83*8
fillslope082: FILL_SLOPE -82*8
fillslope081: FILL_SLOPE -81*8
fillslope080: FILL_SLOPE -80*8
fillslope079: FILL_SLOPE -79*8
fillslope078: FILL_SLOPE -78*8
fillslope077: FILL_SLOPE -77*8
fillslope076: FILL_SLOPE -76*8
fillslope075: FILL_SLOPE -75*8
fillslope074: FILL_SLOPE -74*8
fillslope073: FILL_SLOPE -73*8
fillslope072: FILL_SLOPE -72*8
fillslope071: FILL_SLOPE -71*8
fillslope070: FILL_SLOPE -70*8
fillslope069: FILL_SLOPE -69*8
fillslope068: FILL_SLOPE -68*8
fillslope067: FILL_SLOPE -67*8
fillslope066: FILL_SLOPE -66*8
fillslope065: FILL_SLOPE -65*8
fillslope064: FILL_SLOPE -64*8
fillslope063: FILL_SLOPE -63*8
fillslope062: FILL_SLOPE -62*8
fillslope061: FILL_SLOPE -61*8
fillslope060: FILL_SLOPE -60*8
fillslope059: FILL_SLOPE -59*8
fillslope058: FILL_SLOPE -58*8
fillslope057: FILL_SLOPE -57*8
fillslope056: FILL_SLOPE -56*8
fillslope055: FILL_SLOPE -55*8
fillslope054: FILL_SLOPE -54*8
fillslope053: FILL_SLOPE -53*8
fillslope052: FILL_SLOPE -52*8
fillslope051: FILL_SLOPE -51*8
fillslope050: FILL_SLOPE -50*8
fillslope049: FILL_SLOPE -49*8
fillslope048: FILL_SLOPE -48*8
fillslope047: FILL_SLOPE -47*8
fillslope046: FILL_SLOPE -46*8
fillslope045: FILL_SLOPE -45*8
fillslope044: FILL_SLOPE -44*8
fillslope043: FILL_SLOPE -43*8
fillslope042: FILL_SLOPE -42*8
fillslope041: FILL_SLOPE -41*8
fillslope040: FILL_SLOPE -40*8
fillslope039: FILL_SLOPE -39*8
fillslope038: FILL_SLOPE -38*8
fillslope037: FILL_SLOPE -37*8
fillslope036: FILL_SLOPE -36*8
fillslope035: FILL_SLOPE -35*8
fillslope034: FILL_SLOPE -34*8
fillslope033: FILL_SLOPE -33*8
fillslope032: FILL_SLOPE -32*8
fillslope031: FILL_SLOPE -31*8
fillslope030: FILL_SLOPE -30*8
fillslope029: FILL_SLOPE -29*8
fillslope028: FILL_SLOPE -28*8
fillslope027: FILL_SLOPE -27*8
fillslope026: FILL_SLOPE -26*8
fillslope025: FILL_SLOPE -25*8
fillslope024: FILL_SLOPE -24*8
fillslope023: FILL_SLOPE -23*8
fillslope022: FILL_SLOPE -22*8
fillslope021: FILL_SLOPE -21*8
fillslope020: FILL_SLOPE -20*8
fillslope019: FILL_SLOPE -19*8
fillslope018: FILL_SLOPE -18*8
fillslope017: FILL_SLOPE -17*8
fillslope016: FILL_SLOPE -16*8
fillslope015: FILL_SLOPE -15*8
fillslope014: FILL_SLOPE -14*8
fillslope013: FILL_SLOPE -13*8
fillslope012: FILL_SLOPE -12*8
fillslope011: FILL_SLOPE -11*8
fillslope010: FILL_SLOPE -10*8
fillslope009: FILL_SLOPE -9*8
fillslope008: FILL_SLOPE -8*8
fillslope007: FILL_SLOPE -7*8
fillslope006: FILL_SLOPE -6*8
fillslope005: FILL_SLOPE -5*8
fillslope004: FILL_SLOPE -4*8
fillslope003: FILL_SLOPE -3*8
fillslope002: FILL_SLOPE -2*8
fillslope001: FILL_SLOPE -1*8
fillslope000: FILL_SLOPE -0*8

point_next:
		lea 	8(a0),a0 		; point to next points
;		dbeq 	d7,point_loop
		subq.w 	#1,d7
		bpl 	point_loop		;	dbeq d7,point_loop 	; dec point count

	move.l	d5,d7 ; biggest

	sub.w 	d4,d7 		; count=end-start
	bls 	fill_end 	; exit if same or negative ;	beq .fill_end  	; exit if same ;	bcs .fill_end 		; exit if nothing to fill
	
	move.l	mainscreenp,a0	; address of screenybuffer
	lsl.w	#2,d4		; start *4
	move.l 	(a0,d4.w),a1 	; screen = ver pos

	lea 	linetable,a0 	; fill points table
	add.w 	d4,d4 		; start *8
	lea 	(a0,d4.w),a0 	; start position of fill points
	
	LEA	ANDOR_C0,A4 	; colour
	LEA	HOR,A5
	LEA	FILLJUMP,A6 		; 

LINE_NEXT	 ; ;a0=point table pos a1=screenaddress a4=jump table a5=colour(or) table a6=hor msb table a7=and table ;d7=count
		move.w 	(a0),d1 		; get right edge
		move.w 	4(a0),d0 	; get left edge

		cmp.w 	d0,d1		; test only
		bcc 	.test_no_swap	; test only 
			exg 	d0,d1	; test only
.test_no_swap:				; test only

		add.w	d0,d0 	; *2
		add.w	d1,d1 	; *2

		MOVE.W	(A5,D0.W),D2	; left hor pos
		LEA 	(A1,D2.W),A3	; left screen = ver pos + 2 byte hor pos

		move.w	(a5,d1.w),d3 	; get right
		move.w	(a5,d0.w),d2 	; get left
		sub.w	d2,d3		; right - left
		move.l	(a6,d3.w),a2 	; get fill code address

		jmp 	(a2) 							; jump to fill code
fill_end	RTS

FILLLINE_036
		FILL_LEFT_RIGHT
		MOVEM.L	4(a4),D4/d5/d6
		move.l	d4,d0
		move.l	d5,d1
		move.l	d6,d2
		movem.L	d4/d0,4(a3)
		movem.L	d5/d1,BP1+4(a3)
		movem.L	d6/d2,BP2+4(a3)
		movem.L	d4/d0,12(a3)
		movem.L	d5/d1,BP1+12(a3)
		movem.L	d6/d2,BP2+12(a3)
		movem.L	d4/d0,20(a3)
		movem.L	d5/d1,BP1+20(a3)
		movem.L	d6/d2,BP2+20(a3)
		movem.L	d4/d0,28(a3)
		movem.L	d5/d1,BP1+28(a3)
		movem.L	d6/d2,BP2+28(a3)
		FILL_NEXT_LINE
	RTS

FILLLINE_032
		FILL_LEFT_RIGHT
		MOVEM.L	4(a4),D4/d5/d6
		move.l	d4,d0
		move.l	d5,d1
		move.l	d6,d2
		movem.L	d4/d0,4(a3)
		movem.L	d5/d1,BP1+4(a3)
		movem.L	d6/d2,BP2+4(a3)
		movem.L	d4/d0,12(a3)
		movem.L	d5/d1,BP1+12(a3)
		movem.L	d6/d2,BP2+12(a3)
		movem.L	d4/d0,20(a3)
		movem.L	d5/d1,BP1+20(a3)
		movem.L	d6/d2,BP2+20(a3)
		move.L	d4,28(a3)
		move.L	d5,BP1+28(a3)
		move.L	d6,BP2+28(a3)
		FILL_NEXT_LINE
	RTS

FILLLINE_028
		FILL_LEFT_RIGHT
		MOVEM.L	4(a4),D4/d5/d6
		move.l	d4,d0
		move.l	d5,d1
		move.l	d6,d2
		movem.L	d4/d0,4(a3)
		movem.L	d5/d1,BP1+4(a3)
		movem.L	d6/d2,BP2+4(a3)
		movem.L	d4/d0,12(a3)
		movem.L	d5/d1,BP1+12(a3)
		movem.L	d6/d2,BP2+12(a3)
		movem.L	d4/d0,20(a3)
		movem.L	d5/d1,BP1+20(a3)
		movem.L	d6/d2,BP2+20(a3)
		FILL_NEXT_LINE
	RTS

FILLLINE_024
		FILL_LEFT_RIGHT
		MOVEM.L	4(a4),D4/d5/d6
		move.l	d4,d0
		move.l	d5,d1
		move.l	d6,d2
		movem.L	d4/d0,4(a3)
		movem.L	d5/d1,BP1+4(a3)
		movem.L	d6/d2,BP2+4(a3)
		movem.L	d4/d0,12(a3)
		movem.L	d5/d1,BP1+12(a3)
		movem.L	d6/d2,BP2+12(a3)
		move.L	d4,20(a3)
		move.L	d5,BP1+20(a3)
		move.L	d6,BP2+20(a3)
		FILL_NEXT_LINE
	RTS

FILLLINE_020
		FILL_LEFT_RIGHT
		MOVEM.L	4(a4),D4/d5/d6
		move.l	d4,d0
		move.l	d5,d1
		move.l	d6,d2
		movem.L	d4/d0,4(a3)
		movem.L	d5/d1,BP1+4(a3)
		movem.L	d6/d2,BP2+4(a3)
		movem.L	d4/d0,12(a3)
		movem.L	d5/d1,BP1+12(a3)
		movem.L	d6/d2,BP2+12(a3)
		FILL_NEXT_LINE
	RTS

FILLLINE_016
		FILL_LEFT_RIGHT
		MOVEM.L	4(a4),D4/d5/d6
		move.l	d4,d0
		move.l	d5,d1
		move.l	d6,d2
		movem.L	d4/d0,4(a3)
		movem.L	d5/d1,BP1+4(a3)
		movem.L	d6/d2,BP2+4(a3)
		move.L	d4,12(a3)
		move.L	d5,BP1+12(a3)
		move.L	d6,BP2+12(a3)
		FILL_NEXT_LINE
	RTS

FILLLINE_012
		FILL_LEFT_RIGHT
		MOVEM.L	4(a4),D4/d5/d6
		move.l	d4,d0
		move.l	d5,d1
		move.l	d6,d2
		movem.L	d4/d0,4(a3)
		movem.L	d5/d1,BP1+4(a3)
		movem.L	d6/d2,BP2+4(a3)
		FILL_NEXT_LINE
	RTS

FILLLINE_008
		FILL_LEFT_RIGHT
		MOVEM.L	4(a4),D4/d5/d6
		move.l	d4,4(a3)
		move.l	d5,BP1+4(a3)
		move.l	d6,BP2+4(a3)
		
;		FILL_LEFT_RIGHT
		FILL_NEXT_LINE
	RTS

FILLLINE_004	FILL_LEFT_RIGHT
		FILL_NEXT_LINE
	RTS

FILLLINE_000	FILL_SAME ; temp
FILLLINE_END	FILL_NEXT_LINE
	RTS

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
 		cmp #199,d0
 		bne .testxr
			neg.l d2
.testxr
 		cmp #319,d1
		bne .testyr
			neg.l d3
.testyr
		movem.l d2-d3,(a1)
 
		lea 8(a0),a0
 		lea 8(a1),a1
 	dbra d7,.moveloop
	rts

mainscreenp:	dc.l	scrptr1,scrptr2,scrptr3

HOR	DC.W	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DC.W	04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04,04
	DC.W	08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08
	DC.W	12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
	DC.W	16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
	DC.W	20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20
	DC.W	24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24
	DC.W	28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28
	DC.W	32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
	DC.W	36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36

	
ANDOR_C0
	REPT 10
	DC.L	$80000000,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF ; LEFT - AND FOLLOWED BY 3 ORS
	DC.L	$FFFFFFFF,$80000000,$80000000,$80000000 ; RIGHT - AND FOLLOWED BY 3 ORDS
	DC.L	$C0000000,$7FFFFFFF,$7FFFFFFF,$7FFFFFFF
	DC.L	$7FFFFFFF,$C0000000,$C0000000,$C0000000
	DC.L	$E0000000,$3FFFFFFF,$3FFFFFFF,$3FFFFFFF
	DC.L	$3FFFFFFF,$E0000000,$E0000000,$E0000000
	DC.L	$F0000000,$1FFFFFFF,$1FFFFFFF,$1FFFFFFF
	DC.L	$1FFFFFFF,$F0000000,$F0000000,$F0000000
	DC.L	$F8000000,$0FFFFFFF,$0FFFFFFF,$0FFFFFFF
	DC.L	$0FFFFFFF,$F8000000,$F8000000,$F8000000
	DC.L	$FC000000,$07FFFFFF,$07FFFFFF,$07FFFFFF
	DC.L	$07FFFFFF,$FC000000,$FC000000,$FC000000
	DC.L	$FE000000,$03FFFFFF,$03FFFFFF,$03FFFFFF
	DC.L	$03FFFFFF,$FE000000,$FE000000,$FE000000
	DC.L	$FF000000,$01FFFFFF,$01FFFFFF,$01FFFFFF
	DC.L	$01FFFFFF,$FF000000,$FF000000,$FF000000
	DC.L	$FF800000,$00FFFFFF,$00FFFFFF,$00FFFFFF
	DC.L	$00FFFFFF,$FF800000,$FF800000,$FF800000
	DC.L	$FFC00000,$007FFFFF,$007FFFFF,$007FFFFF
	DC.L	$007FFFFF,$FFC00000,$FFC00000,$FFC00000
	DC.L	$FFE00000,$003FFFFF,$003FFFFF,$003FFFFF
	DC.L	$003FFFFF,$FFE00000,$FFE00000,$FFE00000
	DC.L	$FFF00000,$001FFFFF,$001FFFFF,$001FFFFF
	DC.L	$001FFFFF,$FFF00000,$FFF00000,$FFF00000
	DC.L	$FFF80000,$000FFFFF,$000FFFFF,$000FFFFF
	DC.L	$000FFFFF,$FFF80000,$FFF80000,$FFF80000
	DC.L	$FFFC0000,$0007FFFF,$0007FFFF,$0007FFFF
	DC.L	$0007FFFF,$FFFC0000,$FFFC0000,$FFFC0000
	DC.L	$FFFE0000,$0003FFFF,$0003FFFF,$0003FFFF
	DC.L	$0003FFFF,$FFFE0000,$FFFE0000,$FFFE0000
	DC.L	$FFFF0000,$0001FFFF,$0001FFFF,$0001FFFF
	DC.L	$0001FFFF,$FFFF0000,$FFFF0000,$FFFF0000
	DC.L	$FFFF8000,$0000FFFF,$0000FFFF,$0000FFFF
	DC.L	$0000FFFF,$FFFF8000,$FFFF8000,$FFFF8000
	DC.L	$FFFFC000,$00007FFF,$00007FFF,$00007FFF
	DC.L 	$00007FFF,$FFFFC000,$FFFFC000,$FFFFC000
	DC.L 	$FFFFE000,$00003FFF,$00003FFF,$00003FFF
	DC.L 	$00003FFF,$FFFFE000,$FFFFE000,$FFFFE000
	DC.L 	$FFFFF000,$00001FFF,$00001FFF,$00001FFF
	DC.L 	$00001FFF,$FFFFF000,$FFFFF000,$FFFFF000
	DC.L 	$FFFFF800,$00000FFF,$00000FFF,$00000FFF
	DC.L 	$00000FFF,$FFFFF800,$FFFFF800,$FFFFF800
	DC.L 	$FFFFFC00,$000007FF,$000007FF,$000007FF
	DC.L 	$000007FF,$FFFFFC00,$FFFFFC00,$FFFFFC00
	DC.L 	$FFFFFE00,$000003FF,$000003FF,$000003FF
	DC.L 	$000003FF,$FFFFFE00,$FFFFFE00,$FFFFFE00
	DC.L 	$FFFFFF00,$000001FF,$000001FF,$000001FF
	DC.L 	$000001FF,$FFFFFF00,$FFFFFF00,$FFFFFF00
	DC.L 	$FFFFFF80,$000000FF,$000000FF,$000000FF
	DC.L 	$000000FF,$FFFFFF80,$FFFFFF80,$FFFFFF80
	DC.L	$FFFFFFC0,$0000007F,$0000007F,$0000007F
	DC.L	$0000007F,$FFFFFFC0,$FFFFFFC0,$FFFFFFC0
	DC.L 	$FFFFFFE0,$0000003F,$0000003F,$0000003F
	DC.L 	$0000003F,$FFFFFFE0,$FFFFFFE0,$FFFFFFE0
	DC.L 	$FFFFFFF0,$0000001F,$0000001F,$0000001F
	DC.L 	$0000001F,$FFFFFFF0,$FFFFFFF0,$FFFFFFF0
	DC.L 	$FFFFFFF8,$0000000F,$0000000F,$0000000F
	DC.L 	$0000000F,$FFFFFFF8,$FFFFFFF8,$FFFFFFF8
	DC.L 	$FFFFFFFC,$00000007,$00000007,$00000007
	DC.L 	$00000007,$FFFFFFFC,$FFFFFFFC,$FFFFFFFC
	DC.L 	$FFFFFFFE,$00000003,$00000003,$00000003
	DC.L 	$00000003,$FFFFFFFE,$FFFFFFFE,$FFFFFFFE
	DC.L 	$FFFFFFFF,$00000001,$00000001,$00000001
	DC.L	$00000001,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF
	ENDR

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

slopejumptable:
	dc.l fillslope000,fillslope001,fillslope002,fillslope003,fillslope004,fillslope005,fillslope006,fillslope007,fillslope008,fillslope009
	dc.l fillslope010,fillslope011,fillslope012,fillslope013,fillslope014,fillslope015,fillslope016,fillslope017,fillslope018,fillslope019
	dc.l fillslope020,fillslope021,fillslope022,fillslope023,fillslope024,fillslope025,fillslope026,fillslope027,fillslope028,fillslope029
	dc.l fillslope030,fillslope031,fillslope032,fillslope033,fillslope034,fillslope035,fillslope036,fillslope037,fillslope038,fillslope039
	dc.l fillslope040,fillslope041,fillslope042,fillslope043,fillslope044,fillslope045,fillslope046,fillslope047,fillslope048,fillslope049
	dc.l fillslope050,fillslope051,fillslope052,fillslope053,fillslope054,fillslope055,fillslope056,fillslope057,fillslope058,fillslope059
	dc.l fillslope060,fillslope061,fillslope062,fillslope063,fillslope064,fillslope065,fillslope066,fillslope067,fillslope068,fillslope069
	dc.l fillslope070,fillslope071,fillslope072,fillslope073,fillslope074,fillslope075,fillslope076,fillslope077,fillslope078,fillslope079
	dc.l fillslope080,fillslope081,fillslope082,fillslope083,fillslope084,fillslope085,fillslope086,fillslope087,fillslope088,fillslope089
	dc.l fillslope090,fillslope091,fillslope092,fillslope093,fillslope094,fillslope095,fillslope096,fillslope097,fillslope098,fillslope099
	dc.l fillslope100,fillslope101,fillslope102,fillslope103,fillslope104,fillslope105,fillslope106,fillslope107,fillslope108,fillslope109
	dc.l fillslope110,fillslope111,fillslope112,fillslope113,fillslope114,fillslope115,fillslope116,fillslope117,fillslope118,fillslope119
	dc.l fillslope120,fillslope121,fillslope122,fillslope123,fillslope124,fillslope125,fillslope126,fillslope127,fillslope128,fillslope129
	dc.l fillslope130,fillslope131,fillslope132,fillslope133,fillslope134,fillslope135,fillslope136,fillslope137,fillslope138,fillslope139
	dc.l fillslope140,fillslope141,fillslope142,fillslope143,fillslope144,fillslope145,fillslope146,fillslope147,fillslope148,fillslope149
	dc.l fillslope150,fillslope151,fillslope152,fillslope153,fillslope154,fillslope155,fillslope156,fillslope157,fillslope158,fillslope159
	dc.l fillslope160,fillslope161,fillslope162,fillslope163,fillslope164,fillslope165,fillslope166,fillslope167,fillslope168,fillslope169
	dc.l fillslope170,fillslope171,fillslope172,fillslope173,fillslope174,fillslope175,fillslope176,fillslope177,fillslope178,fillslope179
	dc.l fillslope180,fillslope181,fillslope182,fillslope183,fillslope184,fillslope185,fillslope186,fillslope187,fillslope188,fillslope189
	dc.l fillslope190,fillslope191,fillslope192,fillslope193,fillslope194,fillslope195,fillslope196,fillslope197,fillslope198,fillslope199,fillslope200

ColourTab: dc.l	8 ; list of 8 colour addresses

linetable:	ds.l 400
	
	DC.L FILLLINE_END,FILLLINE_END,FILLLINE_END,FILLLINE_END
FILLJUMP	DC.L FILLLINE_000,FILLLINE_004,FILLLINE_008,FILLLINE_012,FILLLINE_016,FILLLINE_020,FILLLINE_024,FILLLINE_028,FILLLINE_032,FILLLINE_036
	
; blitter address (long)

; left - and allbitplanes word, or bitplane1 word , or bitplane2 word, or bitplane3 word ; 4 words
; right - and allbitplanes word, or bitplane1 word , or bitplane2 word, or bitplane3 word ; 4 words
; = 8 words per bitplane * 16 bits * 20 words across = 5k per colour?
	
		section ChipRAM,Data_c
		
copperlist:	dc.l	$01800000 ; COLOR00
		dc.l	$01820f00 ; COLOR01
		dc.l	$01840008	; COLOR02
		dc.l	$0186000f	; COLOR03
		dc.l	$01880080	; COLOR04
		dc.l	$018a00f0	; COLOR05
		dc.l	$018c0800	; COLOR06
		dc.l	$018e0fff	; COLOR07

		dc.l	$008e2c81,$0090f4c1 ; 2cc1		; 08e=DIWSTRT and 090=DIWSTOP ; $F4C1 320x200
		dc.l	$00920038,$009400d0		; 092=DDFSTRT and 094=DDFSTOP

		dc.l	$01080000+80	; BPL1MOD ; 40 byte = 1 line / 80 = 2 lines down
		dc.l	$010a0000+80	; BPL2MOD ; 40 byte = 1 line / 80 = 2 lines down	
mainbitp:	dc.l	$00e00000	; BPL1PTH
		dc.l	$00e20000	; BPL1PTL
		dc.l	$00e40000	; BPL2PTH
		dc.l	$00e60000	; BPL2PTL
		dc.l	$00e80000	; BPL3PTH
		dc.l	$00ea0000	; BPL3PTL
		dc.l	$01003200	; BPLCON0

		dc.l	$fffffffe

scr1:	blk.b	40*200*3,$AA
scr2:	blk.b	40*200*3,$AA
scr3:	blk.b	40*200*3,$AA

;blitw	=Spritewidth/16			;sprite width in words
;blith	=Spriteheight			;sprite height in lines

;	lea $dff000,a6
;	bsr BlitWait
;	move.l #$09f00000,BLTCON0(a6)	;A->D copy, no shifts, ascending mode
;	move.l #$ffffffff,BLTAFWM(a6)	;no masking of first/last word
;	move.w #0,BLTAMOD(a6)		;A modulo=bytes to skip between lines
;	move.w #Screenwidth/8-blitw*2,BLTDMOD(a6)	;D modulo
;	move.l #Sprite,BLTAPTH(a6)	;source graphic top left corner
;	move.l #Screen+byteoffset,BLTDPTH(a6)	;destination top left corner
;	move.w #blith*64+blitw,BLTSIZE(a6)	;rectangle size, starts blit
