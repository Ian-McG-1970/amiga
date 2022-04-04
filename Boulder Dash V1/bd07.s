DMACON = $dff096
INTENA = $dff09a
BPL1PTL = $dff0e2
BPL1PTH = $dff0e0
BPL2PTH = $dff0e4
BPL2PTL = $dff0e6
COLOR00 = $dff180
COLOR01 = $dff182
COLOR02 = $dff184
COLOR03 = $dff186


INTREQ = $dff09c
INTREQR = $dff01e
VPOSR = $dff004
DIWSTRT = $dff08e
DIWSTOP = $dff090
DDFSTRT = $dff092
DDFSTOP = $dff094
BPLCON0 = $dff100
BPLCON1 = $dff102
BPL1MOD = $dff108
BPL2MOD = $dff10a

BOULDER = 128
DIAMOND = 129
AMOEBA = 130
SPACE = 1
TITANIUM = 2
WALL = 3
GROUND = 4

BD = BOULDER
DM = DIAMOND
AM = AMOEBA
SP = SPACE
GR = GROUND
WL = WALL
TW = TITANIUM

SCREEN_BITPLANE_SIZE = (320/8)*176
SCREEN_LINE = 40

start:		bsr 	getvbr
		move.l 	d0,a0
		move.l 	$6c(a0),oldvbi
	
		bsr 	WaitRaster
		move.w 	$dff01c,oldintena
		move.w 	$dff002,olddma

		bsr 	disable_dma_and_irqs

		lea 	irq_level3(pc),a1
		move.l 	a1,$6c(a0)

		move.w 	#$c020,INTENA	; INTENA = c020 = SET|INTEN|VERTB
		move.w 	#$8360,DMACON	; DMACON = DMACON=SET|DMAEN|BPLEN|BLTEN|SPREN

		bsr	DrawMap
		
waitmouse:		btst #6,$bfe001
			bne waitmouse

		bsr 	WaitRaster
		bsr 	disable_dma_and_irqs

		move.l 	oldvbi(pc),$6c(a0)

		move.w 	olddma(pc),d0	; restore irq requests
		or.w 	#$8000,d0
		move.w 	d0,DMACON

		move.w 	oldintena(pc),d0
		or.w 	#$c000,d0
		move.w 	d0,INTENA

		rts
	
setscreen:	move.w 	#$2c81,DIWSTRT	; Setup 320x176x2 screen
		move.w 	#$dcc1,DIWSTOP	; #$2cc1 = 256 / #$f4c1 = 200 / #$dcc1 = 176
		move.w 	#$0038,DDFSTRT
		move.w 	#$00d0,DDFSTOP
		move.w 	#0000,BPLCON1
		move.w 	#0000,BPL1MOD
		move.w 	#0000,BPL2MOD
		move.w 	#$2200,BPLCON0 ; (2 planes)
		rts
	
disable_dma_and_irqs:	lea 	$dff000,a6
			move.w 	#$7fff,d2
			move.w 	d2,$9A(a6)
			move.w 	d2,$96(a6)
			move.w 	d2,$9C(a6)
			move.w 	d2,$9C(a6)	; twice for A4000
			rts
	
; VBR helper code
; ---------------
	
getvbr:		moveq 	#0,d0		; on 68k default is 0

		move.l 	$4.w,a6		; Check for 68010+
		btst 	#0,296+1(a6)
		beq.b 	is68k

		lea 	svr_getvbr(pc),a5	; Switch to supervisor mode (movec is priviledged instruction)
		jsr 	-30(a6)		; Supervisor() - returns vbr in d0
		lea 	vbroffset(pc),a0
		move.l 	d0,(a0)		; store vbr
is68k:		rts

svr_getvbr:	dc.l 	$4e7a0801		; movec vbr,d0
		rte

; Level3 interrupt handler
; ------------------------

irq_level3:	movem.l 	d0-a6,-(a7)
		move.w 	INTREQR,d0		; Check bits of INTREQR
		btst 	#5,d0		; VBLANK?
		beq 	.quit

.vertb:		move.w 	#$0000,COLOR00
		move.w 	#$0f00,COLOR01
		move.w 	#$00f0,COLOR02
		move.w 	#$000f,COLOR03

		bsr 	setscreen
		move.l 	#Scr1,d1
		move.w 	d1,BPL1PTL
		swap 	d1
		move.w 	d1,BPL1PTH
		move.l 	#Scr2,d1
		move.w 	d1,BPL2PTL
		swap 	d1
		move.w 	d1,BPL2PTH		
		
		move.w 	#$fff,COLOR00
		bsr	CheckMap
		move.w 	#$000,COLOR00
		
		move.w 	#$4020,INTREQ
		move.w 	#$4020,INTREQ
		
.quit:		movem.l 	(a7)+,d0-a6
		rte

WaitRaster:	move.l 	d0,-(a7)
.loop:			move.l VPOSR,d0 
			and.l #$1ff00,d0
			cmp.l #303<<8,d0
			bne.b .loop
		move.l 	(a7)+,d0
		rts

DrawMap	lea	MapLast,a4
	moveq.l	#21,d3
.vloop		moveq.l	#39,d4
	
.hloop			moveq.l	#0,d2
			move.b	-(a4),d2 ; pre decrement before getting value
			move.w	d3,d0
			move.w	d4,d1
			bsr	DrawChar
		dbra	d4,.hloop	
	dbra	d3,.vloop
	rts

DrawChar	; d0=x d1=y d2=char
	lea	ScrPtr1,a6
	add.w	d0,d0
	add.w	d0,d0 ; *4
	move.l	(a6,d0.w),a6
	lea	(a6,d1.w),a6

	lea	CharTab,a5
	move.b	d2,d0 ; saves clearing the word as d0 byte will still be clear
	lsl.w	#5,d0
	lea	(a5,d0.w),a5

	move.l	(a5),d1
	move.b	d1,(a6)
	swap	d1
	move.b	d1,SCREEN_BITPLANE_SIZE(a6)

	move.l	4(a5),d1
	move.b	d1,(SCREEN_LINE)(a6)
	swap	d1
	move.b	d1,(SCREEN_LINE)+SCREEN_BITPLANE_SIZE(a6)

	move.l	8(a5),d1
	move.b	d1,(SCREEN_LINE*2)(a6)
	swap	d1
	move.b	d1,(SCREEN_LINE*2)+SCREEN_BITPLANE_SIZE(a6)

	move.l	12(a5),d1
	move.b	d1,(SCREEN_LINE*3)(a6)
	swap	d1
	move.b	d1,(SCREEN_LINE*3)+SCREEN_BITPLANE_SIZE(a6)

	move.l	16(a5),d1
	move.b	d1,(SCREEN_LINE*4)(a6)
	swap	d1
	move.b	d1,(SCREEN_LINE*4)+SCREEN_BITPLANE_SIZE(a6)

	move.l	20(a5),d1
	move.b	d1,(SCREEN_LINE*5)(a6)
	swap	d1
	move.b	d1,(SCREEN_LINE*5)+SCREEN_BITPLANE_SIZE(a6)

	move.l	24(a5),d1
	move.b	d1,(SCREEN_LINE*6)(a6)
	swap	d1
	move.b	d1,(SCREEN_LINE*6)+SCREEN_BITPLANE_SIZE(a6)

	move.l	28(a5),d1
	move.b	d1,(SCREEN_LINE*7)(a6)
	swap	d1
	move.b	d1,(SCREEN_LINE*7)+SCREEN_BITPLANE_SIZE(a6)

	rts

boulder_timer	dc.b	0
diamond_timer	dc.b	0
amoeba_timer	dc.b	0
boulder_time	dc.b	22
diamond_time	dc.b	33
amoeba_time	dc.b	44
amoeba_count	dc.w	0
amoeba_space	dc.w	0

CheckMap	
	moveq.l	#0,d0
	move.w	d0,amoeba_count
	move.w	d0,amoeba_space

	move.b	boulder_timer,d5
	bne.s	.bd_tmr
		move.b	boulder_time,d5
.bd_tmr	subq.b	#1,d5
	move.b	d5,boulder_timer

	move.b	diamond_timer,d6
	bne.s	.dm_tmr
		move.b	diamond_time,d6
.dm_tmr	subq.b	#1,d6
	move.b	d6,diamond_timer

	move.b	amoeba_timer,d7
	bne.s	.am_tmr
		move.b	amoeba_time,d7
.am_tmr	subq.b	#1,d7
	move.b	d7,amoeba_timer
	
	lea	MapLast,a4
	moveq.l	#21,d3
.vloop		moveq.l	#39,d4
	
.hloop			move.b	-(a4),d2 ; pre decrement before getting value
			BPL.s	.hcont	; not boulder or diamond or amoeba
			
				cmp.b	#BD,d2		; boulder?
				bne.s	.dmd_tst		; no

					tst.b	d5	; boulder counted down?
					bne.s	.hcont	; no
					bra.s	.bld_dmd	; yes
	
.dmd_tst				cmp.b	#DM,d2		; diamond?
				bne.s	.amb_tst		; no
					tst.b	d6	; diamond counted down?
					bne.s	.hcont	; no
					bra.s	.bld_dmd	; yes

.amb_tst				tst.b	d7	; amoeba counted down?
				bne.s	.hcont	; no
				bra	.amoeba	; yes
						
.hcont		dbra	d4,.hloop	
	dbra	d3,.vloop
	
	move.w	amoeba_count,d0	; are there amoebas
	beq.s	.exit
		move.w	amoeba_space,d1	; are there any spaces to move into
		beq	.amoeba_dmd_bld	; convert to boulders or diamonds
	
.exit	rts

.bld_dmd
;				move.w	colourtemp,d0
;				add.w	#$111,d0
;				move.w	d0,colourtemp
;				move.w 	d0,COLOR00

				move.b 	40(a4),d1
				cmp.b	#SP,d1
				bne.s	.chk_lft

.move_down			move.b	d2,40(a4)	; move current down and draw it
				move.w	d4,d1
				move.w	d3,d0
				addq.w	#1,d0
;				move.w	d2,d2 		; d2 already set as current?
				bsr	DrawChar
				move.b	#SP,d2		; move space to current and draw it
				move.b	d2,(a4)
				move.w	d4,d1
				move.w	d3,d0
				bsr	DrawChar
				bra.s	.hcont
	
.chk_lft				move.b	-1(a4),d1
				cmp.b	#SP,d1
				bne.s	.chk_rgt
				move.b	+40-1(a4),d1
				cmp.b	#SP,d1
				bne.s	.chk_rgt
				
.move_left			move.b	d2,-1(a4)	; move current left and draw it
				move.w	d4,d1
				subq.w	#1,d1
				move.w	d3,d0
;				move.w	d2,d2 		; d2 already set as current?
				bsr	DrawChar
				move.b	#SP,d2		; move space to current and draw it
				move.b	d2,(a4)
				move.w	d4,d1
				move.w	d3,d0
				bsr	DrawChar
				
				subq.w	#1,d4
				subq.l	#1,a4
				bra.s	.hcont

.chk_rgt				move.b	1(a4),d1
				cmp.b	#SP,d1
				bne	.hcont
				move.b	+40+1(a4),d1
				cmp.b	#SP,d1
				bne	.hcont

.move_right 			move.b	d2,1(a4)		; move current right and draw it
				move.w	d4,d1
				addq.w	#1,d1
				move.w	d3,d0
;				move.w	d2,d2 		; d2 already set as current?
				bsr	DrawChar
				move.b	#SP,d2		; move space to current and draw it
				move.b	d2,(a4)
				move.w	d4,d1
				move.w	d3,d0
				bsr	DrawChar
				bra	.hcont

.amoeba	
			move.w	amoeba_count,d2 ;inc amoeba count
				addq.w	#1,d2
				move.w	d2,amoeba_count

.amo_up				move.b 	-40(a4),d2 	; can amoeba move up into space or ground?
				cmp.b	#SP,d2		; space ?
				beq.s	.amo_up_ok 	; yes
				cmp.b	#GR,d2		; ground
				bne.s	.amo_right	; no
.amo_up_ok			move.w	d4,d1 ; h
				move.w	d3,d0 ; v
				subq.w	#1,d0 ; v-1
				
.amo_space			move.w	amoeba_space,d2 	; inc amoeba space count
				lea	AmoebaSpaceV,a4
				move.b	d0,(a4,d2.w)	; store v
				lea	880(a4),a4
				move.b	d1,(a4,d2.w)	; store h
				addq.w	#1,d2
				move.w	d2,amoeba_space
				bra	.hcont

.amo_right			move.b 	1(a4),d2 	; can amoeba move right into space or ground?
				cmp.b	#SP,d2		; space ?
				beq.s	.amo_right_ok 	; yes
				cmp.b	#GR,d2		; ground
				bne.s	.amo_down	; no
.amo_right_ok			move.w	d4,d1 ; h
				move.w	d3,d0 ; v
				addq.w	#1,d1 ; h+1
				bra.s	.amo_space

.amo_down			move.b 	+40(a4),d2 	; can amoeba move down into space or ground?
				cmp.b	#SP,d2		; space ?
				beq.s	.amo_down_ok 	; yes
				cmp.b	#GR,d2		; ground
				bne.s	.amo_left	; no
.amo_down_ok			move.w	d4,d1 ; h
				move.w	d3,d0 ; v
				addq.w	#1,d0 ; v-1
				bra.s	.amo_space

.amo_left			move.b 	1(a4),d2 	; can amoeba move right into space or ground?
				cmp.b	#SP,d2		; space ?
				beq.s	.amo_left_ok 	; yes
				cmp.b	#GR,d2		; ground
				bne	.hcont		; no
.amo_left_ok			move.w	d4,d1 ; h
				move.w	d3,d0 ; v
				subq.w	#1,d1 ; h+1
				bra.s	.amo_space

.amoeba_dmd_bld	; if d0 gt 200 then fill with boulders else fill with diamonds
; read through map and if amoeba is found convert to boulder/diamond and draw
rts

; at start of screen
;  reset amoeba count

; if (x/y)=amoeba
;  add 1 to amoeba count
;  if amoeba above is space or ground
;   add pos to list
;   exit
;  if amoeba right is space or ground
;   add pos to list  				
;   exit
;  if amoeba below is space or ground
;   add pos to list
;   exit
;  if amoeba left is space or ground
;   add pos to list  				
;   exit

; at end of screen
;  if amoeba count is not zero
;   if amoeba count is greater than 200
;    change all amoebas to rocks
;   if none added to list (then no amoebas can increase in size)
;    change all amoebas to diamonds


	dc.l	0	; required to align char tab
CharTab
Char000	dcb.w	16,0
Char001
	dc.w	%00000000,%00000000
	dc.w	%00000000,%00000000
	dc.w	%00000000,%00000000
	dc.w	%00000000,%00000000
	dc.w	%00000000,%00000000
	dc.w	%00000000,%00000000
	dc.w	%00000000,%00000000
	dc.w	%00000000,%00000000
Char002
	dc.w	%01111111,%01111111
	dc.w	%01111111,%01111111
	dc.w	%01111111,%01111111
	dc.w	%01111111,%01111111
	dc.w	%01111111,%01111111
	dc.w	%01111111,%01111111
	dc.w	%01111111,%01111111
	dc.w	%00000000,%00000000
Char003
	dc.w	%01111111,%00000000
	dc.w	%01111111,%00000000
	dc.w	%01111111,%00000000
	dc.w	%01111111,%00000000
	dc.w	%01111111,%00000000
	dc.w	%01111111,%00000000
	dc.w	%01111111,%00000000
	dc.w	%00000000,%00000000
Char004
	dc.w	%00000000,%01111111
	dc.w	%00000000,%01111111
	dc.w	%00000000,%01111111
	dc.w	%00000000,%01111111
	dc.w	%00000000,%01111111
	dc.w	%00000000,%01111111
	dc.w	%00000000,%01111111
	dc.w	%00000000,%00000000

Char005	dcb.w	16,0
Char006	dcb.w	16,0
Char007	dcb.w	16,0
Char008	dcb.w	16,0
Char009	dcb.w	16,0
Char010	dcb.w	16,0
Char011	dcb.w	16,0
Char012	dcb.w	16,0
Char013	dcb.w	16,0
Char014	dcb.w	16,0
Char015	dcb.w	16,0
Char016	dcb.w	16,0
Char017	dcb.w	16,0
Char018	dcb.w	16,0
Char019	dcb.w	16,0

Char020	dcb.w	16,0
Char021	dcb.w	16,0
Char022	dcb.w	16,0
Char023	dcb.w	16,0
Char024	dcb.w	16,0
Char025	dcb.w	16,0
Char026	dcb.w	16,0
Char027	dcb.w	16,0
Char028	dcb.w	16,0
Char029	dcb.w	16,0

Char030	dcb.w	16,0
Char031	dcb.w	16,0
Char032	dcb.w	16,0
Char033	dcb.w	16,0
Char034	dcb.w	16,0
Char035	dcb.w	16,0
Char036	dcb.w	16,0
Char037	dcb.w	16,0
Char038	dcb.w	16,0
Char039	dcb.w	16,0

Char040	dcb.w	16,0
Char041	dcb.w	16,0
Char042	dcb.w	16,0
Char043	dcb.w	16,0
Char044	dcb.w	16,0
Char045	dcb.w	16,0
Char046	dcb.w	16,0
Char047	dcb.w	16,0
Char048	dcb.w	16,0
Char049	dcb.w	16,0

Char050	dcb.w	16,0
Char051	dcb.w	16,0
Char052	dcb.w	16,0
Char053	dcb.w	16,0
Char054	dcb.w	16,0
Char055	dcb.w	16,0
Char056	dcb.w	16,0
Char057	dcb.w	16,0
Char058	dcb.w	16,0
Char059	dcb.w	16,0

Char060	dcb.w	16,0
Char061	dcb.w	16,0
Char062	dcb.w	16,0
Char063	dcb.w	16,0
Char064	dcb.w	16,0
Char065	dcb.w	16,0
Char066	dcb.w	16,0
Char067	dcb.w	16,0
Char068	dcb.w	16,0
Char069	dcb.w	16,0

Char070	dcb.w	16,0
Char071	dcb.w	16,0
Char072	dcb.w	16,0
Char073	dcb.w	16,0
Char074	dcb.w	16,0
Char075	dcb.w	16,0
Char076	dcb.w	16,0
Char077	dcb.w	16,0
Char078	dcb.w	16,0
Char079	dcb.w	16,0

Char080	dcb.w	16,0
Char081	dcb.w	16,0
Char082	dcb.w	16,0
Char083	dcb.w	16,0
Char084	dcb.w	16,0
Char085	dcb.w	16,0
Char086	dcb.w	16,0
Char087	dcb.w	16,0
Char088	dcb.w	16,0
Char089	dcb.w	16,0

Char090	dcb.w	16,0
Char091	dcb.w	16,0
Char092	dcb.w	16,0
Char093	dcb.w	16,0
Char094	dcb.w	16,0
Char095	dcb.w	16,0
Char096	dcb.w	16,0
Char097	dcb.w	16,0
Char098	dcb.w	16,0
Char099	dcb.w	16,0

Char100	dcb.w	16,0
Char101	dcb.w	16,0
Char102	dcb.w	16,0
Char103	dcb.w	16,0
Char104	dcb.w	16,0
Char105	dcb.w	16,0
Char106	dcb.w	16,0
Char107	dcb.w	16,0
Char108	dcb.w	16,0
Char109	dcb.w	16,0

Char110	dcb.w	16,0
Char111	dcb.w	16,0
Char112	dcb.w	16,0
Char113	dcb.w	16,0
Char114	dcb.w	16,0
Char115	dcb.w	16,0
Char116	dcb.w	16,0
Char117	dcb.w	16,0
Char118	dcb.w	16,0
Char119	dcb.w	16,0

Char120	dcb.w	16,0
Char121	dcb.w	16,0
Char122	dcb.w	16,0
Char123	dcb.w	16,0
Char124	dcb.w	16,0
Char125	dcb.w	16,0
Char126	dcb.w	16,0
Char127	dcb.w	16,0
Char128
	dc.w	%00000000,%00111100
	dc.w	%00000000,%01111110
	dc.w	%00000000,%11111111
	dc.w	%00000000,%11111111
	dc.w	%00000000,%11111111
	dc.w	%00000000,%01111110
	dc.w	%00000000,%00111100
	dc.w	%00000000,%00000000

Char129
	dc.w	%00001000,%00001000
	dc.w	%00011100,%00011100
	dc.w	%00111110,%00111110
	dc.w	%01111111,%01111111
	dc.w	%00111110,%00111110
	dc.w	%00011100,%00011100
	dc.w	%00001000,%00001000
	dc.w	%00000000,%00000000

Char130	
	dc.w	%00000000,%00111110
	dc.w	%00000000,%01000001
	dc.w	%00000000,%01000001
	dc.w	%00000000,%01111111
	dc.w	%00000000,%01000001
	dc.w	%00000000,%01000001
	dc.w	%00000000,%01000001
	dc.w	%00000000,%00000000

Char131	dcb.w	16,0
Char132	dcb.w	16,0
Char133	dcb.w	16,0
Char134	dcb.w	16,0
Char135	dcb.w	16,0
Char136	dcb.w	16,0
Char137	dcb.w	16,0
Char138	dcb.w	16,0
Char139	dcb.w	16,0

	dc.w	%00111110,%00111110
	dc.w	%01000001,%01000001
	dc.w	%01000000,%01000000
	dc.w	%00111110,%00111110
	dc.w	%00000001,%00000001
	dc.w	%01000001,%01000001
	dc.w	%00111110,%00111110
	dc.w	%00000000,%00000000
	
	dc.w	%00111110,%00111110
	dc.w	%01000001,%01000001
	dc.w	%01000000,%01000000
	dc.w	%01000000,%01000000
	dc.w	%01000111,%01000111
	dc.w	%01000001,%01000001
	dc.w	%00111110,%00111110
	dc.w	%00000000,%00000000
	
	dc.w	%01111110,%01111110
	dc.w	%01000001,%01000001
	dc.w	%01000001,%01000001
	dc.w	%01111110,%01111110
	dc.w	%01000000,%01000000
	dc.w	%01000000,%01000000
	dc.w	%01000000,%01000000
	dc.w	%00000000,%00000000

	dc.w	%01000001,%01000001
	dc.w	%01000001,%01000001
	dc.w	%01000001,%01000001
	dc.w	%00001001,%01001001
	dc.w	%01010101,%01010101
	dc.w	%01100011,%01100011
	dc.w	%01000001,%01000001
	dc.w	%00000000,%00000000

ScrPtr1	dc.l	Scr1+(000*320),Scr1+(001*320),Scr1+(002*320),Scr1+(003*320),Scr1+(004*320),Scr1+(005*320),Scr1+(006*320),Scr1+(007*320),Scr1+(008*320),Scr1+(009*320)
	dc.l	Scr1+(010*320),Scr1+(011*320),Scr1+(012*320),Scr1+(013*320),Scr1+(014*320),Scr1+(015*320),Scr1+(016*320),Scr1+(017*320),Scr1+(018*320),Scr1+(019*320)
	dc.l	Scr1+(020*320),Scr1+(021*320)

MapPtr	dc.l	Map00,Map01,Map02,Map03,Map04,Map05,Map06,Map07,Map08,Map09,Map10,Map11,Map12,Map13,Map14,Map15,Map16,Map17,Map18,Map19,Map20,Map21

Map00:	dc.b	TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW
Map01	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map02	dc.b	TW,SP,SP,SP,SP,SP,AM,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,DM,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map03	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,BD,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map04	dc.b	TW,SP,SP,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,SP,SP,SP,SP,TW
Map05	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,GR,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map06	dc.b	TW,SP,SP,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,SP,SP,SP,SP,TW
Map07	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map08	dc.b	TW,SP,SP,SP,BD,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map09	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,SP,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,SP,TW
Map10	dc.b	TW,SP,SP,SP,DM,SP,BD,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,WL,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map11	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,DM,SP,SP,SP,SP,SP,SP,SP,SP,SP,WL,SP,SP,SP,SP,SP,SP,SP,DM,SP,SP,SP,SP,SP,TW
Map12	dc.b	TW,SP,SP,SP,BD,SP,SP,SP,SP,SP,BD,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,BD,WL,WL,WL,SP,AM,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map13	dc.b	TW,SP,SP,SP,BD,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map14	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,SP,SP,TW
Map15	dc.b	TW,SP,SP,SP,DM,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map16	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,BD,SP,SP,SP,SP,SP,SP,SP,SP,AM,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map17	dc.b	TW,SP,SP,SP,WL,WL,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map18	dc.b	TW,SP,SP,SP,SP,SP,SP,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,WL,SP,SP,SP,SP,SP,SP,SP,TW
Map19	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map20	dc.b	TW,SP,SP,SP,SP,SP,SP,SP,SP,SP,DM,SP,AM,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,SP,TW
Map21	dc.b	TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW,TW
MapLast

AmoebaSpaceV	dcb.b	40*22,0
AmoebaSpaceH	dcb.b	40*22,0	
		
vbroffset:	dc.l 0 
oldvbi:		dc.l 0
oldintena: 	dc.l 0
olddma:		dc.l 0
colourtemp	dc.w	0

gfxlib:	dc.b "graphics.library",0,0

	SECTION ChipData,DATA_C
	
Scr1	dcb.l SCREEN_BITPLANE_SIZE/4,$AA55AA55
Scr2	dcb.l SCREEN_BITPLANE_SIZE/4,$A55A5AA5
