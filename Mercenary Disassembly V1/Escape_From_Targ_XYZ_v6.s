; IRA V2.09 (Mar  5 2018) (c)1993-1995 Tim Ruehsen
; (c)2009-2015 Frank Wille, (c)2014-2017 Nicolas Bastien

EXT_0000	EQU	$0
ABSEXECBASE	EQU	$4
TRAP_01		EQU	$80
EXT_0003	EQU	$1000


EXT_0004	EQU	$544
;EXT_0005	EQU	$1000
EXT_0006	EQU	$401C0
COP1LCH		EQU	$DFF080
COPJMP1		EQU	$DFF088
DMACON		EQU	$DFF096
SPR0DATA	EQU	$DFF144

	SECTION S_0,CODE

start:		MOVE.L	A7,err_stk
			BSR.W	start_dos
			BSR.W	grasp_mem
	
			MOVE.L	#$0000012c,D1
			MOVEA.L	_DOSBase,A6
			JSR		-198(A6)
			BRA.W	doit

start_dos:	MOVEA.L	#dosname,A1
			MOVEQ	#0,D0
			MOVEA.L	ABSEXECBASE.W,A6
			JSR		-552(A6)
			TST.L	D0
			BEQ.W	exit_fast
			MOVE.L	D0,_DOSBase
			RTS

exit_fast:	MOVEA.L	err_stk,A7
			RTS

_DOSBase:
	DS.L	1
err_stk:
	DS.L	1
dosname:
	DC.B	"dos.library",0

grasp_mem:	MOVE.L	Block0_Size,D0
			MOVE.L	#$00010002,D1
			MOVEA.L	ABSEXECBASE.W,A6
			JSR		-198(A6)
			TST.L	D0
			BEQ.W	BOMB
			MOVE.L	D0,Block0_Data
			RTS
BOMB:		JMP		EXT_0000

Block0_Size:
	DC.L	$0002a000

Block0_Data:
	DS.L	1

;File_handle:
;	DS.L	1

doit:		LEA		Game_Start,A0
			MOVE.L	A0,D6
			LEA		L_002(PC),A0
			MOVE.L	A0,TRAP_01
			TRAP	#0
L_002:
			MOVE	#$2700,SR
			LEA		doit2(PC),A0
			LEA		EXT_0003,A1
			MOVE.L	#$00000800,D0

doit1:			MOVE.L	(A0)+,(A1)+
				DBF		D0,doit1
			JMP		EXT_0003

;	ORG	$1000

doit2:

LAB_000C:
	MOVEA.W	#$6000,A7		;000c0: 3e7c6000
	LEA	Copper_List(PC),A0		;000c4: 41fa0082
	LEA	EXT_0004,A1		;000c8: 43f900000544
	MOVE.W	#$0020,D0		;000ce: 303c0020
L_003:
	MOVE.L	(A0)+,(A1)+		;000d2: 22d8
	DBF	D0,L_003		;000d4: 51c8fffc
	BSR.W	L_019		;000d8: 6100003a
	MOVE.L	#$0000a800,D0		;000dc: 203c0000a800
	MOVEA.L	D6,A0			;000e2: 2046
	MOVEA.L	#$00040000,A1		;000e4: 227c00040000
	CMPA.L	A0,A1			;000ea: b3c8
	BCS.W	copy_up		;000ec: 6500001a
	ADDA.L	#$0002a000,A0		;000f0: d1fc0002a000
	ADDA.L	#$0002a000,A1		;000f6: d3fc0002a000
copy_down:
	MOVE.L	-(A0),-(A1)		;000fc: 2320
	DBF	D0,copy_down		;000fe: 51c8fffc
	JMP	EXT_0006		;00102: 4ef9000401c0
copy_up:
	MOVE.L	(A0)+,(A1)+		;00108: 22d8
	DBF	D0,copy_up		;0010a: 51c8fffc
	JMP	EXT_0006		;0010e: 4ef9000401c0
L_019:
	MOVE.W	#$8390,DMACON		;00114: 33fc839000dff096
	MOVE.W	#$006f,DMACON		;0011c: 33fc006f00dff096
	MOVEQ	#7,D0			;00124: 7007
	LEA	SPR0DATA,A0		;00126: 41f900dff144
L_021:
	CLR.L	(A0)			;0012c: 4290
	ADDQ.L	#8,A0			;0012e: 5088
	DBF	D0,L_021		;00130: 51c8fffa
	LEA	EXT_0004,A0		;00134: 41f900000544
	MOVE.L	A0,COP1LCH		;0013a: 23c800dff080
	MOVE.W	COPJMP1,D0		;00140: 303900dff088
	RTS				;00146: 4e75
;LAB_0012:	
	
;	DC.L	$3e7c6000,$41fa0082,
;	$43f90000,$0544303c
;	DC.W	$0020
;L_003:
;	DC.L	$22d851c8,$fffc6100,$003a203c,$0000a800
;	DC.L	$2046227c,$00040000,$b3c86500,$001ad1fc
;	DC.L	$0002a000,$d3fc0002
;	DC.W	$a000
;copy_down:
;	DC.L	$232051c8,$fffc4ef9,$000401c0
;copy_up:
;	DC.L	$22d851c8,$fffc4ef9,$000401c0
;L_019:
;	DC.L	$33fc8390,$00dff096,$33fc006f,$00dff096
;	DC.L	$700741f9,$00dff144
;L_021:
;	DC.L	$42905088,$51c8fffa,$41f90000,$054423c8
;	DC.L	$00dff080,$303900df,$f0884e75
	
	
Copper_List:
	DC.L	$2b01fffe,$01004200,$01020000,$01040000
	DC.L	$008e2c81,$0090f4c1,$00920038,$009400d0
	DC.L	$01080000,$010a0000,$00e00007,$00e28000
	DC.L	$00e40007,$00e6a000,$00e80007,$00eac000
	DC.L	$00ec0007,$00eee000,$fffffffe
	DS.W	1

Game_Start:
	incbin 'XYZ_Game1_Code'
Game_End:

	END
