*----------------------------------------------------------------------------*
*			     						     *
*		                   "ZOOL" 				     *
*			     						     *
*                      By George Allan ... (1991/1992)		 	     *
*			     						     *
*----------------------------------------------------------------------------*

		regs	pc=start,sr=$2700
		org	$1000
		even

; The Quick Brown Fox Jumped Over The Lazy Dog ...

		; Game modes ...

swn		equ	0
swn2		equ	0

download	equ	1
dsksel0		equ 	3		3=df0:,4=df1:
diskmode	equ	1		1 = disk version . (maps etc)
protection	equ	0		1 = protection on ...
makedisk	equ	1		1 = ready to upload.

codesum		equ	$bd799691	Check sum of the protection data ...
progsum		equ	0 $23423423	Check sum of the program ...

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*


; ** Macros to port over to Amiga 4000 and execute
; ** Use:	DOWNLD	ProgramStart,ProgramEnd
; **	RUN	ProgramStart
; **  NB. Be sure not to download and execute the macros!

SETADR		equ	$01
EXECUTE		equ	$02

		bra	Invisible

RUN		MACRO
		move.l	#EXECUTE,d0
		move.l	#\1,d1
		jsr	SendCommand
		ENDM

DOWNLD		MACRO
		move.l	#SETADR,d0
		move.l	#\1,d1
		move.l	d1,a0
		jsr	SendCommand
		move.l	#\2,d2
		sub.l	d1,d2
PortNxtByte\@	move.b	(a0)+,d0
		jsr	SendDataByte
		subq.l	#1,d2
		bne.s	PortNxtByte\@
		ENDM

SendCommand	move.l	d1,-(sp)
		bsr.s	SendCommByte
		move.l	d1,d0
		move.w	#3,d1
NxtRotDat	rol.l	#8,d0
		jsr	SendCommByte
		dbf	d1,NxtRotDat
		move.l	(sp)+,d1
		rts

SendCommByte	bset	#1,$bfd000
		bra.s	SendByte
SendDataByte	bclr	#1,$bfd000

SendByte	move.b	#%00000010,$bfd200	; ddr
		move.b	#$FF,$BFE301
DatNotReady	btst.b	#0,$BFD000
		bne.s	DatNotReady
		move.b	D0,$BFE101
		move.b	#$00,$bfde00
		move.b	#$7f,$bfdd00
		move.b	#8,$bfd400
		move.b	#0,$bfd500
		move.b	#$09,$bfde00
Wait_Timer	btst	#0,$bfdd00
		beq.s	Wait_Timer
		rts
Invisible

pushall		Macro
		movem.l	d0-d7/a0-a6,-(sp)
		EndM

pullall		Macro
		movem.l	(sp)+,d0-d7/a0-a6
		EndM


start		if	download = 1
		DOWNLD	$1000,end
		RUN	prog
.t		trap	#0
		bra	.t
		endif

		if	makedisk = 1
		illegal
		endif

		bra	prog

goto		equ	$8000
repeat		equ	$8001

mh		equ	16*6
planesize	equ	288*44


setplane1	equ	$52700
setplane2	equ	$62700
scrplanes	equ	$72700

; Bullet equates.

bds		equ	8
bt		equ	0
bd		equ	1
bba		equ	2
bhs		equ	6

blbheights	equ	0
blbwides	equ	4
blbyoffs	equ	8
blbplaces	equ	12

; Sprite equates.

sds		equ	80
x		equ	0		World x.
y		equ	2		World y.
fr		equ	4		Frame number.
blbset		equ	6		Blob set.
psp		equ	10		Previous sprite pointer.
nsp		equ	14		Next sprite pointer.
snp		equ	18		Pointer to position in alien map.
t		equ	22		Alien type.
htg		equ	23		Hits to go.
flal		equ	24		Flash flag.
ht		equ	25		Height control.
jsp		equ	26		Pointer to joint sprite.
any		equ	30

; Replacer equates.

resize		equ	14
scroff		equ	0
size		equ	4
scrandoff	equ	6
blitsm		equ	10
fwme		equ	12

;	      0 equ  0

sln1		equ	3			world 1 (tools)
sln2		equ	1			world 2 (music)
sln3		equ	2			world 3 (Fruit)
sln4		equ	6			world 4 (shoot)
sln		equ	6			world 4 (shoot)
sln5		equ	4			world 5 (toys)
sln6		equ	5			world 6 (fun fair)


worldnos	dc.b	0,2,3,1,5,6,4
	;       0,1,2,3,4,5,6

		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

blankspr	dc.l	0,0
stack		ds.b	1024
stacktop
tcc		dc.b	0
actualzool	dc.b	0
ontogame	dc.b	0
frameno		dc.b	0
areano		dc.l	0
worldno		dc.l	0
lastworld	dc.l	0
musicno		dc.w	0
collectables	dc.l	0
collected	dc.l	0
hearttime	dc.b	0
zoolx		dc.w	0
zooly		dc.w	0
zoolx1		dc.w	0
zooly1		dc.w	0
scrx		dc.b	0
scry		dc.w	0
scrx1		dc.b	0
scry1		dc.w	0
scrollpos	dc.l	0
scrollpos1	dc.l	0
remember	ds.l	20
shield		dc.w	0

timeleft	dc.l	500
timeleftlast	dc.l	0

zspr1		ds.l	71
zspr2		ds.l	71
zspr3		ds.l	71
zspr4		ds.l	71
zspr5		ds.l	71
zspr6		ds.l	71
zspr7

dope		dc.b	0
arrowfr		dc.b	0
		include	d:\source\joytest.s
endx		dc.l	0
endy		dc.l	0
onnl		dc.b	0
zoolrun		dc.l	0
zoolship	dc.l	0
nextspr		dc.l	0
alienjumps	dc.l	nextalien2
		dc.l	alien1
		dc.l	alien2
		dc.l	nextalien	alien3
		dc.l	alien4
		dc.l	alien5
		dc.l	alien6
		dc.l	alien7
		dc.l	alien8
		dc.l	alien9
		dc.l	alien10
		dc.l	alien11
		dc.l	alien12
		dc.l	alien13
		dc.l	alien14
		dc.l	alien15
		dc.l	alien16
		dc.l	alien17
		dc.l	alien18
		dc.l	alien19
		dc.l	alien20
		dc.l	alien21
		dc.l	alien22
		dc.l	alien23
		dc.l	alien24
		dc.l	alien25
		dc.l	alien26
		dc.l	alien27
		dc.l	alien28
		dc.l	alien29
		dc.l	alien30
		dc.l	alien31
		dc.l	alien32
		dc.l	alien33
		dc.l	alien34
		dc.l	alien35
		dc.l	alien36
		dc.l	alien37
		dc.l	alien38
		dc.l	alien39
		dc.l	alien40
		dc.l	alien41
		dc.l	alien42
		dc.l	alien43
		dc.l	alien44
		dc.l	alien45
		dc.l	alien46
		dc.l	alien47
		dc.l	alien48
		dc.l	alien49
		dc.l	alien50
		dc.l	alien51
		dc.l	alien52
		dc.l	alien53
		dc.l	alien54
		dc.l	alien55
		dc.l	alien56
		dc.l	alien57
		dc.l	alien58
		dc.l	alien59
		dc.l	alien60
		dc.l	alien61
		dc.l	alien62
		dc.l	alien63
		dc.l	alien64
		dc.l	alien65
		dc.l	alien66
		dc.l	alien67
		dc.l	alien68
		dc.l	alien69
		dc.l	alien70
		dc.l	alien71
		dc.l	alien72
		dc.l	alien73
		dc.l	alien74
		dc.l	alien75
		dc.l	alien76
		dc.l	alien77
		dc.l	alien78
		dc.l	alien79
		dc.l	alien80
		dc.l	alien81
		dc.l	alien82
		dc.l	alien83
		dc.l	alien84
		dc.l	alien85
		dc.l	alien86
		dc.l	alien87
		dc.l	alien88
		dc.l	alien89
		dc.l	alien90
		dc.l	alien91
		dc.l	alien92
		dc.l	alien93
		dc.l	alien94
		dc.l	alien95
		dc.l	alien96
		dc.l	alien97
		dc.l	alien98
		dc.l	alien99
		dc.l	alien100
		dc.l	alien101
		dc.l	alien102
		dc.l	alien103
		dc.l	alien104
		dc.l	alien105
		dc.l	alien106
		dc.l	alien107
		dc.l	alien108
		dc.l	alien109
		dc.l	alien110
		dc.l	alien111
		dc.l	alien112
		dc.l	alien113
		dc.l	alien114
		dc.l	alien115
		dc.l	alien116
		dc.l	alien117
		dc.l	alien118
		dc.l	alien119
		dc.l	alien120
		dc.l	alien121
		dc.l	alien122
		dc.l	alien123
		dc.l	alien124
		dc.l	alien125
		dc.l	alien126
		dc.l	alien127
		dc.l	alien128
		dc.l	alien129
		dc.l	alien130
		dc.l	alien131
		dc.l	alien132
		dc.l	alien133
		dc.l	alien134
		dc.l	alien135
		dc.l	alien136
		dc.l	alien137
		dc.l	alien138
		dc.l	alien139
		dc.l	alien140
		dc.l	alien141
		dc.l	alien142
		dc.l	alien143
		dc.l	alien144

fail		dc.b	2			Fails to go ...

		even
zooloffs	incbin	d:\various\zooloffs.dat
zooloffs2	ds.l	3

homepal		dc.w	0,$fc1,$090,$6f6,$02f,$f00,$5af,$f4f,$044,$fff,$f9c,$810,$08f,$070,$f73,$050
		dc.w	$0000,$0fff,$0888,$0000,$0fff,$0da0,$0520,$0160
		dc.w	$0240,$0b00,$0193,$0222,$0000,$0e52,$0800,$0b80
planetpal	dc.w	0,$aaa,$1b0,$090,$070,$00f,$fc0,$0af,$d70,$960,$0de,$a20,$06f,$f10,$fff,$fff
		dc.w	$0000,$0fff,$0888,$0000,$0222,$0444,$0530,$0666
		dc.w	$0750,$0800,$0b00,$0d20,$0888,$0fa1,$0111,$0fff
bakpal		dc.w	0,$789,$124,$345,$568,0,0,$ccc
		dc.w	0,0,$016,$002,$12d,$019,$34f,$004

mapx		dc.l	0
mapy		dc.l	0
scrwx		dc.w	0
scrwy		dc.w	0
zoolwx		dc.w	0
zoolwy		dc.w	0
leftfoot	dc.b	0			MBE
rightfoot	dc.b	0
splitpower	dc.w	0
blinkt		dc.b	0
tempocnt	dc.b	0



wrkplanes	dc.l	0
displanes	dc.l	0

ssetupm		dc.b	0
ssetupcnt	dc.b	0
horzy		dc.b	0
sidemappos	dc.l	0

vertx		dc.b	0
vertcount	dc.b	0
vertmappos	dc.l	0
setprintflip	dc.b	0
othervertscr	dc.l	0
othervertscr2	dc.l	0
loadspace	dc.l	0
mem		dc.l	0
sparemem	dc.l	0
musicplace	dc.l	0
temposp		dc.b	0
chrblb		ds.w	30
hplane1		dc.l	0
hplane2		dc.l	0
topmem		dc.l	0		Loaded up to this address.
onlift		dc.w	0

copplcmem	ds.l	10

copwait1	dc.w	$50df,$fffe
scrollbpls2	dc.w	$00e2,$1b2a
		dc.w	$00e6,$1b2a
		dc.w	$00ea,$1b2a
		dc.w	$00ee,$1b2a
		dc.w	$00f2,$1b2a
		dc.w	$00f0,$0000

fromtitle	dc.b	0
mapdata		dc.l	0
alienmap	dc.l	0
almapxs		dc.l	0
whitepal	dc.w	$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff
blackpal	ds.w	32

*** SPRITE DATA STRUCTURES ***

		even
firstsprite	dc.l	$ffffffff
lastsprite	dc.l	$ffffffff
sprites		ds.b	90*sds
freesprpointer	dc.l	eol
freesprlist	ds.l	8		First .
nomore		ds.l	80		Second .
eol		dc.l	0		Eol .

hiscomess	dc.w	8,14
		dc.b	'   ZOOL HISCORES',$ff
hiscomess3	dc.w	8,6,0
		dc.b	0,0,0,0,0,0,0,0,0,'10',$ff
hiscomess2	dc.w	0,6
		dc.b	0,0
hiscorestart	dc.b 	' 1 GEORGE... 020000 ',0
		dc.b 	' 2 ADE...... 009000 ',0
		dc.b 	' 3 TONY..... 008000 ',0
		dc.b 	' 4 PAT...... 007000 ',0
		dc.b 	' 5 SHORTY... 006000 ',0
		dc.b 	' 6 GREGGS... 005000 ',0
		dc.b 	' 7 MARK..... 004000 ',0
		dc.b 	' 8 ASH...... 003000 ',0
		dc.b 	' 9 SIZ...... 002000 ',0
ten		dc.b 	'   GORDON... 001000 ',0,$ff
		ds.b	42

		even
protracker	incbin	d:\various\protrack.old
		include	d:\source\equates.s
		even
		include	d:\source\amidisc3.s
		include	d:\source\keys.s
		include	d:\source\fades.s
		include	d:\source\dechomp.s


addthis		dc.l	0
clearlist1	ds.l	6*60
clearlist2	ds.l	6*60
snpglob		dc.l	0
zooll		dc.w	3
zooll2		dc.w	3
zooll3		dc.w	3
startw		dc.l	swn

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

prog		; Brief's not so bad ...

     		lea	stacktop,sp

		move.l	#.priv,4*8.w		Set supervisor mode.
.priv		move.w	#$2700,sr
		move.w	#$2100,sr

		move.w	#$7fff,$dff096		All DMA off.
		move.w	#$7fff,$dff09a		All interrupts off.

		move.l	#gameirq,$6c		Use scroll irq.

		move.w	#$f,$dff096		Turn sound off.
		jsr	set_keys

		move.w	#$87e0,$dff096		Our DMA on.
		move.w	#$c020,$dff09a		Out interrupts on.	c020

		; MEM
		move.l	$4.w,a0			Find extra mem ...
		move.l	78(a0),d0
		move.l	62(a0),d1
		cmp.l	#$80000,d1
		beq.s	no_extra_chip
		move.l	#$80000,d2
		jmp	store
no_extra_chip	move.l	#$d00000,d2
		cmp.l	#$d80000,d0
		beq	store
		move.l	#$c00000,d2
		cmp.l	#$c80000,d0
		beq	store
		move.l	#$200000,d2
		move.w	#$1234,$240000
		cmp.w	#$1234,$240000
		beq	store
		moveq	#0,d2
store		move.l	d2,mem		 	Store for later use.
		add.l	#$10000,mem
		lea	exstart,a0		Relocate map.
		move.l	mem,a1
		move.w	#((exend-exstart)/4)+20,d0
.mrr		move.l	(a0)+,(a1)+
		dbra	d0,.mrr
		move.l	mem,zoolrun		Reset variables ...
		add.l	#57188,zoolrun
		move.l	mem,sparemem
		add.l	#(exend-exstart),sparemem
		move.l	mem,maps
		move.l	#swn2,areano
		; END MEM

		if	protection = 1
		lea	codes,a0
		moveq	#0,d0
		move.w	#((lastc-codes)/4)-1,d1
.more		move.l	(a0)+,d2
		add.l	d2,d0
		dbra	d1,.more
		cmp.l	#codesum,d0
		beq	pok1
kill1    	lea	frame1,a0
.kill1		clr.l	(a0)+
		bra	.kill1
pok1
		else
		bra	gototitle
		endif

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PROCHECK	move.l	#safebye,inaframe+2

		move.w	$dff006,d0
		move.w	d0,seed
		move.w	d0,seed+4
		move.w	d0,seed+8
		neg.w	d0
		move.w	d0,seed+2
		move.w	d0,seed+6
		move.w	d0,seed+10

		move.w	#'..',codepl+4
		move.b	#'.',codepl+6
		clr.w	prop

		lea	zoolptec+2+(96*36*4),a0
		lea	procols+2,a1
		move.w	#15-1,d0
.oim		move.w	(a0)+,(a1)+
		addq.w	#2,a1
		dbra	d0,.oim

.wait		jsr	random
		btst.b	#7,$bfe001
		beq	.wait
		lea	$60000,a0		Blank planes.
		move.w	#(256*40*5)/4,d0
.moreclr	clr.l	(a0)+
		dbra	d0,.moreclr
		move.l	mem,a0			Get chrs ...
		add.l	#55012,a0
		lea	$6d000,a1
		jsr	dechomp
		move.l	#$6d000,chrplace

.ag		bsr	random
		btst.b	#7,$bfe001
		bne	.ag

		move.l	#bluecop,$dff080
		bsr	synchup
.wi		bsr	random

		move.w	#'..',codepl+4
		move.b	#'.',codepl+6
		clr.w	prop


.again		btst.b	#7,$bfe001
		beq	.again
		lea	codes,a0		Get code place ...
		jsr	random
		andi.w	#$ff,d0
		mulu	#code2-codes,d0
		add.w	d0,a0
		cmp.l	#lastc,a0
		bpl	.again
		move.b	2(a0),nopo		Set number to print ...
		move.b	3(a0),nopo+1
		cmp.b	#' ',nopo+1
		bne	.km
   		move.b	2(a0),nopo+1
		move.b	#'0',nopo
.km

 ;		move.b	0(a0),pronos
  ;		add.b	#$30,pronos
   ;		move.b	1(a0),pronos+1
    ;		add.b	#$30,pronos+1
     ;		move.b	2(a0),pronos+2
      ;		move.b	3(a0),pronos+3
       ;  	move.b	4(a0),pronos+4
       	;	move.b	5(a0),pronos+5
	 ;     	move.b	6(a0),pronos+6

		lea	procols7,a2
		moveq	#0,d0
		move.b	(a0),d0
		subq.b	#1,d0
		add.w	d0,d0
		move.w	(a2,d0.w),topy+2
		move.b	1(a0),d0
		subq.b	#1,d0
		add.w	d0,d0
		move.w	(a2,d0.w),boty+2

		lea	$60000,a1		Blank planes.
		move.w	#(256*40*5)/4,d0
.moreclr2	clr.l	(a1)+
		dbra	d0,.moreclr2

		lea	zoolptec,a4
		moveq	#0,d0
		move.b	(a0),d0
		subq.b	#1,d0
		cmp.b	#6,d0
		bmi	.nohand
       		subq.b	#6,d0
		add.l	#28*36,a4
.nohand		mulu	#6,d0
		add.w	d0,a4
		lea	$60000+(40*40)+16,a5
		moveq	#28-1,d6
.morelines	move.l	(a4),(a5)
		move.w	4(a4),4(a5)
		move.l	(96*36)(a4),(256*40)(a5)
		move.w	4+(96*36)(a4),4+(256*40)(a5)
		move.l	(96*36*2)(a4),(256*40*2)(a5)
		move.w	4+(96*36*2)(a4),4+(256*40*2)(a5)
		move.l	(96*36*3)(a4),(256*40*3)(a5)
		move.w	4+(96*36*3)(a4),4+(256*40*3)(a5)
		add.w	#36,a4
		add.w	#40,a5
		dbra	d6,.morelines

		lea	zoolptec+(28*2*36),a4
		moveq	#0,d0
		move.b	1(a0),d0
		subq.b	#1,d0
		cmp.b	#6,d0
		bmi	.nohandle
       		subq.b	#6,d0
		add.l	#20*36,a4
.nohandle	mulu	#6,d0
		add.w	d0,a4
		lea	$60000+((28+40)*40)+16,a5
		moveq	#19-1,d6
.morelines2	move.l	(a4),(a5)
		move.w	4(a4),4(a5)
		move.l	(96*36)(a4),(256*40)(a5)
		move.w	4+(96*36)(a4),4+(256*40)(a5)
		move.l	(96*36*2)(a4),(256*40*2)(a5)
		move.w	4+(96*36*2)(a4),4+(256*40*2)(a5)
		move.l	(96*36*3)(a4),(256*40*3)(a5)
		move.w	4+(96*36*3)(a4),4+(256*40*3)(a5)
		add.w	#36,a4
		add.w	#40,a5
		dbra	d6,.morelines2

		move.l	a0,-(sp)

		move.l	#$60000,wrkplanes	Print main mess ...
		lea	promess,a4
		jsr	printmess
		lea	codepl,a4
		jsr	printmess
		move.w	nopm,.mmn+2

		move.l	#procop,$dff080

.wait2		jsr	synchup			Main loop ...
		jsr	random

		cmp.b	#$46,key
		beq	.jn
		cmp.b	#$41,key
		bne	.nodel
.jn		clr.b	key
		tst.w	prop
		beq	.nodel
		subq.w	#1,prop
		move.w	prop,d0
		lea	codepl+4,a0
		move.b	#'.',(a0,d0.w)
.nodel
		cmp.w	#3,prop
		beq	.no
		moveq	#0,d0
		move.b	key,d0
		cmp.b	#$40,d0
		beq	.no
		btst	#7,d0
		bne	.no
		move.l	#keycodes,a0
		tst.b	(a0,d0.w)
		beq	.no
		clr.b	key
		move.w	prop,d1
		lea	codepl+4,a1
		move.b	(a0,d0.w),(a1,d1.w)
		addq.w	#1,prop
.no
		btst.b	#7,$bfe001
		beq	.do
		cmp.b	#$44,key
		bne	.now
.do   		move.l	(sp)+,a0
		lea	codepl+4,a1
		moveq	#2,d5
.next		move.b	4(a0),d0
		cmp.b	#' ',d0
		bne	.kmfg
		move.b	#'.',d0
.kmfg		cmp.b	(a1),d0
		beq	.thsiok
		move.l	#bluecop,$dff080
		jsr	synchup
		tst.b	fail
		bne	.noki
		lea	.now,a0
.kmk		clr.l	(a0)+
		bra	.kmk
.noki		subq.b	#1,fail
		clr.b	key
     		bra	.again
.thsiok		addq.w	#1,a0
		addq.w	#1,a1
		dbra	d5,.next
		bra	.sk
		illegal
.sk		move.w	#$2000,d0
.mmn		nop
		illegal
		dbra	d0,.mmn
		move.l	#blankcop,$dff080
		jsr	synchup
     		bra	outofpro
.now
		move.l	#$60000+(194*40),a0
		move.w	#(16*40)/4,d0
.oe		clr.l	(a0)+
		clr.l	(256*40)-4(a0)
		clr.l	(256*40*2)-4(a0)
		clr.l	(256*40*3)-4(a0)
		add.l	#(256*40*4),a0
		clr.l	-4(a0)
		sub.l	#(256*40*4),a0
		dbra	d0,.oe

		lea	codepl,a4
		jsr	printmess

		bra	.wait2
proend		illegal
		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

outofpro	if	protection = 1
		lea	procheck,a0
		move.w	#((proend-procheck)/4)-1,d0
		moveq	#0,d2
.mlm		move.l	(a0)+,d1
		add.l	d1,d2
		dbra	d0,.mlm
		nop	illegal
		endif

gototitle	jsr	resetgamevars
		st.b	lastworld
		jmp	frontend
backhere2	lea	stacktop,sp
		move.b	conts,conts2
		move.w	startw+2,worldno+2
		move.l	#56,lastworld
loadnextlevel	move.b	#'2',dno1
		bsr	correctdisk
		jsr	loadarea

restart	     	lea	stacktop,sp
		jsr	resetvars
		jsr	copyscolines

		st.b	ontogame
		jsr	disgetready		Get ready ...
		clr.b	ontogame

startfromzen
		move.l	mapx,d0			Calc correct scrollpos.
smod1		divu	#mh,d0
		andi.l	#$ffff,d0
		move.l	#inaframestuff,inaframe+2
		lsl.l	#1,d0
		move.l	d0,scrollpos
		move.l	#setplane1,wrkplanes	Set start planes.
		move.l	#setplane2,displanes
		move.l	#setplane1,d7		Set up screen and als.
		jsr	createscreen
		move.l	#setplane2,d7
		jsr	createscreen
		move.l	#scrplanes,d7
		jsr	createscreen
		jsr	createscreenals

		move.l	#blankcop,$dff080	Last minute stuff.
		clr.w	$dff088
		move.b	#15,gamefadetmr

		jsr	createcoords

		move.b	#3,energylevel
		clr.b	stophorz

		move.w	#120,shield

		cmp.b	#6,worldno+3
		beq	frame1
		tst.l	collectedst
		beq	frame1
		move.l	collectedst,collected
		clr.l	collectedst

FRAME1		jsr	synchup			Wait for the top.
 		clr.b	synched
	     	lea	stacktop,sp

	 	jsr	createcoords
   		tst.b	gamefadetmr
		bne	.ski
		tst.b	controlmode
		bne	.ski
		jsr	momscroll		Scroll once ...
		st.b	actualzool
		jsr	jumper
		move.w	xlift,d0
		add.w	d0,zoolx
		move.w	ylift,d0
		add.w	d0,zooly
	 	jsr	createcoords
		jsr	scroller
.ski		jsr	createcoords
		move.w	scry,scry1		Save scroll values ...
		move.b	scrx,scrx1
		move.l	scrollpos,scrollpos1
		move.w	zoolx,zoolx1
		move.w	zooly,zooly1
		move.b	zoold+1,zoold1
		move.l	skypl,skypl1
		move.w	skyy,skyy1
		jsr	checkcollect		Control stuff.

		tst.b	gamefadetmr
		bne	.ski2
		tst.b	controlmode
		bne	.ski2
		jsr	momscroll		Scroll once ...
		jsr	checkcollect		Control stuff.
		st.b	actualzool
		jsr	jumper
		jsr	createcoords
		clr.b	actualzool
   		move.w	xlift2,d0		For dstick.
		tst.w	d0
		bne	.dol
		move.w	xlift,d0
.dol		add.w	d0,zoolx
		move.w	ylift2,d0		For dstick.
		tst.w	d0
		bne	.dol2
		move.w	ylift,d0
.dol2		add.w	d0,zooly
		jsr	createcoords
     		jsr	scroller
.ski2
.no1	 	jsr	createcoords

		tst.b	zoolisdead
		bne	.notimedec

		tst.w	musicno
		bne	.nookaa

		tst.b	spining
		beq	.kmkm
		subq.b	#1,spinc
		bpl	.kmkm
     		move.b	#3,spinc
		moveq	#$b,d0
		jsr	playsample
.kmkm
		cmp.w	#5,xmom
		beq	.ch
   		cmp.w	#-5,xmom
		bne	.u2
.ch		cmp.b	#0,zooll+1		Foot steps ...
		beq	.u
		cmp.b	#4,zooll+1		Foot steps ...
		beq	.u
		cmp.b	#11,zooll+1		Foot steps ...
		beq	.u
  		cmp.b	#11+4,zooll+1
		bne	.u2
.u		moveq	#$c,d0
		jsr	playsample
.u2		subq.b	#1,hearttime		Heart beat ...
		bpl	.ok3434

		move.l	timeleft,d0
		divu	#10,d0
		cmp.w	#24,d0
		bmi	.ok
   		move.w	#24,d0
.ok		add.b	#13,d0
		move.b	d0,hearttime
		moveq	#3,d0
		move.b	energylevel,d1
		sub.b	d1,d0
.okff		tst.b	d0
		beq	.no
		subq.b	#1,d0
		subq.b	#8,hearttime
   		cmp.b	#13,hearttime
		bpl	.okff
		move.b	#13,hearttime
.no
.ok3434		cmp.b	#1,hearttime
		beq	.okaa
		cmp.b	#5,hearttime
		bne	.nookaa
.okaa		move.l	#$d,d0
		jsr	playsample2
.nookaa
		subq.b	#1,tcc		Dec time and kill...
		bpl	.notimedec
   		move.b	#15,tcc
		tst.b	stophorz
		bne	.notimedec
		subq.l	#1,timeleft
		bpl	.notimedec
		move.b	#$7f,tcc
		jsr	byebyezool
.notimedec
		moveq	#0,d2
		move.l	endx,d0
		move.w	zoolwx,d1
		lsr.w	#4,d1
		andi.l	#$ffff,d1		
		sub.l	d1,d0
		move.l	d0,d1
		tst.l	d0
		bpl	.ok23
   		neg.l	d0
.ok23		cmp.l	#10,d0
		bmi	.pos
		tst.l	d1
		bpl	.that
		bset	#0,d2
		jmp	.pos
.that		bset	#1,d2
.pos
		move.l	endy,d0
		move.w	zoolwy,d1
		lsr.w	#4,d1
		andi.l	#$ffff,d1
		sub.l	d1,d0
		move.l	d0,d1
		tst.l	d0
		bpl	.ok2
   		neg.l	d0
.ok2		cmp.l	#10,d0
		bmi	.pos2
		tst.l	d1
		bpl	.that2
		bset	#2,d2
		jmp	.pos2
.that2		bset	#3,d2
.pos2
		lea	arowfrs,a0
		move.b	(a0,d2.w),arrowfr

		cmp.b	#sln,worldno+3
		bne	.mnnn
		clr.b	arrowfr
.mnnn
		addq.b	#1,arowfrs
		cmp.b	#8,arowfrs
		bne	.n
  		clr.b	arowfrs
.n
		jsr	printsco
		jsr	zooldeath
		jsr	levelcompleted    
	
		jsr	dobullets
		jsr	movealiens
		jsr	clearblobs
		jsr	placeblobs

		tst.b	gs
		bne	.nod
.wait		tst.b	synched			Wait for second frame ...
		beq	.wait
.nod		move.l	wrkplanes,d0		Swap screens ...
		move.l	displanes,wrkplanes
		move.l	d0,displanes
		clr.b	frameno

		jsr	random
		and.w	#$3f,d0
		add.w	#$100-$1f,d0
		move.w	d0,sam3+4
		move.w	d0,sam5+4
		move.w	d0,sam1+4
	
		tst.b	gamefadetmr		Fade game in after 5 frames.
		beq	.alright
		subq.b	#1,gamefadetmr
		bne	.notfadedin
		move.l	#gamecop,$dff080
.alright	jsr	pausemode
.notfadedin
   		sub.b	#1,bcount
		bpl	.okf
   		move.b	#70,bcount
.okf
		jmp	frame1			Loop round and do it agian.


bird1		dc.w	1,1,1,0,0,1,1,1,1,1,0,0,0,1,1,1,$ffff
		dc.l	bird1
bird2		dc.w	3,3,3,3,3,3,2,2,2,3,3,3,3,2,2,3,3,3,3,3,$ffff
		dc.l	bird2
bird3		dc.w	5,5,4,4,5,5,5,5,5,5,5,4,4,4,5,5,5,4,4,5,5,5,5,$ffff
		dc.l	bird3
bird4		dc.w	7,7,7,7,7,7,7,7,7,6,6,6,7,7,7,7,7,6,6,7,7,7,$ffff
		dc.l	bird4
baby		dc.w	8,8,8,8,8,8,8,9,9,10,10,11,11,8,8,8,8,8,8
		dc.w	8,8,9,9,10,10,11,11,8,8,8,8,8,8,8,8,8
		dc.w	9,9,10,10,11,11,8,8,9,9,10,10,11,11,8,8,8,8,$ffff
		dc.l	baby

lovemoves	dc.w	3,-2,3,-2,2,-2,1,-2,0,-2,0,-2
		dc.w	-1,-2,-1,-2,-2,-2,-2,-2,-3,-2,-3,-2,-3,-2,-3,-2,-2,-2,-2,-2,-1,-2,-1,-2
		dc.w	0,-2,0,-2,1,-2,1,-2,2,-2,2,-2,3,-2,3,-2,goto
		dc.l	lovemoves
loveani		dc.w	12,12,12,13,13,13,13,14,14,14,14,15,15,15,15
.here		dc.w	16,$ffff
		dc.l	.here

hedge		dc.w	17,17,17,18,18,18,19,19,19,20,20,20,$ffff
		dc.l	hedge

shipdat		dc.w	0,0,6
		dc.w	6,0,6
		dc.w	12,0,5
		dc.w	17,0,5
		dc.w	22,16,4
		dc.w	26,16,4
		dc.w	30,16,3
		dc.w	33,16,3
		dc.w	36,16,3
		dc.w	39,36,1
shipf		dc.w	0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

BACKEND		; End of game stuff ...
	
		move.b	#'1',dno1
		bsr	correctdisk
		lea	overprog,a0		Store prog...
		move.l	sparemem,a1
		move.w	#(realend-overprog)/4,d0
.mrr1		move.l	(a0)+,(a1)+
		dbra	d0,.mrr1
		bsr	.back
		lea	overprog,a0		Restore prog...
		move.l	sparemem,a1
		move.w	#(realend-overprog)/4,d0
.mrr2		move.l	(a1)+,(a0)+
		dbra	d0,.mrr2
		move.b	#$0,sprson+2
		move.b	#$38,dfs+3
		rts

.back		jsr	resetvars
		clr.l	scrollpos
		st	fromtitle
		move.l	#introcop,$dff080
		clr.w	$dff088
		move.l	#safebye,inaframe+2

    		; Lets mess up memory ...

		clr.b	dir_valid		Start disk drive.
		jsr	initialise
		jsr	startdrv

		lea	zoolplann,a0		Planet scape ...
		lea	overprog,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
	
		lea	zoolhomen,a0		Home scape ...
		lea	overprog+31464,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file

		lea	zoolfamn,a0		Zool family ...
		lea	overprog+45476,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		move.l	a1,a0
		bsr	dechomp

		lea	zoolshipn,a0		Zool ship ...
		lea	overprog+61572,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file

		lea	zoolspacn,a0		Zoolspace ...
		lea	overprog+140102,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		move.l	a1,a0
		bsr	dechomp

		lea	shipfrsn,a0		Ship frs ...
		lea	overprog+169638,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		move.l	a1,a0
		bsr	dechomp

		lea	demon,a0		Music ...
		lea	overprog+196240,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file

		jsr	stopdrv

		move.b	#1,tempocnt
		move.b	#$72,temposp	Play zool demo music ...
		moveq	#0,d0		Set up player spec.
		moveq	#0,d1		Interrupt type (not used on selfcall).
		moveq	#0,d2		0=Selfcall/1=AutoCall.
		moveq	#0,d3		0=Pal/1=Ntsc.
		jsr	protracker
		move.l	a0,-(sp)
		moveq	#1,d0		Tell protracker where the mod is.
		lea	overprog+196240,a0
		jsr	protracker
		moveq	#4,d0		Set maximum vol.
		moveq	#64,d1		Vol (0-64).
		jsr	protracker
		moveq	#8,d0		Make sure all the cannels are on.
		moveq	#0,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#1,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#2,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#3,d1
		jsr	protracker
		move.l	(sp)+,musicplace Addr to call on interrupt.
.sk
redoback	moveq	#6-1,d4
.more		bsr	getsprite2		Get all ship sprites ...
		move.l	#overprog+169638,blbset(a6)
		dbra	d4,.more

		move.l	#introcop,$dff080
		clr.w	$dff088
		move.l	#safebye,inaframe+2
		move.b	#1,planpl
		move.b	#5,planpl+1
		clr.w	shipf
   		move.l	#introcop,$dff080
		clr.w	$dff088
		move.b	#$36,starty1+2
      		move.b	#$ff,starty2
		move.w	#$200,scrwx
		move.w	#$200,scrwy
		move.l	#scrplanes,wrkplanes
		move.l	#setplane2,displanes
		move.w	#$24,pri+2

		lea	introbpls,a0
		move.l	#overprog+140102,d1
		move.l	d1,d0			Set plane 1 copper bpls.
		move.w	d0,14(a0)
		swap	d0
		move.w	d0,10(a0)
		move.l	d1,d0			Set plane 2 copper bpls.
		add.l	#246*40,d0
		move.w	d0,30(a0)
		swap	d0
		move.w	d0,26(a0)
		move.l	d1,d0			Set plane 3 copper bpls.
		add.l	#246*40*2,d0			         
		move.w	d0,46(a0)
		swap	d0
		move.w	d0,42(a0)

       		move.l	#setplane2,a0
		move.l	#scrplanes,a1
		move.w	#(44*288*4)/4,d0
.morecopy	clr.l	(a0)+
		clr.l	(a1)+
		dbra	d0,.morecopy

		st.b	doingintro
		st.b	doingintro2

		move.l	#irqbpls,inaframe+2

.iframe1	jsr	synchup
		clr.b	frameno
		bsr	clearblobs
		bsr	placeblobs

		move.w	shipf,d0
		mulu	#6,d0
		lea	shipdat,a0
		move.w	(a0,d0.w),d5
		move.w	2(a0,d0.w),d2
		move.w	4(a0,d0.w),d6
		add.w	#$280,d2
		move.w	#$240,d3
		move.l	firstsprite,a6
		moveq	#0,d7
      		moveq	#6-1,d1
.nexts		cmp.w	d6,d7
		bmi	.okg
		moveq	#0,d3
.okg		move.w	d2,x(a6)
		move.w	d3,y(a6)
		move.w	d5,fr(a6)
		addq.b	#1,d5
		add.w	#16,d2
		addq.b	#1,d7
		move.l	nsp(a6),a6
		dbra	d1,.nexts

		tst.b	planpl+1
		beq	.pllp
		subq.b	#1,planpl+1
		bra	.no
.pllp		tst.w	introcols+10
		bne	.skfade
		lea	introcols+2,a0
		lea	bakpal,a1
		move.w	#16-1,a4     
		jsr	fadeacross
		move.w	#$40,d7
.pa		bsr	synchup
		dbra	d7,.pa
.skfade 
   		subq.b	#1,planpl
		bpl	.no
		move.b	#2,planpl
		cmp.w	#$ff0,eng+2
		beq	.kmk
    		add.w	#$110,eng+2
.kmk		cmp.w	#$aa0,eng+2
		bmi	.no
		move.b	#1,planpl
		addq.w	#1,shipf
		cmp.w	#11,shipf
		bne	.no
  		lea	introcols+2,a0
		lea	blackpal,a1
		move.w	#16-1,a4     
		jsr	fadeacross

		move.l	firstsprite,a6
		jsr	freesprite2
		move.l	nsp(a6),a6
		jsr	freesprite2
		move.l	nsp(a6),a6
		jsr	freesprite2
		move.l	nsp(a6),a6
		jsr	freesprite2
		move.l	nsp(a6),a6
		jsr	freesprite2
		move.l	nsp(a6),a6
		jsr	freesprite2
		clr.b	doingintro
		move.l	#safebye,inaframe+2
		bra	toybit
.no
		bra	.iframe1

irqbpls		move.l	wrkplanes,d0		Swap screens ...
		move.l	displanes,wrkplanes
		move.l	d0,displanes

		lea	introbpls,a0
		move.l	displanes,d1
		move.l	d1,d0			Set plane 1 copper bpls.
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		move.l	d1,d0			Set plane 2 copper bpls.
		add.l	#44,d0
		move.w	d0,22(a0)
		swap	d0
		move.w	d0,18(a0)
		move.l	d1,d0			Set plane 3 copper bpls.
		add.l	#44*2,d0
		move.w	d0,38(a0)
		swap	d0
		move.w	d0,34(a0)
		rts

******************************************************************************
		; THROUGH TOY TOWN BIT ...
******************************************************************************

toybit		move.b	#$40,bplam+2
		move.b	#$30,ws1+2
		move.b	#$28,ws2+2
		move.b	#$30,dfs+3
		move.w	#$64,pri+2
		move.w	#38,planmods+2
		move.w	#38,planmods+6
		move.w	#0,planmods+10
		move.l	#titlecop,$dff080
		clr.w	$dff088
		move.b	#sln,worldno+3
		move.l	#overprog+61572,zoolship
		move.w	#1,zooll
		move.w	#$40,zoolx
		move.w	#$90,zooly
		move.b	#$80,sprson+2
		st.b	doingintro2

		jsr	getsprite2		Bird 1 ...
		move.l	#overprog+45476,blbset(a6)
		clr.b	fr+1(a6)
		move.w	#$20b,x(a6)
		move.w	#$248,y(a6)
		move.b	#140,t(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		clr.l	any+14(a6)
		move.l	#bird1,any+10(a6)
	
		jsr	getsprite2		Bird 2 ...
		move.l	#overprog+45476,blbset(a6)
		move.b	#2,fr+1(a6)
		move.w	#$240,x(a6)
		move.w	#$213,y(a6)
		move.b	#140,t(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		clr.l	any+14(a6)
		move.l	#bird2,any+10(a6)

		jsr	getsprite2		Bird 3 ...
		move.l	#overprog+45476,blbset(a6)
		move.b	#2,fr+1(a6)
		move.w	#$300,x(a6)
		move.w	#$215,y(a6)
		move.b	#140,t(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		clr.l	any+14(a6)
		move.l	#bird3,any+10(a6)

		jsr	getsprite2		Bird 4 ...
		move.l	#overprog+45476,blbset(a6)
		move.b	#2,fr+1(a6)
		move.w	#$32c,x(a6)
		move.w	#$24b,y(a6)
		move.b	#140,t(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		clr.l	any+14(a6)
		move.l	#bird4,any+10(a6)

		jsr	getsprite2		Baby 1 ...
		move.l	#overprog+45476,blbset(a6)
		move.b	#2,fr+1(a6)
		move.w	#$2dd,x(a6)
		move.w	#$2d2,y(a6)
		move.b	#140,t(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		clr.l	any+14(a6)
		move.l	#baby+20,any+10(a6)
		jsr	getsprite2		Baby 2 ...
		move.l	#overprog+45476,blbset(a6)
		move.b	#2,fr+1(a6)
		move.w	#$2d1,x(a6)
		move.w	#$2db,y(a6)
		move.b	#140,t(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		clr.l	any+14(a6)
		move.l	#baby,any+10(a6)

		jsr	getsprite2		Sonic ...
		move.l	#overprog+45476,blbset(a6)
		move.b	#17,fr+1(a6)
		move.w	#$1f0,x(a6)
		move.w	#$2e4-10,y(a6)
		move.b	#144,t(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		clr.l	any+14(a6)
		move.l	#hedge,any+10(a6)

	
		jsr	getsprite2		Wife 1 ...
		move.l	#overprog+45476,blbset(a6)
		move.b	#21,fr+1(a6)
		move.w	#$2b2,x(a6)
		move.w	#$2ba,y(a6)
		move.b	#140,t(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		move.w	#20,any+8(a6)
		clr.l	any+10(a6)
		move.l	a6,a4
		jsr	getsprite2		Wife 1 p2 ...
		move.l	#overprog+45476,blbset(a6)
		move.b	#22,fr+1(a6)
		move.w	#$2b2+32,x(a6)
		move.w	#$2ba,y(a6)
		move.b	#0,t(a6)
		clr.l	any(a6)
		clr.l	any+10(a6)
		move.l	a6,jsp(a4)

		lea	zspr1,a0
.more		clr.l	(a0)+
		cmp.l	#zspr7-8,a0
		bmi	.more

		lea	overprog,a0		Dechomp planet scape ...
		lea	$60000,a1
		bsr	dechomp
		lea	titlebpls,a0
		move.l	#$60000,d1
		move.l	#(80*256),d2
		bsr	setplanes
		move.b	#$81,startblink+1
		move.l	#$60000,planpl
		move.l	#placeirq,inaframe+2
		lea	titlecols+2,a0
		lea	planetpal,a1
		move.w	#32-1,a4
		jsr	fadeacross

.waitforfire	
		jsr	placezool
		jsr	synchup
		jsr	placezool
		addq.w	#2,zoolx 
		cmp.w	#$1d0,zoolx
		bmi	.waitforfire

    		lea	titlecols+2,a0
		lea	whitepal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		move.l	#safebye,inaframe+2

HOMEPAGE	; HOMEPAGE ...

		move.l	#setplane1,displanes
		move.l	#setplane2,wrkplanes
		move.l	#$02000200,scrwx

		lea	overprog+31464,a0	Dechomp planet scape ...
		lea	$72700,a1
		bsr	dechomp

		lea	$72700,a0
		lea	$52700,a1
	  	move.w	#256-1,d1
.morelines	moveq	#10-1,d0	
.moreline	move.l	(a0),(a1)
		move.l	(40*256)(a0),44(a1)
		move.l	(40*256*2)(a0),44*2(a1)
		move.l	(40*256*3)(a0),44*3(a1)
		addq.w	#4,a0
		addq.w	#4,a1
		dbra	d0,.moreline
		add.w	#(44*3)+4,a1
		dbra	d1,.morelines
		lea	$52700,a0
		lea	$62700,a1
		lea	$72700,a2
		move.w	#(44*256*4)/4,d0
.moretrans	move.l	(a0),(a1)
		move.l	(a0)+,(a2)+
		add.w	#4,a1
		dbra	d0,.moretrans

		move.b	#$2c,ws2+2
		move.b	#$3c,dfs+3
		move.b	#$38,dfs+3
		move.w	#(44*3)+4,planmods+2
		move.w	#(44*3)+4,planmods+6
		move.w	#0,planmods+10
		move.l	#titlecop,$dff080
		clr.w	$dff088
		clr.b	worldno+3
		move.w	#4,zooll
		move.b	#50,cutewait
		move.w	#$38,zoolx
		move.w	#$ec,zooly
		move.b	#$80,sprson+2
		move.b	#4,dno1
		move.w	#$1005,planpl
		move.l	#placeirq2,inaframe+2

.waitforfire	jsr	synchup

		cmp.w	#$114,zoolx		Standing ?
		bmi	.nostand
      		move.w	#8,zooll
		subq.b	#1,planpl
		bpl	.noyet
		move.b	#39,planpl
		subq.b	#1,planpl+1
		bmi	.outofbak
		jsr	getsprite2		Make love ...
		move.l	#overprog+45476,blbset(a6)
		move.b	#12,fr+1(a6)
		move.w	#$2b2-6,x(a6)
		move.w	#$2ba-10,y(a6)
		move.b	#140,t(a6)
		move.l	#lovemoves,any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		clr.l	any+14(a6)
		move.l	#loveani,any+10(a6)
		bra	.noyet
.nostand	tst.b	cutewait		Small pause ...
		beq	.okok
     		subq.b	#1,cutewait
		bra	.noyet
.okok		addq.w	#3,zoolx		Animate ...
		subq.b	#1,dno1
		bpl	.noyet
		move.b	#4,dno1
		addq.w	#1,zooll
		cmp.w	#8,zooll
		bne	.noyet
      		clr.w	zooll
.noyet		
		move.l	displanes,d0
		move.l	wrkplanes,displanes
		move.l	d0,wrkplanes
		bsr	clearblobs
		bsr	movealiens
		bsr	placeblobs
		tst.w	titlecols+2
		beq	.skipfade
		lea	titlecols+2,a0
		lea	homepal,a1
		move.w	#32-1,a4
		jsr	fadeacross
.skipfade	bra	.waitforfire

.outofbak	lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		move.l	#safebye,inaframe+2
		move.b	#$0,sprson+2
		move.l	firstsprite,a6
.mn		move.l	nsp(a6),a4
		bsr	freesprite2
		cmp.l	#-1,a4
		beq	commess
		move.l	a4,a6
		bra	.mn

placeirq2	bsr	placezool
		lea	titlebpls,a0
		move.l	wrkplanes,d1
		move.l	#44,d2
		bra	setplanes

cutewait	dc.w	0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

COMMESS		move.l	#safebye,inaframe+2

		move.w	#$5000,bplam+2
		move.b	#$81,startblink+1
		st	fromtitle
		st	lastworld+3
		move.b	#$30,ws1+2
		move.b	#$23,ws2+2
		move.b	#$38,dfs+3
   		clr.w	planmods+2
		clr.w	planmods+6
		clr.w	planmods+10

		lea	titlebpls,a0
		move.l	#$60000,d1
		move.l	#(40*256),d2
		bsr	setplanes

		move.l	#titlecop,$dff080		      
		clr.w	$dff088

		lea	$60000,a0		Blank planes.
		move.w	#(256*40*5)/4,d0
.moreclr	clr.l	(a0)+
		dbra	d0,.moreclr

		move.l	#titlecop,$dff080
		clr.w	$dff088

		move.l	mem,a0
		add.l	#55012,a0
		lea	$6d000,a1
		jsr	dechomp
		move.l	#$6d000,chrplace

		move.l	#$60000,wrkplanes
		lea	compmess,a4
		jsr	printmess

		move.w	#$400,$60000+(256*40*5)
		clr.w	$60000+(40*256*5)+32
		move.l	#$06000900,$60000+(40*256*5)+34	Set text pal.
		move.l	#$0b000d00,$60000+(40*256*5)+38
		move.l	#$0e000f00,$60000+(40*256*5)+42

      		lea	titlecols+2,a0		Display hisco scr.
		lea	$60000+(40*256*5),a1
		move.w	#32-1,a4
		jsr	fadein

		move.w	#$200,d0

.wait		jsr	synchup
		btst.b	#7,$bfe001
		beq	.oiwait
		dbra	d0,.wait
	 	lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		bra	redoback

.oiwait	 	lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		clr.l	musicplace
		move.w	#$f,$dff096
		rts

compmess	dc.w	8,4
		dc.b	0,0
		dc.b	'     WELL DONE',0,0
		dc.b	' ZOOL HAS RETURNED',0
		dc.b	'TO HIS HOMELAND AND',0
		dc.b	' HIS LOVING FAMILY',0,0
		dc.b	'     THANK YOU',0,$ff

		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien140	; Zool fam stuff...

		cmp.b	#21,fr+1(a6)
		bmi	.notwife
		subq.w	#1,any+8(a6)
		bpl	.nojust
		move.w	#60,any+8(a6)
		cmp.w	#$100,zoolx
		bpl	.nojust
		move.l	#-$70000,any+4(a6)
.nojust		add.l	#$c000,any+4(a6)
		move.w	any+4(a6),d0
		add.w	d0,y(a6)
		cmp.w	#$2ba,y(a6)
		bmi	.nog
    		move.w	#$2ba,y(a6)
		clr.l	any+4(a6)
.nog		move.b	#21,fr+1(a6)
		tst.w	any+4(a6)
		beq	.ofk
		move.b	#25,fr+1(a6)
.ofk		move.l	jsp(a6),a5
		move.w	y(a6),y(a5)
		move.b	fr+1(a6),fr+1(a5)
		addq.b	#1,fr+1(a5)
		bra	nextalien2
.notwife

		cmp.w	#$1d0,y(a6)
		bpl	.n
		jsr	freesprite2
		bra	nextalien2
.n
		bsr	defmov

		subq.b	#1,any+14(a6)
		bpl	nextalien2
		move.b	#2,any+14(a6)
		cmp.l	#0,any+10(a6)
		beq	nextalien2
		move.l	any+10(a6),a0
		cmp.w	#$ffff,(a0)
		beq	.goto
		move.w	(a0),fr(a6)
		addq.l	#2,any+10(a6)
		bra	nextalien2
.goto		move.l	2(a0),any+10(a6)
		jmp	alien140

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien144	; That bloody hedgehog ...

		move.w	scrwx,d0
		add.w	zoolx,d0
		sub.w	#64+$40,d0
		move.w	d0,zoolwx

		move.l	any+10(a6),a0
		cmp.w	#$ffff,(a0)
		beq	.goto
		move.w	(a0),fr(a6)
		addq.l	#2,any+10(a6)
		bra	.nextalien2
.goto		move.l	2(a0),any+10(a6)
		bra	alien144
.nextalien2
		addq.w	#1,x(a6)
		move.w	zoolwx,d0
		add.w	#$22,d0
		cmp.w	x(a6),d0
		bmi	.zoolb
		addq.w	#2,x(a6)
		add.l	#$5000,any(a6)
		move.w	any(a6),d0
		add.w	d0,y(a6)
		jmp	nextalien2
.zoolb		move.w	#-3,any(a6)
		jmp	nextalien2

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*


demon		dc.b	'rock.mod',0
zoolplann	dc.b	'zoolplan.chm',0
zoolhomen	dc.b	'zoolhome.chm',0
zoolspacn	dc.b	'zoolspac.chm',0
shipfrsn	dc.b	'shipfrs.chm',0
zoolfamn	dc.b	'zoolfam.chm',0

placeirq
		jsr	placezool
		sub.w	#$cc,planmods+10
		bpl	.skj
		add.w	#$110,planmods+10
		addq.l	#2,planpl
.skj		lea	titlebpls,a0
		move.l	planpl,d1
		move.l	#(80*256),d2
		jsr	setplanes
		rts



planpl		dc.l	0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

TITLECOP	; Main game copper.

		dc.w	$2801,$fffe		Wait a bit for mainline.
sprson		dc.w	$0096,$0020		Sprite DMA on.
dfs		dc.w	$0092,$0038		Data fetch start.
		dc.w	$0094,$00d0		Data fetch stop.
ws1		dc.w	$008e,$3081		Window start.
ws2		dc.w	$0090,$23c1		Window stop.
planmods	dc.w	$0108,$0000		Mods.
		dc.w	$010a,$0000		Mods.
		dc.w	$0102,$0000		X shifts.
		dc.w	$0104,$0064		Priorities.
titlebpls
six		dc.w	$00e0,$0006		Set top bitplane pointers.
zoolleft	dc.w	$00e2,0
		dc.w	$00e4,$0006
		dc.w	$00e6,(256*40)
		dc.w	$00e8,$0006
		dc.w	$00ea,(256*40)*2
		dc.w	$00ec,$0006
zoolleft2	dc.w	$00ee,(256*40)*3
		dc.w	$00f0,$0007
		dc.w	$00f2,(256*40)*4

titlecols	dc.w	$0180,$0		Main game colours.
		dc.w	$0182,$0
		dc.w	$0184,$0
		dc.w	$0186,$0
		dc.w	$0188,$0
		dc.w	$018a,$0
		dc.w	$018c,$0
		dc.w	$018e,$0
		dc.w	$0190,$0
		dc.w	$0192,$0
		dc.w	$0194,$0
		dc.w	$0196,$0
		dc.w	$0198,$0
		dc.w	$019a,$0
		dc.w	$019c,$0
		dc.w	$019e,$0
		dc.w	$01a0,$0
		dc.w	$01a2,$0
		dc.w	$01a4,$0
		dc.w	$01a6,$0
		dc.w	$01a8,$0
		dc.w	$01aa,$0
		dc.w	$01ac,$0
		dc.w	$01ae,$0
		dc.w	$01b0,$0
		dc.w	$01b2,$0
		dc.w	$01b4,$0
		dc.w	$01b6,$0
		dc.w	$01b8,$0
		dc.w	$01ba,$0
		dc.w	$01bc,$0
		dc.w	$01be,$0

bplam		dc.w	$100,$4000


		dc.w	$a001,$fffe,$008e
startblink	dc.w	$3001
		dc.w	$b401,$fffe,$008e,$3081

		dc.w	$ffd7,$fffe,$134,$0,$134,$0,$134,$0,$134,$0,$134,$0
		dc.w	$2501,$fffe

		dc.w	$122,zspr1
		dc.w	$126,zspr2
		dc.w	$12a,zspr3
		dc.w	$12e,zspr4
		dc.w	$132,zspr5
		dc.w	$136,zspr6
		dc.w	$120,$0
		dc.w	$124,$0
		dc.w	$128,$0
		dc.w	$12c,$0
		dc.w	$130,$0
		dc.w	$134,$0

		dc.w	$ffff,$fffe

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

FRONTEND	move.b	#'1',dno1
		bsr	correctdisk
		lea	overprog,a0		Store prog...
		move.l	sparemem,a1
		move.w	#(realend-overprog)/4,d0
.mrr2		move.l	(a0)+,(a1)+
		dbra	d0,.mrr2
		move.l	#blobs,loadspace

		jmp	dointrobit
backhere
     		lea	stacktop,sp

		lea	overprog,a0		Restore prog...
		move.l	sparemem,a1
		move.w	#(realend-overprog)/4,d0
.mrr3		move.l	(a1)+,(a0)+
		dbra	d0,.mrr3
		move.b	#$0,sprson+2
		st.b	lastworld

		clr.b	doingintro
		clr.b	doingintro2
		clr.b	nom

		move.l	#blobs,loadspace

		tst.w	musicno
		beq	backhere2
	
		move.b	#'1',dno1
		bsr	correctdisk
		clr.b	dir_valid		Load correct sound...
		jsr	initialise
		jsr	startdrv
		lea	mns1,a0		
		move.w	musicno,d0
		mulu	#mns2-mns1,d0
		add.w	d0,a0
		move.l	ms1-mns1(a0),d0
		add.l	#blobs+200,d0
		move.l	d0,loadspace
		lea	blobs,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		jsr	stopdrv
		bra	backhere2

mns1		dc.b	'        ',0,0
ms1		dc.l	0
mns2		dc.b	'rock.mod',0,0
		dc.l	61206
		dc.b	'sonc.mod',0,0
		dc.l	60994
		dc.b	'zool.mod',0,0
		dc.l	60002
		dc.b	'rave.mod',0,0
		dc.l	61800

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

dname		dc.b	'disk'
dno1		dc.b	'1.dsk',0

checkthis	st.b	fic

		move.b	dno1,dno2

cagian		clr.b	dir_valid		Start disk drive.

		jsr	initregs		Initialise ...
		move.w	#$4000,dsklen(a6)
		move.w	#$0020,dmacon(a6)
		move.w	#$8210,dmacon(a6)	
		move.w	#$7f00,adkcon(a6)	
		move.w	#soutsync,dsksync(a6)
		move.b	#$ff,ddrbb(a5)
		move.b	#$03,ddraa(a5)
	 	jsr	initregs		Startdrv...
dsmod5		bset	#4,prbb(a5)
		nop
		bclr	#dskmotor,prbb(a5)
		nop
dsmod6		bclr	#4,prbb(a5)		
		move.w	#40000,d0
.delay01	dbra	d0,.delay01
		moveq	#10,d1
.loop19b	move.w	#$7fff,d0
.loop19		btst	#dskrdy,praa(a5)
	   	beq.s	.ok
		dbra	d0,.loop19
		dbra	d1,.loop19b
		bra	.insert
.ok		bsr	seekzero
		bsr	stopdrv

		jsr	startdrv
		bsr	get_directory
		lea	directory,a1

		moveq	#0,d0			;Slot counter
.find		lea	dname,a0
		move.l	a1,-(sp)
		
.loop		move.b	(a0)+,d1
		bne	.nomatch
		move.l	(sp)+,a1
		move.l	(sp)+,a1		
		rts
.nomatch	cmp.b	(a1)+,d1
		beq	.loop

		move.l	(sp)+,a1

		lea	24(a1),a1		;Next slot

		addq	#1,d0
		cmp.w	#255,d0
		bne	.find

.insert		bsr	stopdrv
		tst.b	fic
		beq	safebye
		clr.b	fic
		bra	cagian



CORRECTDISK	; Check for correct disk ...

		move.b	#dsksel0,dsmod1+3
		move.b	#dsksel0,dsmod2+3
		move.b	#dsksel0,dsmod3+3
		move.b	#dsksel0,dsmod4+3
		move.b	#dsksel0,dsmod5+3
		move.b	#dsksel0,dsmod6+3
		bsr	checkthis

		; Do insert disk bit ...

.insert		move.l	mem,a0
		add.l	#55012,a0
		lea	$6d000,a1
		bsr	dechomp
		move.l	#$6d000,chrplace

		lea	$60000,a1		Blank next plane.
		move.w	#((256*40*5)/4)-1,d0
.moreclr	clr.l	(a1)+
		dbra	d0,.moreclr
	
		move.l	#$60000,wrkplanes
		lea	.insertd1mess,a4		Overlay hiscores.
		jsr	printmess
    		lea	.im2,a4	
		jsr	printmess
    		lea	im3,a4	
		jsr	printmess


		move.w	#$5,$60000+(40*256*5)
		clr.w	$60000+(40*256*5)+32
		move.l	#$03030636,$60000+(40*256*5)+34	Set text pal.
		move.l	#$07470a4c,$60000+(40*256*5)+38
		move.l	#$0c8e0ddd,$60000+(40*256*5)+42
		move.b	#$50,bplam+2
		move.b	#$2c,ws1+2
		move.b	#$2c,ws2+2
		move.b	#$81,startblink+1

		lea	titlebpls,a0
		move.l	#$60000,d1
		move.l	#(40*256),d2
		bsr	setplanes
		move.l	#titlecop,$dff080
		clr.w	$dff088
		lea	titlecols+2,a0
		lea	$60000+(40*256*5),a1
		move.w	#32-1,a4
		jsr	fadeacross
.waitforfire	btst.b	#7,$bfe001
		bne	.waitforfire
		lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		bra	correctdisk

.insertd1mess	dc.w	8,90
		dc.b	'   PLEASE INSERT',$ff
.im2		dc.w	0,110
		dc.b	'       DISK '
dno2		dc.b	'1',$ff
im3		dc.w	0,130
		dc.b	'   AND PRESS FIRE',$ff
fic		dc.b	0
		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

INTROCOP2	; Intro game copper.

		dc.w	$1001,$fffe		Wait a bit for mainline.

		dc.w	$0092,$0030		Data fetch start.
		dc.w	$0094,$00d8		Data fetch stop.
		dc.w	$008e,$4681 $3681		Window start.
		dc.w	$0090,$2cc1		Window stop.		
		dc.w	$0108,$0084		Mods.
		dc.w	$010a,-4		Mods.
		dc.w	$0102,$f0		X shifts.
		dc.w	$0104,$0064		Priorities.

introbpls2	dc.w	$00e0,0			Set top bitplane pointers.
		dc.w	$00e2,0
		dc.w	$00e4,7	
		dc.w	$00e6,0
		dc.w	$00e8,0	
		dc.w	$00ea,0
		dc.w	$00ec,7	
		dc.w	$00ee,(40*48)
		dc.w	$00f0,0
		dc.w	$00f2,0
		dc.w	$00f4,7
		dc.w	$00f6,(40*48*2)

		
		dc.w	$0180,$5	Main game colours.
		dc.w	$0182,$c91
		dc.w	$0184,$193
		dc.w	$0186,$000
		dc.w	$0188,$b00
		dc.w	$018a,$e52
		dc.w	$018c,$050
		dc.w	$018e,$fff
		dc.w	$0190,$000
		dc.w	$0192,$fff
		dc.w	$0194,$ea1
		dc.w	$0196,$35d
		dc.w	$0198,$800
		dc.w	$019a,$b00
		dc.w	$019c,$e70
		dc.w	$019e,$039


		dc.w	$9001,$fffe
		dc.w	$0104,$0024		Priorities.
		dc.w	$0192,$eee
		dc.w	$0194,$f61
		dc.w	$0196,$da0
		dc.w	$0198,$c80
		dc.w	$019a,$c00
		dc.w	$019c,$05b
		dc.w	$019e,$2a1

		dc.w	$ffd7,$fffe,$190,$0,$190,$0,$190,$0,$190,$0,$190,$0
		dc.w	$1801,$fffe
		dc.w	$0192,$fc0
		dc.w	$0194,$d80
		dc.w	$0196,$407
		dc.w	$0198,$940
		dc.w	$019a,$b50
		dc.w	$019c,$b70
		dc.w	$019e,$720

		dc.w	$100,$6400

		dc.w	$ffff,$fffe

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

DOINTROBIT	; Do the zool running bit ...

		lea	introbpls2,a0
		move.l	#overprog+318000,d1
		move.l	d1,d0			Set plane 1 copper bpls.
		move.w	d0,14(a0)
		swap	d0
		move.w	d0,10(a0)
		move.l	d1,d0			Set plane 2 copper bpls.
		add.l	#246*40,d0
		move.w	d0,30(a0)
		swap	d0
		move.w	d0,26(a0)
		move.l	d1,d0			Set plane 3 copper bpls.
		add.l	#246*40*2,d0
		move.w	d0,46(a0)
		swap	d0
		move.w	d0,42(a0)

		move.b	#$40,bplam+2
		clr.w	planmods+2
		clr.w	planmods+6
		clr.w	planmods+10
		clr.l	scrollpos
		move.b	#$40,bplam+2
		st	fromtitle
		jsr	resetvars
		move.l	#blankcop,$dff080
		clr.w	$dff088
		jsr	getsprite
		jsr	getsprite
		jsr	getsprite
	
		move.l	#safebye,inaframe+2

    		; Lets mess up memory ...

		clr.b	dir_valid		Start disk drive.
		jsr	initialise
		jsr	startdrv

		move.b	#'1',zin		Zoolint1.
		lea	zoolintn,a0		
		lea	overprog,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		move.l	a1,a0
		bsr	dechomp

		move.b	#'2',zin		Zoolint2.
		lea	zoolintn,a0		
		lea	overprog+22570,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		move.l	a1,a0
		bsr	dechomp

		move.b	#'3',zin		Zoolint3.
		lea	zoolintn,a0		
		lea	overprog+51790,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		move.l	a1,a0
		bsr	dechomp

		move.b	#'4',zin      		Zoolint4.
		lea	zoolintn,a0		
		lea	overprog+82690,a1
  		moveq  	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		lea	overprog+82690,a1
		move.l	a1,a0
		bsr	dechomp

		lea	zoollogon,a0		Zoollogo.
		lea	overprog+98614,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file

		lea	titlen,a0		Title screen.
		move.l	mem,a1
		add.l	#$50000,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file

		lea	hiscon,a0		Hiscore screen.
		move.l	mem,a1
		add.l	#$60000,a1
  		moveq	#-1,d0
  		moveq	#0,d1
   		jsr	read_file

		lea	zoolphobn,a0		Zoolphob music...
		lea	overprog+107158,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		move.l	a1,a0
		bsr	dechomp

		jsr	stopdrv

		if	protection = 1
		lea	codes,a0
		moveq	#0,d0
		move.w	#((lastc-codes)/4)-1,d1
.more23		move.l	(a0)+,d2
		add.l	d2,d0
		dbra	d1,.more23
		cmp.l	#codesum,d0
		beq	.mdd
  		move.l	#redointro,a0
.mf		move.l	#$f0f0f0f0,(a0)+
		bra	.mf
.mdd		endif


		; PLAY THE TUNE ...

;		bra	.skip 
		move.b	#1,tempocnt
		move.b	#2,temposp	
		moveq	#0,d0		Set up player spec.
		moveq	#0,d1		Interrupt type (not used on selfcall).
		moveq	#0,d2		0=Selfcall/1=AutoCall.
		moveq	#0,d3		0=Pal/1=Ntsc.
		move.b	#1,tempocnt
		jsr	protracker
		move.l	a0,-(sp)
		moveq	#1,d0		Tell protracker where the mod is.
		lea	overprog+107158,a0
		jsr	protracker
		moveq	#4,d0		Set maximum vol.
		moveq	#64,d1		Vol (0-64).
		jsr	protracker
		moveq	#8,d0		Make sure all the cannels are on.
		moveq	#0,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#1,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#2,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#3,d1
		jsr	protracker
		move.l	(sp)+,musicplace Addr to call on interrupt.
.skip
redointro	move.l	#blankcop,$dff080
		jsr	synchup

		btst.b	#7,$bfe001
		beq	redointro

		lea	overprog+98614,a0
		lea	overprog+318000,a1
		bsr	dechomp

	
		lea	introbpls2,a0
		move.l	#overprog+318000,d1
		move.l	d1,d0			Set plane 1 copper bpls.
		move.w	d0,14(a0)
		swap	d0
		move.w	d0,10(a0)
		move.l	d1,d0			Set plane 2 copper bpls.
		add.l	#246*40,d0
		move.w	d0,30(a0)
		swap	d0
		move.w	d0,26(a0)
		move.l	d1,d0			Set plane 3 copper bpls.
		add.l	#246*40*2,d0
		move.w	d0,46(a0)
		swap	d0
		move.w	d0,42(a0)

		move.b	#$40,bplam+2
		clr.w	planmods+2
		clr.w	planmods+6
		clr.w	planmods+10
		clr.l	scrollpos

		move.l	#blankcop,$dff080
		clr.w	$dff088
 		st.b	doingintro2
		st.b	doingintro

		move.b	#$40,starty1+2
      		move.b	#$6f,starty2
		move.w	#$200,scrwx
		move.w	#$200,scrwy
		move.l	#scrplanes,wrkplanes
		move.l	#setplane2,displanes
	
		move.l	firstsprite,a6
		move.w	scrwx,x(a6)
		sub.w	#$60,x(a6)
		move.w	scrwy,y(a6)
		add.w	#$4a,y(a6)
		move.l	#zoolmovement,any(a6)

		move.l	nsp(a6),a6	
		move.w	scrwx,x(a6)
		sub.w	#$40,x(a6)
		move.w	scrwy,y(a6)
		add.w	#$4a,y(a6)

		move.l	nsp(a6),a6	
		move.w	scrwx,x(a6)
		sub.w	#$20,x(a6)
		move.w	scrwy,y(a6)
		add.w	#$4a,y(a6)

       		move.l	#setplane2,a0
		move.l	#scrplanes,a1
		move.l	#$70000,a2
       		move.l	#$5a000,a3
		move.w	#(44*288*4)/4,d0
.morecopy	clr.l	(a0)+
		clr.l	(a1)+
		clr.l	(a2)+
		clr.l	(a3)+
		dbra	d0,.morecopy

		lea	overprog+98614,a0
		lea	overprog+318000,a1
		bsr	dechomp
		
iframe1		jsr	synchup
		move.l	#introcop2,$dff080
		bsr	setworld
		clr.b	frameno
		move.l	wrkplanes,d0		Swap screens ...
		move.l	displanes,wrkplanes
		move.l	d0,displanes
		lea	introbpls2,a0
		move.l	displanes,d1
		move.l	d1,d0
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		move.l	d1,d0			Set plane 2 copper bpls.
		add.l	#44,d0
		move.w	d0,22(a0)
		swap	d0
		move.w	d0,18(a0)
		move.l	d1,d0			Set plane 3 copper bpls.
		add.l	#44*2,d0
		move.w	d0,38(a0)
		swap	d0
		move.w	d0,34(a0)

		bsr	clearblobs
		bsr	placeblobs

		move.l	firstsprite,a6
		move.l	any(a6),a0
		cmp.w	#$8000,(a0)		Out of intro ?
		bne	.movezool
		clr.b	doingintro2
		clr.b	doingintro
		clr.b	nom
		move.w	#$20,d0			Yes ...
.wait		jsr	synchup
		dbra	d0,.wait
		move.l	#blankcop,$dff080
		bsr	synchup
		bra	maintitle

.movezool	move.l	firstsprite,a6
		move.l	nsp(a6),a5
		move.l	nsp(a5),a4
		move.w	(a0)+,d0
		add.w	d0,x(a6)
		add.w	d0,x(a5)
		add.w	d0,x(a4)
		move.w	(a0)+,d0
		add.w	d0,y(a6)
		add.w	d0,y(a5)
		add.w	d0,y(a4)
		move.w	(a0)+,d0
		move.l	#overprog,blbset(a6)
		cmp.w	#4,d0
		bmi	.right
		subq.w	#4,d0
		move.l	#overprog+22570,blbset(a6)
		cmp.w	#4,d0
		bmi	.right
		subq.w	#4,d0
		move.l	#overprog+51790,blbset(a6)
		cmp.w	#4,d0
		bmi	.right
		subq.w	#4,d0
		move.l	#overprog+82690,blbset(a6)
.right		mulu	#3,d0
		move.w	d0,fr(a6)
		addq.b	#1,d0
		move.w	d0,fr(a5)
		addq.b	#1,d0
		move.w	d0,fr(a4)
		move.l	blbset(a6),blbset(a5)
		move.l	blbset(a5),blbset(a4)
		addq.l	#6,any(a6)

		jsr	enternames
		cmp.b	#$40,key
		bne	.jm
		clr.b	nom
    		lea	introcols+2,a0		Turn to black for next.
		lea	blackpal,a1
		move.w	#16-1,a4     
		move.l	#blankcop,$dff080
		bsr	synchup
		bra	gamesetup
.jm		btst.b	#7,$bfe001
		bne	.jm2
		move.l	#blankcop,$dff080
		bsr	synchup
		bra	out2
.jm2
		st.b	nom
		tst.l	musicplace
		beq	.ntune
		move.l	musicplace,a0
		jsr	(a0)
		subq.b	#1,tempocnt
		bpl	.ntune
		move.b	temposp,tempocnt
		move.l	musicplace,a0
		jsr	(a0)
.ntune		      
		bra	iframe1

******************************************************************

maintitle	; MAIN TITLE STUFF.

		st	fromtitle
		st	lastworld+3
		move.b	#$40,bplam+2
		move.b	#$30,ws1+2
		move.b	#$23,ws2+2
	
		move.l	#titlecop,$dff080
		clr.w	$dff088

		lea	introcols+2,a0		Turn to black for next.
		lea	blackpal,a1
		move.w	#16-1,a4     
		bsr	fadeacross
	
		move.l	mem,a0
		add.l	#$50000,a0
		lea	$70000,a1
		bsr	dechomp

		lea	titlebpls,a0
		move.l	#$70000,d1
		move.l	#(40*245),d2
		bsr	setplanes
		move.b	#1,startblink+1
		lea	titlecols+2,a0
		lea	$70000+(40*245*4),a1
		move.w	#32-1,a4
		jsr	fadeacross
		move.w	#$120,d0
		moveq	#0,d1
.waitforfire	jsr	synchup
		bsr	setworld

		pushall
		jsr	enternames
		pullall
		cmp.b	#$40,key
		bne	.jm
    		lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		bra	gamesetup
.jm
		move.b	#1,startblink+1
		subq.b	#1,d1
		andi.b	#$1f,d1
		cmp.w	#$10,d1
		bpl	.nolit
		move.b	#$81,startblink+1
.nolit		jsr	setworld
		btst.b	#7,$bfe001
		beq	outoftitle
		dbra	d0,.waitforfire
    		lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross


******************************************************************

hiscoretable	; HISCORE STUFF.

		move.l	mem,a0
		add.l	#55012,a0
		lea	$60000,a1
		bsr	dechomp
		move.l	#$60000,chrplace

		move.l	mem,a0
		add.l	#$60000,a0
		lea	$70000,a1
		bsr	dechomp
		lea	$70000+(256*40*4),a0	Copy pal.
		lea	$70000+(256*40*5),a1
		move.w	#32-1,d0
.moret		move.w	(a0)+,(a1)+
		dbra	d0,.moret
		lea	$70000+(256*40*4),a1	Blank next plane.
		move.w	#((256*40)/4)-1,d0
.moreclr	clr.l	(a1)+
		dbra	d0,.moreclr

		clr.w	$70000+(40*256*5)+32
		move.l	#$03030636,$70000+(40*256*5)+34	Set text pal.
		move.l	#$07470a4c,$70000+(40*256*5)+38
		move.l	#$0c8e0ddd,$70000+(40*256*5)+42
		move.l	#$0dd00cc0,$70000+(40*256*5)+46
		move.l	#$0bb00aa0,$70000+(40*256*5)+50
		move.l	#$09900770,$70000+(40*256*5)+54
		move.l	#$05500330,$70000+(40*256*5)+58

		; PRINTMES

		move.b	#$50,bplam+2
		move.b	#$2c,ws1+2
		move.b	#$2c,ws2+2
	
		lea	hiscomess,a4		Overlay hiscores.
		move.l	#$70000,wrkplanes
		jsr	printmess
		lea	hiscomess2,a4
		jsr	printmess
		lea	hiscomess3,a4
		jsr	printmess		PRINTMES

		lea	titlebpls,a0
		move.l	#$70000,d1
		move.l	#(40*256),d2
		bsr	setplanes
		move.b	#$81,startblink+1
		lea	titlecols+2,a0
		lea	$70000+(40*256*5),a1
		move.w	#32-1,a4
		jsr	fadeacross
		move.w	#$120,d0
.waitforfire	jsr	synchup
		bsr	setworld
		move.l	d0,-(sp)
		jsr	enternames
		move.l	(sp)+,d0
		cmp.b	#$40,key
		bne	.jm
    		lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		bra	gamesetup
.jm		jsr	setworld
		btst.b	#7,$bfe001
		beq	outoftitle
		dbra	d0,.waitforfire
		lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross

		bra	redointro

outoftitle	lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
out2		clr.l	musicplace
		move.w	#$f,$dff096
		clr.l	ch1timer
		clr.l	forcesample1
		jmp	backhere

nom		dc.b	0
doingintro	dc.b	0
doingintro2	dc.b	0

zoolmovement	dc.w	0,0,0
		dc.w	0,0,0
		dc.w	0,0,0
		dc.w	0,0,0
		dc.w	0,0,0
		dc.w	0,0,0
		dc.w	0,0,0
		dc.w	0,1,0
		dc.w	0,0,0
		dc.w	0,0,0
		dc.w	0,0,0
		dc.w	0,0,0
		dc.w	10,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	6,0,0
		dc.w	5,0,1
		dc.w	5,0,1
		dc.w	5,0,1
		dc.w	5,0,1
		dc.w	5,0,1
		dc.w	5,0,1
		dc.w	4,0,2
		dc.w	4,0,2
		dc.w	4,0,2
		dc.w	4,0,2
		dc.w	4,0,2
		dc.w	4,0,2
		dc.w	3,-5,3
		dc.w	3,-5,3
		dc.w	3,-7,3
		dc.w	3,-7,4
		dc.w	3,-6,4
		dc.w	3,-6,4
		dc.w	3,-6,5
		dc.w	3,-5,5
		dc.w	3,-5,5
		dc.w	3,-5,5
		dc.w	3,-3,6
		dc.w	3,-3,6
		dc.w	3,-3,6
		dc.w	3,-2,7
		dc.w	3,-2,7
		dc.w	3,-2,7
		dc.w	3,-1,8
		dc.w	3,-1,8
		dc.w	3,-1,8
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,9
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
		dc.w	0,-0,10
zoolhere	dc.w	0,-0,11
		dc.w	0,-0,11
		dc.w	0,-0,11
		dc.w	0,-0,11
		dc.w	0,-0,11
		dc.w	0,-0,11
		dc.w	0,-0,11
		dc.w	0,-0,11
		dc.w	0,-0,12
		dc.w	0,-0,12
		dc.w	0,-0,12
		dc.w	0,-0,12
		dc.w	0,-0,12
		dc.w	0,-0,12
		dc.w	0,-0,12
		dc.w	0,-0,12
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	0,-0,13
		dc.w	$8000

chrsn		dc.b	'chrs.chm',0
;zoolphobn	dc.b	'zoolmod.chm',0
zoolphobn	dc.b	'zoolphob.chm',0
hiscon		dc.b	'hisco.chm',0
titlen		dc.b	'title.chm',0
zoollogon	dc.b	'zool8col.chm',0
zoolintn	dc.b	'zoolint'
zin		dc.b	'1.chm',0


******************************************************************
 		even

gsu1		dc.w	8,50
		dc.b	'   GAME SETTINGS',$ff

gsu2a		dc.w	8,100
		dc.b	'  LEVEL ... HARD  ',$ff
gsu2b		dc.w	8,100
		dc.b	'  LEVEL ... NORMAL',$ff
gsu2c		dc.w	8,100
		dc.b	'  LEVEL ... EASY  ',$ff

gsu3a		dc.w	8,120
		dc.b	'  MUSIC ... EFFECTS  ',$ff
gsu3b		dc.w	8,120
		dc.b	'  MUSIC ... ROCK     ',$ff
gsu3c		dc.w	8,120
		dc.b	'  MUSIC ... GREEN    ',$ff
gsu3d		dc.w	8,120
		dc.b	'  MUSIC ... RAVE     ',$ff
gsu3e		dc.w	8,120
		dc.b	'  MUSIC ... FUNK     ',$ff

gsu4a		dc.w	8,140
		dc.b	'INERTIA ... ON',0,$ff
gsu4b		dc.w	8,140
		dc.b	'INERTIA ... OFF',0,$ff

gsu5		dc.w	8,160
		dc.b	'  CONTS ... '
conts		dc.b	'0',$ff

gsu6a		dc.w	8,180
		dc.b	'  SPEED ... NORMAL',0,$ff
gsu6b		dc.w	8,180
		dc.b	'  SPEED ... FAST  ',0,$ff
gs		dc.b	0

diffuculty	dc.w	1
intt		dc.b	0

  		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

GAMESETUP	; HISCORE STUFF.

		clr.b	nom
		move.l	mem,a0
		add.l	#55012,a0
		lea	$7d000,a1
		bsr	dechomp
		move.l	#$7d000,chrplace

		lea	$60000,a0			Blank next plane.
		lea	$70000,a1
		move.w	#((256*40*5)/4)-1,d0
.moreclr	clr.l	(a0)+
		clr.l	(a1)+
		dbra	d0,.moreclr

		; PRINTMES

		move.w	#0,.pointy

		move.l	#$60000,.screen

.waitforfire	
		move.l	.screen,a0
		move.w	#((256*40*5)/4)-1,d0
.moreclr2	clr.l	(a0)+
		dbra	d0,.moreclr2
		move.l	.screen,wrkplanes
		lea	gsu1,a4
		jsr	printmess
		lea	gsu2a,a4	
		move.w	diffuculty,d0
		mulu	#gsu2b-gsu2a,d0
		add.w	d0,a4
		jsr	printmess
		lea	gsu3a,a4	
		move.w	musicno,d0
		mulu	#gsu3b-gsu3a,d0
		add.w	d0,a4
		jsr	printmess
		lea	gsu4a,a4	
		tst.b	intt
		beq	.nor2
    		lea	gsu4b,a4
.nor2		jsr	printmess
		lea	gsu5,a4	
		jsr	printmess
		lea	gsu6a,a4	
		tst.b	gs
		beq	.nor
    		lea	gsu6b,a4
.nor		jsr	printmess

	    	bsr	synchup
		move.w	.screen,d0
		move.w	d0,.high+2
		move.w	d0,.high+10
		move.w	d0,.high+18
		move.w	d0,.high+26
		move.w	d0,.high+34
		cmp.l	#$60000,.screen
		beq	.s
		move.l	#$60000,.screen
		bra	.ss
.s		move.l	#$70000,.screen
.ss

.update		move.w	.pointy,d0
		mulu	#20,d0
		add.b	#$8e,d0
		move.b	d0,.pos1
		add.b	#17,d0
		move.b	d0,.pos2
       
		move.l	#.settingscop,$dff080

.wait		btst.b	#7,$bfe001
		beq	redointro
		tst.l	up			Clear joy.
		bne	.wait

.nox 		btst.b	#7,$bfe001
		beq	redointro
		tst.w	up			Move pointer.
		beq	.noy
		tst.b	up
		bne	.up
		cmp.w	#4,.pointy
		beq	.no
   		addq.w	#1,.pointy
		bra	.no
.up		tst.w	.pointy
		beq	.no
   		subq.w	#1,.pointy
.no		bra	.update
.noy		tst.w	left
		beq	.nox

		cmp.w	#3,.pointy
		bne	.nocont
       		tst.b	left
		beq	.upc
		cmp.b	#'0',conts
		beq	.wait
		subq.b	#1,conts
		bra	.waitforfire
.upc		cmp.b	#'5',conts
		beq	.wait
		addq.b	#1,conts
		bra	.waitforfire
.nocont
		cmp.w	#1,.pointy
		bne	.nomusic
       		tst.b	left
		beq	.upm
		subq.w	#1,musicno
		bpl	.waitforfire
		move.w	#4,musicno
		bra	.waitforfire
.upm		addq.w	#1,musicno
		cmp.w	#5,musicno
		bne	.waitforfire
		clr.w	musicno
		bra	.waitforfire
.nomusic
		cmp.w	#4,.pointy
		bne	.nosp
     		eori.b	#1,gs
.nosp
		cmp.w	#2,.pointy
		bne	.nosp2
     		eori.b	#1,intt
.nosp2

		cmp.w	#0,.pointy
		bne	.nodiff
       		tst.b	left
		bne	.upd
		cmp.w	#0,diffuculty
		beq	.wait
		subq.w	#1,diffuculty
		bra	.waitforfire
.upd		cmp.w	#2,diffuculty
		beq	.wait
		addq.w	#1,diffuculty
		bra	.waitforfire
.nodiff
		bra	.waitforfire

.pointy		dc.w	0
.screen		dc.l	$60000
.settingscop	; Main game copper.

		dc.w	$0180,$0000		Raster colour.
		dc.w	$1001,$fffe		Wait a bit for mainline.
		dc.w	$0092,$0038		Data fetch start.
		dc.w	$0094,$00d0		Data fetch stop.
		dc.w	$008e,$2c81		Window start.
		dc.w	$0090,$2cc1		Window stop.		
		dc.w	$0108,$0000		Mods.
		dc.w	$010a,$0000		Mods.
		dc.w	$0102,$0000		X shifts.
		dc.w	$0104,$0064		Priorities.

.high		dc.w	$00e0,$0007		Set top bitplane pointers.
		dc.w	$00e2,0
		dc.w	$00e4,$0007	
		dc.w	$00e6,(256*40)
		dc.w	$00e8,$0007	
		dc.w	$00ea,(256*40)*2
		dc.w	$00ec,$0007	
		dc.w	$00ee,(256*40)*3
		dc.w	$00f0,$0007
		dc.w	$00f2,(256*40)*4

		dc.w	$0180,$400		Main game colours.
		dc.w	$01a0,$440
		dc.w	$01a2,$770
		dc.w	$01a4,$aa0
		dc.w	$01a6,$dd0
		dc.w	$01a8,$ee0
		dc.w	$01aa,$ff0

		dc.w	$8c01,$fffe
		dc.w	$01a0,$700
		dc.w	$01a2,$900
		dc.w	$01a4,$b00
		dc.w	$01a6,$d00
		dc.w	$01a8,$e00
		dc.w	$01aa,$f00

.pos1		dc.w	$8e01,$fffe
		dc.w	$01a0,$444
		dc.w	$01a2,$777
		dc.w	$01a4,$aaa
		dc.w	$01a6,$ddd
		dc.w	$01a8,$eee
		dc.w	$01aa,$fff

.pos2		dc.w	$9001,$fffe
		dc.w	$01a0,$700
		dc.w	$01a2,$900
		dc.w	$01a4,$b00
		dc.w	$01a6,$d00
		dc.w	$01a8,$e00
		dc.w	$01aa,$f00

		dc.w	$100,$5000
		dc.w	$ffff,$fffe

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

DEFMOV		; Move sprite (a6) acording to -
		; any+0(a6)  = place.
		; any+4(a6)  = loop cnt.
		; any+6(a6)  = wave x pos - pass this point first.
		; any+8(a6)  = speed of any+6(a6).

		tst.w	any+6(a6)		Past the point of no return ?
		beq	.donethat
		move.w	any+8(a6),d0
		sub.w	d0,x(a6)
		move.w	any+6(a6),d0
		cmp.w	x(a6),d0
		bmi	safebye
		clr.w	any+6(a6)
.donethat
		tst.l	any(a6)
		beq	safebye

		move.l	any(a6),a0
		
		cmp.w	#goto,(a0)		Goto command ...
		bne	.notgoto
		move.l	2(a0),any(a6)
		jmp	defmov
.notgoto	
		cmp.w	#repeat,(a0)		Repeat command ...
		bne	.notrepeat
	  	tst.b	any+4(a6)
		beq	.setit
		subq.b	#1,any+4(a6)		Dec loop cnt
		bne	.notpast
		add.l	#8,any(a6)		Finished.
		jmp	defmov
.notpast	move.l	4(a0),any(a6)		Loop back (not fin yet).
		jmp	defmov
.setit		move.b	3(a0),any+4(a6)
		jmp	.notpast
.notrepeat
		move.w	(a0),d1
		add.w	d1,x(a6)
		move.w	2(a0),d1
		add.w	d1,y(a6)
		addq.l	#4,any(a6)

		rts

*----------------------------------------------------------------------------*

INTROCOP	; Intro game copper.

		dc.w	$1001,$fffe		Wait a bit for mainline.

		dc.w	$0092,$0030		Data fetch start.
		dc.w	$0094,$00d8		Data fetch stop.
starty1		dc.w	$008e,$4081		Window start.
		dc.w	$0090,$2cc1		Window stop.		
		dc.w	$0108,$0084		Mods.
		dc.w	$010a,-4		Mods.
		dc.w	$0102,$f0		X shifts.
pri		dc.w	$0104,$0064		Priorities.

introbpls	dc.w	$00e0,0			Set top bitplane pointers.
		dc.w	$00e2,0
		dc.w	$00e4,7	
		dc.w	$00e6,0
		dc.w	$00e8,0	
		dc.w	$00ea,0
		dc.w	$00ec,7	
		dc.w	$00ee,(40*48)
		dc.w	$00f0,0
		dc.w	$00f2,0
		dc.w	$00f4,7
		dc.w	$00f6,(40*48*2)

introcols	dc.w	$0180,$0		Main game colours.
		dc.w	$0182,$0
		dc.w	$0184,$0
		dc.w	$0186,$0
		dc.w	$0188,$0
		dc.w	$018a,$0
eng		dc.w	$018c,$0
		dc.w	$018e,$0
		dc.w	$0190,$0
		dc.w	$0192,$0
		dc.w	$0194,$0
		dc.w	$0196,$0
		dc.w	$0198,$0
		dc.w	$019a,$0
		dc.w	$019c,$0
		dc.w	$019e,$0

starty2		dc.w	$70d7,$fffe,$100,$6400,$100,$640,$100,$6400,$100,$6400,$100,$6400
		dc.w	$2c01,$fffe
		dc.w	$010a,-44		Mods.
		dc.w	$00e6,0
		dc.w	$00ee,0
		dc.w	$00f6,0
		dc.w	$100,$6400
		dc.w	$ffff,$fffe

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

setplanes	move.l	d1,d0			Set plane 1 copper bpls.
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		add.l	d2,d1
		move.l	d1,d0			Set plane 2 copper bpls.
		move.w	d0,14(a0)
		swap	d0
		move.w	d0,10(a0)
		add.l	d2,d1
		move.l	d1,d0	 		Set plane 3 copper bpls.
		move.w	d0,22(a0)
		swap	d0
		move.w	d0,18(a0)
		add.l	d2,d1
		move.l	d1,d0	 		Set plane 4 copper bpls.
		move.w	d0,30(a0)
		swap	d0
		move.w	d0,26(a0)
		add.l	d2,d1
		move.l	d1,d0	 		Set plane 5 copper bpls.
		move.w	d0,38(a0)
		swap	d0
		move.w	d0,34(a0)
		rts	

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

SETWORLD	; Set start through fs and nos.

		tst.b	goldfish
		beq	safebye
		
		move.b	key,d6
		tst.b	d6
		beq	safebye
		move.b	d6,d7
	 	andi.b	#$0f,d6
		andi.b	#$f0,d7
		cmp.b	#$50,d7
		bne	.notworld
		move.b	d6,startw+3
		rts
.notworld	tst.b	d7
		bne	safebye
		subq.b	#1,d6
		move.b	d6,areano+3
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

ENTERNAMES	; Message controller ...

		move.b	key,d0			Get key ...
		btst	#7,d0
		bne	safebye

		tst.w	namepl			Check the first letter ...
		bne	.doin
		lea	ent,a0
.ch2		cmp.b	2(a0),d0
		bne	.nextc
		move.w	#1,(a0)
		st.b	key
		move.w	#1,namepl
.nextc		add.w	#ent2-ent,a0
		cmp.l	#ent3,a0
		bne	.ch2
		rts

.doin		st.b	d7
		lea	ent,a0
		move.w	namepl,d1
		add.w	#2,d1
.ch3		tst.w	(a0)
		beq	.next
		cmp.b	(a0,d1.w),d0
		beq	.check
		cmp.b	-1(a0,d1.w),d0
		beq	.next
      		clr.w	(a0)
		bra	.next
.check		clr.b	d7
		tst.b	1(a0,d1.w)
		bne	.next
		move.l	entn-ent(a0),kmkmd
		cmp.l	#goldfish,kmkmd
		bne	.print
		st.b	goldfish
		bsr	synchup
		move.w	#$4000,d0
.mm		move.w	#$8,$dff180
		dbra	d0,.mm
		clr.b	synched
.wwww		move.w	#$8,$dff180
		tst.b	synched
		beq	.wwww
		bra	.ert
.next  		add.w	#ent2-ent,a0
		cmp.l	#ent3,a0
		bne	.ch3

    		tst.b	d7			Get any ?
		beq	.there
.ert   		clr.w	namepl	
		clr.l	namead
		lea	ent,a0
.more		clr.w	(a0)
		add.w	#ent2-ent,a0
		cmp.l	#ent3,a0
		bne	.more
		rts
.there		add.w	#1,namepl
		st.b	key
		rts

.print		move.l	#blankcop,$dff080

    		lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross

   		clr.w	namepl
		clr.l	namead
		st.b	key

		clr.b	nom
		move.l	mem,a0
		add.l	#55012,a0
		lea	$7d000,a1
		bsr	dechomp
		move.l	#$7d000,chrplace

		lea	$70000,a0
		move.w	#((256*40*5)/4)-1,d0
.moreclr	clr.l	(a0)+
		dbra	d0,.moreclr

		move.l	#$70000,wrkplanes
		move.l	kmkmd,a4
		jsr	printmess
		move.l	#.settingscop,$dff080

.wait		btst.b	#7,$bfe001
		bne	.wait

		move.l	#blankcop,$dff080
		bsr	synchup

		jmp	redointro

.settingscop	; Main game copper.

		dc.w	$0180,$0000		Raster colour.
		dc.w	$1001,$fffe		Wait a bit for mainline.
		dc.w	$0092,$0038		Data fetch start.
		dc.w	$0094,$00d0		Data fetch stop.
		dc.w	$008e,$2c81		Window start.
		dc.w	$0090,$2cc1		Window stop.		
		dc.w	$0108,$0000		Mods.
		dc.w	$010a,$0000		Mods.
		dc.w	$0102,$0000		X shifts.
		dc.w	$0104,$0064		Priorities.

.high		dc.w	$00e0,$0007		Set top bitplane pointers.
		dc.w	$00e2,0
		dc.w	$00e4,$0007	
		dc.w	$00e6,(256*40)
		dc.w	$00e8,$0007	
		dc.w	$00ea,(256*40)*2
		dc.w	$00ec,$0007	
		dc.w	$00ee,(256*40)*3
		dc.w	$00f0,$0007
		dc.w	$00f2,(256*40)*4

		dc.w	$0180,$400		Main game colours.
		dc.w	$01a0,$600
		dc.w	$01a2,$800
		dc.w	$01a4,$a00
		dc.w	$01a6,$c00
		dc.w	$01a8,$e00
		dc.w	$01aa,$f00

		dc.w	$100,$5000
		dc.w	$ffff,$fffe

namead		dc.l	0
namepl		dc.w	0

ent		dc.w	0
		dc.b	$20,$22,$12,0,0,0,0,0,0,0
entn		dc.l	ade

ent2		dc.w	0
		dc.b	$14,$18,$36,$15,0,0,0,0,0,0
		dc.l	tony

		dc.w	0
		dc.b	$24,$12,$18,$13,$24,$12,0,0,0,0
		dc.l	george

		dc.w	0
		dc.b	$24,$13,$12,$24,$24,$21,0,0,0,0
		dc.l	greggs

		dc.w	0
		dc.b	$19,$20,$14,0,0,0,0,0,0,0
		dc.l	pat

		dc.w	0
		dc.b	$21,$17,$31,0,0,0,0,0,0,0
		dc.l	siz

		dc.w	0
		dc.b	$21,$15,$22,0,0,0,0,0,0,0
		dc.l	syd

		dc.w	0
		dc.b	$33,$20,$21,$21,$18,$36,0,0,0,0
		dc.l	casson

		dc.w	0
		dc.b	$21,$25,$18,$13,$14,$17,$12,0,0,0
		dc.l	shortie

		dc.w	0
		dc.b	$22,$18,$36,$27,$17,$36,0,0,0,0
		dc.l	donkin

		dc.w	0
		dc.b	$37,$17,$33,$27,0,0,0,0,0,0
		dc.l	mick

		dc.w	0
		dc.b	$19,$20,$16,$28,0,0,0,0,0,0
		dc.l	paul

		dc.w	0
		dc.b	$13,$17,$14,$33,$25,$17,$12,0,0,0
		dc.l	ritchie

		dc.w	0
		dc.b	$20,$21,$25,0,0,0,0,0,0,0
		dc.l	ash

		dc.w	0
		dc.b	$37,$20,$13,$27,0,0,0,0,0,0
		dc.l	mark

		dc.w	0
		dc.b	$35,$12,$13,$36,$17,0,0,0,0,0
		dc.l	berni

		dc.w	0
		dc.b	$25,$17,$28,$12,$15,0,0,0,0,0
		dc.l	hiley

		dc.w	0
		dc.b	$24,$18,$28,$22,$23,$17,$21,$25,0,0
		dc.l	goldfish
ent3

goldfish	dc.b	0

		even
ade		dc.w	16,50
		dc.b	' A LARGE LO TO MY',0,0
		dc.b	' NATURAL PARENTS',0
		dc.b	'   MY UNNATURAL',0
		dc.b	'   BROTHER AND',0
		dc.b	' THE SMITH POSSE',0,0
		dc.b	'    FROM ADE.',$ff
		even
greggs		dc.w	0,2
		dc.b	'BUY A BIKE ...',0
		dc.b	'      ... NOT A CAR',0,0
		dc.b	'  IM NOT GOING ON ',0
		dc.b	' HOLIDAY THIS YEAR',0,0
		dc.b	'      HI TO ...',0
		dc.b	' SCOTT BOB LEE JOE ',0
		dc.b	' ME CHEESE HYDROGEN',0
		dc.b	' HELIUM ...',0,$ff
		even
berni		dc.w	0,48
		dc.b	'  TO ALL YOU GUYS',0
		dc.b	'  WITH THE V.W.BUG',0,0
		dc.b	'    KEEP BUGGIN ',0,0
		dc.b	'   FROM BERNI AND',0
		dc.b	' COOKIE ... MY BUG.',0,$ff
		even
ash		dc.w	0,108
		dc.b	'      NOTHING',$ff
		even
mark		dc.w	0,8
		dc.b	'    HELLO THERE',0,0
		dc.b	'WHAT CODE DO I HAVE',0
		dc.b	'TO TYPE IN FOR THIS',0,0
		dc.b	'HA HA HA ... NO ...',0
		dc.b	'     HMMMMMMMM ',0,0
		dc.b	'  I WILL THINK OF',0
		dc.b	'  SOMETHING LATER',0,0
		dc.b	'   HELLO TO KEV.',$ff
		even
george		dc.w	0,70
		dc.b	'  HELLO TO GORDON ',0
		dc.b	'DAVE ANDREW CHARLIE',0
		dc.b	' ALAN ALISTER AND',0
		dc.b	' EVERYONE ELSE IN',0
		dc.b	'    ABERDEEN...',$ff
		even
hiley		dc.w	8,25
		dc.b	'    HI TO ARREN',0
		dc.b	'     AND DANDY',0,0
		dc.b	'     REMEMBER.',0,0
		dc.b	'  I USED TO BE A',0
		dc.b	'  BARCODE READER',0
		dc.b	'  BUT IM ALRIGHT',0,0
		dc.b	'       BEEP',$ff
		even
ritchie		dc.w	55,0
		dc.b	'LOVE TO JENNIE FROM',0
		dc.b	'RITCHIE',$ff
		even
paul		dc.w	0,2
		dc.b	'.ASTRAGARDS HELLOS.',0
		dc.b	'TARZANBOY STEPHENS',0
		dc.b	'WOLFGANG MARRISON',0
		dc.b	'TEA ANYONE MARRISON',0
		dc.b	'     KAZ HOARE',0
		dc.b	'   SWELTER VERNON',0
		dc.b	'  INVASION NORMAN',0
		dc.b	'  SKULKING GREYSON',0
		dc.b	'  19 HITS GREYSON',0
		dc.b	'  CHARCHAY CHEERS',0
		dc.b	'  RIK DICK DARWIN',0
		dc.b	'   DEZ THE DICE',0
		dc.b	'   DOG FRANKLIN',$ff
		even
mick		dc.w	0,0
		dc.b	0,0,0,'THUD ... OH NO CHRIS',0
		dc.b	' STRIKES AGAIN ...',0,0
		dc.b	'HI TO HIRSTY.LEE AND',0
		dc.b	'ANN.PRIMUS.WEEG AND ',0
		dc.b	'D.C. CLASS OF 92 ...',$ff
		even
donkin		dc.w	0,0
		dc.b	'GREETS FROM MATTHEW ',0,0
		dc.b	'TRACIE AND KARL ... ',0
		dc.b	'LOVE YOU BOTH VERY  ',0
		dc.b	'MUCH ...',0,0
		dc.b	'RUSSELL.YVONNE.JOHN',0
		dc.b	'THE TWO PAULS.CYRIL',0
		dc.b	'PAULA.DARREN WHOS HE',0
		dc.b	'MUST HAVE FORGOT ...',0
		dc.b	'TREV AND JANET TENAJ',0,$ff
		even
tony		dc.w	0,70
		dc.b	'      GINSTER',0
		dc.b	'      GENSTERS',0
		dc.b	'      GEASTER',0
		dc.b	'      GINDERES',0,$ff
		even
pat		dc.w	0,0
		dc.b	0,'HELLO TO...DAVE.PAT.',0
		dc.b	'BRIAN.ANDREW.SHEPTHE',0
		dc.b	'DOG.STEVE.MILES.TIE.',0
		dc.b	'TIM.RICHARD.RUSSEL. ',0
		dc.b	'EVERYONE AT PCI',0
		dc.b	'CHUCK THE SNOWBALL.',0
		dc.b	'THE PEEPS IN TEST',0
		dc.b	'AND LASTLY TO MY',0
		dc.b	'DARLINK CHRISTINE',0
		dc.b	'XXXXXXX XXXXXXXXX',0
		dc.b	'CHEERS ... PATRICK',$ff
		even
siz		dc.w	0,100
		dc.b	'   MAKE MINE J.D.',$ff
		even
syd		dc.w	0,2
		dc.b	'   TO ORIGINAL POSSE',0
		dc.b	'WEVE ALL DRIFTED',0
		dc.b	'OUR OWN PATHS NOW.',0
		dc.b	'NOT ALL OF THEM',0
		dc.b	'POSITIVE BUT HOPFULY',0
		dc.b	'PROGRESSIVE.',0
		dc.b	'    SPECIAL MENTIONS',0
		dc.b	'FAMILY. ALWAYS THERE',0
		dc.b	'NODRUBHARAS.YOU KNOW',0
		dc.b	'LOUIS. GOOD LUCK',0
		dc.b	'BRYAN. MARTIN. STAN.',0
		dc.b	'BUISNESS AS USUAL',0
		dc.b	'PAUL. STOP SMOKING.',$ff
		even
casson		dc.w	0,2
		dc.b	'JUST A QUICK MENTION',0
		dc.b	'TO ALL THE PEOPLE I ',0
		dc.b	'KNOW...',0,0
		dc.b	' HELLOS TO ALISON',0
		dc.b	'   MACHO MAN',0
		dc.b	'   STEVEY B...',0
		dc.b	'   JAMES YEAH',0
		dc.b	' THE UNDERTAKER ...',0,0
		dc.b	'AND GOODBYE FROM ME',0,$ff
		even
shortie		dc.w	0,50
		dc.b	'  HELLO TO MADONNA ',0
		dc.b	'  LOUISE VERONICA ',0
		dc.b	'CICCONE .THE WORLDS',0
		dc.b	'NUMBER 1 SEX GODDESS',0,0
		dc.b	'      SIMON ...',0,$ff

kmkmd		dc.l	0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

GAMEIRQ		movem.l	d0-d7/a0-a6,-(sp)	Save registors.
		move	sr,-(sp)
		move.w	($dff01e).l,d0		Vertical yet ?
		btst	#$05,d0
		beq	restoreregs

		tst.b	sprson+2
		bne	.here
    		move.l	#blankspr,$dff120
    		move.l	#blankspr,$dff124
    		move.l	#blankspr,$dff128
    		move.l	#blankspr,$dff12c
    		move.l	#blankspr,$dff130
    		move.l	#blankspr,$dff134
.here		move.l	#blankspr,$dff138
  		move.l	#blankspr,$dff13c

inaframe     	jsr	bye

		jsr	joytest

		if	makedisk=0
		trap	#0
		endif

    		moveq	#0,d0
 		move.b	arrowfr,d0
  		add.w	d0,d0
   		add.l	#arrows2+(16*2),d0
    		move.l	d0,a0
     		move.l	#botscolines+22,a5
      		moveq	#12-1,d0
.me2   		move.w	(a0),(a5)
		clr.w	4(a5)
	 	add.w	#16,a0
	  	add.w	#bl2-botscolines,a5
	   	dbra	d0,.me2

		move.b	#1,synched
	    
		tst.b	nom
		bne	.ntune
		tst.l	musicplace
		beq	.ntune
		move.l	musicplace,a0
		jsr	(a0)
		subq.b	#1,tempocnt
		bpl	.ntune
		move.b	temposp,tempocnt
		move.l	musicplace,a0
		jsr	(a0)
.ntune		

restoreregs	move	(sp)+,sr		
		movem.l	(sp)+,d0-d7/a0-a6	
		move.w	 #$0070,$dff09c
		rte

codes		dc.b	1,1,'25','57 '
code2		dc.b	1,2,'13','83 '
		dc.b	1,3,'46','70 '
		dc.b	1,4,'26','53 '
		dc.b	1,5,'31','39 '
		dc.b	1,6,'7 ','29 '
		dc.b	1,7,'10','35 '
		dc.b	1,8,'45','65 '
		dc.b	1,9,'29','51 '
		dc.b	1,10,'14','104'
		dc.b	1,11,'35','R  '
		dc.b	1,12,'32','52 '

		dc.b	2,1,'13','77 '
		dc.b	2,2,'2 ','29 '
		dc.b	2,3,'4 ','32 '
		dc.b	2,4,'37','N  '
		dc.b	2,5,'29','37 '
		dc.b	2,6,'5 ','26 '
		dc.b	2,7,'34','B  '
		dc.b	2,8,'44','67 '
		dc.b	2,9,'30','56 '
		dc.b	2,10,'31','55 '
		dc.b	2,11,'33','O  '
		dc.b	2,12,'31','51 '

		dc.b	3,1,'43','L  '
		dc.b	3,2,'1 ','28 '
		dc.b	3,3,'28','41 '
		dc.b	3,4,'4 ','32 '
		dc.b	3,5,'32','46 '
		dc.b	3,6,'34','F  '
		dc.b	3,7,'19','106'
		dc.b	3,8,'22','99 '
		dc.b	3,9,'25','45 '
		dc.b	3,10,'27','49 '
		dc.b	3,11,'27','47 '
		dc.b	3,12,'30','52 '

		dc.b	4,1,'30','52 '
		dc.b	4,2,'28','45 '
		dc.b	4,3,'17','99 '
		dc.b	4,4,'32','50 '
		dc.b	4,5,'35','L  '
		dc.b	4,6,'9 ','8  '
		dc.b	4,7,'19','103'
		dc.b	4,8,'9 ','2  '
		dc.b	4,9,'24','45 '
		dc.b	4,10,'26','47 '
		dc.b	4,11,'16','91 '
		dc.b	4,12,'48','71 '

		dc.b	5,1,'33','Q  '
		dc.b	5,2,'38','B  '
		dc.b	5,3,'32','54 '
		dc.b	5,4,'48','62 '
		dc.b	5,5,'41','B  '
		dc.b	5,6,'42','A  '
		dc.b	5,7,'35','J  '
		dc.b	5,8,'47','68 '
		dc.b	5,9,'41','R  '
		dc.b	5,10,'44','66 '
		dc.b	5,11,'38','H  '
		dc.b	5,12,'45','65 '

		dc.b	6,1,'40','J  '
		dc.b	6,2,'32','58 '
		dc.b	6,3,'44','70 '
		dc.b	6,4,'39','B  '
		dc.b	6,5,'33','K  '
		dc.b	6,6,'19','94 '
		dc.b	6,7,'22','87 '
		dc.b	6,8,'20','95 '
		dc.b	6,9,'8 ','1  '
		dc.b	6,10,'12','12 '
		dc.b	6,11,'2 ','14 '
		dc.b	6,12,'29','55 '

		dc.b	7,1,'45','64 '
		dc.b	7,2,'27','49 '
		dc.b	7,3,'40','H  '
		dc.b	7,4,'2 ','2  '
		dc.b	7,5,'25','37 '
		dc.b	7,6,'6 ','9  '
		dc.b	7,7,'30','46 '
		dc.b	7,8,'21','90 '
		dc.b	7,9,'35','J  '
		dc.b	7,10,'43','B  '
		dc.b	7,11,'1 ','13 '
		dc.b	7,12,'46','61 '
	
		dc.b	8,1,'14','92 '
		dc.b	8,2,'5 ','20 '
		dc.b	8,3,'30','56 '
		dc.b	8,4,'33','Q  '
		dc.b	8,5,'48','72 '
		dc.b	8,6,'42','G  '
		dc.b	8,7,'43','J  '
		dc.b	8,8,'27','39 '
		dc.b	8,9,'29','41 '
		dc.b	8,10,'47','67 '
		dc.b	8,11,'23','47 '
		dc.b	8,12,'12','12 '

		dc.b	9,1,'2 ','17 '
		dc.b	9,2,'7 ','29 '
		dc.b	9,3,'7 ','26 '
		dc.b	9,4,'43','R  '
		dc.b	9,5,'29','51 '
		dc.b	9,6,'6 ','15 '
		dc.b	9,7,'40','D  '
		dc.b	9,8,'48','62 '
		dc.b	9,9,'26','59 '
		dc.b	9,10,'29','41 '
		dc.b	9,11,'2 ','23 '
		dc.b	9,12,'38','N  '
	
		dc.b	10,1,'13','89 '
		dc.b	10,2,'25','49 '
		dc.b	10,3,'17','81 '
		dc.b	10,4,'34','X  '
		dc.b	10,5,'12','3  '
		dc.b	10,6,'35','V  '
		dc.b	10,7,'34','R  '
		dc.b	10,8,'21','81 '
		dc.b	10,9,'44','72 '
		dc.b	10,10,'26','59 '
		dc.b	10,11,'10','14 '
		dc.b	10,12,'1 ','19 '
	
		dc.b	11,1,'3 ','27 '
		dc.b	11,2,'14','86 '
		dc.b	11,3,'2 ','17 '
		dc.b	11,4,'19','73 '
		dc.b	11,5,'7 ','26 '
		dc.b	11,6,'32','60 '
		dc.b	11,7,'45','66 '
		dc.b	11,8,'48','72 '
		dc.b	11,9,'2 ','35 '
		dc.b	11,10,'6 ','9  '
		dc.b	11,11,'17','102'
		dc.b	11,12,'35','L  '
	
		dc.b	12,1,'1 ','22 '
		dc.b	12,2,'19','100'
		dc.b	12,3,'45','61 '
		dc.b	12,4,'33','A  '
		dc.b	12,5,'27','53 '
		dc.b	12,6,'34','X  '
		dc.b	12,7,'12','3  '
		dc.b	12,8,'48','71 '
		dc.b	12,9,'33','O  '
		dc.b	12,10,'22','78 '
		dc.b	12,11,'21','84 '
		dc.b	12,12,'41','B  '
	
		dc.b	1,1,'2 ','29 '
		dc.b	1,3,'34','H  '
		dc.b	1,5,'21','99 '
		dc.b	1,7,'26','47 '
		dc.b	1,9,'45','66 '
		dc.b	1,11,'9 ','20 '

		dc.b	2,2,'43','H  '
		dc.b	2,4,'12','18 '
		dc.b	2,6,'44','65 '
		dc.b	2,8,'8 ','28 '
		dc.b	2,10,'4 ','11 '
		dc.b	2,12,'2 ','35 '

		dc.b	3,1,'7 ','14 '
		dc.b	3,3,'2 ','29 '
		dc.b	3,5,'4 ','29 '
		dc.b	3,7,'34','D  '
		dc.b	3,9,'12','6  '
		dc.b	3,11,'48','71 '

		dc.b	4,2,'1 ','31 '
		dc.b	4,4,'3 ','33 '
		dc.b	4,6,'2 ','23 '
		dc.b	4,8,'22','96 '
		dc.b	4,10,'33','U  '
		dc.b	4,12,'44','69 '

		dc.b	5,1,'7 ','20 '
		dc.b	5,3,'17','96 '
		dc.b	5,5,'13','80 '
		dc.b	5,7,'27','59 '
		dc.b	5,9,'43','X  '
		dc.b	5,11,'21','105'

		dc.b	6,2,'48','71 '
		dc.b	6,4,'46','66 '
		dc.b	6,6,'12','24 '
		dc.b	6,8,'43','D  '
		dc.b	6,10,'25','49 '
		dc.b	6,12,'14','95 '

		dc.b	7,1,'9 ','32 '
		dc.b	7,3,'1 ','1  '
		dc.b	7,5,'48','61 '
		dc.b	7,7,'21','87 '
		dc.b	7,9,'32','46 '
		dc.b	7,11,'33','A  '

		dc.b	8,2,'33','U  '
		dc.b	8,4,'22','72 '
		dc.b	8,6,'11','25 '
		dc.b	8,8,'44','61 '
		dc.b	8,10,'32','46 '
		dc.b	8,12,'22','42 '

		dc.b	9,1,'45','62 '
		dc.b	9,3,'32','38 '
		dc.b	9,5,'48','71 '
		dc.b	9,7,'1 ','31 '
		dc.b	9,9,'2 ','29 '
		dc.b	9,11,'3 ','27 '

		dc.b	10,2,'5 ','26 '
		dc.b	10,4,'5 ','20 '
		dc.b	10,6,'7 ','20 '
		dc.b	10,8,'2 ','35 '
		dc.b	10,10,'10','17 '
		dc.b	10,12,'11','13 '

		dc.b	11,1,'12','18 '
		dc.b	11,3,'13','92 '
		dc.b	11,5,'14','95 '
		dc.b	11,7,'15','95 '
		dc.b	11,9,'16','100'
		dc.b	11,11,'17','102'

		dc.b	12,1,'20','92 '
		dc.b	12,3,'30','40 '
		dc.b	12,5,'40','N  '
		dc.b	12,7,'19','79 '
		dc.b	12,9,'29','49 '
lastc		dc.b	12,11,'39','X  '

		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

SYNCHUP		; Wait for the top of the frame.

		clr.b	synched
.wait		tst.b	synched
		beq	.wait
safebye		rts

synched		dc.b	0

		even

******************************************************************************
*						                             *
*      	                    BLOB PLACING ROUTINES	                     *
*						                             *
******************************************************************************

WAITBLIT	tst.b	$dff002
		btst.b	#6,$dff002
		bne.s	.here
		rts
.here		tst.b	$bfe001
		tst.b	$bfe001
		btst.b	#6,$dff002
		bne	.here
		tst.b	$dff002
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PRINTBLOB	; Blit an object on

		cmp.w	#$20,x(a0)
		bpl	esafebye
		rts
esafebye 
		tst.b	ht(a0)
		beq	safebye

		cmp.b	#$ff,fr+1(a0)
		beq	safebye

		move.w	#$fca,blitmode+2

		move.w	#$ffff,fwm+2
		move.w	#3,blitwide+2
		move.w	#-2,blitmod3+2
		move.w	#-2,blitmod4+2
		
		move.l	blbset(a0),a1
		add.l	blbwides(a1),a1
		add.w	fr(a0),a1
		tst.b	(a1)
		beq	.16wide
		move.w	#$e54b,lslcon
		move.w	#38+(44*3),blitmod1+2
		move.w	#38+(44*3),blitmod2+2
		move.w	#3,blitwide+2
		move.w	#$e548,lsl1
		move.w	#$e548,lsl2
		jmp	d32wide
.16wide		move.w	#$e34b,lslcon
bltmodw		move.w	#40+(44*3),blitmod1+2
bltmodw2	move.w	#40+(44*3),blitmod2+2
		move.w	#2,blitwide+2
		move.w	#$e348,lsl1
		move.w	#$e348,lsl2
d32wide
		moveq	#0,d6
		move.w	x(a0),d6
		add.w	#$160,d6
		sub.w	scrwx,d6		Do x stuff.
		move.b	scrx,d0
		andi.w	#$f,d0
		eori.b	#$f,d0
		add.w	d0,d6
		
		cmp.l	#$150,d6		If off screen dont blit.
		bls	safebye
		cmp.l	#$160,d6		If off screen dont blit.
		bpl	.notkillmask
		clr.w	fwm+2
.notkillmask	
		cmp.l	#$2c0,d6		If off screen dont blit.
		bpl	safebye
		cmp.l	#$2b0,d6
		bmi	.notmaskoff
		move.l	blbset(a0),a1
		add.l	blbwides(a1),a1
		add.w	fr(a0),a1
		tst.b	(a1)
		beq	.notmaskoff
		move.w	#2,blitwide+2
		move.w	#40+(44*3),blitmod1+2
		move.w	#40+(44*3),blitmod2+2
		clr.w	blitmod3+2
		clr.w	blitmod4+2
.notmaskoff		
		move.l	d6,d7			Calculate x stuff.
		lsr	#3,d7
		bclr	#0,d7
		add.l	scrollpos,d7		d7 = screen offset (bytes).
		andi.w	#$f,d6		
		lsl	#8,d6		
		lsl	#4,d6			d6 = offset (pixels).
		add.w	#44*3,d7		

		move.w	y(a0),d4		d4 = starting world y loc.
		move.w	y(a0),d5
		move.l	blbset(a0),a3
		add.l	blbyoffs(a3),a3
		move.w	fr(a0),d0
		moveq	#0,d3
		move.b	(a3,d0.w),d3
		add.w	d3,d4
		add.w	d3,d5			d5 = end world y 
		
		cmp.b	#$ff,ht(a0)
		beq	.okay
		moveq	#0,d3
		move.b	ht(a0),d3
		jmp	.addit
.okay		move.l	blbset(a0),a3
		add.l	blbheights(a3),a3
		move.w	fr(a0),d0
		moveq	#0,d3
		move.b	(a3,d0.w),d3
.addit		add.w	d3,d5			d5 = end world y loc.

		; On screen ?

		cmp.w	scrwy,d5
		ble	safebye
		move.w	scrwy,d3
		add.w	#278,d3			d3 = bottom scrwy.
		cmp.w	d4,d3
		bls	safebye

		; Clip top and bottom.

		clr.l	addthis			Clip top.
		cmp.w	scrwy,d4
		bpl	notofftop
		move.w	scrwy,d0
		sub.w	d4,d0
lsl1		lsl.w	#1,d0
		andi.l	#$ffff,d0
		move.l	d0,addthis
		move.w	scrwy,d4
notofftop	cmp.w	d3,d5			Clip bottom.
		bls	.notoffbottom
		move.w	d3,d5
.notoffbottom
		; d4,d5 = screen y locs from 0.

		sub.w	scrwy,d4
		sub.w	scrwy,d5

		cmp.b	#105,t(a0)
		beq	.thisone
		move.l	acme,d3
		cmp.l	blbset(a0),d3
		beq	.thisone
		cmp.b	#75,t(a0)
		bne	.noea
.thisone	move.l	any(a0),d3
		add.l	d3,addthis
.noea

		; d4 = y start : d5 = y stop.
     
		move.w	scry,d3
		cmp.w	d3,d4			Catch totaly below ?
		bmi	notbelow
.blitbothalf	move.w	d4,d2
		subq.w	#1,d2
		sub.w	d3,d2
mul1		mulu	#44*4,d2
		add.w	d2,d7
		jmp	contblit
notbelow
		cmp.w	d5,d3			Catch totaly above ?
		ble	notabove
.blittophalf	move.w	d4,d2
		add.w	#$11f,d2
		sub.w	d3,d2
mul2		mulu	#44*4,d2
		add.w	d2,d7
		jmp	contblit
notabove	
		movem.l	d0-d7/a0-a5,-(sp)	Slice it.
		move.w	d4,d2
		add.w	#$11f,d2
		sub.w	d3,d2
mul3		mulu	#44*4,d2
		add.w	d2,d7
		move.w	d3,d5
		jsr	contblit
		movem.l	(sp)+,d0-d7/a0-a5
		move.w	d3,d0
		sub.w	d4,d0
lsl2		lsl.w	#1,d0
		andi.l	#$ffff,d0
		add.l	d0,addthis
		move.w	d3,d4
		sub.w	#44*4,d7

		; Blit it.

contblit	move.w	d5,d0			Calc blit size.
		sub.w	d4,d0
		tst.w	d0
		beq	safebye
		move.w	d0,d3
		lsl.w	#8,d3
		lsl.w	#6,d0
blitwide	add.w	#3,d0
		add.w	blitwide+2,d3
		move.w	d0,blitsize+2

		subq.w	#2,d7
		move.w	d3,size(a6)		Save blit size.
		move.l	d7,scroff(a6)		Save address in clear list.
		move.l	d7,d0
		add.l	wrkplanes,d0
		move.l	d0,scrandoff(a6)
		move.w	blitmod1+2,blitsm(a6)
		add.l	#resize,a6
		clr.l	(a6)
.no
		move.l	blbset(a0),a2
		add.l	blbheights(a2),a2
		add.w	fr(a0),a2
		moveq	#0,d3
		move.b	(a2),d3
lslcon		lsl.w	#2,d3
		move.l	d3,a2			a2 = place size.

		moveq	#0,d3
		move.w	fr(a0),d3
		lsl.w	#2,d3
		move.l	blbset(a0),a1
		add.l	blbplaces(a1),a1
		move.l	(a1,d3.w),d3

		cmp.b	#$78,htg(a0)
		bne	.nosame
		clr.l	addthis
		clr.l	d3
		move.w	#-4,blitmod3+2
		move.w	#-4,blitmod4+2
.nosame
		move.l	blbset(a0),d0		Set mask address.
		add.l	a2,d0
		add.l	a2,d0
		add.l	a2,d0
		add.l	a2,d0
		add.l	addthis,d0
		add.l	d3,d0
		
		move.l	blbset(a0),d1		Set image address.
		add.l	addthis,d1
		add.l	d3,d1
		
		move.l	wrkplanes,d2		Set destination address.
		add.l	d7,d2

fwm		move.w	#$0000,$dff044		FWM
		move.w	#$0000,$dff046		LWM		
blitmod3	move.w	#-2,$dff062
blitmod4	move.w	#-2,$dff064
blitmode	move.w	#$fca,d3
		add.w	d6,d3
		move.w	d3,$dff040		set minterm
		move.w	d6,$dff042
blitmod1	move.w	#38,$dff060		bltmod C
blitmod2	move.w	#38,$dff066		bltmod D

		lea	flashpls,a4
		cmp.b	#sln5,worldno+3
		bne	.kmf
		lea	flashpls2,a4
.kmf
		move.w	#4-1,d5

		move.l	d4,-(sp)
		
morepbplanes	move.l	d0,$dff050		Mask adress.

		move.w	blitmod3+2,$dff062
		tst.b	flal(a0)
		beq	.noflash
		move.l	d0,$dff04c		Image address.
		tst.b	(a4,d5.w)
		beq	.flash
		move.l	#blankbit,$dff04c
		move.w	#-6,$dff062
		cmp.w	#38,blitmod1+2
		beq	.flash
		move.w	#-4,$dff062
		jmp	.flash

.noflash	move.l	d1,$dff04c		Image address.
.flash		move.l	d2,$dff048		Destination address.
		move.l	d2,$dff054		Destination address.

		bset.b	#6,$dff002
		move.w	#$8400,$dff096		Our DMA on.
blitsize	move.w	#$803,$dff058		size and triger
		nop
		nop
		nop
		jsr	waitblit

wideblt		add.l	#44,d2			Used to be planesize.
		add.l	a2,d1

		dbra	d5,morepbplanes

		move.l	(sp)+,d4

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PLACEBLOBS	lea	clearlist1,a6
		cmp.l	#setplane2,displanes
		beq	.notother
		lea	clearlist2,a6
.notother	clr.l	(a6)

		cmp.l	#$ffffffff,firstsprite	Any sprites ?
		beq	safebye

		move.l	firstsprite,a0
.nextsprite	
		tst.b	doingintro2
		bne	.alreadydone
		move.l	blbset(a0),d0		Set real blbset for this -
		cmp.l	#blobs2,d0		alien if not already set.
		bpl	.alreadydone
		move.l	blbset(a0),a1
		move.l	(a1),blbset(a0)
.alreadydone
		jsr	printblob

		tst.b	flal(a0)		Clear alien flash.
		beq	.noflash4
		subq.b	#1,flal(a0)
.noflash4
		cmp.l	#$ffffffff,nsp(a0)
		beq	safebye
		move.l	nsp(a0),a0

		jmp	.nextsprite

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*

CLEARBLOBS	; Blit an object on

		lea	clearlist1,a6
		cmp.l	#setplane2,displanes
		beq	.notother
		lea	clearlist2,a6
.notother	tst.l	(a6)
		beq	safebye
		jsr	.clearit
		add.l	#resize,a6
		jmp	.notother

.clearit	move.l	#scrplanes,d4		set source C
		tst.b	doingintro
		bne	.mn
		add.l	scroff(a6),d4
.mn		move.l	scrandoff(a6),d3	set source C

		move.w	blitsm(a6),d0
		sub.w	#44*3,d0
		move.w	d0,$dff064	        Source A mod.

		move.w	blitsm(a6),d0
		sub.w	#44*3,d0
		move.w	d0,$dff066	        Destination mod.
		
		move.w	#$ffff,$dff044	        First word mask.
		move.w	#$ffff,$dff046	        Last word mask.
		move.w	#38,$dff060		?
		move.w	#38,$dff062		?
		move.w	#$9f0,$dff040	        Control reg 1.
		move.w	#$000,$dff042	        Control reg 2.
		move.l	#0,$dff048		?
		move.l	#0,$dff04c		?
		move.l	d4,$dff050	        Set source A.
		move.l	d3,$dff054	        Set destination.
		bset.b	#6,$dff002
		move.w	#$8400,$dff096		Our DMA on.

		move.w	size(a6),$dff058	        Size and start.
		nop
		nop
		nop
		jsr	waitblit
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PRINTCHR	; Blit an object on
		
		cmp.b	#$ff,fr+1(a0)
		beq	safebye

		moveq	#0,d6

		move.w	x(a0),d6
	
		move.l	d6,d7			Calculate x stuff.
		lsr	#3,d7
		bclr	#0,d7
		andi.w	#$f,d6
		lsl	#8,d6		
		lsl	#4,d6			d6 = offset (pixels).

		; Y stuff.
	
		move.w	y(a0),d4		d4 = starting world y loc.
		move.w	y(a0),d5
		add.w	#16,d5

		; d4 = y start : d5 = y stop.
		
		move.w	d4,d2			Calc screen offset (d7).
		subq.w	#1,d2
		mulu	#40,d2
		add.w	d2,d7

		move.w	d5,d0			Calc blit size.
		sub.w	d4,d0
		lsl.w	#6,d0
		add.w	#2,d0			Blit wide.
		move.w	d0,.blitsize+2

		move.l	#16*2,a2		a2 = place size.
	
		moveq	#0,d3
		move.w	fr(a0),d3
		lsl.w	#2,d3
		move.l	blbset(a0),a1
		add.l	blbplaces(a1),a1
		move.l	(a1,d3.w),d3

		move.l	blbset(a0),d0		Set mask address.
		add.l	a2,d0
		add.l	a2,d0
		add.l	a2,d0
		add.l	a2,d0

		add.l	d3,d0
		
		move.l	blbset(a0),d1		Set image address.
		add.l	d3,d1
		
		move.l	wrkplanes,d2	Set destination address.
		add.l	d7,d2

		move.w	#$ffff,$dff044		FWM
		move.w	#$0000,$dff046		LWM		
		move.w	#-2,$dff062
		move.w	#-2,$dff064
		move.w	#$fca,d3
		add.w	d6,d3
		move.w	d3,$dff040		set minterm
		move.w	d6,$dff042
.blitmod1	move.w	#36,$dff060		bltmod C
.blitmod2	move.w	#36,$dff066		bltmod D

		move.w	#5-1,d5
		
.morepbplanes	move.l	d0,$dff050		Mask adress.

		move.w	#-2,$dff062
		move.l	d1,$dff04c		Image address.
		move.l	d2,$dff048		Destination address.
		move.l	d2,$dff054		Destination address.
		
		bset.b	#6,$dff002
		move.w	#$8400,$dff096		Our DMA on.
.blitsize	move.w	#$802,$dff058		size and triger
		nop
		nop
		nop
		jsr	waitblit
		
		add.l	#256*40,d2

		add.l	a2,d1
		cmp.b	#1,d5
		bne	.n
  		move.l	d0,d1
.n
		dbra	d5,.morepbplanes
		
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PRINTMESS	; Still can ...

		clr.l	scrwx

		lea	chrblb,a0
		move.l	chrplace,blbset(a0)
		move.l	(a4)+,x(a0)
		move.l	x(a0),any(a0)

.next		move.b	(a4)+,d3

		cmp.b	#$ff,d3
		beq	safebye

		tst.b	d3
		bne	.nonextline
		add.w	#20,any+2(a0)
	   	move.l	any(a0),x(a0)
		jmp	.next
.nonextline	
		cmp.b	#' ',d3
		bne	.nospace
		add.w	#16,x(a0)
		jmp	.next
.nospace
		cmp.b	#'+',d3
		bne	.nosmiley
		move.b	#38,d3	 
		jmp	.doit
.nosmiley
		cmp.b	#'=',d3
		bne	.noarrow
		move.b	#37,d3	 
		jmp	.doit
.noarrow
		cmp.b	#'.',d3
		bne	.nodot
		move.b	#36,d3	 
		jmp	.doit
.nodot
		cmp.b	#'0',d3
		bmi	.nonum
		cmp.b	#'9',d3
		bgt	.nonum
		sub.w	#'0'-26,d3
		jmp	.doit
.nonum		cmp.b	#'A',d3
		bmi	.nochr
		cmp.b	#'Z',d3
		bgt	.nochr
		sub.w	#'A',d3
.nochr
.doit
		move.b	d3,fr+1(a0)

		pushall
		jsr	printchr
		pullall
		add.w	#16,x(a0)

		jmp	.next

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PLACEZOOL	; Put the correct zool sprite on (inc flash and split).

		tst.b	onnl
		beq	.nonnl
	     	move.b	#$56,zooll+1
		tst.w	zoold
		bne	.nonnl
		move.b	#$5b,zooll+1
.nonnl
		cmp.b	#sln,worldno+3		World 4 ? (space ship).
		bne	.notspaced
		move.w	#1,zooll
		tst.b	up
		beq	.noupani
		move.w	#2,zooll
		jmp	.notspaced
.noupani	tst.b	down
		beq	.notspaced
	  	clr.w	zooll
.notspaced
		tst.w	splitpower
		beq	place
		subq.w	#1,splitpower

		add.l	#1,zoolmem
		cmp.l	#40,zoolmem
		bne	.notyet
       		clr.l	zoolmem
.notyet		move.l	zoolmem,d0
		lsl.w	#3,d0
		lea	zoolmems,a0
		move.l	zoolwx,(a0,d0.w)
		move.w	zooll,4(a0,d0.w)
		move.b	zoold,6(a0,d0.w)

		tst.b	zoolisdead
		bne	place
		eori.b	#1,whichzool   
		tst.b	whichzool
		beq	place

		bset.b	#0,shield+1
		move.l	zoolx,-(sp)		
		move.w	zooll,-(sp)		

		move.l	zoolmem,d0
		sub.w	#25,d0
		bpl	.zok
   		add.l	#40,d0
.zok		lsl.w	#3,d0
		move.w	(a0,d0.w),d1
		add.w	#64+$40,d1
		sub.w	scrwx,d1
		cmp.w	zoolx,d1
		beq	.sk
		move.w	4(a0,d0.w),zooll
		move.w	d1,zoolx
.sk		move.w	2(a0,d0.w),d2
		add.w	#44,d2
		sub.w	scrwy,d2
		cmp.w	zooly,d2
		beq	.sk2
		move.w	4(a0,d0.w),zooll
		move.w	d2,zooly
.sk2
.pl		jsr	place
		move.w	(sp)+,zooll
		move.l	(sp)+,zoolx

		rts

place		lea	remember,a5		Replace any clear stuff.
.moremem	tst.l	(a5)
		beq	.dontrem
		move.l	(a5),a0
		move.l	4(a5),(a0)
		add.w	#8,a5
		jmp	.moremem
.dontrem	lea	remember,a5
		clr.l	(a5)
		
		move.w	zoolx,d6

		cmp.b	#$20,bellm
		bne	.bell
		clr.w	d6
.bell
		move.w	zooly,d7

		tst.w	shield			Flash for shield ...	
		beq	.clearflash
   		subq.w	#1,shield
		tst.w	noblink
		beq	.nodec
		subq.w	#1,noblink
.nodec		btst.b	#0,shield+1
		beq	.clearflash
		btst.b	#1,shield+1
		beq	.clearflash
	
		tst.w	noblink
		beq	.no
		move.w	noblink,d0
		cmp.w	shield,d0
		bpl	.clearflash
.no
      		lea	spritecols+2+4+4+4+4,a1
		moveq	#11,d0
.more2		move.w	#$ddd,(a1)
		addq.w	#4,a1
		dbra	d0,.more2
		move.w	#$ddd,extracols+14
		move.w	#$ddd,blinkcol3+2
		jmp	.notflash
.clearflash	cmp.w	#$ddd,spritecols+26
		bne	.notflash
		tst.b	gamefadetmr	
		bne	.notflash
		lea	sprpal,a0	
		cmp.b	#sln,worldno+3
		bne	.nw4
    		lea	sprpal2,a0
.nw4		lea	spritecols+2,a1
		moveq	#16-1,d0
.more		move.w	(a0)+,(a1)+
		addq.w	#2,a1
		dbra	d0,.more
		cmp.b	#sln,worldno+3
		bne	.norm
   		move.w	#$193,extracols+14
		move.w	#$222,blinkcol1+2
		move.w	#$444,blinkcol2+2
		move.w	#$888,blinkcol3+2
		jmp	.notflash
.norm		clr.w	extracols+14
		move.w	#$fff,blinkcol1+2
		move.w	#$da0,blinkcol2+2
		clr.w	blinkcol3+2
.notflash	
		cmp.b	#sln,worldno+3
		beq	.noblink
		tst.w	shield			Zool blink ...
		bne	.noblink
		tst.b	magic
		bne	.open
		eori.b	#1,blinkt
		tst.b	blinkt
		beq	.noblink
		subq.b	#1,blink
		beq	.open
		cmp.b	#3,blink
		bpl	.noblink
		move.w	#$999,blinkcol1+2
		move.w	#$830,blinkcol2+2
		jmp	.noblink
.open		move.w	#$fff,blinkcol1+2
		move.w	#$da0,blinkcol2+2
		jsr	random	
		andi.b	#$1f,d0
		add.b	#10,d0
		move.b	d0,blink
.noblink
		move.w	blinkcol2+2,extracols+2
		move.w	blinkcol2+2+4,extracols+2+4
		move.w	blinkcol2+2+8,extracols+2+8

		move.w	zooll,d5
		lsl.w	#2,d5
		lea	zooloffs,a3
		cmp.b	#sln,worldno+3
		bne	.fok
		lea	zooloffs2,a3
.fok		add.w	(a3,d5.w),d6
		add.w	2(a3,d5.w),d7
		move.w	d7,d3
		move.l	zoolrun,a6
		cmp.b	#sln,worldno+3
		bne	.nff
		move.l	zoolship,a6
.nff		add.l	(a6,d5.w),a6

		add.w	8(a6),d7
		move.w	#$100,d0
		sub.w	2(a6),d0
		move.w	2(a6),d1
		add.w	#2,d1
		move.l	14(a6),a0
		jsr	createsprite
		lea	zspr1,a1
.morec		move.l	(a0)+,(a1)+
		dbra	d1,.morec
		move.l	#zspr1,d1
;		move.l	d1,$dff120
		move.w	d1,zoolsprptrs+6
		swap.w	d1
		move.w	d1,zoolsprptrs+2
		move.l	18(a6),a0
		jsr	createsprite
		ori.w	#$80,2(a0)
		lea	zspr2,a1
		move.w	2(a6),d1
		add.w	#2,d1
.morec2		move.l	(a0)+,(a1)+
		dbra	d1,.morec2
		move.l	#zspr2,d1
 ;		move.l	d1,$dff124
		move.w	d1,zoolsprptrs+6+(8*1)
		swap.w	d1
		move.w	d1,zoolsprptrs+2+(8*1)

		add.w	#16,d6
		move.w	d3,d7
		add.w	10(a6),d7
		move.w	#$100,d0
		sub.w	4(a6),d0
		move.w	4(a6),d1
		add.w	#2,d1
		move.l	22(a6),a0
		jsr	createsprite
		lea	zspr3,a1
.morec3		move.l	(a0)+,(a1)+
		dbra	d1,.morec3
		move.l	#zspr3,d1
  ;		move.l	d1,$dff128
		move.w	d1,zoolsprptrs+6+(8*2)
		swap.w	d1
		move.w	d1,zoolsprptrs+2+(8*2)
		move.l	26(a6),a0
		jsr	createsprite
		ori.w	#$80,2(a0)
		lea	zspr4,a1
		move.w	4(a6),d1
		add.w	#2,d1
.morec4		move.l	(a0)+,(a1)+
		dbra	d1,.morec4
		move.l	#zspr4,d1
   ;		move.l	d1,$dff12c
		move.w	d1,zoolsprptrs+6+(8*3)
		swap.w	d1
		move.w	d1,zoolsprptrs+2+(8*3)
	
		tst.b	1(a6)
		bne	.do3rd
      		move.l	#blankspr,d0
		move.l	d0,$dff130
		move.l	d0,$dff134
		move.w	d0,zoolsprptrs+6+(8*4)
		move.w	d0,zoolsprptrs+6+(8*5)
  		swap	d0
		move.w	d0,zoolsprptrs+2+(8*4)
		move.w	d0,zoolsprptrs+2+(8*5)
		rts

.do3rd		add.w	#16,d6
		move.w	d3,d7
		add.w	12(a6),d7
		move.w	#$100,d0
		sub.w	6(a6),d0
		move.w	6(a6),d1
		add.w	#2,d1
		move.l	30(a6),a0
		jsr	createsprite
		lea	zspr5,a1
.morec5		move.l	(a0)+,(a1)+
		dbra	d1,.morec5
		move.l	#zspr5,d1
    ;		move.l	d1,$dff130
		move.w	d1,zoolsprptrs+6+(8*4)
		swap.w	d1
		move.w	d1,zoolsprptrs+2+(8*4)
		move.l	34(a6),a0
		jsr	createsprite
		ori.w	#$80,2(a0)
		lea	zspr6,a1
		move.w	6(a6),d1
		add.w	#2,d1
.morec6		move.l	(a0)+,(a1)+
		dbra	d1,.morec6
		move.l	#zspr6,d1
     ;		move.l	d1,$dff134
		move.w	d1,zoolsprptrs+6+(8*5)
		swap.w	d1
		move.w	d1,zoolsprptrs+2+(8*5)
	
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CREATESPRITE	; Setup a hardware sprite ready for display.

		; a0 = sprites address.
		; d0 = $100-sprites height.
		; d6 = sprites X pos.
		; d7 = sprites Y pos.

		move.l	zoolrun,d4
		cmp.b	#sln,worldno+3
		bne	.nff2
		move.l	zoolship,d4
.nff2		add.l	d4,a0

		movem.l	d0-d7,-(sp)

		cmp.w	#$42,d7			Above catch ?
		bpl	.onorover
		move.w	#$42,d1			How much above ?
		sub.w	d7,d1
		move.w	#$100,d2
		sub.w	d0,d2
		cmp.w	d1,d2	 		Enough height.
		bpl	.notalloff
		move.w	#$10,d6
		move.w	#$80,d7
		jmp	.onorover
.notalloff	add.w	d1,d0
		lsl.w	#2,d1
		add.w	d1,a0
		move.l	a0,(a5)+
		move.l	(a0),(a5)+
		clr.l	(a5)
		clr.l	(a0)
		move.w	#$42,d7
.onorover
		move.w	d7,d1
		move.w	#$100,d2
		sub.w	d0,d2
		add.w	d2,d1
		cmp.w	#$118,d1
		ble	.onorless
	 	sub.w	#$118,d1
		move.w	d0,d2
		add.w	d1,d0
		cmp.w	#$100,d0
		bmi	.allthere
	 	move.w	d2,d0
		move.w	#$10,d6
		move.w	#$80,d7
		jmp	.onorless
.allthere	move.w	#$100,d1
		sub.w	d0,d1
		addq.w	#1,d1
		lsl.w	#2,d1
		move.l	a0,a1
		add.w	d1,a1
		move.l	a1,(a5)+
		move.l	(a1),(a5)+
		clr.l	(a5)
		clr.l	(a1)
.onorless

		moveq	#0,d1			Set up regs.
		moveq	#0,d2
		moveq	#0,d3
		moveq	#0,d4
		move.w	d6,d1
		
		btst	#0,d1			X bit set ?
		beq	.noxbitset
		bset	#0,d2
		bset	#0,d4
.noxbitset
		lsr.w	d1			Set X pos.
		move.w	d1,d3
		addq.w	#8,d3
		
		move.w	d7,d5			Set Y pos.
		lsl.w	#8,d5
		or.w	d5,d1
		or.w	d5,d2
		or.w	d5,d3
		or.w	d5,d4
		move.w	#$100,d6		d6 = height * $100.
		sub.w	d0,d6
		lsl.w	#8,d6
		add.w	d6,d2
		add.w	d6,d4
		
		cmp.w	d0,d7			Set vstop bit if needed.
		bmi	.dontsetbit1
		bset	#1,d2
		bset	#1,d4
.dontsetbit1	cmp.w	#$100,d7		Set vstart bit if needed.
		bmi	.dontsetbit2
		bset	#2,d2
		bset	#2,d4
.dontsetbit2
		move.w	d1,(a0)			Setup sprite 0
		move.w	d2,2(a0)

		movem.l	(sp)+,d0-d7

		rts

TOPSCOLINE	Macro
		dc.w	(\1*$100)+$01,$fffe
		dc.w	$140,$8043,$144,$0000,$146,$0000	SC
		dc.w	$148,$8052,$14c,$0000,$14e,$0000	00
		dc.w	(\1*$100)+$4b,$fffe
		dc.w	$140,$805a,$144,$0000,$146,$0000	00
		dc.w	$148,$8062,$14c,$0000,$14e,$0000	00
		
		dc.w	$150,$8082,$154,$0000,$156,$0000	--
		dc.w	$158,$808a,$15c,$0000,$15e,$0000	--
		dc.w	(\1*$100)+$75,$fffe
		dc.w	$150,$8092,$154,$0000,$156,$0000	--
		dc.w	(\1*$100)+$a5,$fffe
		dc.w	$140,$80b5,$144,$0000,$146,$0000	HI
		dc.w	$148,$80c4,$14c,$0000,$14e,$0000	00
		dc.w	$140,$80cc,$144,$0000,$146,$0000	00
		dc.w	$148,$80d4,$14c,$0000,$14e,$0000	00
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		EndM

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*
		
BOTSCOLINE	Macro
		dc.w	(\1*$100)+$01,$fffe
		dc.w	$140,$8046,$144,$0000,$146,$0	==
		dc.w	$148,$804e,$14c,$0000,$14e,$0	==
		dc.w	(\1*$100)+$6f,$fffe
		dc.w	$150,$8082,$154,$0000,$156,$0	==
		dc.w	$148,$808a,$14c,$0000,$14e,$0	B
		dc.w	$140,$8092,$144,$0000,$146,$0	00
		dc.w	(\1*$100)+$a7,$fffe
		dc.w	$140,$80cb,$144,$0000,$146,$0	==
		dc.w	$148,$80d4,$14c,$0000,$14e,$0	==
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

		EndM

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*
		
GAMECOP		; Main game copper.

		dc.w	$2401,$fffe

		dc.w	$1001,$fffe		Wait a bit for mainline.
		dc.w	$0092,$0030		Data fetch start.
		dc.w	$0094,$00d8		Data fetch stop.
		dc.w	$008e,$2c81		Window start.
		dc.w	$0090,$2cc1		Window stop.		
		dc.w	$0108,$0084		Mods.
		dc.w	$010a,$0084		Mods.
scrollx		dc.w	$0102,$0000		X shifts.
		dc.w	$0104,$0064		Priorities.

scrollbpls	dc.w	$00e0,$0005		Set top bitplane pointers.
		dc.w	$00e2,$1b2a
		dc.w	$00e4,$0005	
		dc.w	$00e6,$1b2a
		dc.w	$00e8,$0005	
		dc.w	$00ea,$1b2a
		dc.w	$00ec,$0005	
		dc.w	$00ee,$1b2a
		dc.w	$00f0,$0005	
		dc.w	$00f2,$1b2a
		
		dc.w	$0180,$0000		Raster colour.
gamecols	dc.w	$0182,$0		Main game colours.
		dc.w	$0184,$0
		dc.w	$0186,$0
		dc.w	$0188,$0
		dc.w	$018a,$0
		dc.w	$018c,$0
		dc.w	$018e,$0
		dc.w	$0190,$0
skycol2		dc.w	$0192,$0
		dc.w	$0194,$0
		dc.w	$0196,$0
		dc.w	$0198,$0
		dc.w	$019a,$0
		dc.w	$019c,$0
		dc.w	$019e,$0
spritecols	dc.w	$01a0,$0
		dc.w	$01a2,$0
		dc.w	$01a4,$0
		dc.w	$01a6,$0
blinkcol1	dc.w	$01a8,$0
blinkcol2	dc.w	$01aa,$0
		dc.w	$01ac,$0
		dc.w	$01ae,$0
		dc.w	$01b0,$0
		dc.w	$01b2,$0
		dc.w	$01b4,$0
		dc.w	$01b6,$0
blinkcol3	dc.w	$01b8,$0
		dc.w	$01ba,$0
		dc.w	$01bc,$0
		dc.w	$01be,$0

spincols	dc.w	$1aa,$f40
		dc.w	$1ac,$c20
		dc.w	$1ae,$800

		dc.w	$1101,$fffe		Wait for first split.
		dc.w	$0100,$4000		Turn on screen.
		
		dc.w	$0096,$0020		Sprite DMA off.
		
		dc.w	$0144,$0000
		dc.w	$0146,$0000
		dc.w	$014c,$0000
		dc.w	$014e,$0000
		dc.w	$0154,$0000
		dc.w	$0156,$0000
		dc.w	$015c,$0000
		dc.w	$015e,$0000

		dc.w	$142,$4000
		dc.w	$14a,$4000
		dc.w	$152,$4000
		dc.w	$15a,$4000
		dc.w	$162,$4000
		dc.w	$16a,$4000

******************** BETWEEN START AND TOP SCORELINES ***********************
		
lttsl1		dc.w	$0180,$0		Above top scoline stuff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
      
lttsl2		dc.w	$0180,$0		Above top scoline stuff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

		dc.w	$0180,$0		Above top scoline stuff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

******************** TOP SCORELINES ***********************
		
topscolines	topscoline	$33		In top scoline stuff.
tl2		topscoline	$34
		topscoline	$35
		topscoline	$36
		topscoline	$37
		topscoline	$38
		topscoline	$39
		topscoline	$3a
		topscoline	$3b
		topscoline	$3c
		topscoline	$3d
		topscoline	$3e

		dc.w	$3e01,$fffe		Blank sprs ready for zool.
extracols	dc.w	$1aa,$fe0
		dc.w	$1ac,$ba0
		dc.w	$1ae,$430
		dc.w	$1a6,$000
sprdma1		dc.w	$0096,$8020		Sprite DMA on.
		dc.w	$0144,$0000
		dc.w	$0146,$0000
		dc.w	$014c,$0000
		dc.w	$014e,$0000
zoolsprptrs	dc.w	$0120,$0000		Hiros pointers.
		dc.w	$0122,$0000
		dc.w	$0124,$0000
		dc.w	$0126,$0000
		dc.w	$0128,$0000
		dc.w	$012a,$0000
		dc.w	$012c,$0000
		dc.w	$012e,$0000
		dc.w	$0130,$0000
		dc.w	$0132,$0000
		dc.w	$0134,$0000
		dc.w	$0136,$0000
		
******************** BETWEEN TOP SCORELINES AND FF ***********************

gttsl1		dc.w	$0180,$0		Between scoline and $ff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
gttsl2		dc.w	$0180,$0		Between scoline and $ff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
gttsl3		dc.w	$0180,$0		Between scoline and $ff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
gttsl4		dc.w	$0180,$0		Between scoline and $ff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
gttsl5		dc.w	$0180,$0		Between scoline and $ff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
gttsl6		dc.w	$0180,$0		Between scoline and $ff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
gttsl7		dc.w	$0180,$0		Between scoline and $ff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
gttsl8		dc.w	$0180,$0		Between scoline and $ff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
gttsl9		dc.w	$0180,$0		Between scoline and $ff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

**************************** FF WAIT ***********************
		
ffwait		dc.w	$ffd3,$fffe		Get over second zero.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

magiccs		dc.w	$1aa,$fe0
		dc.w	$1ac,$ba0
		dc.w	$1ae,$320
		dc.w	$1a6,$000

		dc.w	$01ba,$fff
		dc.w	$01bc,$fff
		dc.w	$01be,$fff
******************** BETWEEN FF AND BOTTOM SCORELINES ***********************
		
ltbsl1		dc.w	$0180,$0		Between $ff and scoline.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

ltbsl2		dc.w	$0180,$0		Between $ff and scoline.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

******************** BOTTOM SCORELINES ***********************

botscolines	botscoline	$19
bl2		botscoline	$1a
		botscoline	$1b
		botscoline	$1c
		botscoline	$1d
		botscoline	$1e
		botscoline	$1f
		botscoline	$20
		botscoline	$21
		botscoline	$22
		botscoline	$23
		botscoline	$24
		
		dc.w	$0144,$0000		Blank bottom sprites.
		dc.w	$0146,$0000
		dc.w	$014c,$0000
		dc.w	$014e,$0000
		dc.w	$0154,$0000		Blank bottom sprites.
		dc.w	$0156,$0000
		dc.w	$015c,$0000
		dc.w	$015e,$0000
		      
******************** BELOW BOTTOM SCORELINES ***********************
	
gtbsl1		dc.w	$0180,$0		Below bottom scoline.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

gtbsl2		dc.w	$0180,$0		Below bottom scoline.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0

		dc.w	$0180,$0		Above top scoline stuff.
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		dc.w	$0180,$0
		
		dc.w	$ffff,$fffe		The end.
		

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*

BLANKCOP	dc.w	$180,$0
		dc.w	$100,$0
		dc.w	$ffff,$fffe

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

NEXTALIEN2	cmp.l	#$ffffffff,nextspr
		beq	safebye
		move.l	nextspr,a6
		jmp	dothisalien


*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

MOVEALIENS	; Control all the active aliens.

		tst.b	silly
		beq	.n
  		st.b	spining
.n		clr.b	closenow
		clr.w	xlift
		clr.b	actualzool
		clr.w	xlift2
		clr.w	ylift
		clr.w	ylift2
		clr.w	onlift
		clr.b	stophorz
		clr.b	jun
		clr.b	dragfactor
		clr.b	trumpeton

		cmp.l	#$ffffffff,firstsprite
		beq	safebye
		move.l	firstsprite,a6
		
dothisalien	move.l	nsp(a6),nextspr
		
		moveq	#0,d0
		move.b	t(a6),d0
		lsl.w	#2,d0
		lea	alienjumps,a1
		move.l	(a1,d0.w),a0
		jmp	(a0)

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

seed		dc.l	12313351,$89abcdef
		dc.l	31415926536

RANDOM		move.l	a0,-(sp)

		lea	seed,a0		;do rnd, long in d0

		addq.l	#1,8(a0)
		
		move.l	(a0),d0
		add.l	4(a0),d0
		eor.l	d0,6(a0)
		add.l	8(a0),d0
		move.l	d0,(a0)
		
		roxr	(a0)+
		roxl	(a0)+
		roxr	(a0)+
		roxl	(a0)+

		move.l	(sp)+,a0
		rts

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*

GETSPRITE2	; Get a sprite from the free sprite list and put -
		; its address in a6. ($ffffffff = error).

		cmp.l	#nomore,freesprpointer Any sprites ?
		bpl	spritesleft
		move.l	#$ffffffff,a6		No sprites left.
		rts
spritesleft	move.l	freesprpointer,a5	Get the sprite's position.
		move.l	(a5),a6
		subq.l	#4,freesprpointer	Point to next free sprite.
		cmp.l	#$ffffffff,lastsprite Is it the first sprite.
		bne	.lastsprokay
		move.l	a6,firstsprite	Set it as the first sprite.
		move.l	a6,lastsprite
		move.l	#$ffffffff,psp(a6)
		move.l	#$ffffffff,nsp(a6)
		clr.l	jsp(a6)
		st	ht(a6)
		move.l	#snpglob,snp(a6)
		st	fr+1(a6)
		clr.b	t(a6)
		clr.b	flal(a6)
		clr.b	htg(a6)
		rts
.lastsprokay	move.l	lastsprite,a5	Join last sprite with -
		move.l	a6,nsp(a5)		new last sprite.
		move.l	a5,psp(a6)
		move.l	#$ffffffff,nsp(a6)
		move.l	a6,lastsprite
		clr.l	jsp(a6)
		st	ht(a6)
		move.l	#snpglob,snp(a6)
		st	fr+1(a6)
		clr.b	t(a6)
		clr.b	flal(a6)
		clr.b	htg(a6)
		rts

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*

GETLOWERSPRITE2	; Get a sprite of a lower priority.
		; This routinte always asumes there is at least
		; one other sprite activated.

		cmp.l	#nomore,freesprpointer Any sprites ?
		bpl	.spritesleft
		move.l	#$ffffffff,a6		No sprites left.
		rts
.spritesleft	move.l	freesprpointer,a5	Get the sprite's position.
		move.l	(a5),a6
		subq.l	#4,freesprpointer	Point to next free sprite.
		cmp.l	#$ffffffff,firstsprite  Is it the first sprite.
		bne	.firstsprokay
		move.l	a6,firstsprite	        Set it as the first sprite.
		move.l	a6,lastsprite
		move.l	#$ffffffff,psp(a6)
		move.l	#$ffffffff,nsp(a6)
		clr.l	jsp(a6)
		st	ht(a6)
		move.l	#snpglob,snp(a6)
		st	fr+1(a6)
		clr.b	t(a6)
		rts
.firstsprokay	move.l	firstsprite,a5		Join last sprite with -
		move.l	a6,psp(a5)		new last sprite.
		move.l	a5,nsp(a6)
		move.l	#$ffffffff,psp(a6)
		move.l	a6,firstsprite
		clr.l	jsp(a6)
		st	ht(a6)
		move.l	#snpglob,snp(a6)
		st	fr+1(a6)
		clr.b	t(a6)
		rts
		
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
		
FREESPRITE2	; Free the sprite held in a6 and put in on the list.
		; The next sprite is NOT pointed to.

		move.l	snp(a6),a0
		bclr.b	#7,(a0)
		
freeplease	addq.l	#4,freesprpointer	Put sprite onto list.
		move.l	freesprpointer,a0
		move.l	a6,(a0)
		cmp.l	#$ffffffff,nsp(a6)	Is it the last sprite.
		bne	.notlast
		cmp.l	#$ffffffff,psp(a6)	Is it the only sprite.
		bne	.notonly
		move.l	#$ffffffff,firstsprite It was the only sprite.
		move.l	#$ffffffff,lastsprite
		rts
.notonly	move.l	psp(a6),a0		It was the last sprite.
		move.l	#$ffffffff,nsp(a0)
		move.l	a0,lastsprite
		rts
.notlast	cmp.l	#$ffffffff,psp(a6)	Is it the first sprite.
		bne	.inmiddle
		move.l	nsp(a6),a0		It is the first sprite.
		move.l	a0,firstsprite
		move.l	#$ffffffff,psp(a0)
		rts
.inmiddle	move.l	nsp(a6),a0		It was in the middle.
		move.l	psp(a6),a1
		move.l	a0,nsp(a1)
		move.l	a1,psp(a0)
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

OVERPROG	; Load over tis (title etc) ...

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*


TIMERSANDSTUFF	; Bits and bobs.

		move.w	#$f40,spincols+2	Flash energy for spin.
		move.w	#$d30,spincols+6
		move.w	#$a10,spincols+10
		subq.b	#1,spinflash
		bpl	.nofl
		move.b	#20,spinflash
		move.w	#$fff,spincols+2
		move.w	#$fff,spincols+6
		move.w	#$fff,spincols+10
.nofl		tst.b	canspin
		beq	.canspin
		subq.b	#1,canspin
		clr.b	spinflash
		move.w	#$888,spincols+2
		move.w	#$777,spincols+6
		move.w	#$555,spincols+10
.canspin
		subq.b	#1,rollcnt
		bpl	.okcnt
		move.b	#3,rollcnt
.okcnt
	
		tst.b	fadeback		Fade back to normal colours.
		beq	.nosmart
		subq.b	#1,fadeback
		move.w	#4,coloffset+2
		lea	gamecols+2,a0
		lea	gamepal,a1
		move.w	#15-1,a4     
		moveq	#0,d6	    
		jmp	fadeacross2
.nosmart	clr.l	skyplmem

		btst.b	#$7,$bfe001		Fire stuff.
		beq	.on
   		clr.b	fireheld
		jmp	.clearfire
.on		cmp.b	#20,fireheld
		beq	.clearfire
		addq.b	#1,fireheld
		cmp.b	#1,fireheld
		bne	.clearfire
		st.b	fire
		jmp	.skip	
.clearfire	clr.b	fire
.skip
		tst.b	tilljump		Can I jump yet ? (from ice).
		beq	.ok2
		subq.b	#1,tilljump
.ok2
		tst.b	justoff
		beq	.noo
    		subq.b	#1,justoff
.noo
		tst.b	smartnow
		beq	.nooo
    		subq.b	#1,smartnow
.nooo
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

ZOOLDEATH	; Bye bye zool ...

		tst.b	goldfish
		beq	.lll
		cmp.b	#1,key
		bne	.lllw
		move.w	#$f000,shield		
		bra	.lll
.lllw		cmp.b	#4,key
		bne	.lll
		jmp	byebyezool

.lll		tst.b	zoolisdead
		beq	safebye
		subq.b	#1,zoolisdead
		bne	safebye

.dd		jsr	synchup			Restart zool.
		clr.l	musicplace
		move.l	#safebye,inaframe+2
		move.w	#$f,$dff096
		clr.l	ch1timer
		clr.l	forcesample1
		move.l	#blankcop,$dff080
		jsr	synchup

		move.l	firstsprite,a0
.more		cmp.l	#-1,a0
		beq	.nomore
		move.l	snp(a0),a1
		bclr.b	#7,(a1)
		move.l	nsp(a0),a0
		jmp	.more
.nomore		move.b	#3,energylevel
		move.b	#$ff,lastep

		cmp.b	#sln4,worldno+3
		beq	bakcfrom

		subq.l	#1,lives
		bpl	restart

		jsr	docont

		jmp	gototitle

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

LEVELCOMPLETED	; Has current level been completed ...

		tst.b	goldfish
		beq	.nocheat
		cmp.b	#3,key
		bne	.no1
    		move.b	#2,areano+3
		bra	.skip
.no1		cmp.b	#2,key			Temporary check.
		beq	.skip
.nocheat	tst.b	endlevel
		beq	bye
.skip		clr.b	endlevel
		move.l	#safebye,inaframe+2
	 	move.l	#blankcop,$dff080     
		clr.l	musicplace
		move.w	#$f,$dff096
		clr.l	ch1timer
		clr.l	forcesample1

		cmp.b	#sln4,worldno+3
		bne	noback
bakcfrom	move.b	tw,worldno+3
		move.b	ta,areano+3
		st.b	fromtitle
		jsr	loadarea
		move.l	restarthereb,restarthere		
		move.l	restarthereb+4,restarthere+4		
		move.l	restarthereb+8,restarthere+8		
		move.l	restarthereb+12,restarthere+12		
		st.b	donebonus
		bra	restart
	
noback		jsr	synchup

		clr.b	donebonus
   		addq.b	#1,areano+3
		cmp.b	#3,areano+3
		bne	loadnextlevel
		addq.b	#1,worldno+3
		cmp.b	#6,worldno+3
		beq	.completedgame
		clr.b	areano+3
		jmp	loadnextlevel
.completedgame	move.l	#safebye,inaframe+2
		bsr	backend
		bsr	checksco
		bra	gototitle

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

DOBULLETS	; Make some bullets.

		tst.b	onrails
		bne	safebye

		tst.w	outofit
		bne	safebye  

		tst.b	gamefadetmr
		bne	safebye

		tst.b	spining
		bne	safebye

		tst.b	fire			Finger on button.
		beq	safebye

		cmp.b	#sln,worldno+3
		bne	notdifferent

		cmp.b	#0,weapon
		beq	zoolbull2
		cmp.b	#1,weapon
		beq	wave
		cmp.b	#2,weapon
		beq	a5way
		cmp.b	#3,weapon
		beq	pulse
		cmp.b	#4,weapon	
		beq	launcherer
		jmp	norem	

pulse		jsr	fireb
		tst.l	fired
		beq	bye
		move.l	fired,a6
		move.l	#pulsetop,any(a6)
		move.b	#25,fr+1(a6)
		move.l	#bangs,blbset(a6)
		move.l	#pulseani,any+20(a6)
		sub.w	#16,x(a6)
		sub.w	#8,y(a6)

		jsr	fireb
		tst.l	fired
		beq	bye
		move.l	fired,a6
		move.l	#pulsebot,any(a6)
		move.b	#25,fr+1(a6)
		move.l	#bangs,blbset(a6)
		move.l	#pulseani,any+20(a6)
		sub.w	#16,x(a6)
		sub.w	#8,y(a6)

		rts

wave		jsr	fireb
		tst.l	fired
		beq	bye
		move.l	fired,a6
		move.l	#wavemovu,any(a6)
		eori.b	#1,wudt
		tst.b	wudt	
		beq	.n
		move.l	#wavemovd,any(a6)
.n		move.b	#5,fr+1(a6)
		move.l	#bangs,blbset(a6)
		move.l	#waveani,any+20(a6)
		sub.w	#16,x(a6)
		sub.w	#16,y(a6)
		rts

launcherer	move.w	#2-1,d0
		jsr	fireit
		move.w	#$e0,sam3+4	
		move.w	#3,d0
		jsr	playsample
		tst.l	fired
		beq	bye
		move.l	fired,a6      
		sub.w	#8,y(a6)
		move.b	#56,htg(a6)
		move.l	#launcher,any(a6)
		move.l	#bangs,blbset(a6)
		move.l	#launcherani,any+20(a6)
		move.l	a6,a4
		move.l	a6,a3
		jsr	getbsprite
		cmp.l	#-1,a6
		beq	.free2
		move.l	#bangs,blbset(a6)
		move.l	x(a4),x(a6)
		move.l	a6,jsp(a4)
		move.l	a6,a4
		jsr	getbsprite
		cmp.l	#-1,a6
		beq	.free
		move.l	#bangs,blbset(a6)
		move.l	x(a4),x(a6)
		move.l	a6,jsp(a4)
		rts

.free		move.l	a4,a6
		jsr	freesprite
		move.l	any+12(a6),a0
		clr.l	(a0)
.free2		move.l	a3,a6
		jsr	freesprite
		move.l	any+12(a6),a0
		clr.l	(a0)
		rts		
	

zoolbull2	move.w	#4-1,d0			Find a space.
      		jsr	fireit
		tst.l	fired
		beq	bye
		move.l	fired,a6	
		sub.w	#8,y(a6)
		move.l	#zoolbull,any(a6)
		move.b	#8,fr+1(a6)
		move.l	#bangs,blbset(a6)
		move.l	#zoolbullani,any+20(a6)
		move.l	a6,a4
		jsr	getbsprite
		cmp.l	#-1,a6
		bne	.nm
   		move.l	a4,a6
		jsr	freesprite
		move.l	any+12(a6),a0
		clr.l	(a0)
		rts
.nm		move.l	#bangs,blbset(a6)
		move.b	#9,fr+1(a6)
		move.l	x(a4),x(a6)
		add.w	#32,x(a6)
		move.l	a6,jsp(a4)
		rts	

a5way		jsr	fireb
		tst.l	fired
		beq	bye
		move.l	fired,a6
		move.l	#a5way3,any(a6)
		move.b	#22,fr+1(a6)
		move.l	#bangs,blbset(a6)
		jsr	fireb
		tst.l	fired
		beq	bye
		move.l	fired,a6
		move.l	#a5way2,any(a6)
		move.b	#21,fr+1(a6)
		move.l	#bangs,blbset(a6)
		jsr	fireb
		tst.l	fired
		beq	bye
		move.l	fired,a6
		move.l	#a5way4,any(a6)
		move.b	#23,fr+1(a6)
		move.l	#bangs,blbset(a6)
		jsr	fireb
		tst.l	fired
		beq	bye
		move.l	fired,a6
		move.l	#a5way1,any(a6)
		move.b	#20,fr+1(a6)
		move.l	#bangs,blbset(a6)
		jsr	fireb
		tst.l	fired
		beq	bye
		move.l	#a5way5,any(a6)
		move.l	fired,a6
		move.b	#24,fr+1(a6)
		move.l	#bangs,blbset(a6)
		rts

norem		jsr	fireb
		tst.l	fired
		beq	bye
		move.l	fired,a6
		move.l	#a5way3,any(a6)
		move.b	#22,fr+1(a6)
		move.l	#bangs,blbset(a6)
		rts

notdifferent

		tst.w	splitpower
		beq	fireb

		jsr	fireb

		move.l	zoolwx,-(sp)
		move.w	zoold,-(sp)

		lea	zoolmems,a0
		move.l	zoolmem,d0
		sub.l	#25,d0
		bpl	.zok
   		add.l	#40,d0
.zok		lsl.w	#3,d0
		move.l	(a0,d0.w),zoolwx
		move.b	6(a0,d0.w),zoold

		jsr	fireb

		move.w	(sp)+,zoold
		move.l	(sp)+,zoolwx

		rts

wudt		dc.w	0
waveani		dc.w	5,5,5,5,6,6,6,6,6
w22		dc.w	7,$ffff
		dc.l	w22
wavemovu	dc.w	14,-1,goto
		dc.l	wavemovu
wavemovd	dc.w	14,1,goto
		dc.l	wavemovd
			

pulseani	dc.w	25,25,25,25,25,26,26,26,26,26
.p22		dc.w	28,$ffff
		dc.l	.p22

pulsetop	dc.w	-2,-5,repeat,7
		dc.l	pulsetop	
.pt2		dc.w	15,2,goto
		dc.l	.pt2

pulsebot	dc.w	-2,5,repeat,7
		dc.l	pulsebot	
.pb2		dc.w	15,-2,goto
		dc.l	.pb2




still		dc.w	0,0,goto
		dc.l	still
bombani		dc.w	16,16,17,17,18,18,19,19,$fffe

launcher	dc.w	8,0,8,0,8,0,8,0,8,0,8,0,8,0,8,0,8,0,8,0,8,0,8,0
		dc.w	8,0,7,0,7,0,6,0,5,0,4,0,3,0,2,0,1,0
la2		dc.w	0,0,0,0,goto
		dc.l	la2

launcherani	dc.w	14,14,15,15,$ffff
		dc.l	launcherani

zoolbullani	dc.w	8,8,10,10,$ffff
		dc.l	zoolbullani

zoolbull	dc.w	13,0,goto
		dc.l	zoolbull

a5way1		dc.w	12,-6,goto
		dc.l	a5way1

a5way2		dc.w	13,-3,goto
		dc.l	a5way2
	
a5way3		dc.w	14,0,goto
		dc.l	a5way3
	
a5way4		dc.w	13,3,goto
		dc.l	a5way4

a5way5		dc.w	12,6,goto
		dc.l	a5way5
		


FIREB		; Fire a bullet from zoolwx,zoolwy

		move.w	#4-1,d0			Find a space.
fireit		clr.l	fired
		lea	bulletsprs,a0
fireit2
.more		tst.l	(a0)
		beq	.thisone
		addq.w	#4,a0
		dbra	d0,.more
		rts

.thisone	move.w	#$100,sam3+4	
		move.w	#$f,d0
		jsr	playsample
	
		jsr	getbsprite		Make a bullet.
		cmp.l	#-1,a6
		beq	.notfireing
		move.l	a6,bulspr
		move.l	a6,(a0)
		move.l	a6,fired
		move.l	a0,any+12(a6)
		move.b	#22,t(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		clr.b	any+10(a6)
		clr.b	any+16(a6)
		clr.l	any+20(a6)
		move.l	#lev1stuf,blbset(a6)
		move.w	zoolwx,x(a6)
		add.w	#21,x(a6)
		clr.b	fr+1(a6)  
		tst.b	zoold
		beq	.sk
  		add.w	#14,x(a6)
		move.b	#2,fr+1(a6)
.sk		move.w	zoolwy,y(a6)
  		add.w	#27,y(a6)
		tst.b	slideing
		bne	.downmore  
		cmp.b	#sln,worldno+3
		bne	.mn
		add.l	#$0015000e,x(a6)
.mn		tst.b	ducking
		beq	.noduck
.downmore	add.w	#12,y(a6)
.noduck		move.b	zoold,any+11(a6)
.notfireing	rts


***********************************************************************
***********************************************************************

DISGETREADY	; Get ready stuff.

		clr.w	mint
		move.w	#2,zooldir
		move.l	#compirq,inaframe+2
   
		lea	titlebpls,a0
		move.l	#$60000,d1
		move.l	#(40*256),d2
		bsr	setplanes
		move.b	#$50,bplam+2
   		move.b	#$2c,ws1+2
		move.b	#$2c,ws2+2
		move.b	#$81,startblink+1

		
		move.b	worldno+3,nos
		add.b	#$31,nos
		move.b	areano+3,nos+2
		add.b	#$31,nos+2

		move.l	#titlecop,$dff080
		clr.w	$dff088

		lea	$60000,a0
		move.w	#(256*41*5)/4,d0
.moresc		clr.l	(a0)+
		dbra	d0,.moresc

		move.l	mem,a0
		add.l	#55012,a0
 		lea	$78000,a1
		jsr	dechomp
		move.l	#$78000,chrplace
		move.l	#$60000,wrkplanes
		cmp.b	#6,worldno+3
		bne	.dothis
       		lea	bonusmess,a4
		jsr	printmess
		bra	.skme
.dothis		lea	getreadymess,a4		Display message.
		jsr	printmess
		lea	getreadymess2,a4
		jsr	printmess
.skme
		move.l	mem,a0
		add.l	#zoolpic-exstart,a0
 		lea	$60000+(256*40*3),a1
		jsr	dechomp

   		clr.w	$60000+(256*40*4)+2		Set pallete.
		move.w	#$5,$60000+(40*256*5)
		move.w	#$6,$60000+(40*256*5)+16
		clr.w	$60000+(40*256*5)+32
		move.l	#$03300663,$60000+(40*256*5)+34
		move.l	#$07740ac4,$60000+(40*256*5)+38
		move.l	#$0ce80eee,$60000+(40*256*5)+42
		clr.w	$60000+(40*256*5)+32+16
		move.l	#$05500885,$60000+(40*256*5)+34+16
		move.l	#$09960ce6,$60000+(40*256*5)+38+16
		move.l	#$0efa0fff,$60000+(40*256*5)+42+16
   
		tst.b	ontogame
		beq	.nomusic
		move.b	#1,tempocnt
		tst.w	musicno
		beq	.nomusic
		move.b	#$7f,temposp	
		cmp.b	#2,musicno+1
		beq	.m2
		cmp.b	#4,musicno+1
		bne	.nm2
.m2		move.b	#2,temposp
.nm2		moveq	#0,d0		Set up player spec.
		moveq	#0,d1		Interrupt type (not used on selfcall).
		moveq	#0,d2		0=Selfcall/1=AutoCall.
		moveq	#0,d3		0=Pal/1=Ntsc.
		jsr	protracker
		move.l	a0,-(sp)
		moveq	#1,d0		Tell protracker where the mod is.
		lea	blobs,a0
		jsr	protracker
		moveq	#4,d0		Set maximum vol.
		moveq	#64,d1		Vol (0-64).
		jsr	protracker
		moveq	#8,d0		Make sure all the cannels are on.
		moveq	#0,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#1,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#2,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#3,d1
		jsr	protracker
		move.l	(sp)+,musicplace Addr to call on interrupt.
.nomusic
	
	
		lea	titlecols+2,a0
		lea	$60000+(40*256*5),a1
		move.w	#32-1,a4
		jsr	fadein
.wait		btst.b	#7,$bfe001
		bne	.wait

		tst.b	ontogame
		beq	.nomusic3
		tst.w	musicno
		bne	.nomusic4
.nomusic3	clr.l	musicplace
		move.w	#$f,$dff096
		clr.l	ch1timer
		clr.l	forcesample1
.nomusic4	
		lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		move.w	#(256*40*3),zoolleft2+2
		move.l	#bye,inaframe+2
		rts

getreadymess	dc.w	8,95
		dc.b	'     AREA '
nos		dc.b	'2.3',$ff
getreadymess2	dc.w	16,125
		dc.b	'    GET READY',$ff
bonusmess	dc.w	16,115
		dc.b	'   BONUS LEVEL',$ff


completemess	dc.w	8,95
		dc.b	'  LEVEL COMPLETE',0,0
		dc.b	'TIME BONUS .. '
timeleftb	dc.b	'0000',0,$ff

preparemess	dc.w	0,115
		dc.b	'  PREPARE YOURSELF',$ff

hellomess	dc.w	0,70




zooldir		dc.w	0
mint		dc.w	0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

compirq		tst.w	mint
		beq	.done
     		subq.w	#1,mint
.done		move.w	zooldir,d0
		add.w	d0,zoolleft2+2
		cmp.w	#(256*40*3)+(2*150),zoolleft2+2
		beq	.other
		cmp.w	#(256*40*3),zoolleft2+2
		bne	.noother
.other		neg.w	zooldir
.noother	rts

DISCOMPLETE	; Get ready stuff.

		tst.b	fromtitle
		bne	.otherone

		move.l	timeleft,d0
		move.l	d0,d1
		mulu	#10,d1
		add.l	d1,score
		jsr	convertno
		move.b	result+3,timeleftb+0
		add.b	#$30,timeleftb+0
		move.b	result+4,timeleftb+1
		add.b	#$30,timeleftb+1
		move.b	result+5,timeleftb+2
		add.b	#$30,timeleftb+2

.otherone	lea	titlebpls,a0
		move.l	#$60000,d1
		move.l	#(40*256),d2
		bsr	setplanes
		move.b	#$50,bplam+2
	
		move.w	#$c5,mint
		move.w	#2,zooldir
		move.l	#compirq,inaframe+2
		move.b	#$2c,ws1+2
		move.b	#$2c,ws2+2
		move.b	#$81,startblink+1


		move.l	#titlecop,$dff080
		clr.w	$dff088

		lea	$60000,a0
		move.w	#(256*41*5)/4,d0
.moresc		clr.l	(a0)+
		dbra	d0,.moresc

		move.l	mem,a0
		add.l	#55012,a0
 		lea	$78000,a1
		jsr	dechomp
		move.l	#$78000,chrplace
		move.l	#$60000,wrkplanes
		lea	preparemess,a4
		tst.b	fromtitle
		bne	.other
		lea	completemess,a4
.other		jsr	printmess
		clr.b	fromtitle      

		clr.b	dir_valid		Start disk drive.
		jsr	initialise
		jsr	startdrv

		lea	musiccompn,a0		Load music.
 		lea	$53000,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file

		lea	compn,a0		Load title.
 		lea	$60000+(256*40*3),a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
 		move.l	a1,a0
		jsr	dechomp

		jsr	stopdrv

		; PLAY THE TUNE ...
	
		move.b	#1,tempocnt
		move.b	#$7f,temposp	
		moveq	#0,d0		Set up player spec.
		moveq	#0,d1		Interrupt type (not used on selfcall).
		moveq	#0,d2		0=Selfcall/1=AutoCall.
		moveq	#0,d3		0=Pal/1=Ntsc.
		jsr	protracker
		move.l	a0,-(sp)
		moveq	#1,d0		Tell protracker where the mod is.
		lea	$53000,a0
		jsr	protracker
		moveq	#4,d0		Set maximum vol.
		moveq	#64,d1		Vol (0-64).
		jsr	protracker
		moveq	#8,d0		Make sure all the cannels are on.
		moveq	#0,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#1,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#2,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#3,d1
		jsr	protracker
		move.l	(sp)+,musicplace Addr to call on interrupt.
.skipmusic


   		;clr.w	$60000+(256*40*4)+2
		;move.w	#$400,$60000+(40*256*5)
		;move.w	#$500,$60000+(40*256*5)+16
		;clr.w	$60000+(40*256*5)+32
		;move.l	#$00330066,$60000+(40*256*5)+34	Set text pal.
		;move.l	#$007700ac,$60000+(40*256*5)+38
		;move.l	#$00ce00ee,$60000+(40*256*5)+42
		;clr.w	$60000+(40*256*5)+32+16
		;move.l	#$00550088,$60000+(40*256*5)+34+16	Set text pal.
		;move.l	#$009900ce,$60000+(40*256*5)+38+16
		;move.l	#$00ef00ff,$60000+(40*256*5)+42+16
	
		clr.w	$60000+(256*40*4)+2		Set pallete.
		move.w	#$5,$60000+(40*256*5)
		move.w	#$6,$60000+(40*256*5)+16
		clr.w	$60000+(40*256*5)+32
		move.l	#$03300663,$60000+(40*256*5)+34
		move.l	#$07740ac4,$60000+(40*256*5)+38
		move.l	#$0ce80eee,$60000+(40*256*5)+42
		clr.w	$60000+(40*256*5)+32+16
		move.l	#$05500885,$60000+(40*256*5)+34+16
		move.l	#$09960ce6,$60000+(40*256*5)+38+16
		move.l	#$0efa0fff,$60000+(40*256*5)+42+16
		
		lea	titlecols+2,a0
		lea	$60000+(40*256*5),a1
		move.w	#32-1,a4
		jmp	fadein

CLEARCOMPLETE	tst.w	mint
		bne	clearcomplete
		lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		move.l	#bye,inaframe+2
		move.w	#(256*40*3),zoolleft2+2   
		rts

musiccompn	dc.b	'zoolacid.mod',0
		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

HELLO		; Messages...

		move.w	#2,zooldir
		move.l	#compirq,inaframe+2
		add.l	#2000,score

		move.l	#titlecop,$dff080
		clr.w	$dff088

		lea	$60000,a0
		move.w	#(256*41*5)/4,d0
.moresc		clr.l	(a0)+
		dbra	d0,.moresc

		clr.b	dir_valid		Start disk drive.
		jsr	initialise
		jsr	startdrv
		lea	compn,a0		Load title.
 		lea	$60000+(256*40*3),a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
 		move.l	a1,a0
		jsr	dechomp
		lea	chrsn,a0		Load hisco.
 		lea	$78000,a1
		move.l	#$78000,chrplace
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file
		move.l	a1,a0
		jsr	dechomp
		jsr	stopdrv

		move.l	#$60000,wrkplanes
		lea	hellomess,a4
		jsr	printmess
	
   		clr.w	$60000+(256*40*4)+2
		move.w	#$440,$60000+(40*256*5)
		move.w	#$550,$60000+(40*256*5)+16
		clr.w	$60000+(40*256*5)+32
		move.l	#$05500880,$60000+(40*256*5)+34		Set text pal.
		move.l	#$09900ce0,$60000+(40*256*5)+38
		move.l	#$0ef00ff0,$60000+(40*256*5)+42
		clr.w	$60000+(40*256*5)+32+16
		move.l	#$05500880,$60000+(40*256*5)+34+16	Set text pal.
		move.l	#$09900dd0,$60000+(40*256*5)+38+16
		move.l	#$0ee00ff0,$60000+(40*256*5)+42+16
	
		lea	titlecols+2,a0
		lea	$60000+(40*256*5),a1
		move.w	#32-1,a4
		jsr	fadein

.wa		btst.b	#$7,$bfe001
		bne	.wa
	
		lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		move.l	#bye,inaframe+2
		move.w	#(256*40*3),zoolleft2+2   
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

		   
sco1		ds.w	3

PLACESCO	moveq	#0,d7

		move.l	score,d0
		jsr	convertno
		move.l	result,sco1
		move.w	result+4,sco1+4

		lea	hiscorestart+13,a0	Set hiscorestart.
		lea	sco1,a1			Set score pointer.
nextscore	moveq	#0,d2
notendsc	move.b	(a0,d2.w),d0		Hiscore digit.
		move.b	(a1,d2.w),d1		Score digit.
		add.b	#$30,d1
		cmp.b	d0,d1
		bne	notequal
		addq.l	#1,d2
		cmp.l	#6,d2
		bne	notendsc
		jmp	thisone
notequal	cmp.b	d0,d1
		bpl	thisone
		cmp.l	#hiscorestart+(9*21)+13,a0
		beq	safebye
		add.l	#21,a0
		jmp	nextscore
thisone		move.l	#hiscorestart+(10*21)+13,d0
		sub.l	a0,d0
		move.l	a0,a1
		add.l	#21,a1
scrollmore	move.b	(a0,d0.l),(a1,d0.l)
		move.b	1(a0,d0.l),1(a1,d0.l)
		move.b	2(a0,d0.l),2(a1,d0.l)
		move.b	3(a0,d0.l),3(a1,d0.l)
		move.b	4(a0,d0.l),4(a1,d0.l)
		move.b	5(a0,d0.l),5(a1,d0.l)
		move.b	6(a0,d0.l),6(a1,d0.l)
		move.b	-10(a0,d0.l),-10(a1,d0.l)
		move.b	-9(a0,d0.l),-9(a1,d0.l)
		move.b	-8(a0,d0.l),-8(a1,d0.l)
		move.b	-7(a0,d0.l),-7(a1,d0.l)
		move.b	-6(a0,d0.l),-6(a1,d0.l)
		move.b	-5(a0,d0.l),-5(a1,d0.l)
		move.b	-4(a0,d0.l),-4(a1,d0.l)
		move.b	-3(a0,d0.l),-3(a1,d0.l)
		move.b	-2(a0,d0.l),-2(a1,d0.l)
		tst.l	d0
		beq	putscin
		sub.w	#21,d0
		jmp	scrollmore
putscin		moveq	#5,d0
		lea	(sco1).l,a1
nfpsc		move.b	(a1,d0.l),(a0,d0.l)
		add.b	#$30,(a0,d0.l)
		dbra	d0,nfpsc
		moveq	#8,d0			Place full stops in place.
.morestops	move.b	#'A',-10(a0,d0.l)
		dbra	d0,.morestops
		move.l	a0,namepospos
		st	d7
		rts
		
namepospos	dc.l	0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

contmess	dc.w	0,100
		dc.b	'  CONTINUES ... '
conts2		dc.b	'3',$ff
contn		dc.w	144,130
		dc.b	'4',$ff

DOCONT		cmp.b	#'0',conts2
		beq	.nocont

		move.l	#bye,inaframe+2
		move.b	#'9',contn+4
		move.w	#$5000,bplam+2

		move.l	#titlecop,$dff080
		clr.w	$dff088

		lea	$60000,a0	
		move.w	#$d000/4,d0
.moreclr	clr.l	(a0)+
		dbra	d0,.moreclr
		move.b	#$81,startblink+1

		lea	titlebpls,a0
		move.l	#$60000,d1
		move.l	#(40*256),d2
		bsr	setplanes
	
		move.l	mem,a0
		add.l	#55012,a0
		lea	$6d000,a1
		jsr	dechomp
		move.l	#$6d000,chrplace

		move.l	#$60000,wrkplanes
		lea	contmess,a4		Print mess.
		jsr	printmess
		lea	contn,a4		
		jsr	printmess

		move.w	#$5,$60000+(40*256*5)
		clr.w	$60000+(40*256*5)+32
		move.l	#$03300663,$60000+(40*256*5)+34	Set text pal.
		move.l	#$07740ac4,$60000+(40*256*5)+38
		move.l	#$0ce80eee,$60000+(40*256*5)+42
		move.l	#$0dd00cc0,$60000+(40*256*5)+46
		move.l	#$0bb00aa0,$60000+(40*256*5)+50
		move.l	#$09900770,$60000+(40*256*5)+54
		move.l	#$05500330,$60000+(40*256*5)+58

      		lea	titlecols+2,a0		Display hisco scr.
		lea	$60000+(40*256*5),a1
		move.w	#32-1,a4
		jsr	fadein

		move.w	#50,d5
.wait		jsr	synchup
		move.w	d5,-(sp)
		lea	$60000+(130*40)+18,a0
		moveq	#15,d0
.morex22	clr.w	(a0)
		clr.w	(256*40)(a0)
		clr.w	(256*40*2)(a0)
		clr.w	(256*40*3)(a0)
		add.l	#(256*40*4),a0
		clr.w	(a0)
		sub.l	#(256*40*4),a0
		add.w	#40,a0
		dbra	d0,.morex22
		lea	contn,a4		
		jsr	printmess
		move.w	(sp)+,d5
		btst.b	#7,$bfe001
		beq	.okay

		cmp.w	#8,d5
		bmi	.done
     		tst.l	up
		beq	.done
		move.w	#8,d5
.done
		dbra	d5,.wait

		move.w	#50,d5
		cmp.b	#'0',contn+4
		beq	.okd
		subq.b	#1,contn+4
		bra	.wait
		
.okd	 	lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
.nocont		bsr	checksco
		bra	gototitle

.okay	 	lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross
		move.l	#5,lives
		subq.b	#1,conts2
		bra	restart

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CHECKSCO	move.l	#bye,inaframe+2

		move.w	#$5000,bplam+2
		move.b	#$81,startblink+1

		jsr	placesco
		tst.b	d7
		beq	safebye

		move.b	#'2',dno1
		jsr	correctdisk

		move.l	namepospos,a0
		lea	namestuff+1,a1
		moveq	#19-1,d0
.more		move.b	-12(a0,d0.w),(a1,d0.w)
		dbra	d0,.more

	      	lea	name,a0
		lea	namestuff,a1
		move.w	#9-1,d0
.mo2		move.b	(a0,d0.w),3(a1,d0.w)
		dbra	d0,.mo2

		clr.b	dir_valid		Start disk drive.
		jsr	initialise
		jsr	startdrv
		lea	hiscon,a0		Hiscore screen.
 		lea	$60000,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file

		lea	musiccompn,a0		Load music.
 		lea	$53000,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		jsr	read_file

		jsr	stopdrv

		; PLAY THE TUNE ...
	
		move.b	#1,tempocnt
		move.b	#$7f,temposp	
		moveq	#0,d0		Set up player spec.
		moveq	#0,d1		Interrupt type (not used on selfcall).
		moveq	#0,d2		0=Selfcall/1=AutoCall.
		moveq	#0,d3		0=Pal/1=Ntsc.
		jsr	protracker
		move.l	a0,-(sp)
		moveq	#1,d0		Tell protracker where the mod is.
		lea	$53000,a0
		jsr	protracker
		moveq	#4,d0		Set maximum vol.
		moveq	#64,d1		Vol (0-64).
		jsr	protracker
		moveq	#8,d0		Make sure all the cannels are on.
		moveq	#0,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#1,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#2,d1
		jsr	protracker
		moveq	#8,d0
		moveq	#3,d1
		jsr	protracker
		move.l	(sp)+,musicplace Addr to call on interrupt.
.skipmusic

		if	protection = 1
		lea	codes,a0
		moveq	#0,d0
		move.w	#((lastc-codes)/4)-1,d1
.more23		move.l	(a0)+,d2
		add.l	d2,d0
		dbra	d1,.more23
		cmp.l	#codesum,d0
		bne	kill1
		endif

		lea	titlebpls,a0
		move.l	#$60000,d1
		move.l	#(40*256),d2
		bsr	setplanes
	
		move.l	#titlecop,$dff080
		clr.w	$dff088
		lea	$60000,a0		Dechomp hisco screen.
		lea	$60000,a1
		jsr	dechomp
		lea	$60000+(256*40*4),a0	Copy pal.
		lea	$60000+(256*40*5),a1
		move.w	#32-1,d0
.moret		move.w	(a0)+,(a1)+
		dbra	d0,.moret
		lea	$60000+(256*40*4),a1	Blank next plane.
		move.w	#((256*40)/4)-1,d0
.moreclr	clr.l	(a1)+
		dbra	d0,.moreclr

		lea	$60000+(149*40)+6,a0
		lea	$70000,a1
		moveq	#26-1,d1      
.morey		moveq	#10-1,d0
.morex		move.w	(a0),(a1)+
		move.w	(256*40)(a0),(a1)+
		move.w	(256*40*2)(a0),(a1)+
		move.w	(256*40*3)(a0),(a1)+
		addq.w	#2,a0
		dbra	d0,.morex
		add.w	#(20-10)*2,a0
		dbra	d1,.morey

		move.l	#titlecop,$dff080
		clr.w	$dff088

		move.l	mem,a0
		add.l	#55012,a0
		lea	$6d000,a1
		jsr	dechomp
		move.l	#$6d000,chrplace

		move.l	#$60000,wrkplanes
		lea	enternamemess,a4	Print mess.
		jsr	printmess
		lea	enm2,a4		
		jsr	printmess

		clr.w	$60000+(40*256*5)+32
		move.l	#$03300663,$60000+(40*256*5)+34	Set text pal.
		move.l	#$07740ac4,$60000+(40*256*5)+38
		move.l	#$0ce80eee,$60000+(40*256*5)+42
		move.l	#$0dd00cc0,$60000+(40*256*5)+46
		move.l	#$0bb00aa0,$60000+(40*256*5)+50
		move.l	#$09900770,$60000+(40*256*5)+54
		move.l	#$05500330,$60000+(40*256*5)+58

		clr.w	pointermess
		move.l	#$60000,wrkplanes
		lea	pointermess,a4
		jsr	printmess

		cmp.l	#ten,namepospos
		bmi	.no10
     		lea	tenmess,a4
		jsr	printmess
.no10
      		lea	titlecols+2,a0		Display hisco scr.
		lea	$60000+(40*256*5),a1
		move.w	#32-1,a4
		jsr	fadein

		clr.b	key
		clr.w	namepos
		jsr	synchup

.wait		

      		lea	namemess,a4		Print name.
		move.l	#$60000,wrkplanes
		jsr	printmess
   		cmp.w	#9,namepos
		beq	.nodis
		move.w	namepos,d0
		lsl.w	#4,d0
		move.w	d0,pointermess
		lea	pointermess,a4
		jsr	printmess
.nodis

		jsr	entername

.waitforit	move.w	$dff004,d0
		btst	#0,d0
		beq	.waitforit

		lea	$60000+(149*40)+6,a0	Put background on.
		lea	$70000,a1
		moveq	#26-1,d1      
.morey2		moveq	#10-1,d0
.morex22	move.w	(a1)+,(a0)
		move.w	(a1)+,(256*40)(a0)
		move.w	(a1)+,(256*40*2)(a0)
		move.w	(a1)+,(256*40*3)(a0)
		add.l	#(256*40*4),a0
		clr.w	(a0)
		sub.l	#(256*40*4),a0
		addq.w	#2,a0
		dbra	d0,.morex22
		add.w	#(20-10)*2,a0
		dbra	d1,.morey2



		cmp.b	#$44,key
		beq	.enterit

		btst.b	#7,$bfe001
		bne	.wait

.enterit      	move.l	#name,a0
		move.l	namepospos,a1
		move.w	#9-1,d0
.mo		move.b	(a0,d0.w),-10(a1,d0.w)
		dbra	d0,.mo

	 	lea	titlecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		jsr	fadeacross

		clr.l	musicplace
		move.w	#$f,$dff096

		rts

tenmess		dc.w	8,70,0,0
		dc.b	'10',$ff
pointermess	dc.w	0,66
		dc.b	0,0,0,0,0,'   '
		dc.b	'=',$ff		
enternamemess	dc.w	0,70
		dc.b	'    PLEASE ENTER',0,0,0,0
namestuff	dc.b 	' 8 AAAAAAAAA 003000 ',$ff

enm2		dc.w	8,70
		dc.b	0
		dc.b	'     YOUR NAME',0,$ff

namemess	dc.w	0,70
		dc.b	0,0,0,0,'   '
name		dc.b	'          ',$ff		
		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

entername	moveq	#0,d0
		move.b	key,d0

		cmp.b	#$45,d0
		bne	.ok	
		clr.b	key
		cmp.w	#9,namepos
		beq	safebye
		move.l	#name,a1
		move.w	namepos,d1
		move.b	#'+',(a1,d1.w)
		addq.w	#1,namepos
		rts
.ok		
		cmp.b	#$44,d0
		beq	safebye

		cmp.w	#$42,d0
		bpl	safebye
		
		clr.b	key
		cmp.b	#$41,d0
		bne	.nodel
		tst.w	namepos
		beq	safebye
		move.l	#name,a0
		move.w	namepos,d0
		subq.w	#1,namepos
		move.b	#32,(a0,d0.w)
		move.b	#32,-1(a0,d0.w)
		rts
.nodel		
		cmp.w	#9,namepos
		beq	safebye
		move.l	#keycodes,a0
		tst.b	(a0,d0.w)
		beq	safebye
		move.l	#name,a1
		move.w	namepos,d1
		move.b	(a0,d0.w),(a1,d1.w)
		addq.w	#1,namepos
		rts

namepos		dc.w	0

	
keycodes	dc.b	0,'1','2','3'
		dc.b	'4','5','6','7'
		dc.b	'8','9','0',0
		dc.b	0,0,0,0

		dc.b	$51
		dc.b	$57
		dc.b	$45
		dc.b	$52
		dc.b	$54
		dc.b	$59
		dc.b	$55
		dc.b	$49
		dc.b	$4f
		dc.b	$50
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0

		dc.b	$41
		dc.b	$53
		dc.b	$44
		dc.b	$46
		dc.b	$47
		dc.b	$48
		dc.b	$4a
		dc.b	$4b
		dc.b	$4c
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0

		dc.b	0
		dc.b	$5a
		dc.b	$58
		dc.b	$43
		dc.b	$56
		dc.b	$42
		dc.b	$4e
		dc.b	$4d
		dc.b	0
		dc.b	'.'
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	0
		dc.b	' '
		dc.b	0

		ds.b	50
		even
		
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

LOADAREA	; Load up the correct area and set starting variables.

		clr.b	aldone
		move.w	worldno+2,-(sp)
		move.l	worldno,d0
		lea	worldnos,a0
		move.b	(a0,d0.w),worldno+3
		bsr	.load
     		move.w	(sp)+,worldno+2
		rts

.load		lea	sprpal,a0		Choose correct spr cols.
		cmp.b	#sln,worldno+3
		bne	.lk
   		lea	sprpal2,a0
.lk		lea	sppl,a1
		move.w	#16-1,d0
.lk2		move.w	(a0)+,(a1)+
		dbra	d0,.lk2

		move.b	#20,onealevel		Record bonuses per level ...

		jsr	discomplete

		move.l	worldno,d0
		lsl.w	#2,d0	
		lea	allists,a0
		move.l	(a0,d0.w),alienlist
    	
		lea	detecs,a0
		move.l	worldno,d1
		add.w	d1,d1		
		add.w	d1,d1	 
		move.l	(a0,d1.w),detecplace	

		lea	areastarts,a0
		move.l	worldno,d1
		move.b	d1,levworldn
		add.b	#$31,levworldn
		cmp.b	#sln5,worldno+3
		bne	.km
  		move.b	#'1',levworldn
.km		mulu	#areastarts2-areastarts,d1
		add.w	d1,a0
		move.l	areano,d0
		mulu	#a2s-areastarts,d0
		move.l	(a0,d0.w),restarthere
		move.l	4(a0,d0.w),restarthere+4
		move.l	8(a0,d0.w),restarthere+12
		move.w	12(a0,d0.w),restarthere+8
		move.w	14(a0,d0.w),restarthere+10
		move.l	16(a0,d0.w),arealenght+2
		move.b	20(a0,d0.w),mapno
		move.b	21(a0,d0.w),mapno+1
		moveq	#0,d1
		move.w	22(a0,d0.w),d1
 
		move.l	24(a0,d0.w),a4
		move.l	28(a0,d0.w),(a1any-a1)(a4)
		move.l	32(a0,d0.w),(a1any-a1)+4(a4)
		move.l	36(a0,d0.w),a4
		move.l	40(a0,d0.w),(a1any-a1)(a4)
		move.l	44(a0,d0.w),(a1any-a1)+4(a4)
		move.l	48(a0,d0.w),a4
		move.l	52(a0,d0.w),(a1any-a1)(a4)
		move.l	56(a0,d0.w),(a1any-a1)+4(a4)
		move.l	60(a0,d0.w),a4
		move.l	64(a0,d0.w),(a1any-a1)(a4)
		move.l	68(a0,d0.w),(a1any-a1)+4(a4)
		move.w	72(a0,d0.w),endx+2
		move.w	74(a0,d0.w),endy+2

		move.l	d1,d3
		lsr.l	#1,d3
		mulu	#12,d3
		move.l	d3,smoda+2

		move.w	d1,smod1+2
		move.l	d1,d2
		lsl.l	#1,d2
		addq.w	#2,d2
		move.w	d1,d2
		lsr.l	#1,d2
		move.w	d1,smod7+2
		move.w	d2,smod8+2
		move.w	d2,smod9+2
		move.l	d1,d3
		mulu	#21,d3
		move.l	d3,smods1+2
		move.l	#18,d4
		sub.l	d1,d4
		move.l	d1,d5
		mulu	#22,d5
		move.w	d1,smod14+2
		move.l	d1,mht
		move.l	d1,d2
		mulu	#21,d2
		addq.l	#1,d2
		move.l	d1,d2
		mulu	#22,d2
		add.l	#$12,d2

		move.l	d1,d2
		add.l	d1,d2			Start at 2*mh

		clr.b	dir_valid		Start disk drive.
		jsr	initialise
		jsr	startdrv
		lea	mapname,a0
 		move.l	maps,a1
  		moveq	#-1,d0
  		moveq	#0,d1
		if	diskmode=1
		jsr	read_file
		move.l	a1,a0
		jsr	dechomp
		endif
		clr.l	collectables		Calculate collectables.
		move.l	maps,a0
		move.l	detecplace,a1
		move.l	#40000,d7
.nextcol	moveq	#0,d1
		move.b	(a0)+,d1
		add.w	d1,d1	
		tst.b	(a1,d1.w)
		bne	.noadd
		btst.b	#2,1(a1,d1.w)
	 	beq	.noadd
      		addq.l	#1,collectables
.noadd		dbra	d7,.nextcol   
		clr.l	collected
		addq.l	#1,collectables
		move.l	collectables,d0
		mulu	#2,d0
		divu	#3,d0
		move.w	d0,collectables+2
      		addq.l	#1,collectables
		clr.w	collectables

		moveq	#0,d0			Set begining time ...
		move.w	diffuculty,d0
		mulu	#100,d0
		add.w	#350,d0
		move.l	d0,timeleft

		move.l	worldno,d0
		cmp.l	lastworld,d0
		beq	.nomoreload
		move.l	worldno,lastworld

		move.b	worldno+3,blockno
		add.b	#$31,blockno
		lea	blocksname,a0		Load title.
 		lea	blocks,a1
  		moveq	#-1,d0
 		moveq	#0,d1
		if	diskmode=1
		jsr	read_file
		move.l	a1,a0	
		jsr	dechomp
		endif

		lea	sam0,a0
.mc		clr.l	(a0)
		add.w	#sam1-sam0,a0
		cmp.l	#sam12,a0
		bmi	.mc

		move.l	loadspace,topmem	Temp till we get memory back.
		move.l	loadspace,a2
		move.l	worldno,d0
		lsl.w	#2,d0
		lea	worldblobs,a0
		move.l	(a0,d0.w),a0
.nextblob	move.l	(a0),d0
		cmp.l	#-1,d0
		beq	.nomoreblobs
		lea	thingstodo,a1
.nextcheck	cmp.l	#-1,(a1)
		bne	.noerror
.red		move.w	#$ff0,$dff180
		illegal
		jmp	.red
.noerror	cmp.l	(a1),d0
		beq	.loadthis
	 	add.l	#ttd2-thingstodo,a1
		jmp	.nextcheck
.loadthis
		cmp.l	#sam0,(a1)  	Is it a sample ...
		bmi	.thisno
		tst.w	musicno
		bne	.nomoreblobs
.thisno		movem.l	a0-a2,-(sp)
		move.l	(a1),a0		Store address for printer.
		move.l	a2,(a0)
		move.l	4(a1),a0	Set name for load.
		move.l	a2,a1
		moveq	#-1,d0		Set load lenght.
		moveq	#0,d1		Set attribute lenght.
		jsr	read_file
		movem.l	(sp)+,a0-a2
		move.l	4(a1),a5	Dechomp it (check name) ...
.nex		cmp.b	#'.',(a5)
		beq	.ch
   		addq.w	#1,a5
		jmp	.nex
.ch 		cmp.b	#'c',1(a5)
		bne	.no
   		cmp.b	#'h',2(a5)
		bne	.no
		pushall
		move.l	a2,a1
		move.l	a1,a0
		jsr	dechomp
		pullall
.no		move.l	8(a1),d0
		add.l	d0,a2
		addq.w	#4,a0
		jmp	.nextblob
.nomoreblobs	move.l	a2,topmem
.nomoreload	jsr	stopdrv			Stop disk drive.

		lea	blocks+2,a0		Setup game colours.
		lea	gamepal,a1
		move.w	#15-1,d0
.morepal	move.w	(a0)+,(a1)+
		dbra	d0,.morepal

		move.l	maps,a0
		move.l	maps,d0
		move.l	(a0),mapdata		Setup map places.
		add.l	d0,mapdata
		move.l	4(a0),alienmap
		add.l	d0,alienmap
		move.l	8(a0),almapxs
		add.l	d0,almapxs

		move.l	alienmap,a0		Clear all flags on almap.
		move.w	#10000-1,d0		Use map size here.
.morebclr	bclr.b	#7,(a0)+
		dbra	d0,.morebclr

		lea	gamecols+2,a0
		lea	gamepal,a1
		move.w	#31-1,d0
.moret		move.w	(a1)+,(a0)+
		addq.w	#2,a0
		dbra	d0,.moret

		jsr	clearcomplete

		if	protection = 1
		lea	codes,a0
		moveq	#0,d0
		move.w	#((lastc-codes)/4)-1,d1
.more23		move.l	(a0)+,d2
		add.l	d2,d0
		dbra	d1,.more23
		cmp.l	#codesum,d0
		bne	.kill1
		endif

		rts
.kill1		lea	resetgamevars,a0
.lw		clr.l	(a0)+
		jmp	.lw

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

RESETGAMEVARS	; Reset lives,score,etc...	

		clr.b	actualzool
		clr.b	donetune4
		clr.l	collectedst
		move.b	#6,one1
		move.b	#6,one2
		move.b	#6,one3
		move.b	#5,weapon
		move.l	#-1,lastscore		Reset game stuff.
		clr.b	donebonus
		clr.l	score
		move.l	#5,lives
		move.l	#4545454,lastlives
		move.b	#-1,lastep
		move.b	#3,energylevel
		st.b	magicpmem	
		st	lastworld
 		clr.l	worldno
		move.l	#swn,worldno
		clr.l	areano
		move.l	#swn2,areano
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

RESETVARS	; Reset all map/scroll stuff.

		clr.l	clearlist1
		clr.l	clearlist2

		move.b	#5,weapon
		cmp.l	#150,timeleft
		bpl	.okoke
		move.l	#150,timeleft
.okoke		move.l	#4545454,lastlives
		move.b	#-1,lastep
		move.l	#-1,timeleftlast

		cmp.b	#sln1,worldno+3		Replace floor ...
		bne	.jknk
   		cmp.b	#2,areano+3
		bne	.jknk
		move.l	mem,a0
		add.l	#($42*200)+$1c,a0
		move.w	#15,d0
.klm		move.b	#71,(a0)
		add.w	#200,a0
		dbra	d0,.klm
.jknk
		move.b	#10,tcc
		clr.l	xhitmom	
		clr.l	yhitmom	
		clr.l	scrollpos
		st.b	lastpercent
		clr.b	doingintro
		clr.b	doingintro2

		clr.l	skyplmem
		clr.b	bellm
		clr.b	trumpeton
		clr.b	onnl
		move.w	#2,scrollspeed

		clr.l	chainspr
		clr.b	chain1

		st.b	lastep

		clr.w	punching
		clr.b	ducking
		clr.b	icing
		clr.b	jumping
		clr.b	flatice
		clr.b	skid
		clr.b	clinging
		clr.b	spining
		st.b	magicpmem

		clr.w	skyy
		move.l	restarthere+12,skypl

		move.l	restarthere,mapx
		move.l	restarthere+4,mapy
		move.w	restarthere+8,zoolx
		move.w	restarthere+10,zooly
		clr.b	scrx
		clr.w	scry
		clr.w	tscrx
		clr.l	xmom
		clr.l	ymom
		clr.l	scrxmom
		clr.l	scrymom
		st.b	zoold

		; Reset all sprite stuff.

		moveq	#8-1,d0
		lea	bulletsprs,a0
.n		clr.l	(a0)+
		dbra	d0,.n

		lea	sprites,a1		Clear sprite data 1.
		lea	freesprlist,a0
.moresprs	move.l	a1,(a0)+
		move.l	#$ffffffff,nsp(a1)
		move.l	#$ffffffff,psp(a1)
		clr.b	t(a1)
		add.w	#sds,a1
		cmp.l	#eol+1,a0
		bmi	.moresprs
		move.l	#eol,freesprpointer
		move.l	#$ffffffff,firstsprite
		move.l	#$ffffffff,lastsprite

		clr.w	splitpower

		move.l	mapdata,a0		Reset falling plats.
		move.l	alienmap,d0
.morers		cmp.b	#sln3,worldno+3
		beq	.n247
		cmp.b	#247,(a0)
		bne	.n247
		st.b	(a0)
		jmp	.n2
.n247		cmp.b	#$fc,(a0)
		bne	.n2
  		move.b	#242,(a0)
.n2		cmp.b	#sln1,worldno+3
		bne	.kmd
    		cmp.b	#249,(a0)
		bne	.kmd
    		st.b	(a0)
.kmd		cmp.b	#sln5,worldno+3
		bne	.okl
		cmp.b	#242,(a0)
		bne	.okl
		st.b	(a0)
.okl		addq.w	#1,a0
		cmp.l	a0,d0
		bpl	.morers

bye6		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PAUSEMODE	cmp.b	#$19,key
		bne	bye6
		st.b	paused
.wait		btst	#7,key
		beq	.wait
.waitmore	cmp.b	#$10,key
		beq	.endgame
		cmp.b	#$19,key
		bne	.waitmore
.waitsomemore	btst	#7,key
		beq	.waitsomemore
		jsr	synchup
		clr.b	paused
		rts
.endgame	move.l	#blankcop,$dff080
		move.l	#bye,inaframe+2
		clr.l	musicplace
		move.w	#$f,$dff096
		clr.l	ch1timer
		clr.l	forcesample1
	
		clr.b	paused
		jsr	synchup
		
		jmp	gototitle

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

JUMPER		cmp.b	#sln,worldno+3
		beq	bye
		tst.b	magic
		bne	bye
		tst.w	outofit
		bne	bye

		tst.w	jumppower
		beq	.okokokok
	 	subq.w	#1,jumppower
.okokokok
		tst.b	onrails			Hanging from bars.
		beq	.noton

		move.w	#$52,zooll		Monkey animation.
		tst.b	zoold
		bne	.rightdir
		move.w	#$57,zooll
.rightdir	tst.w	left
		bne	.doinglr
		move.b	#$16,zambo
		jmp	.nores
.doinglr	subq.b	#$1,zambo
		bpl	.nores
		move.b	#31,zambo
.nores		move.b	zambo,d0
		eori.b	#$1f,d0
		lsr.b	#$3,d0
		add.b	d0,zooll+1

		tst.b	bellm
		bne	.bi		
		tst.w	xlift
		beq	.nobi
		tst.b	right
		bne	.set		
    		tst.b	left
		beq	.noset
		clr.b	zoold
		jmp	.noset
.set		st.b	zoold
.noset		tst.b	down
		bne	.drop
		jmp	.bi
.nobi		tst.b	up
		beq	.notpushingup
.bi	     	move.b	#$56,zooll+1
		tst.w	zoold
		bne	.nodrop
		move.b	#$5b,zooll+1
		jmp	.nodrop
.notpushingup

		move.w	zoolwx,d0
		add.w	#12,d0
		tst.b	left
		beq	.nlb
    		subq.w	#2,zoolx
		clr.b	zoold
.nlb		tst.b	right
		beq	.nrb
		add.w	#16,d0
    		addq.w	#2,zoolx
		st.b	zoold
.nrb		tst.b	down
		bne	.drop
		tst.b	onnl
		bne	.nodrop
		move.w	zoolwy,d1		Check zool head.
		sub.w	#4,d1
		jsr	checkblock
		btst.b	#$7,1(a1,d3.w)		Check for monkey bars.
		bne	.nodrop
.drop		clr.b	onrails
		clr.w	xlift
		move.l	#$20000,ymom
		move.l	#2,d0
		jsr	playsample
.nodrop		rts
.noton

		tst.b	clinging		Cling to a wall ?
		beq	.notclinging
	    	clr.b	jumping
		clr.b	spining
		tst.b	zoold
		beq	.lft
		tst.b	left
		beq	bye
		move.l	#-$10000,xmom
		jmp	.off
.lft		tst.b	right
		beq	bye
		move.l	#$10000,xmom
.off		clr.b	clinging
		jmp	.jump
.notclinging

		jsr	gravity		
      		clr.b	flatice			Clear ground vars.
		clr.b	icing
		clr.b	stonlift
		clr.w	lrmust

		tst.l	ymom			Check up or down.
		bpl	.notupcheck
		move.w	zoolwx,d0		Check zool head.
		add.w	#12,d0
		move.w	zoolwy,d1
		sub.w	#4,d1
		jsr	checkblock
		add.l	d2,a0
		add.l	d3,a1
		move.l	a0,a4
		move.l	a1,a5
		add.w	#16,d0
		jsr	checkblock
		cmp.b	#$fc,(a4)		Pass through lifts.
		beq	.spin
		cmp.b	#$fc,(a0,d2.l)
		beq	.spin
		tst.b	zoold	      		Check for monkey bars.
		bne	.that
     		btst.b	#$7,1(a5)
		beq	.sp
		jmp	.geton
.that		btst.b	#$7,1(a1,d3.w)
		beq	.sp
.geton		st.b	onrails
		clr.l	xmom
		clr.l	ymom
		add.w	#12,zoolwy
		andi.w	#$fff0,zoolwy
		move.w	zoolwy,d0
		sub.w	scrwy,d0
		add.w	#44,d0
		move.w	d0,zooly
		jmp	createycoords
.sp		btst.b	#0,1(a5)			Hut ?
		bne	.huthead
		btst.b	#0,1(a1,d3.w)
		beq	.spin
.huthead	move.l	#$20000,ymom
		move.l	#2,d0
		jsr	playsample

		jmp	.spin
.notupcheck
		tst.b	onstick
		bne	.onfloor
		tst.w	xlift
		bne	.onfloor

		tst.b	zoolhut	 		Check ground.
		beq	.clch
     		jsr	checkdown
		jmp	.cont
.clch		jsr	checkdown
		tst.b	zoolhut			Just been hut ?
		bne	.spin
.cont		tst.b	d0
		beq	.spin

.onfloor	tst.b	silly			Smily cheat ?
		beq	.noff
		tst.l	xmom
		bne	.sks
		tst.w	sillyani
		bne	.sks
     		move.w	#13,sillyani
		jmp	.sks
.noff

		clr.b	mustup

		tst.b	zoolhut			If on ground - not hut.
		beq	.ok
   		clr.b	zoolhut
		clr.l	xmom
.ok
		clr.b	spining			Clear jump vars.
		clr.b	jumping
		clr.b	upjump

		tst.w	punching
		bne	.notducked
		tst.b	icing			Ducking ...
		bne	.noduck
		tst.l	xmom
		bne	.notducked
		tst.b	down
		beq	.noduck
		cmp.b	#3,ducking
		beq	.notducked
		addq.b	#1,ducking
		jmp	.notducked
.noduck		tst.b	ducking
		beq	.notducked
		subq.b	#1,ducking
.notducked

 ;		tst.b	up			BETTER NOW ?
;		bne	.sks
    		clr.b	upheld
.sks
		clr.l	ymom

		tst.b	up 			Does Zool want to jump ?
		beq	bye
		tst.b	ducking
		bne	bye
		tst.w	punching
		bne	bye
		tst.b	upheld			Released up yet ?
		bne	bye
.jump		
		tst.b	tilljump
		bne	bye

		move.l	#2,d0
		jsr	playsample

		move.l	#-$68000,ymom
		tst.w	jumppower
		beq	.nopower
		sub.w	#$2,ymom
.nopower	move.b	#20,uptime
		tst.b	silly
		beq	.nomore
       		move.b	#20,uptime
.nomore		st.b	upheld
		st.b	upjump
		st.b	jumping			Set flag.
		rts

.spin		clr.b	onslope
	
		st.b	jumping			Set flag.
		tst.b	upjump
		beq	bye
		cmp.b	#4,fireheld
		bne	.noabove
		tst.b	canspin
		bne	bye
;		move.b	#$60,canspin
		st.b	spining
		rts
.noabove	cmp.b	#5,fireheld
		bmi	bye
		move.b	#5,fireheld
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

GRAVITY		cmp.w	#8,ymom			Gravity.
		beq	.noadd
		tst.b	uptime			Holding up for bigger jump ?
		beq	.noup2
		tst.b	mustup
		bne	.uping
		tst.b	up
		beq	.noup	
.uping		subq.b	#1,uptime
		add.l	#$3800,ymom
		jmp	.noadd
.noup		clr.b	uptime
.noup2		cmp.w	#8,ymom
		beq	.noadd
		add.l	#$7800,ymom
.noadd		move.w	ymom,d0			Move zool.
		add.w	d0,zooly
		jmp	createycoords

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

INAFRAMESTUFF	; All the stuff that has to be done in a frame.

		jsr	playsamples
		jsr	forcesamples
	
		tst.b	paused
		bne	.paused

		tst.b	frameno
		bne	.notframe0
		jsr 	setscrollstuff		Top of frame stuff.
		jsr	timersandstuff
		jsr	placezool
		jmp	.notframe1
.notframe0
		move.b	zoold+1,-(sp)
		move.w	scry,-(sp)
		move.w	scrx,-(sp)
		move.l	scrollpos,-(sp)
		move.w	zoolx,-(sp)
		move.w	zooly,-(sp)
		move.l	skypl,-(sp)
		move.w	skyy,-(sp)

		move.b	zoold1,zoold+1
		move.l	skypl1,skypl
		move.w	skyy1,skyy
		move.w	scry1,scry
		move.b	scrx1,scrx
		move.l	scrollpos1,scrollpos
		move.w	zoolx1,zoolx
		move.w	zooly1,zooly
		jsr 	setscrollstuff		Top of frame stuff.
		jsr	placezool
		move.w	(sp)+,skyy
		move.l	(sp)+,skypl
		move.w	(sp)+,zooly
		move.w	(sp)+,zoolx
		move.l	(sp)+,scrollpos
		move.w	(sp)+,scrx
		move.w	(sp)+,scry
		move.b	(sp)+,zoold+1
.notframe1
		st.b	frameno
.paused	
		jmp	joytest			Poll the joystick.


******************************************************************************
*						                             *
*      	                  ALIEN CREATION ROUTINES	                     *
*						                             *
******************************************************************************
	
CREATESCREENALS	; Setup a screen full of aliens.

		moveq	#14,d6
		jsr	calcalplace		Set a6 = alienmap place.
		move.l	mht,d0
		sub.l	d0,a4			sub.l	#(mh/2)*2,a4

.nextcol	moveq	#10,d7
		move.l	d6,-(sp)
		move.l	a4,-(sp)

.nextcheck	jsr	createal
		add.l	#1,a4

		dbra	d7,.nextcheck

		move.l	(sp)+,a4
		move.l	(sp)+,d6
	
		move.l	mht,d0
		lsr.l	#1,d0
		add.l	d0,a4
	
		dbra	d6,.nextcol
		
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CALCALPLACE	move.l	mapx,d0
		move.l	mht,d2
		divu	d2,d0

		and.l	#$ffff,d0
		lsr.l	#1,d0
		lsr.l	#1,d2
		mulu	d2,d0
		move.l	mapy,d1
		lsr.l	#1,d1
		add.l	d1,d0
		move.l	d0,a4
		add.l	alienmap,a4

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CREATEUPALS	cmp.b	#sln,worldno+3
		beq	bye

		moveq	#14,d7
		jsr	calcalplace		Set a6 = alienmap place.
		move.l	mht,d0
		andi.l	#$fffffffe,d0
		sub.l	d0,a4			sub.l	#(mh/2)*2,a4
		subq.w	#2,a4			1

.nextcheck	jsr	createal
		move.l	mht,d0
		lsr.l	#1,d0
		add.l	d0,a4

		dbra	d7,.nextcheck

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
             
CREATEDOWNALS	cmp.b	#sln,worldno+3
		beq	bye

		moveq	#14,d7
		jsr	calcalplace		Set a6 = alienmap place.
		move.l	mht,d0
		andi.l	#$fffffffe,d0
		sub.l	d0,a4			sub.l	#(mh/2)*2,a4
		add.w	#9,a4

.nextcheck	jsr	createal
		move.l	mht,d0
		lsr.l	#1,d0
		add.l	d0,a4

		dbra	d7,.nextcheck
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CREATELEFTALS	moveq	#13,d7
		jsr	calcalplace		Set a6 = alienmap place.
		move.l	mht,d0
		sub.l	d0,a4			sub.l	#(mh/2)*2,a4
		subq.w	#2,a4			1

.nextcheck	jsr	createal
		add.l	#1,a4

		dbra	d7,.nextcheck

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CREATERIGHTALS	cmp.b	#sln,worldno+3
		bne	.normal

		moveq	#(8*3)-1,d7			Do the hole right side.

		move.l	mapx,d0
		move.l	mht,d2
		divu	d2,d0
		and.l	#$ffff,d0
		lsr.l	#1,d0
		lsr.l	#1,d2
		mulu	d2,d0
		move.l	d0,a4
		add.l	alienmap,a4

		add.l	smoda+2,a4

.nextcheck2	jsr	createal
		add.l	#1,a4

		dbra	d7,.nextcheck2

		rts

.normal		moveq	#13,d7			Normal scroll alien setup.
		jsr	calcalplace		Set a6 = alienmap place.
smoda		add.l	#(mh/2)*12,a4
		subq.w	#2,a4

.nextcheck	jsr	createal
		add.l	#1,a4

		dbra	d7,.nextcheck

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
		
CREATEAL	clr.l	startp
		move.l	#linkplaces,linkpl
		clr.l	linkplaces
		move.l	#0,a2			Create first alien if there.
		jsr	createfullal
.nextal		move.l	joinsp,joinsp2		Do all the linking stuff...
		move.l	linkpl,a5		Place sprite ad. on list.
		move.l	joinsp,(a5)
		clr.l	4(a5)
		addq.l	#4,linkpl
		cmp.l	#0,a2			Was last sprite linked ?
		beq	bye
		tst.l	12(a2)
		beq	bye
		move.l	a4,-(sp)		Create next sprite.
		move.l	a2,a4
		add.w	#12+3,a4
		move.l	#0,a2
		jsr	createit
		cmp.l	#-1,joinsp
		bne	.ok
.www		lea	linkplaces,a3		Free all obtained sprites ...
.wwww		tst.l	(a3)
		beq	safebye
		move.l	(a3)+,a6
		jsr	freeplease
		jmp	.wwww
.ok		move.l	(sp)+,a4
		move.l	joinsp2,a1
		move.l	joinsp,jsp(a1)
 		jmp	.nextal
 		rts

linkpl		dc.l	0
linkplaces	ds.l	20

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CREATEFULLAL	; Create an alien.
		
		move.b	(a4),d0			Clear space ?
		andi.b	#$3f,d0
		beq	bye

		btst	#7,(a4)			Is this alien active ?
		bne	bye

		move.l	a4,d0			Off the map ?
		sub.l	alienmap,d0
		tst.l	d0
		bmi	bye
		cmp.l	#10000,d0
		bpl	bye

		moveq	#0,d0			If collect to end then ...
		move.b	(a4),d0
		andi.b	#$3f,d0
		lsl.w	#2,d0
		move.l	alienlist,a1
		move.l	(a1,d0.w),a1
		cmp.b	#16,8(a1)
		beq	.yes
		cmp.b	#29,8(a1)
		bne	.normm
.yes		jsr	getbsprite
		bra.s	.rre
.normm		jsr	getsprite
.rre		move.l	a6,joinsp
		cmp.l	#-1,a6
		beq	bye

		bset	#7,(a4)			Flag the sprite as used.

		bra	creat

createit	jsr	getsprite		Choose a sprite.
		move.l	a6,joinsp
		cmp.l	#$ffffffff,a6		Did you get one ?
		beq	bye
		
creat		moveq	#0,d0	    		Find alien setup info.
		move.b	(a4),d0
		andi.b	#$3f,d0
		lsl.w	#2,d0
		move.l	alienlist,a1
		move.l	(a1,d0.w),a1		a1 = address of setup info.
		cmp.l	#0,a1
		bne	.gotit
      		jsr	freesprite
		rts
.gotit

		tst.l	startp
		beq	.first
		move.l	startp,x(a6)
		jmp	.wassecond
.first		move.l	a4,d0
		sub.l	alienmap,d0
		move.l	d0,d2
		move.l	mht,d3
		lsr.w	#1,d3
		divu	d3,d0		mh/2
		move.l	d0,d1
		lsl.w	#5,d0
		add.w	#$10,d0
		andi.w	#$fff0,d0
		move.w	d0,x(a6)

		swap	d1
		lsl.w	#5,d1
		add.w	#15,d1
		move.w	d1,y(a6)
		move.l	d2,d1
		andi.w	#7,d1
		lsr.l	#3,d2
		move.l	almapxs,a0
		add.w	d2,a0
		btst.b	d1,(a0)
		beq	.xclear
       		add.w	#16,x(a6)
.xclear		btst.b	#6,(a4)
		beq	.yclear
  		add.w	#16,y(a6)
.yclear		move.l	x(a6),startp
.wassecond
		move.w	(a1),d0			Add x offset.
		add.w	d0,x(a6)
		move.w	2(a1),d0		Add y offset.
		add.w	d0,y(a6)
		move.l	4(a1),blbset(a6)	Set blob set.
		move.b	8(a1),t(a6)		Set type number.
		move.b	9(a1),fr+1(a6)		Set looks.
		move.b	10(a1),htg(a6)		Set hits to go.
		move.b	11(a1),ht(a6)		Set height control.
		clr.b	flal(a6)
		move.l	a4,snp(a6)		Set flag address.
		
		move.l	a6,joinsp
		move.l	a1,a2

		move.w	#(sds-any)-1,d0
		add.w	#a1any-a1,a1
		add.w	#any,a6
.moresetup	move.b	(a1)+,(a6)+
		dbra	d0,.moresetup
		
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien1		;  Blanking square (also used by alien2).

		subq.b	#1,any(a6)
		bpl	nextalien
		jsr	freeplease
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien2		;  Smiley.
	
		subq.w	#3,y(a6)
		subq.b	#1,any(a6)
		bpl	nextalien
		jsr	freeplease
		tst.b	hunds
		beq	.hmm
    		subq.b	#1,hunds
.hmm		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien4		; Cloud.

		jsr	freesprite
		jmp	nextalien
				 
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien5		; Allsort.

		move.l	#175,scoid
		move.b	#23,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien

		tst.b	any(a6)			Pause and look about stuff.
		beq	.notpaused
		lea	allsortani,a0
		moveq	#0,d0
		move.b	any(a6),d0
		cmp.b	#-1,(a0,d0.w)
		bne	.itsaframe
.da		move.b	#4,fr+1(a6)
		jsr	mallsortb
		jmp	.subit
.itsaframe	move.b	(a0,d0.w),fr+1(a6)	
.skipfr		eori.b	#1,any+5(a6)
		beq	.frompaused
.subit	  	subq.b	#1,any(a6)
		tst.b	any(a6)
		bne	.frompaused
		move.b	any+4(a6),any+1(a6)
.notpaused
		
		subq.b	#1,any+3(a6)
		bpl	.nores
		move.b	#3,any+3(a6)
		subq.b	#1,any+2(a6)
		bpl	.nores
   		move.b	#3,any+2(a6)
.nores
		move.w	x(a6),d0  		Check with background.
		tst.b	any+1(a6)
		bpl	.rightdir
	 	sub.w	#18,d0
.rightdir	move.w	y(a6),d1
		add.w	#28,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.swap
		btst.b	#3,1(a1,d3.w)
		bne	.swap
    		btst.b	#0,1(a1,d3.w)
		bne	.swap
	 	add.w	#16,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.swap
    		btst.b	#3,1(a1,d3.w)
		bne	.noswap
    		btst.b	#0,1(a1,d3.w)
		bne	.noswap
.swap  		neg.b	any+1(a6)
.noswap

		tst.b	any+1(a6)		Move left and right.
		bpl	.right
		subq.w	#2,x(a6)
		addq.b	#1,any+1(a6)
		bne	.notstopped
		subq.b	#1,any+1(a6)
		jmp	.setnextdir
.right		addq.w	#2,x(a6)
		subq.b	#1,any+1(a6)
		bne	.notstopped
		addq.b	#1,any+1(a6)
.setnextdir   	move.b	#(allsortmoms-allsortani)-1,any(a6)
		jsr	random
		andi.b	#$3f,d0
		add.b	#$f,d0
		btst	#0,d0
		beq	.noneg
		neg.b	d0
.noneg		move.b	d0,any+4(a6)
.notstopped	
		move.b	any+2(a6),fr+1(a6)
.frompaused	tst.b	any+1(a6)
		bmi	.aniok
      		add.b	#9,fr+1(a6)
.aniok
		jmp	offandend

MALLSORTB	; Fire one bullet...

		move.l	a6,a0
		cmp.b	#24,t(a0)
		bne	.above
      		jsr	getlowersprite
		jmp	.hetit
.above		jsr	getsprite
.hetit		move.l	a6,a5
		cmp.l	#-1,a6
		beq	.end
		move.w	#$d0,sam3+4	
		move.w	#3,d0
		jsr	playsample
		move.w	x(a0),x(a6)
		add.w	#12,x(a6)
		move.w	y(a0),y(a6)
		add.w	#17,y(a6)
		move.b	#6,t(a6)
		clr.b	any+30(a6)
		move.l	#$7000,any+12(a6)
		jsr	random
		andi.w	#7,d0
		cmp.b	#5,d0
		bmi	.thisok
       		subq.b	#4,d0
.thisok		add.b	#18,d0
		move.b	d0,fr+1(a6)
		jsr	random
		andi.w	#$7,d0
		add.w	d0,d0
		add.w	d0,d0
		lea	allsortmoms,a1
		move.l	(a1,d0.w),any(a6)
		clr.w	any+4(a6)
		move.l	#allsort,blbset(a6)
		jsr	random
		andi.w	#$7,d0
		subq.w	#1,d1
		add.w	#50,d0
		move.w	d0,any+6(a6)
		move.b	t(a0),any+10(a6)
		tst.b	any+1(a0)
		bpl	.end
		neg.w	any(a6)
		sub.w	#13,x(a6)
.end		move.l	a0,a6
		rts
		
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien6		; Allsort bullet (The most friged routine in the whole game) ...

		tst.b	any+30(a6)
		beq	.kmkm
		move.w	x(a6),d0		Hoveroid hut ground ?
		sub.w	#8,d0
		move.w	y(a6),d1
		add.w	#$10,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.b22ounce2
    		btst.b	#3,1(a1,d3.w)
		bne	.b22ounce2
    		btst.b	#0,1(a1,d3.w)
		beq	.kmkm
.b22ounce2	jsr	freesprite
		bra	nextalien
.kmkm
		cmp.b	#$ff,any+10(a6)
		beq	.detec

		cmp.b	#49,any+10(a6)
		bne	.nnnnn
		subq.b	#1,any+11(a6)
		bpl	.nnnnn
		jsr	freesprite	
		jmp	nextalien
.nnnnn	
		cmp.b	#21,any+10(a6)
		beq	.nokill
		cmp.b	#24,any+10(a6)
		beq	.norm6
		cmp.w	#$ffff,any+6(a6)
		bne	.norm6
		cmp.b	#20,any+10(a6)
		bne	.justmove
.detec		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.justmove
		jsr	hurtzool
		jmp	.justmove
.norm6		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	norangecheck
		beq	.ok
		jsr	hurtzool
.ok
		cmp.b	#24,any+10(a6)
		beq	.nokill

		subq.w	#1,any+6(a6)
		beq	.kill
		cmp.w	#2,any+6(a6)
		bgt	.nokill
		cmp.w	#1,any+6(a6)
		bgt	.that
     		move.b	#3,fr+1(a6)
		jmp	.c
.that		move.b	#2,fr+1(a6)
.c		move.l	#lev1stuf,blbset(a6)
		jmp	nextalien
.kill		jsr	freesprite
		jmp	nextalien
.nokill
		move.w	x(a6),d0
		move.w	y(a6),d1
		cmp.b	#21,any+10(a6)		Collectable ?
		bne	.nocoll
		cmp.w	#$f,any+2(a6)		Mom...
		bpl	.nomore2
		add.l	#$e000,any+2(a6)
.nomore2	move.w	any+2(a6),d0
	        add.w	d0,y(a6)
		move.w	y(a6),d1
		add.w	#$8,d1
		move.w	x(a6),d0
		jmp	.col
.nocoll		sub.w	#8,d1
.col		
		tst.w	any+2(a6)		Do a ceiling check ?
		bpl	.goingdownch
		sub.w	#43,d1
		jsr	checkblock
    		btst.b	#0,1(a1,d3.w)
		beq	.allcl
		neg.w	any+2(a6)
		jmp	.allcl
.goingdownch	
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.bounce
    		btst.b	#3,1(a1,d3.w)
		bne	.bounce
    		btst.b	#0,1(a1,d3.w)
		beq	.allcl
.bounce		cmp.b	#116,any+10(a6)		Toy tank bullet ?
		beq	.yes
		cmp.b	#24,any+10(a6)		Eolg 1 (Explode bomb).
		bne	.noteolg1
.yes		move.l	#lev1stuf,blbset(a6)
		move.b	#0,t(a6)
		move.b	#0,any+10(a6)
		move.l	#$81,d4
		jsr	createexplo2
		jsr	freesprite
		jmp	nextalien
.noteolg1	
		cmp.b	#21,any+10(a6)		Collectable (Snap and wait).
		bne	.notcollects
		add.w	#$8,y(a6)
		andi.w	#$fff0,y(a6)
		subq.w	#1,y(a6)

    		tst.b	(a1,d3.w)
		beq	.no
		lea	det,a4
		move.w	(a1,d3.w),d4
		move.w	x(a6),d3
		andi.w	#$f,d3
		add.w	d3,d4
		moveq	#0,d0
		move.b	(a4,d4.w),d0
		add.w	d0,y(a6)				
.no
		move.b	#1,any(a6)
		move.b	#9,t(a6)
		moveq	#7,d0
		jsr	playsample
		jmp	nextalien
.notcollects	neg.w	any+2(a6)
.allcl
		move.w	x(a6),d0	
		sub.w	#20,d0
		tst.w	any(a6)
		bmi	.skix
     		add.w	#40,d0
.skix		move.w	y(a6),d1
		sub.w	#18,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.bounce2
    		btst.b	#0,1(a1,d3.w)
		beq	.allcl2
.bounce2	neg.w	any(a6)
.allcl2

.justmove	move.w	any(a6),d0		Move sprite .
		add.w	d0,x(a6)
		
		cmp.b	#21,any+10(a6)		Collectable (Snap and wait).
		beq	offandend
	
		move.l	any+12(a6),d0		Y acceleration ...
		cmp.l	#$7000,d0
		bne	.justadd
		cmp.w	#$a,any+2(a6)
		bpl	.nomore
.justadd	add.l	d0,any+2(a6)
.nomore		move.w	any+2(a6),d0
	        add.w	d0,y(a6)

		cmp.b	#116,any+10(a6)		Bomb ani ...
		bne	.fno
 		tst.w	any(a6)		
		bmi	.fno2
		move.b	#66,fr+1(a6)
   		cmp.w	#1,any+2(a6)
		bgt	.pos2
   		cmp.w	#-1,any+2(a6)
		bpl	.fno
		move.b	#65,fr+1(a6)
		jmp	.fno
.pos2		move.b	#67,fr+1(a6)
		jmp	.fno
.fno2		move.b	#51,fr+1(a6)
   		cmp.w	#1,any+2(a6)
		bgt	.pos
   		cmp.w	#-1,any+2(a6)
		bpl	.fno
		move.b	#52,fr+1(a6)
		jmp	.fno
.pos		move.b	#50,fr+1(a6)
.fno
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien7		; Bumble bee.

     		bclr.b	#1,fr+1(a6)
		move.w	x(a6),d0
		cmp.w	zoolwx,d0
		bpl	.nset
     		bset.b	#1,fr+1(a6)
.nset
		move.l	#50,scoid
		move.b	#4,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien

		eori.b	#1,fr+1(a6)

		tst.b	any+12(a6)
		beq	.notdoingit

		subq.b	#1,any+12(a6)
		tst.b	any+12(a6)
		
		bne	.stillok

    		move.l	x(a6),any(a6)
		jsr	random
		andi.w	#$f,d0
		sub.w	#8,d0
		add.w	d0,any(a6)
		jsr	random
		andi.w	#$f,d0
		sub.w	#8,d0
		add.w	d0,any+2(a6)
		move.b	#20,any+13(a6)
		clr.l	any+4(a6)
		clr.l	any+8(a6)
		jmp	.notdoingit

.stillok	move.l	any+4(a6),d0
		add.l	d0,any+14(a6)
		move.w	any+14(a6),x(a6)
		move.l	any+8(a6),d0
		add.l	d0,any+18(a6)
		move.w	any+18(a6),y(a6)

		jmp	nextalien

.notdoingit
		subq.b	#1,any+13(a6)
		bpl	.cont
     		
		move.w	x(a6),any+14(a6)
		clr.w	any+16(a6)
		move.w	y(a6),any+18(a6)
		clr.w	any+20(a6)

		moveq	#0,d0
		moveq	#0,d1
		move.w	zoolwx,d0
		swap	d0
		move.w	x(a6),d1
		swap	d1
		cmp.l	d1,d0
		bpl	.d0more1
		sub.l	d0,d1
		lsr.l	#4,d1
		neg.l	d1
		move.l	d1,any+4(a6)
		jmp	.doy
.d0more1	sub.l	d1,d0
		lsr.l	#4,d0		
		move.l	d0,any+4(a6)
.doy
		moveq	#0,d0
		move.w	zoolwy,d0
		swap	d0
		moveq	#0,d1
		move.w	y(a6),d1
		swap	d1
		cmp.l	d1,d0
		bpl	.d0more2
		sub.l	d0,d1
		lsr.l	#4,d1
		neg.l	d1
		move.l	d1,any+8(a6)
		jmp	.doc
.d0more2	sub.l	d1,d0
		lsr.l	#4,d0		
		move.l	d0,any+8(a6)
.doc

		move.l	any+4(a6),d0		Even out x and y speeds.
		move.l	any+8(a6),d1
		btst.b	#7,any+4(a6)		Make x and y speeds pos.
		beq	.nn1
   		neg.l	d0
.nn1		btst.b	#7,any+8(a6)
		beq	.nn2
   		neg.l	d1
.nn2 		move.l	d0,d4			Calc step.
		move.l	d1,d5
		lsr.l	#5,d4
		lsr.l	#5,d5
		move.l	d0,d2			To slow or to fast ?
		add.l	d1,d2
		cmp.l	#$000a0000,d2
		bmi	.lessthanloop
.morethanloop	sub.l	d4,d0			Subtract till correct.
		sub.l	d5,d1
		move.l	d0,d2
		add.l	d1,d2
		cmp.l	#$000a0000,d2
		bpl	.morethanloop
		jmp	.gotit
.lessthanloop	add.l	d4,d0			Add till correct.
		add.l	d5,d1
		move.l	d0,d2
		add.l	d1,d2
		cmp.l	#$000a0000,d2
		bmi	.lessthanloop
.gotit 		btst.b	#7,any+4(a6)		Give x and y correct signs.
		beq	.nn3
   		neg.l	d0
.nn3		btst.b	#7,any+8(a6)
		beq	.nn4
   		neg.l	d1
.nn4 		move.l	d0,any+4(a6)
      		move.l	d1,any+8(a6)

		move.b	#10,any+12(a6)
		jmp	nextalien		
.cont		
		tst.w	any(a6)
		bne	.done
		move.l	x(a6),any(a6)
		add.w	#8,any(a6)
		add.w	#4,any+2(a6)
.done
		move.w	any(a6),d0
		cmp.w	x(a6),d0
		bpl	.r
		cmp.w	#-7,any+4(a6)
		beq	.finlr
  		sub.l	#$8000,any+4(a6)
		jmp	.finlr
.r    		cmp.w	#7,any+4(a6)
		beq	.finlr
		add.l	#$8000,any+4(a6)
.finlr
		move.w	any+2(a6),d0
		cmp.w	y(a6),d0
		bpl	.d
		cmp.w	#-7,any+8(a6)
		beq	.findu
  		sub.l	#$8000,any+8(a6)
		jmp	.findu
.d    		cmp.w	#7,any+8(a6)
		beq	.findu
		add.l	#$8000,any+8(a6)
.findu


  		move.w	any+4(a6),d0
		add.w	d0,x(a6)
  		move.w	any+8(a6),d0
		add.w	d0,y(a6)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien8		; Bumble bee.

		addq.b	#1,any(a6)
		cmp.b	#2,any(a6)
		bne	nextalien
		clr.b	any(a6)
		addq.b	#1,fr+1(a6)
		cmp.b	#38,fr+1(a6)
		bne	nextalien
		jsr	freesprite
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien9		; Collectable.

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#31,d2
		move.w	#32,d3
		cmp.b	#1,any(a6)
		bne	.no16
     		move.w	#16,d2
.no16		st.b	liftcheck
		jsr	norangecheck
		tst.b	d7
		beq	.ok
		cmp.b	#1,any(a6)
		beq	.sk
		add.w	#8,y(a6)
		add.l	#1000,score
		add.l	#5,collected
		jmp	.skadd
.sk		add.l	#100,score
		add.l	#1,collected
.skadd		move.b	#8,fr+1(a6) 
		move.l	#lev1stuf,blbset(a6) collect,blbset(a6)
		move.b	#20,any(a6)
		moveq	#5,d0
		jsr	playsample
		move.b	#2,t(a6)

		jmp	nextalien
.ok
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien10		; Sloper.

		subq.b	#1,any+4(a6)
		bpl	.nores
      		move.b	#3,any+4(a6)
.nores
		tst.w	any(a6)
		bne	.alreadyset
	   	move.l	x(a6),any(a6)
.alreadyset
		tst.b	any+6(a6)
		bne	.noset
		move.b	#2,any+6(a6)
		move.w	zoolwx,d0 		X movement.
		cmp.w	x(a6),d0
		bmi	.noset
		move.b	#3,any+6(a6)
.noset
     		cmp.b	#2,any+6(a6)
    		beq	.left
   		addq.w	#2,any(a6)
  		moveq	#0,d0
 		jmp	.cont
.left		subq.w	#2,any(a6)	
.cont		
		move.w	any(a6),d0  		Check with background.
	 	add.w	#$1e+4,d0
		cmp.b	#2,any+6(a6)
		bne	.rightdir
	 	sub.w	#34+8,d0
.rightdir	move.w	any+2(a6),d1
		add.w	#$10,d1		
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.noswap
    		btst.b	#0,1(a1,d3.w)
		beq	.noswap
  		eori.b	#1,any+6(a6)
.noswap
		tst.b	any+14(a6)
		bne	.set
		add.l	#$9000,any+10(a6)
		move.w	any+10(a6),d0
		add.w	d0,any+2(a6)
		jmp	.nograv
.set		clr.l	any+10(a6)
.nograv
		move.b	#4,alslopefr
		move.l	zoolx,-(sp)
		move.l	zoolwx,-(sp)
		move.w	leftfoot,-(sp)
		move.b	jumping,-(sp)

		move.l	any(a6),d0
		move.l	d0,zoolwx
		sub.l	scrwx,d0
		add.l	#$0080002c,d0
		move.l	d0,zoolx
		clr.b	jumping

		move.l	a6,-(sp)
		st.b	almove
		clr.b	any+14(a6)
		jsr	checkdown
		beq	.nosetme
		st.b	any+14(a6)
.nosetme 
		clr.b	almove
		move.l	(sp)+,a6

		move.l	zoolx,d0
		add.l	scrwx,d0
		sub.l	#$0080002c,d0	
		move.l	d0,any(a6)

		move.b	(sp)+,jumping
		move.w	(sp)+,leftfoot
		move.l	(sp)+,zoolwx
		move.l	(sp)+,zoolx

		move.l	any(a6),x(a6)
		add.l	#$000c0024,x(a6)

		move.b	alslopefr,fr+1(a6)
		btst.b	#1,any+4(a6)
		beq	.skfradd
		addq.b	#1,fr+1(a6)
.skfradd
		move.l	#150,scoid
		move.b	#10,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien
	
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien11		; Y lift.

		clr.b	onrails

		move.w	x(a6),d0		Place screen info.
		sub.w	#16,d0
		move.w	y(a6),d1
		sub.w	#16,d1
		jsr	checkblock
		add.w	d2,a0
		move.l	a0,-(sp)
		move.b	#$ff,-1(a0)
		move.b	#$fd,(a0)
		move.b	#$fe,1(a0)
		move.b	#$fe,2(a0)
		move.b	#$fe,3(a0)
		move.b	#$fe,4(a0)
		move.b	#$ff,5(a0)

		subq.b	#1,any+1(a6)		Move lift.
		bpl	.noswap
		neg.w	any+2(a6)
       		move.b	any(a6),any+1(a6)
.noswap		move.w	any+2(a6),d0
		add.w	d0,y(a6)

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#16,d0
		sub.w	#52,d1
		move.w	#48,d2
		move.w	#100,d3
		jsr	norangecheck
		beq	.ok
		move.w	y(a6),d0		Set lift Y position.
		sub.w	scrwy,d0
		sub.w	#16,d0
		move.w	d0,onlift
		tst.b	any+2(a6)
		bpl	.na
   		subq.w	#1,onlift
.na		move.w	#1,ylift
		tst.b	any+2(a6)
		beq	.ok
		move.w	#-1,ylift
.ok
		move.l	(sp)+,a0
		move.w	scrwx,d0		Off screen ?
		move.w	x(a6),d1
		add.w	#100,d1
		cmp.w	d1,d0
		bhi	.offit
		add.w	#320,d0
		sub.w	#200,d1
		bmi	nextalien
		cmp.w	d1,d0
		bgt	nextalien		ls
.offit		st.b	(a0)
		st.b	1(a0)
		st.b	2(a0)
		st.b	3(a0)
		st.b	4(a0)
		move.l	snp(a6),a0
		bclr.b	#7,(a0)
		jsr	freesprite
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien12		; Mine switch.

		move.l	firstsprite,a5
.nextplease	cmp.l	#-1,a5
		beq	offandend
		cmp.b	#42,t(a5)
		beq	.foundone
		move.l	nsp(a5),a5
		jmp	.nextplease	     	
.foundone
		moveq	#0,d6
		tst.b	any+4(a6)
		beq	.no4
		cmp.b	#20,any+4(a6)
		bne	.f2
		tst.b	any+9(a5)		Y movement.
		beq	.ok
   		cmp.l	#0,any+4(a5)
		bne	.ok
.f2		subq.w	#4,y(a5)
		moveq	#2,d6
    		subq.b	#1,any+4(a6)
		bne	.ok
   		move.b	#20,any+6(a6)
		jmp	.ok
.no4
		tst.b	any+6(a6)
		beq	.no6
		moveq	#2,d6
    		subq.b	#1,any+6(a6)
		move.b	#20,any+5(a6)
		jmp	.ok
.no6
		tst.b	any+5(a6)
		beq	.no5
		moveq	#2,d6
		cmp.b	#20,any+5(a6)
		bne	.f1
		tst.b	any+9(a5)		Y movement.
		beq	.ok
   		cmp.l	#$8000*1,any+4(a5)
		bne	.ok
.f1		addq.w	#4,y(a5)
		moveq	#1,d6
    		subq.b	#1,any+5(a6)
		bne	.ok
		move.b	#0,fr+1(a6)
		sub.w	#28,x(a6)
		jmp	.ok
.no5
		move.w	x(a6),d0		Zool punched alien ?
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	checkpunch
		beq	.ok
		move.b	#1,fr+1(a6)
		add.w	#28,x(a6)
		move.b	#20,any+4(a6)
.ok
		sub.b	d6,any+12(a6)
		bpl	.nosm
     		add.b	#5,any+12(a6)
		move.l	a6,a4
		move.l	a5,-(sp)
		jsr	getsprite
		move.l	(sp)+,a5
		cmp.l	#-1,a6
		beq	.nosm2
		move.l	#lev1stuf,blbset(a6)
		move.b	#30,t(a6)
		move.l	#$060000ff,any(a6)
		move.l	any+10(a5),x(a6)
		sub.w	#$e,x(a6)
		add.w	#$43,y(a6)
		move.b	#14,fr+1(a6)
		move.l	a5,-(sp)
		jsr	getsprite
		move.l	(sp)+,a5
		cmp.l	#-1,a6
		beq	.nosm2
		move.l	#lev1stuf,blbset(a6)
		move.b	#30,t(a6)
		move.l	#$06ff00ff,any(a6)
		move.l	any+10(a5),x(a6)
		sub.w	#$e,x(a6)
		add.w	#$43,y(a6)
		add.w	#45,x(a6)
		move.b	#14,fr+1(a6)
.nosm2		move.l	a4,a6
.nosm
		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien13		; Greener ...

		move.l	#122,scoid
		move.b	#2,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien
	
		subq.b	#1,any+4(a6)
		bpl	.nores
      		move.b	#1,any+4(a6)
		eori.b	#1,fr+1(a6)
.nores
		tst.b	any+3(a6)
		beq	.nopause
       		subq.b	#1,any+3(a6)
		jmp	.paused
.nopause
		move.w	zoolwx,d0 		X movement.
		cmp.w	x(a6),d0
		bmi	.mi
		cmp.w	#4,any+6(a6)
		beq	.pl
		add.l	#$4000,any+6(a6)
		jmp	.pl
.mi		cmp.w	#-4,any+6(a6)
		beq	.pl
		sub.l	#$4000,any+6(a6)
.pl		move.w	any+6(a6),d0
		add.w	d0,x(a6)
		move.b	#2,any(a6)
		tst.w	any+6(a6)
		bmi	.notleft
		move.b	#1,any(a6)
.notleft

		move.w	x(a6),d0  		Check with background.
	 	add.w	#8,d0
		cmp.b	#2,any(a6)
		bne	.rightdir
	 	sub.w	#20,d0
.rightdir	move.w	y(a6),d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.swap
		btst.b	#3,1(a1,d3.w)
		bne	.swap
    		btst.b	#0,1(a1,d3.w)
		bne	.swap
		add.w	#16,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.swap
    		btst.b	#3,1(a1,d3.w)
		bne	.noswap
    		btst.b	#0,1(a1,d3.w)
		bne	.noswap
.swap  		tst.l	any+6(a6)
		beq	.noswap
		bmi	.pos
		move.l	#-$30000,any+6(a6)
		jmp	.noswap
.pos		move.l	#$40000,any+6(a6)
.noswap
.paused
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien14		; Wallslob ...

		subq.b	#1,any+4(a6)		Animation.
		bpl	.nores
      		move.b	#2,any+4(a6)
		eori.b	#1,fr+1(a6)
.nores
		tst.b	any+3(a6)		Pause stuff.
		beq	.nopause
		subq.b	#1,any+3(a6)
		jmp	.noch
.nopause
		tst.b	any+2(a6)		Y movement.
		beq	.up
		addq.w	#2,y(a6)
		jmp	.wasdown
.up		subq.w	#2,y(a6)
.wasdown	subq.b	#1,any+1(a6)		Change direction ?
		bpl	.noch
     		eori.b	#1,any+2(a6)
		move.b	any(a6),any+1(a6)
		move.b	#8,any+3(a6)
.noch
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		add.w	#12,d0
		move.w	#8,d2
		move.w	#20,d3
		jsr	alwithzool
		beq	.ok
		jsr	hurtzool
.ok
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien15		; Polo ...

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		add.w	#8,d0
		add.w	#8,d1
		move.w	#48,d2
		move.w	#48,d3
		jsr	alwithzool
		beq	.ok
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.no
		move.l	#lev1stuf,blbset(a6)
		st.b	fr+1(a6)
		clr.l	any(a6)
		move.b	#142,t(a6)
.no		move.l	a4,a6
		add.l	#10000,score
		add.w	#8+8,y(a6)
		move.b	#27,fr+1(a6)
		move.b	#19,any(a6)
		move.b	#2,t(a6)
		move.l	jsp(a6),a6
		add.w	#$b+8,y(a6)
		move.b	#28,fr+1(a6)
		move.b	#20,any(a6)
		move.b	#2,t(a6)
.ok
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien16		; Restart points ...

		tst.b	any+12(a6)
		bne	.k
  		st.b	any+12(a6)
		lea	offs34,a0
		move.l	worldno,d0
		lsl.w	#2,d0
		move.w	(a0,d0.w),d1
		add.w	d1,x(a6)
		move.w	2(a0,d0.w),d1
		add.w	d1,y(a6)
		move.w	x(a6),any+30(a6)
.k
		move.w	any+30(a6),x(a6)

		move.l	any(a6),d0
		cmp.l	restarthere,d0
		bne	.nook
		move.l	any+4(a6),d0
		cmp.l	restarthere+4,d0
		beq	.ok
.nook		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#16,d0
		move.w	#32,d2
		move.w	#40,d3
		jsr	checkpunch
		beq	.ok
		move.l	any(a6),restarthere
		move.l	any+4(a6),restarthere+4
		move.l	#$01100090,restarthere+8
		move.l	skypl,restarthere+12
		moveq	#$11,d0
		tst.l	sam11
		bne	.ll
		moveq	#6,d0
.ll		jsr	playsample
.ok
    		st.b	ht(a6)
		move.l	any+4(a6),d0
		cmp.l	restarthere+4,d0
		bne	.nolit
		move.l	any(a6),d0
		cmp.l	restarthere,d0
		beq	.lit
.nolit 		clr.b	ht(a6)
.lit
		move.w	any+30(a6),x(a6)
		jmp	offandend
		   
offs34		dc.w	-1,-3		0
		dc.w	-9,5		2
		dc.w	-4,-6		3
		dc.w	-10,4		1
		dc.w	-1,-3		5
		dc.w	-1,5		6
		dc.w	0,0		4
	
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien17		; Punchable wall ...

		tst.l	worldno
		bne	.mnmn
     		cmp.l	#2,areano
		bne	.mnmn
		cmp.l	#50,timeleft
		bmi	.mnmn
		jmp	nextalien
.mnmn

		tst.b	any(a6)
		beq	.nocd
     		subq.b	#1,any(a6)
		bne	nextalien
		jsr	freeplease
		jmp	nextalien
.nocd
		move.w	x(a6),d0		Alien hut zool ?
		sub.w	#16,d0
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	checkpunch
		beq	.ok

		move.w	x(a6),d0
		sub.w	#16,d0
		move.w	y(a6),d1
		jsr	checkblock
		add.l	d2,a0
     
		move.l	x(a6),xtemp
		move.l	a0,xtemp+4
	
		move.b	any+1(a6),(a0)
		st.b	1(a0)
		st.b	2(a0)
		move.b	any+3(a6),3(a0)
		add.w	mht+2,a0
		move.b	any+2(a6),(a0)
		st.b	1(a0)
		st.b	2(a0)
		move.b	any+4(a6),3(a0)
		
		move.b	#$ff,ht(a6)
		move.b	#3,any(a6)
		move.l	wrkplanes,-(sp)
		move.l	#scrplanes,wrkplanes
		move.l	a6,a0
		move.l	a6,-(sp)		
		lea	alittlespace,a6
		jsr	printblob
		move.l	(sp)+,a6
		move.l	(sp)+,wrkplanes
	
		add.w	#10,x(a6)
		add.w	#$18,y(a6)
		moveq	#3,d4
		jsr	createexplo2
		add.w	#$14,y(a6)
		moveq	#4,d4
		jsr	createexplo2
		sub.w	#$2c,y(a6)
		sub.w	#10,x(a6)
		jmp	nextalien
.ok
		jmp	offandend

xtemp		dc.l	0,0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien18		; Bounce thing ...

		tst.b	any+4(a6)
		beq	.nopause
		subq.b	#1,any+4(a6)
		jmp	.nofin
.nopause
		addq.w	#1,any+2(a6)
		move.w	any+2(a6),d0
		lea	bouncefrs,a0
		cmp.b	#-1,(a0,d0.w)
		bne	.no
   		clr.b	ht(a6)
		jmp	.nofin
.no   		cmp.b	#$80,(a0,d0.w)
		bne	.frame
		clr.w	any+2(a6)
		jsr	random
		andi.b	#$1f,d0
		add.b	#$10,d0
		move.b	d0,any+4(a6)
		jmp	.nofin
.frame		move.b	(a0,d0.w),fr+1(a6)
		st.b	ht(a6)
		cmp.b	#8,any+3(a6)
		bpl	.nofin
		tst.w	ymom
		bmi	.nofin
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		add.w	#4,d0
		sub.w	#10,d1
		move.w	#32-8,d2
		move.w	#10,d3
		jsr	norangecheck
		beq	.nofin
		st.b	mustup
		clr.b	ducking
		clr.b	punching
		clr.b	slideing
		st.b	jumping
		move.l	#-$a0000,ymom
		move.b	#20,uptime
		moveq	#$a,d0
		jsr	playsample
.nofin
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien19		; Eggy blob ...

		cmp.b	#1,fr+1(a6)
		beq	.justcheck

		subq.b	#1,any(a6)
		bpl	.nofire
		jsr	random
		andi.b	#$1f,d0
		add.b	#$30,d0
		move.b	d0,any(a6)
		jsr	mallsortb
		cmp.l	#-1,a5
		beq	.nofire
    		sub.w	#4,y(a5)
		move.b	#21,fr+1(a5)
  		move.w	#-$5,any(a5)
  		move.w	#$4,any+2(a5)
    		sub.w	#10,x(a5)
		move.l	a5,a4
		jsr	mallsortb
		cmp.l	#-1,a5
		beq	.nofire
    		sub.w	#4,y(a5)
		move.b	#21,fr+1(a5)
		move.w	#$5,any(a5)
		move.w	#$4,any+2(a5)
		move.w	#ee-eggyys,any+4(a6)
		eori.b	#1,any+2(a6)
		btst.b	#0,any+2(a6)
		beq	.nofire
    		sub.w	#12,y(a4)
		move.w	#-5,any+2(a4)
    		sub.w	#12,y(a5)
		move.w	#-5,any+2(a5)
		;clr.b	any+5(a6)
.nofire		
		tst.b	any+5(a6)
		beq	.noystuff
	 	subq.w	#2,any+4(a6)
		move.w	any+4(a6),d0
		lea	eggyys,a0
		move.w	(a0,d0.w),d0
		add.w	d0,y(a6)
.noystuff
		move.l	#500,scoid
		move.b	#1,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien
.justcheck	
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien20		; Cherry ...
			
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		sub.w	#24,d0
		sub.w	#24,d1
		move.w	#16+48,d2
		move.w	#16+48,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.ok
		moveq	#1,d4
		jsr	createexplo2
		jsr	freesprite
		jmp	nextalien
.ok
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien21		; Lolly top.

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#31,d2
		move.w	#32,d3
		jsr	alwithzool
		beq	.ok
		tst.b	spining
		beq	.ok
		add.l	#$00080008,x(a6)
		jsr	bonusexplo
		jsr	freeplease
		jmp	nextalien
.ok
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien22		; Bullet controler.

		move.l	any+20(a6),a0
		jsr	animateal
		move.l	a0,any+20(a6)
		cmp.w	#$fffe,(a0)		Launcher ani stop.
		beq	.killbullet

		cmp.b	#sln4,worldno+3
		bne	.nonono
		jsr	defmov
.nonono
		cmp.b	#sln,worldno+3
		bne	.no
		move.w	scrollspeed,d0
		add.w	d0,x(a6)
		add.w	d0,x(a6)
.no		
		tst.b	any+10(a6)
		beq	.norm
     		sub.w	#12,y(a6)
		jmp	.right
.norm
		tst.l	any(a6)
		bne	.right
		clr.b	htg(a6)
		tst.b	any+11(a6)		Move bullet.
		beq	.left
		add.w	#12,x(a6)
		jmp	.right
.left		sub.w	#12,x(a6)
.right
		move.w	x(a6),d0 		Hut back ground.
		move.w	y(a6),d1
		sub.w	#8,d0
		sub.w	#8,d1
		jsr	checkblock
		cmp.b	#$fc,(a0,d2.l)
		beq	.checkd
		btst.b	#0,1(a1,d3.w)
		beq	.checkd
		cmp.b	#56,htg(a6)
		beq	.kil
		jmp	.killbullet
.checkd
		cmp.b	#56,htg(a6)		Launcher stuff.
		bne	.notla
		move.l	jsp(a6),a5
		move.l	jsp(a5),a4
      		move.b	fr+1(a6),fr+1(a5)
		clr.b	ht(a4)
		cmp.b	#14,fr+1(a5)
		bne	.no23
   		st.b	ht(a4)
		move.b	#13,fr+1(a4)
.no23		move.b	#12,fr+1(a6)
		move.l	x(a6),x(a5)
		sub.w	#32,x(a5)
		move.l	x(a5),x(a4)
		sub.w	#16,x(a4)
		cmp.l	#la2,any(a6)
		bpl	.kil
		tst.b	any+16(a6)
		beq	.nokill
.kil		move.l	a6,a5
		move.l	jsp(a5),a6
		jsr	freesprite
		move.l	jsp(a6),a6
		jsr	freesprite
		move.w	#$000d,any(a5)
		clr.b	ht(a5)
		move.b	#115,t(a5)
.nokill		jmp	.noit
.notla
		tst.b	any+16(a6)
		bne	.killbullet

		tst.l	jsp(a6)			ZOOLBULL.
		beq	.noit 
		move.l	jsp(a6),a4
   		move.l	x(a6),x(a4)
		add.w	#32,x(a4)
		move.b	fr+1(a6),fr+1(a4)
		addq.b	#1,fr+1(a4)
		jmp	.n
.noit		move.l	lev1stuf,d0		Normal ani.
		cmp.l	blbset(a6),d0
		bne	.n
		eori.b	#1,fr+1(a6)		Animate.
.n

		jsr	offscreen		Are we still on the screen.
		tst.b	d0
		beq	nextalien
.killbullet	move.l	any+12(a6),a0
		clr.l	(a0)
.kill		jsr	freesprite
		move.l	jsp(a6),a6
		cmp.l	#0,a6
		bne	.kill
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien23		; Heart ...

		eori.b	#1,fr+1(a6)
		subq.w	#2,y(a6)

		subq.w	#2,any(a6)
		bpl	.nores
      		move.w	#(hxe-heartxs)-2,any(a6)
.nores		move.w	any(a6),d0
		lea	heartxs,a0
		move.w	(a0,d0.w),d0
		add.w	d0,x(a6)

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		st.b	liftcheck
		jsr	norangecheck		Zool on this alien ?
		beq	.ok
		move.w	#$e0,sam5+4
		moveq	#5,d0
		jsr	playsample
		move.w	#$f0,sam5+4
		moveq	#5,d0
		jsr	playsample
		move.w	#$100,sam5+4
		moveq	#5,d0
		jsr	playsample
		move.w	#$110,sam5+4
		moveq	#5,d0
		jsr	playsample
		cmp.b	#3,energylevel
		beq	.justpoints
		addq.b	#1,energylevel
		jsr	freesprite
		jmp	nextalien
.justpoints	move.b	#8,fr+1(a6)
		move.l	#lev1stuf,blbset(a6) collect,blbset(a6)
		move.b	#20,any(a6)
		move.b	#2,t(a6)
		add.l	#100,score
		jmp	nextalien
.ok
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien24		; Eolg1 (Bee thing) ...

		move.l	jsp(a6),a5
		move.l	jsp(a5),a4

		tst.b	any+11(a6)
		bne	.done
		tst.b	aldone
		beq	.lll
		jsr	freesprite
		move.l	jsp(a6),a6
		jsr	freesprite
		move.l	jsp(a6),a6
		jsr	freesprite
		bra	nextalien
.lll		sub.w	#$20,x(a6)
		sub.w	#$20,x(a5)
		sub.w	#$20,x(a4)
		clr.w	any+40(a6)
		move.b	#40,htg(a6)
		move.b	#$30,any+20(a6)
     		st.b	any+11(a6)
		move.l	#eolg1dat,any+12(a6)
		move.l	x(a6),any+16(a6)
		move.l	x(a5),any+16(a5)
		move.l	x(a4),any+16(a4)
.done
		st.b	stophorz

		tst.b	any+40(a6)
		beq	.notdead
		st.b	stophorz
		move.w	#120,shield
		st.b	aldone
		sub.w	#1,x(a6)
		sub.w	#1,x(a5)
		sub.w	#1,x(a4)
		add.l	#$8000,any+6(a6)
		move.w	any+6(a6),d0
		add.w	d0,y(a6)
		add.w	d0,y(a5)
		add.w	d0,y(a4)
		subq.b	#1,any+41(a6)
		bpl	.nn
  		move.b	#2,any+41(a6)
		move.w	#$80,d4
		add.w	#$10,x(a6)
		add.w	#40,y(a6)

		jsr	createexplo2
		sub.w	#$10,x(a6)
		sub.w	#40,y(a6)
.nn		jsr	offscreen
		beq	nextalien
		jsr	freesprite
		move.l	jsp(a6),a6
		jsr	freesprite
		move.l	jsp(a6),a6
		jsr	freesprite
		bra	nextalien
.notdead		
		subq.b	#1,any+10(a6)
		bpl	.still
      		add.l	#2,any+12(a6)
		move.l	any+12(a6),a0
		cmp.b	#$ff,(a0)
		bne	.noreset
		move.l	any+16(a6),x(a6)
		move.l	any+16(a5),x(a5)
		move.l	any+16(a4),x(a4)
		move.l	#eolg1dat,any+12(a6)
		move.l	any+12(a6),a0
.noreset	move.b	1(a0),any+10(a6)
.still		move.l	any+12(a6),a0
		move.b	(a0),d0
		move.b	(a0),d1
		move.b	(a0),d2
		move.b	(a0),d3
		move.b	(a0),d4
		andi.b	#1,d0
		andi.b	#2,d1
		andi.b	#4,d2
		andi.b	#8,d3
		andi.b	#16,d4

		tst.b	d2			Horz control.
		;tst.b	left
		beq	.nol
		cmp.w	#-$6,any+2(a6)
		beq	.doup
    		sub.l	#$7000,any+2(a6)
		jmp	.doup
.nol		tst.b	d3 
		;tst.b	right
		beq	.nor
		cmp.w	#$6,any+2(a6)
		beq	.doup
    		add.l	#$7000,any+2(a6)
		jmp	.doup
.nor		tst.l	any+2(a6)		Horz deac.
		beq	.doup
		bpl	.morethan
		add.l	#$7000,any+2(a6)
		bmi	.doup
		jmp	.cl1
.morethan	sub.l	#$7000,any+2(a6)
		bpl	.doup
.cl1		clr.l	any+2(a6)
.doup		tst.b	d0			Vert control.
		;tst.b	up
		beq	.nou
		cmp.w	#-$6,any+6(a6)
		beq	.dorest
     		sub.l	#$7000,any+6(a6)
		jmp	.dorest
.nou		tst.b	d1
		;tst.b	down
		beq	.nod
		cmp.w	#$6,any+6(a6)
		beq	.dorest
    		add.l	#$7000,any+6(a6)
		jmp	.dorest
.nod   		tst.l	any+6(a6)		Vert deac.
		beq	.dorest
		bpl	.morethan2
		add.l	#$7000,any+6(a6)
		bmi	.dorest
		jmp	.cl2
.morethan2	sub.l	#$7000,any+6(a6)
		bpl	.dorest
.cl2		clr.l	any+6(a6)
.dorest
		move.w	any+2(a6),d0		Move sprite from moms.
		add.w	d0,x(a6)
		add.w	d0,x(a5)
		add.w	d0,x(a4)
		move.w	any+6(a6),d0
		add.w	d0,y(a6)
		add.w	d0,y(a5)
		add.w	d0,y(a4)
.notyet
		subq.b	#1,any(a4)		Animate rudder.
		bpl	.ok
		move.b	#1,any(a4)
		subq.b	#1,fr+1(a4)
		bpl	.ok
   		move.b	#4,fr+1(a4)
.ok
		subq.b	#1,any+20(a6)
		cmp.b	#6,any+20(a6)
		beq	.fi
		cmp.b	#3,any+20(a6)
		beq	.fi
		tst.b	any+20(a6)
		bne	.nobombyet
		move.b	#$18,any+20(a6)
		jsr	random
		andi.b	#$f,d0
		add.b	d0,any+20(a6)
.fi		jsr	mallsortb 		Yes.
		cmp.l	#-1,a5
		beq	.nobombyet
    		add.l	#$00020032,x(a5)
		move.l	#$4000,any+12(a5)
		move.b	#8,fr+1(a5)
		move.l	#eolg1,blbset(a5)
		move.w	#-$4,any(a5)
		move.w	#$1,any+2(a5)
.nobombyet

		move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		add.w	#32,d0
		add.w	#20,d1
		move.w	#32,d2
		move.w	#72,d3
		jsr	bulhutal

		move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		add.w	#32,d1
		move.w	#32,d2
		move.w	#48,d3
		jsr	bulhutal
		tst.b	d7
		beq	.m
  		move.b	#2,flal(a6)
		move.b	#2,flal(a5)
		move.b	#2,flal(a4)
		subq.b	#1,htg(a6)
		bne	.m
		st.b	any+40(a6)
.m

.nothut		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		add.w	#16,d0
		add.w	#16,d1
		move.w	#32,d2
		move.w	#48,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.maybenot
		jsr	hurtzool
.maybenot
		jmp	nextalien



		move.b	up,d0			Record stuff ...
		move.b	down,d1
		move.b	left,d2
		move.b	right,d3
		move.b	fire,d4
		andi.b	#1,d0
		andi.b	#2,d1
		andi.b	#4,d2
		andi.b	#8,d3
		andi.b	#16,d4
		add.b	d1,d0
		add.b	d2,d0
		add.b	d3,d0
		add.b	d4,d0

		move.l	joyplace,a0
		cmp.b	(a0),d0
		bne	.change
		cmp.b	#$ff,1(a0)
		beq	.change
	 	addq.b	#1,1(a0)
		jmp	.nochange
.change		addq.l	#2,joyplace
		cmp.l	#joymem+1000,joyplace
		bne	.okay
     		move.w	#$f00,$dff180
		subq.l	#2,joyplace
.okay		move.l	joyplace,a0
		move.b	d0,(a0)
		clr.b	1(a0)
		st.b	2(a0)
.nochange
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien25		; Binary blocks ...

		move.l	a6,-(sp)
		jsr	.checkblock
		move.l	jsp(a6),a6
		jsr	.checkblock
		move.l	jsp(a6),a6
		jsr	.checkblock
		move.l	jsp(a6),a6
		jsr	.checkblock
		move.l	(sp)+,a6
		move.l	jsp(a6),a5
		move.l	jsp(a5),a4
		move.l	jsp(a4),a3

		move.b	any+1(a6),d0
		move.b	any+2(a6),d1
		move.b	any+3(a6),d2
		move.b	any+4(a6),d3
		cmp.b	fr+1(a6),d0
		bne	.no
   		cmp.b	fr+1(a5),d1
		bne	.no
		cmp.b	fr+1(a4),d2
		bne	.no
		cmp.b	fr+1(a3),d3
		bne	.no

		jsr	.letsdoit
		move.l	jsp(a6),a6
		jsr	.letsdoit
		move.l	jsp(a6),a6
		jsr	.letsdoit
		move.l	jsp(a6),a6
		jsr	.letsdoit
		jmp	nextalien

.letsdoit 	jsr	bonusexplo
		jmp	freeplease
.no
   		jmp	nextalien

.checkblock	tst.b	any(a6)
		beq	.ok
   		subq.b	#1,any(a6)
		rts
.ok		move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	bulhutal
		bne	.ex

		move.w	x(a6),d0		Zool punched alien ?
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	checkpunch
		beq	bye
.ex		eori.b	#1,fr+1(a6)	
		move.b	#5,any(a6)
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien26		; Gone.

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien27		; Normal drill ...

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		addq.w	#4,d0
		move.w	#8,d2
		move.w	#100,d3
		jsr	alwithzool
		beq	.ok
		jsr	hurtzool
.ok
		tst.b	any+1(a6)
		beq	.wait
     		subq.b	#1,any+1(a6)
		jmp	nextalien
.wait

		eori.b	#1,fr+1(a6)

		tst.b	any(a6)
		beq	.godown
     		subq.w	#2,y(a6)
		addq.b	#2,ht(a6)
		cmp.b	#102,ht(a6)
		bne	.noch
		eori.b	#1,any(a6)
		jmp	.noch
.godown		addq.w	#2,y(a6)
		subq.b	#2,ht(a6)
		bne	.noch
		eori.b	#1,any(a6)
		move.b	#15,any+1(a6)
.noch
		move.l	jsp(a6),a5
		eori.b	#1,fr+1(a5)
      		clr.b	ht(a5)
		cmp.b	#10,ht(a6)
		bmi	.noclr
      		st.b	ht(a5)
.noclr
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien28		; X lift ...

		tst.b	magic
		bne	nextalien

		subq.b	#1,any+1(a6)		Move lift.
		bpl	.noswap
		neg.w	any+2(a6)
       		move.b	any(a6),any+1(a6)
.noswap		move.w	any+2(a6),d0
		add.w	d0,x(a6)

		move.l	jsp(a6),a5
		move.w	x(a6),x(a5)
		add.w	#32,x(a5)

		tst.w	ymom
		bmi	.ok
		tst.b	clinging
		bne	.ok
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#16,d1
		cmp.b	#sln6,worldno+3
		bne	.no
		add.w	#25,d1
		move.l	jsp(a6),a5
		move.b	#0,fr+1(a6)
		move.b	#1,fr+1(a5)
		tst.w	any+2(a6)
		bpl	.no
		move.b	#2,fr+1(a6)
		move.b	#3,fr+1(a5)
.no		move.w	#48,d2
		move.w	#10,d3   32
		st.b	liftcheck
		jsr	norangecheck
		beq	.ok

		move.w	y(a6),zoolwy
		sub.w	#60,zoolwy
		cmp.b	#sln6,worldno+3
		bne	.no22
		add.w	#25,zoolwy
.no22		move.w	zoolwy,zooly
		add.w	#44,zooly
		move.w	scrwy,d0
		sub.w	d0,zooly
		clr.l	ymom
		move.w	zooly,zooly1

		move.w	#1,xlift
		tst.b	any+2(a6)
		beq	.ok
		move.w	#-1,xlift
.ok	
		clr.b	liftcheck

		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien29		; Collect to end level thing.

		cmp.b	#sln2,worldno+3
		bne	.kmkm
     		cmp.b	#1,areano+3
		beq	.skip
.kmkm		eori.b	#$ff,ht(a6)
		moveq	#2,d0
		sub.w	diffuculty,d0
		mulu	#25,d0
		add.w	#25,d0
		cmp.w	lastpercent+2,d0
		bgt	.ok

.skip		st.b	ht(a6)
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#8,d0
		sub.w	#8,d1
		move.w	#48,d2
		move.w	#48,d3

		cmp.b	#sln2,worldno+3
		bne	.nomu
     		cmp.b	#1,areano+3
		bne	.nomu
		sub.w	#20,d1
		sub.w	#50,d0
		move.w	#200,d2
.nomu

		jsr	alwithzool
		beq	.ok
		moveq	#5,d0
		jsr	playsample
		jsr	playsample
		st.b	endlevel
.ok
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien30		; Skid smoke ...

		subq.b	#1,any(a6)
		bpl	.notkill
		jsr	freesprite
		jmp	nextalien
.notkill
		eori.b	#1,any+2(a6)
		bne	.na
		cmp.b	#16,fr+1(a6)
		beq	.na
		addq.b	#1,fr+1(a6)
.na		
		tst.b	any+3(a6)
		bne	.sky
		subq.w	#2,y(a6)
.sky		tst.b	any+1(a6)
		beq	.left
 		addq.w	#4,x(a6)
		jmp	nextalien
.left		subq.w	#4,x(a6)
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien31		; Magic box ...

		bsr	freesprite
		bra	nextalien
			
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien32		; Magic trail ...

		jsr	freesprite
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien33		; Magic explode parts ...

		cmp.b	#14,fr+1(a6)
		beq	.sk
		move.w	x(a6),d0		
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	alwithzool		
		beq	.sk
		jsr	hurtzool
.sk
		move.w	any(a6),d0
		add.w	d0,x(a6)
		move.w	any+2(a6),d0
		add.w	d0,y(a6)
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien34		; Zool in zen ...

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien35		; Bridge part ...

		tst.b	any(a6)
		beq	.falling
		subq.b	#1,any(a6)

		jmp	nextalien
.falling
		move.l	any+10(a6),a0
		move.b	#$fc,(a0)

		add.l	#$c000,any+2(a6)
		move.w	any+2(a6),d0
		add.w	d0,y(a6)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien36		; Rolling thing .

		clr.b	ht(a6)
		cmp.b	#2,rollcnt
		bmi	.frok
		st.b	ht(a6)
.frok
		subq.b	#1,any(a6)
		bne	nextalien
		move.l	any+2(a6),a0
		clr.l	(a0)
		jsr	freesprite
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien37		; Saw thing ...

		tst.w	any+6(a6)
		bne	.noreset
		clr.b	any+8(a6)
		move.w	y(a6),any+6(a6)
		add.w	#32,any+6(a6)
.noreset
		tst.b	any+8(a6)
		beq	.nw
   		subq.b	#1,any+8(a6)
		jmp	nextalien
.nw
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		addq.w	#6,d0
		move.w	#20,d2
		move.w	#8,d3
		jsr	alwithzool
		beq	.ok
		jsr	hurtzool
.ok
		move.w	any+2(a6),d0
		add.w	d0,x(a6)

		tst.b	any+4(a6)
		bne	.goingup

		addq.w	#4,y(a6)
		subq.b	#4,ht(a6)
		tst.b	ht(a6)
		bne	.notall
		move.b	#15,any+8(a6)
		eori.b	#1,any+4(a6)
		subq.b	#1,any+1(a6)
		bpl	.notall
		move.b	any(a6),any+1(a6)
		neg.w	any+2(a6)
		eori.b	#1,fr+1(a6)
		jmp	.notall

.goingup	subq.w	#4,y(a6)		Move up.
		addq.b	#4,ht(a6)
		move.l	a6,a4			Create some sawdust.
		subq.b	#1,any+5(a6)
		bpl	.nosm
		move.b	#2,any+5(a6)     
		jsr	getlowersprite
		cmp.l	#-1,a6
		beq	.nosm
		move.l	#lev1stuf,blbset(a6)
		move.b	#30,t(a6)
		move.l	#$06000000,any(a6)
		move.w	x(a4),x(a6)
		move.w	any+6(a4),y(a6)
		sub.w	#4,x(a6)
		move.b	#14,fr+1(a6)
		tst.w	any+2(a4)
		bmi	.nosm
		st.b	any+1(a6)
		add.w	#23+2,x(a6)
.nosm		move.l	a4,a6
		cmp.b	#36,ht(a6)		Full up ?
		bne	.notall
       		eori.b	#1,any+4(a6)
.notall

		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien38		; Rolling ball thing.
	
		move.l	#139,scoid
		move.b	#2,sefid
		cmp.b	#sln5,worldno+3
		bne	.dork
		eori.b	#1,fr+1(a6)
		jmp	.drok2
.dork		cmp.b	#sln3,worldno+3
		bne	.drok
		addq.b	#1,fr+1(a6)
		eori.b	#1,fr+1(a6)
		subq.b	#1,fr+1(a6)
.drok2		move.b	#$81,sefid
.drok		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien

		move.w	x(a6),d0  		X check with background.
	 	sub.w	#$10,d0
		tst.l	any+4(a6)
		bmi	.rightdir
	 	add.w	#$34,d0
.rightdir	move.w	y(a6),d1
		sub.w	#$1e,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.wall
    		btst.b	#0,1(a1,d3.w)
		beq	.wall
		neg.l	any+4(a6)
.wall
		tst.w	any(a6)
		bne	.noset
      		move.l	x(a6),any(a6)
		move.w	x(a6),any+16(a6)
		clr.w	any+18(a6)
.noset
		move.b	#4,alslopefr
		move.l	zoolx,-(sp)
		move.l	zoolwx,-(sp)
		move.w	leftfoot,-(sp)
		move.b	jumping,-(sp)

		move.l	any(a6),d0
		move.l	d0,zoolwx
		sub.l	scrwx,d0
		add.l	#$0080002c,d0
		move.l	d0,zoolx
		clr.b	jumping


		move.l	a6,-(sp)
		clr.b	alslopefr
		st.b	almove
		clr.b	any+14(a6)
		clr.w	almust
		jsr	checkdown
		beq	.nosetme
		st.b	any+14(a6)
.nosetme 
	  	clr.b	almove
		move.l	(sp)+,a6

		move.l	zoolx,d0
		add.l	scrwx,d0
		sub.l	#$0080002c,d0	
		move.l	d0,any(a6)

		move.b	(sp)+,jumping
		move.w	(sp)+,leftfoot
		move.l	(sp)+,zoolwx
		move.l	(sp)+,zoolx

		move.l	any(a6),x(a6)
		add.l	#$000c0020,x(a6)
		cmp.b	#sln3,worldno+3
		bne	.nj
   		subq.w	#5,y(a6)
.nj
		cmp.b	#sln5,worldno+3
		bne	.nj2
   		subq.w	#3,y(a6)
.nj2


		cmp.b	#56,alslopefr
		bne	.no
		move.l	#-$80000,any+10(a6)
.no	
		tst.l	any+10(a6)
		bmi	.fly
		tst.b	any+14(a6)
		bne	.set
.fly		add.l	#$9000,any+10(a6)
		move.w	any+10(a6),d0
		add.w	d0,any+2(a6)
		jmp	.nograv
.set		clr.l	any+10(a6)
.nograv
	
	
		tst.b	almust+1
		beq	.noleft
       		add.l	#$3000,any+4(a6)
.noleft		tst.b	almust
		beq	.noright
       		sub.l	#$3000,any+4(a6)
.noright	move.l	any+4(a6),d0
		add.l	d0,any+16(a6)
		move.w	any+16(a6),any(a6)
   
		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien39		; Chainsaw generator.

    		st.b	any+2(a6)
		   
		subq.b	#1,any+1(a6)		Create a chain ?
		bpl	nextalien
       		move.b	any(a6),any+1(a6)
		moveq	#20,d3
		jsr	chainit
		jmp	bigoffandend

chainit		move.l	a6,a4
		jsr	getlowersprite
		cmp.l	#-1,a6
		beq	.didnt
		move.b	#40,t(a6)
		move.l	#chainsaw,blbset(a6)
		clr.w	fr(a6)
		clr.l	any+2(a6)
		move.l	x(a4),x(a6)
		move.b	d3,any(a6)
.didnt		move.l	a4,a6
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

OFFANDEND	; End current alien routine (checking screen pos before).
	
		jsr	offscreen		Are we still on the screen.
		tst.b	d0
		beq	nextalien
nextdeac	move.l	snp(a6),a0
		bclr.b	#7,(a0)
		jsr	freesprite
		tst.l	jsp(a6)
		beq	nextalien
		move.l	jsp(a6),a6
		cmp.l	#ten,a6
		bpl	nextalien
		jmp	nextdeac

BIGOFFANDEND	; Same as above but this has a bigger range.

		move.w	#250,smadd+2
		move.w	#500,bigadd+2
		jsr	offscreen		Are we still on the screen.
		move.w	#100,smadd+2
		move.w	#200,bigadd+2
		tst.b	d0
		bne	nextdeac
		jmp	nextalien

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*

getbsprite	cmp.l	#freesprpointer,freesprpointer Any sprites ?
		bne	spritesleft
		move.l	#$ffffffff,a6
		rts

getsprite	jmp	getsprite2
getlowersprite	jmp	getlowersprite2
freesprite	jmp	freesprite2		

alien40		; Chainsaw knife.

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		addq.w	#6,d0
		move.w	#4,d2
		move.w	#10,d3
		jsr	alwithzool
		beq	.ok43
		jsr	hurtzool
.ok43
		tst.l	any+2(a6)
		beq	.n
  		addq.l	#6,any+2(a6)
		move.l	any+2(a6),a0
		cmp.w	#$8000,(a0)
		bne	.noend
      		jsr	freesprite
		jmp	nextalien
.noend		move.l	(a0),d0
		add.l	d0,x(a6)
		move.b	4(a0),ht(a6)
		move.b	5(a0),fr+1(a6)
		jmp	nextalien
.n
		subq.w	#4,x(a6)

		tst.b	any(a6)
		beq	.chb
		subq.b	#1,any(a6)
		jmp	nextalien

.chb		move.w	x(a6),d0
		move.w	y(a6),d1
		add.w	#16,d1
		jsr	checkblock
		cmp.b	#64,(a0,d2.l)
		beq	nextalien
		move.l	#sawpls,any+2(a6)
		jmp	nextalien
		
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien41		; Acme ...

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#20,d1
		add.w	#4,d0
		move.w	#40,d2
		moveq	#0,d3
		move.b	ht(a6),d3
		add.w	#10,d3
		jsr	alwithzool
		beq	.ok43
		jsr	hurtzool
.ok43
		move.l	jsp(a6),a5

		tst.b	any+4(a6)
		beq	.up
		add.l	#$8000,any+6(a6)
		move.w	any+6(a6),d0
		add.b	d0,ht(a6)
		add.b	d0,ht(a5)
		lsl.w	#1,d0
		sub.l	d0,any(a5)
		lsl.w	#1,d0
		sub.l	d0,any(a6)
		cmp.b	#100,ht(a6)
		bmi	.off
		eori.b	#1,any+4(a6)
.up		
		add.l	#4*2,any(a6)
		add.l	#2*2,any(a5)
		subq.b	#2,ht(a6)
		subq.b	#2,ht(a5)
		bpl	.off
		clr.b	ht(a6)
		clr.b	ht(a5)
		move.l	#4*111,any(a6)
		move.l	#2*111,any(a5)
		eori.b	#1,any+4(a6)
		move.l	#$10000,any+6(a6)
.off		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien42		; Mine thing ...

		tst.b	any+11(a6)
		bne	.set
    		move.l	x(a6),any+10(a6)
.set
		move.w	x(a6),d0			Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#20,d1
		move.w	#32,d2
		move.w	#42,d3
		st.b	liftcheck		Can I do on head check ...
		jsr	alwithzool
		beq	.ok
		jsr	hurtzool
.ok		clr.b	liftcheck		Can I do on head check ...

		tst.b	any+8(a6)			X movement.
		beq	.nl
		sub.l	#$8000,any(a6)
		cmp.l	#-$8000*3,any(a6)
		bne	.nc
		eori.b	#1,any+8(a6)
		jmp	.nc
.nl		add.l	#$8000,any(a6)
		cmp.l	#$8000*4,any(a6)
		bne	.nc
   		eori.b	#1,any+8(a6)
.nc		move.w	any(a6),d0
		add.w	d0,x(a6)

		tst.b	any+9(a6)		Y movement.
		beq	.nl2
		sub.l	#$8000,any+4(a6)
		cmp.l	#-$8000*8,any+4(a6)
		bne	.nc2
		eori.b	#1,any+9(a6)
		jmp	.nc2
.nl2		add.l	#$8000,any+4(a6)
		cmp.l	#$8000*9,any+4(a6)
		bne	.nc2
   		eori.b	#1,any+9(a6)
.nc2		move.w	any+4(a6),d0
		add.w	d0,y(a6)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien43		; Chris ...

		tst.b	any+8(a6)
		bne	.foundnone
		move.l	firstsprite,a4		Chris with acme.
.next		cmp.l	#-1,a4
		beq	.foundnone
	  	cmp.b	#41,t(a4)
		bne	.nocheck
		move.l	zoolwx,-(sp)		Found it so check it.
		move.l	x(a6),zoolwx
		move.w	x(a4),d0
		move.w	y(a4),d1
		add.w	#4,d0
		moveq	#0,d4
		move.b	ht(a4),d4
		add.w	d4,d1
		move.w	#40,d2
		move.w	#16,d3
		st.b	liftcheck
		jsr	alwithzool
		clr.b	liftcheck
		move.l	(sp)+,zoolwx
		tst.b	d7
		bne	splitalien
.nocheck	move.l	nsp(a4),a4
		jmp	.next
.foundnone

		tst.b	any(a6)			Pause before rejump.
		beq	.no
  		cmp.b	#5,fr+1(a6)
		beq	.n
  		cmp.b	#2,fr+1(a6)
		beq	.n
		addq.b	#1,fr+1(a6)
.n		subq.b	#1,any(a6)
		bne	.cont
		jsr	random
		and.w	#$7,d0
		addq.w	#$6,d0
		neg.w	d0
		move.w	d0,any+2(a6)
		clr.w	any+4(a6)
		jsr	random
		move.b	d0,any+6(a6)
.no		
		move.b	any+8(a6),fr+1(a6)		Use jumping frame.

		subq.w	#3,x(a6)		X movement.
		btst.b	#0,any+6(a6)
		beq	.nx
   		addq.w	#6,x(a6)
.nx
		move.w	x(a6),d0  		X check with background.
	 	add.w	#$10,d0
		btst.b	#0,any+6(a6)
		bne	.rightdir
	 	sub.w	#25,d0
.rightdir	move.w	y(a6),d1
		add.w	#$14,d1		
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.noswap
    		btst.b	#0,1(a1,d3.w)
		beq	.noswap
  		eori.b	#1,any+6(a6)
.noswap
		move.w	any+2(a6),d0		Move Y.
		add.w	d0,y(a6)

		tst.l	any+2(a6)		Up or down detection ?
		bmi	.topch
		move.w	x(a6),d0
		move.w	y(a6),d1
		add.w	#21,d1
		jsr	checkblock
		btst.b	#3,1(a1,d3.l)
		bne	.ong
		btst.b	#0,1(a1,d3.l)
		bne	.ong
		jmp	.grav

.topch		move.w	x(a6),d0		Ceiling check.
		move.w	y(a6),d1
		sub.w	#16,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.l)
		beq	.grav
		move.l	#$10000,any+2(a6)

.grav		cmp.w	#$e,any+2(a6)		Do gravity.
		beq	.cont
		add.l	#$e000,any+2(a6)
		jmp	.cont

.ong		move.b	#20,any(a6)		Just hut ground.
		clr.l	any+2(a6)
		addq.w	#6,y(a6)
		andi.w	#$fff0,y(a6)
		subq.w	#6,y(a6)
.cont

		move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		add.w	#20,d1
		move.w	#16,d2
		move.w	#10,d3
		jsr	bulhutal
		tst.b	d7
		bne	.ex
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		add.w	#5,d0
		sub.w	#8,d1
		move.w	#22,d2
		move.w	#32,d3
		jsr	alwithzool		Zool on this alien ?
		tst.b	onhead
		beq	.nohead
		st.b	jun
		jmp	.ex
.nohead		tst.b	d7
		bne	.hutalien

.noex	     	tst.b	d6			Zool close to this alien ?
		beq	.ok
		move.w	x(a6),d0		Zool punched alien ?
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	checkpunch
		bne	.ex	
 		jmp	.ok
.hutalien	cmp.b	#3,slideing
		beq	.ex
		tst.b	spining
		beq	.hurtzool
.ex		tst.b	any+8(a6)
		beq	splitalien
		add.l	#$00080008,x(a6)
		jsr	makeheart
		moveq	#6,d4
		jsr	createexplo
		jsr	freesprite
		add.l	#150,score
		jmp	nextalien
.hurtzool	jsr	hurtzool
.ok
		jmp	offandend
	  
splitalien	move.b	#3,any+8(a6)		Split alien.
		move.b	#3,fr+1(a6)
		clr.b	any(a6)
		move.l	#-$30000,any+2(a6)
		clr.b	any+6(a6)
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	nextalien
		move.l	#chris,blbset(a6)
		move.b	#43,t(a6)
		move.l	x(a4),x(a6)
		move.b	#3,fr+1(a6)
		move.b	#3,any+8(a6)
		clr.b	any(a6)
		move.l	#-$30000,any+2(a6)
		st.b	any+6(a6)
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien44		; Side drills ...

		tst.b	any+2(a6)		Set all the drill parts ?
		bne	.done

		clr.l	any+6(a6)

		st.b	any+2(a6)
     		move.l	a6,a4			Set stuff.
     		move.l	a6,a3
		moveq	#0,d3			Set the amount to create.
		move.b	any(a6),d3
.moreparts	jsr	getlowersprite		Create a drill part.
		cmp.l	#-1,a6
		beq	.tomany
		move.l	a6,jsp(a3)
		move.l	a6,a3
		move.l	x(a4),x(a6)
		clr.b	t(a6)
		move.l	#drills,blbset(a6)
		move.b	#5,fr+1(a6)
		dbra	d3,.moreparts		Do all the parts.

		jsr	getsprite
		cmp.l	#-1,a6
		beq	.tomany
		clr.l	any+6(a6)
		move.l	x(a4),x(a6)
		add.l	#$fff00008,x(a6)
		move.l	a6,any+6(a4)
		move.l	#drills,blbset(a6)
		move.b	#15,fr+1(a6)
		clr.b	t(a6)
		clr.b	ht(a6)
		tst.b	any+1(a4)
		beq	.tomany
		add.w	#32,x(a6)
		subq.b	#2,fr+1(a6)
.tomany		move.l	a4,a6
.done
		; MOVEMENT ...

		move.b	any+5(a6),d0
		lsr.b	#1,d0
		eori.b	#1,d0
		lsl.b	#1,d0
		move.b	d0,any+5(a6)

		tst.b	any+10(a6)
		beq	.g
		move.l	any+6(a6),a5		Remove sawdust.
		clr.b	ht(a5)
		subq.b	#1,any+10(a6)
		jmp	nextalien
.g
		tst.b	any+4(a6)		Wait for Zool to stand.
		beq	.nopause
		move.l	any+6(a6),a5		Remove sawdust.
		clr.b	ht(a5)
		subq.b	#1,any+4(a6)
		move.b	#249,d6
		tst.b	any+4(a6)
		bne	.n
  		move.b	#$ff,d6
.n		move.l	jsp(a6),a5
.nextplease     cmp.l	#0,a5
		beq	.alldone2
		move.w	x(a5),d0
		sub.w	#16,d0
		move.w	y(a5),d1
		jsr	checkblock
		move.b	d6,(a0,d2.l)
		move.l	jsp(a5),a5
		jmp	.nextplease
.nopause
		jsr	offscreen		Are we still on the screen.
		tst.b	d0
		beq	.notoff
		move.l	snp(a6),a0
		bclr.b	#7,(a0)
		move.l	any+6(a6),a2
.nextal		move.l	jsp(a6),a3
		jsr	freesprite
		cmp.l	#0,a3
		beq	.freelast
		move.l	a3,a6
		jmp	.nextal
.freelast	move.l	a2,a6
		cmp.l	#0,a6
		beq	nextalien
		jsr	freesprite
		jmp	nextalien
.notoff	
		
		tst.b	any+1(a6)
		beq	.firstdetec
		move.l	jsp(a6),a5
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	x(a5),d2
		add.w	#16,d2
		sub.w	d0,d2
		move.w	#8,d3
		st.b	liftcheck
		jsr	alwithzool
		beq	.ok
		jsr	hurtzool
   		jmp	.ok
.firstdetec	move.l	jsp(a6),a5
		move.w	x(a5),d0		Alien hut zool ?
		move.w	y(a5),d1
		move.w	#60,d2
		move.w	#8,d3
		st.b	liftcheck
		jsr	alwithzool
		beq	.ok
		jsr	hurtzool
.ok

		tst.b	any+3(a6)		Which direction ?
		beq	.goout

		move.l	jsp(a6),a5		Pull it in.
		move.w	x(a5),d0
		tst.b	any+1(a6)
		beq	.nrom
     		cmp.w	x(a6),d0
		bgt	.moveinnext
		jmp	.gh
.nrom		cmp.w	x(a6),d0
		bmi	.moveinnext
.gh		clr.b	any+3(a6)
  		move.b	#30,any+10(a6)
		jmp	.alldone
.moveinnext	cmp.l	#0,a5
		beq	.alldone
		move.b	any+5(a6),fr+1(a5)
		addq.b	#5,fr+1(a5)
		move.w	x(a5),d0
		tst.b	any+1(a6)
		beq	.nrom2
		addq.b	#3,fr+1(a5)
     		cmp.w	x(a6),d0
		ble	.nomove
		jmp	.gh2
.nrom2	     	cmp.w	x(a6),d0
		bpl	.nomove
.gh2   		addq.w	#2,x(a5)
		tst.b	any+1(a6)
		beq	.nomove
		subq.w	#4,x(a5)
.nomove		move.l	jsp(a5),a5
		jmp	.moveinnext

.goout		move.l	jsp(a6),a5		Set first sprite.
		cmp.l	#0,a5
		bne	.nextsprite
		jsr	freesprite
		jmp	nextalien
.nextsprite	subq.w	#2,x(a5)		Move this sprite.
		tst.b	any+1(a6)
		beq	.nom2
		addq.w	#4,x(a5)
.nom2		move.b	any+5(a6),fr+1(a5)
		addq.b	#5,fr+1(a5)
		tst.l	jsp(a5)			Is this the last sprite.
		bne	.thereisone
	   	move.w	x(a6),d0  		Time to stop ?
		sub.w	#15,d0
		tst.b	any+1(a6)
		beq	.bb
		addq.b	#3,fr+1(a5)
   		add.w	#30,d0
		cmp.w	x(a5),d0
		bpl	.alldone
		jmp	.dd
.bb		cmp.w	x(a5),d0
		bmi	.alldone
.dd		st.b	any+3(a6)
		move.b	#50,any+4(a6)
		jmp	.alldone
.thereisone	move.l	jsp(a5),a4		Move the next sprite ?
		move.w	x(a4),d0
		sub.w	#17,d0
		tst.b	any+1(a6)
		beq	.bbb
		addq.b	#3,fr+1(a5)
   		add.w	#17*2,d0
		cmp.w	x(a5),d0
		bpl	.alldone
		jmp	.fd
.bbb		cmp.w	x(a5),d0
		bmi	.alldone
.fd		move.l	a4,a5 			Set up next sprite.
		jmp	.nextsprite
.alldone
		move.l	jsp(a6),a5		Blunt end.
		subq.b	#1,fr+1(a5)
		tst.b	any+1(a6)
		beq	.all
		addq.b	#2,fr+1(a5)
.all
		move.l	any+6(a6),a5		Show sawdust.
		st.b	ht(a5)
		subq.b	#1,fr+1(a5)
		eori.b	#1,fr+1(a5)
		addq.b	#1,fr+1(a5)

.alldone2	jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien45		; Big flames ...

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		add.w	#8,d0
		move.w	#16,d2
		move.w	#20,d3
		st.b	liftcheck
		jsr	alwithzool		Zool on this alien ?
		beq	.nh
		jsr	hurtzool
.nh
		subq.w	#1,any(a6)
		bpl	.frok
		move.w	#3,any(a6)
.frok		move.w	any(a6),d0
		lea	flfrs,a0
		move.b	(a0,d0.w),fr+1(a6)


		subq.b	#1,any+2(a6)		Fire a spark ?
		bpl	.nospark
		jsr	random			Set next timer.
		andi.b	#$1f,d0
		add.b	#20,d0
		move.b	d0,any+2(a6)
		move.l	a6,a4
		jsr	getsprite  		Make a spark.
		cmp.l	#-1,a6
		beq	.ok
		move.l	x(a4),x(a6)
		add.l	#$00080010,x(a6)
		move.l	#flames,blbset(a6)
		clr.b	any+6(a6)
		clr.b	fr+1(a6)
		move.b	#46,t(a6)
		jsr	random	  		Set Y movement.
		andi.w	#$3,d0
		add.b	#7,d0
		neg.w	d0
		move.w	d0,any(a6)
		jsr	random			Set X movement.
		andi.w	#$3,d0
		addq.b	#1,d0
		move.w	d0,any+4(a6)
		jsr	random			Set direction.
		btst	#0,d0
		beq	.ok
   		neg.w	any+4(a6)
.ok		move.l	a4,a6
.nospark
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien46		; Flame spark.

		tst.b	any+8(a6)
		beq	.nowait
       		subq.b	#1,any+8(a6)
		clr.b	ht(a6)
		jmp	nextalien
.nowait
		st.b	ht(a6)

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#10,d2
		move.w	#10,d3
		st.b	liftcheck
		jsr	alwithzool		Zool on this alien ?
		beq	.nh
		jsr	hurtzool
.nh
		eori.b	#1,fr+1(a6)

		tst.b	any+6(a6)
		beq	.nod
		subq.b	#1,any+6(a6)
		bne	nextalien
		jsr	freesprite
		jmp	nextalien
.nod	
		move.w	any(a6),d0		Y movement.
		add.w	d0,y(a6)
		add.l	#$c000,any(a6)
		move.w	any+4(a6),d0		X movement.
		add.w	d0,x(a6)

		move.w	y(a6),d1  		X check with background.
		cmp.w	#3,any(a6)
		bmi	.nd
  		sub.w	#14,d1
.nd		move.w	x(a6),d0
		tst.w	any+4(a6)
		bpl	.l
	 	sub.w	#16,d0
.l		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.noswapx
    		btst.b	#0,1(a1,d3.w)
		beq	.noswapx
  		move.b	#$20,any+6(a6)
		move.b	#8,fr+1(a6)
		andi.w	#$fff0,x(a6)
		tst.w	any+4(a6)
		bpl	nextalien
		add.w	#16,x(a6)
		move.b	#6,fr+1(a6)
		jmp	nextalien
.noswapx
		move.w	x(a6),d0  		Y check with background.
		move.w	y(a6),d1
		tst.w	any(a6)
		bpl	.d
	 	sub.w	#16,d1
.d		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.noswap
    		btst.b	#0,1(a1,d3.w)
		beq	.noswap
  		move.b	#$20,any+6(a6)
		move.b	#4,fr+1(a6)
		andi.w	#$fff0,y(a6)
		tst.w	any(a6)
		bpl	.noswap
		add.w	#16,y(a6)
		move.b	#10,fr+1(a6)
.noswap
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien47		; Oil splat generator...

		subq.b	#1,any(a6)
		bpl	.nooil
		jsr	random
		andi.b	#$3f,d0
		add.b	#40,d0
		move.b	d0,any(a6)
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.couldnt
		move.w	any+2(a4),any+10(a6)
		cmp.w	#-9,any+10(a6)
		bne	.nj
   		jsr	random
		btst	#0,d0
		beq	.nj
		neg.w	any+10(a6)
.nj		move.l	#flames,blbset(a6)
		move.l	x(a4),x(a6)
		move.b	#12,fr+1(a6)
		move.b	#48,t(a6)
.couldnt	move.l	a6,a4
.nooil
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien48		; Oil splat ...

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#10,d2
		move.w	#10,d3
		st.b	liftcheck
		jsr	alwithzool		Zool on this alien ?
		beq	.nh
		jsr	hurtzool
.nh
		
		cmp.b	#12,fr+1(a6)		Animation.
		beq	.na
   		eori.b	#1,fr+1(a6)
.na
		move.w	any+10(a6),d0
		add.w	d0,x(a6)		Movement.

		move.l	firstsprite,a4		Oil with flames.
.next		cmp.l	#-1,a4
		beq	.foundnone
	  	cmp.b	#45,t(a4)
		bne	.nocheck
		move.l	zoolwx,-(sp)		Found it so check it.
		move.l	x(a6),zoolwx
		move.w	x(a4),d0
		add.w	#8,d0
		move.w	y(a4),d1
		move.w	#32,d2
		move.w	#64,d3
		st.b	liftcheck
		jsr	alwithzool
		move.l	(sp)+,zoolwx
		tst.b	d7
		beq	.nocheck
		move.b	#2,fr+1(a6)
		jmp	.foundnone
.nocheck	move.l	nsp(a4),a4
		jmp	.next
.foundnone
		move.w	y(a6),d1  		X check with background.
		move.w	x(a6),d0
		sub.w	#$10,d0
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.nosplat
    		btst.b	#0,1(a1,d3.w)
		beq	.nosplat
		cmp.b	#12,fr+1(a6)		Splat it ?
		bne	.flit
		jsr	freesprite
		jmp	nextalien 

.flit  		move.b	#$20,any+6(a6)		Create flames.
		move.b	#6,fr+1(a6)
		andi.w	#$fff0,x(a6)
		add.l	#$000ffffd,x(a6)
		move.b	#46,t(a6)
		clr.b	any+8(a6)
		move.l	x(a6),d4
		sub.l	#10,d4
		move.b	#1,d3
		jsr	createfl
		add.l	#20,d4
		jsr	createfl
		add.l	#10,d4
		move.b	#3,d3
		jsr	createfl
		sub.l	#40,d4
		jsr	createfl
		sub.l	#10,d4
		move.b	#5,d3
		jsr	createfl
		add.l	#60,d4
		jsr	createfl
.nosplat
		jmp	offandend


CREATEFL	; Create a flame for alien48 (above) ...

		pushall
		jsr	getlowersprite
		cmp.l	#-1,a6
		beq	.no
		clr.b	ht(a6)
		move.b	#46,t(a6)
		move.l	#flames,blbset(a6)
		move.b	#6,fr+1(a6)
		move.l	d4,x(a6)
		move.b	d3,any+8(a6)
  		move.b	#$20,any+6(a6)
.no		pullall
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien49		; Woodworm ...

		tst.b	any(a6)
		beq	.nowait
		subq.b	#1,any(a6)
		bne	nextalien
		moveq	#0,d4
		move.l	#lev1stuf,blbset(a6)
		sub.w	#8,y(a6)
		jsr	createexplo2
		add.w	#8,y(a6)
		move.l	#croc,blbset(a6)
		move.l	a6,-(sp)
		move.l	lastsprite,a6
		move.l	psp(a6),a5
		jsr	freesprite
		move.l	(sp)+,a6
		move.l	#$0000fff9,any(a5)
		move.b	#15,fr+1(a5)
		move.l	#$10000,any+12(a5)
		move.b	#56,any+11(a5)
		move.l	psp(a5),a5
		move.l	#$fffefffb,any(a5)
		move.b	#15,fr+1(a5)
		move.l	#$10000,any+12(a5)
		move.b	#56,any+11(a5)
		sub.w	#2,x(a5)
		move.l	psp(a5),a5
		move.l	#$0002fffb,any(a5)
		move.b	#15,fr+1(a5)
		move.l	#$10000,any+12(a5)
		move.b	#56,any+11(a5)
		add.w	#2,x(a5)
		move.l	psp(a5),a5
		move.l	#$fffdfffe,any(a5)
		move.b	#15,fr+1(a5)
		move.l	#$10000,any+12(a5)
		move.b	#56,any+11(a5)
		sub.w	#4,x(a5)
		move.l	psp(a5),a5
		move.l	#$0003fffe,any(a5)
		move.b	#15,fr+1(a5)
		move.l	#$10000,any+12(a5)
		move.b	#56,any+11(a5)
		add.w	#4,x(a5)
		move.l	#-$80000,any+2(a6)
		move.w	zoolwx,d0
		st.b	ht(a6)
		sub.w	#$10,y(a6)
		move.w	#-3,any+6(a6)
		move.b	#10,fr+1(a6)
		cmp.w	x(a6),d0
		bmi	.nowait
		move.w	#3,any+6(a6)
		move.b	#11,fr+1(a6)
.nowait
		move.l	#374,scoid
		move.b	#$81,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien
			    
		tst.b	any+8(a6)
		beq	.nopause
		subq.b	#1,any+8(a6)
		bne	.cont
		move.l	#-$80000,any+2(a6)
		move.w	#-3,any+6(a6)
		move.b	#10,fr+1(a6)
		move.w	zoolwx,d0
		cmp.w	x(a6),d0
		bmi	.nopause
		move.w	#3,any+6(a6)
		move.b	#11,fr+1(a6)
.nopause	
		tst.w	any+6(a6)
		beq	.nowall
		move.w	x(a6),d0  		X check with background.
	 	add.w	#$d,d0
		tst.w	any+6(a6)
		bpl	.rightdir
	 	sub.w	#22,d0
.rightdir	move.w	y(a6),d1
		sub.w	#$e,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.nowall
    		btst.b	#0,1(a1,d3.w)
		beq	.nowall
		jmp	.wall
.nowall		move.w	any+6(a6),d0	
		add.w	d0,x(a6)
.wall
	
       
		move.w	any+2(a6),d0		Move Y.
		add.w	d0,y(a6)

		tst.l	any+2(a6)		Up or down detection ?
		bmi	.topch
		move.w	x(a6),d0
		move.w	y(a6),d1
		add.w	#12,d1
		jsr	checkblock
		btst.b	#3,1(a1,d3.l)
		bne	.ong
		btst.b	#0,1(a1,d3.l)
		bne	.ong
		btst.b	#2,1(a1,d3.l)
		bne	.ong
		jmp	.grav

.topch		move.w	x(a6),d0		Ceiling check.
		move.w	y(a6),d1
		sub.w	#16,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.l)
		beq	.grav
		move.l	#$10000,any+2(a6)

.grav		cmp.w	#$e,any+2(a6)		Do gravity.
		beq	.cont
		add.l	#$e000,any+2(a6)
		jmp	.cont

.ong		move.b	#10,any+8(a6)		Just hut ground.
		clr.l	any+2(a6)
		subq.b	#2,fr+1(a6)
		add.w	#12,y(a6)
		andi.w	#$fff0,y(a6)
		sub.w	#12,y(a6)
.cont
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien50		; Crocadile clip thing ...

		move.w	t(a6),-(sp)
		move.w	fr(a6),-(sp)
		move.l	x(a6),-(sp)
		move.l	blbset(a6),-(sp)
		move.w	ht-1(a6),-(sp)
		jsr	freesprite
		jsr	getlowersprite
		move.w	(sp)+,ht-1(a6)
		move.l	(sp)+,blbset(a6)
		move.l	(sp)+,x(a6)
		move.w	(sp)+,fr(a6)
		move.w	(sp)+,t(a6)

		subq.b	#1,any+10(a6)
		bpl	.n
  		move.b	#3,any+10(a6)
		eori.b	#1,fr+1(a6)
.n
		tst.w	any+6(a6)
		bne	.noreset
		move.w	y(a6),any+6(a6)
		add.w	#32,any+6(a6)
.noreset
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		addq.w	#6,d0
		move.w	#20,d2
		move.w	#8,d3
		jsr	alwithzool
		beq	.ok
		jsr	hurtzool
.ok
		move.w	any+2(a6),d0
		add.w	d0,x(a6)
	
		subq.b	#1,any+9(a6)
		bpl	.sk
   		move.b	#$40,any+9(a6)
		neg.w	any+2(a6)
		eori.b	#2,fr+1(a6)
.sk
		tst.b	any+8(a6)
		beq	.nnhg
		cmp.b	#$10,any+9(a6)
		bmi	.notall
 		subq.b	#1,any+8(a6)
		jmp	.notall
.nnhg
		tst.b	any+4(a6)
		bne	.goingup

		addq.w	#2,y(a6)
		subq.b	#2,ht(a6)
		tst.b	ht(a6)
		bne	.notall
		jsr	random
		andi.b	#$3f,d0
		add.b	#$20,d0
		move.b	d0,any+8(a6)
		eori.b	#1,any+4(a6)
		jmp	.notall

.goingup	subq.w	#2,y(a6)		Move up.
		addq.b	#2,ht(a6)
		cmp.b	#28,ht(a6)		Full up ?
		bne	.notall
       		eori.b	#1,any+4(a6)
.notall
		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien51		; Swiv ...

		subq.b	#1,any+9(a6)
		bpl	.fok
		move.b	#1,any+9(a6)
		subq.b	#1,fr+1(a6)
		bpl	.fok
   		move.b	#2,fr+1(a6)
.fok
		move.l	any(a6),a0
		tst.b	any+8(a6)
		bne	.nonewmove
		add.l	#6,any+4(a6)
		move.l	any+4(a6),d0
		cmp.w	#$8000,(a0,d0.w)
		bne	.noreset
		clr.l	any+4(a6)
		moveq	#0,d0
.noreset	move.b	5(a0,d0.w),any+8(a6)
.nonewmove	subq.b	#1,any+8(a6)
		move.l	any+4(a6),d0
		move.w	(a0,d0.w),d1
		add.w	d1,x(a6)
		move.w	2(a0,d0.w),d1
		add.w	d1,y(a6)


		move.l	#127,scoid
		move.b	#3,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien
			    
		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien52		; Chainsaw setter from left.

		jsr	freesprite
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien53		; Keyboard key.

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		add.w	#8,d0
		sub.w	#16,d1
		move.w	#16,d2
		move.w	#32,d3
		jsr	alwithzool		Zool on this alien ?
		bne	.ping
		clr.b	ht(a6)
		jmp	.noon
.ping		
		move.b	any+5(a6),currentnote	For note controller.
		
		tst.b	ht(a6)
		bne	.noon
		move.w	any(a6),same23
		move.b	any+3(a6),same23+4
		moveq	#$e,d0
		jsr	playsample
		st.b	ht(a6)
.noon
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien54		; Muzakcat ...

		tst.b	any(a6)
		beq	.donewith
		clr.b	any(a6)
		move.l	a6,a4
		move.l	a6,a3
		move.l	#catmem,a2
		move.w	#7-1,d4
.next		jsr	getlowersprite
		cmp.l	#-1,a6
		beq	nextdeac
		add.l	#4*6,a2
		clr.b	flal(a6)
		move.l	a2,any+2(a6)
		move.l	a6,jsp(a3)		Link stuff.
		move.l	a6,a3
		sub.w	#8,x(a5)
		move.l	x(a4),x(a6)
		add.w	#$10,x(a6)
		clr.b	t(a6)
		move.b	#1,fr+1(a6)
		move.l	#muzakcat,blbset(a6)
		dbra	d4,.next
		clr.l	jsp(a6)
		move.l	#catmem,a2
		moveq	#5*10,d0
.mee		clr.l	(a2)+
		dbra	d0,.mee
		move.l	a6,any+12(a4)
		move.b	#3,fr+1(a6)
		sub.w	#$10,x(a6)
		sub.w	#$3,y(a6)
		sub.l	#4*3,any+2(a6)
		move.l	a4,a6
.donewith
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.ping
		jsr	hurtzool
.ping		
		move.l	any+2(a6),a2
		sub.l	#4,a2
		cmp.l	#catmem,a2
		bpl	.noresetset
	   	move.l	#catmem,a2
		add.l	#(4*5*13),a2
.noresetset	move.l	a2,any+2(a6)

		addq.l	#4,any+6(a6)		Move cat Y.
		move.l	any+6(a6),a3
		cmp.w	#$8000,(a3)
		bne	.nores
		lea	catys,a3
      		move.l	a3,any+6(a6)
.nores		move.w	(a3),d0
		tst.b	any+10(a6)
		beq	.sk
   		neg.w	d0
.sk		add.w	d0,x(a6)
		tst.w	d0
		beq	.okfr
		clr.b	fr+1(a6)
		tst.w	d0
		bmi	.okfr
     		move.b	#4,fr+1(a6)
.okfr		move.w	d0,(a2)
		move.w	2(a3),d0
		move.w	d0,2(a2)

		move.l	any+12(a6),d6

		clr.b	flal(a6)
		move.l	a6,a3
		move.l	a6,-(sp)		Move cat parts.
.nextplease	tst.l	jsp(a6)
		beq	.nomore
		move.l	jsp(a6),a6
		clr.b	flal(a6)
		move.l	any+2(a6),a2
		sub.l	#4,a2
		cmp.l	#catmem,a2
		bpl	.noresetset2
	   	move.l	#catmem,a2
		add.l	#(4*5*13),a2
.noresetset2	move.l	a2,any+2(a6)
		move.w	(a2),d0
		add.w	d0,x(a6)
		tst.w	d0	
		beq	.noxmom
		move.b	fr+1(a6),d3
		andi.b	#3,d3
		cmp.b	#1,d3  x
		bne	.decc2
		addq.b	#1,fr+1(a6)
		jmp	.decc2
.decc2		tst.w	d0	
		bgt	.r
		cmp.b	#4,fr+1(a6)
		bmi	.d
  		subq.b	#4,fr+1(a6)
		jmp	.d
.r		cmp.b	#4,fr+1(a6)
		bpl	.d
  		addq.b	#4,fr+1(a6)
		jmp	.d

.noxmom		move.b	fr+1(a6),d0
		andi.b	#3,d0
		cmp.b	#2,d0
		bne	.d
		subq.b	#1,fr+1(a6)

.d		move.w	2(a2),d0
		add.w	d0,y(a6)
		jmp	.nextplease
.nomore		move.l	(sp)+,a6

		move.w	x(a6),d0  		Check with background.
	 	add.w	#8,d0
		tst.b	any+10(a6)
		bne	.rightdir
	 	sub.w	#20,d0
.rightdir	move.w	y(a6),d1
		add.w	#25,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.swap
		btst.b	#3,1(a1,d3.w)
		bne	.swap
    		btst.b	#0,1(a1,d3.w)
		bne	.swap
		add.w	#25,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.swap
    		btst.b	#3,1(a1,d3.w)
		bne	.noswap
    		btst.b	#0,1(a1,d3.w)
		bne	.noswap
.swap  		eori.b	#$1,any+10(a6)
.noswap
	
		jmp	bigoffandend

		rts

catys		dc.w	-1,-2
		dc.w	-1,-2
		dc.w	-1,-1
		dc.w	-2,-1
		dc.w	-2,-0
		dc.w	-3,0
		dc.w	-2,1
		dc.w	-2,1
		dc.w	-2,2
		dc.w	-1,2
		dc.w	-1,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	$8000

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien55		; Drum stick ...

		tst.b	any+2(a6)
		bne	.done
		st.b	any+2(a6)
		move.l	a6,-(sp)
		move.l	a6,a3
		moveq	#7-1,d7
.nextspr	jsr	getsprite
		cmp.l	#-1,a6
		beq	nextdeac
		move.l	a6,jsp(a3)
		move.l	a6,a3
		move.l	#dstick,blbset(a6)
		clr.b	t(a6)
		clr.b	fr+1(a6)
		dbra	d7,.nextspr
		move.l	(sp)+,a6
.done

.notoff		move.l	jsp(a6),a5		Set dsitck positions.
		move.l	a5,d3
		moveq	#0,d6    
		move.w	any(a6),d0
		tst.w	d0
		bpl	.pos
		neg.w	d0
		st.b	d6
.pos		mulu	#ds2-ds1,d0
		lea	ds1,a0
		add.w	d0,a0
		moveq	#7-1,d7
.next		move.l	x(a6),x(a5)

		move.l	(a0)+,d0
		move.b	d0,fr+1(a5)

		move.l	(a0)+,d0
		tst.w	any(a6)
		bpl	.right
		neg.w	d0
		cmp.l	a5,d3
		beq	.b
		add.b	#8,fr+1(a5)
  		sub.w	#(14*2)-16,d0
		jmp	.right
.b		sub.w	#14*2,d0
.right		add.w	d0,x(a5)

		move.l	(a0)+,d0
		add.w	d0,y(a5)
	
		move.l	jsp(a5),a5
		dbra	d7,.next


		move.l	jsp(a6),a5
		clr.b	onstick
		tst.w	ymom			On the end ?
		bmi	.notonend
		tst.b	clinging
		bne	.notonend
		move.w	x(a5),d0
		move.w	y(a5),d1
		sub.w	#20,d1
		move.w	#32,d2
		move.w	#5,d3
		st.b	liftcheck
		jsr	norangecheck
		beq	.notonend

		st.b	onstick
		clr.w	any+8(a6)

		move.w	y(a5),zoolwy		Set Zool Y pos.
		sub.w	#60,zoolwy
		move.w	zoolwy,zooly
		add.w	#47,zooly
		move.w	scrwy,d0
		sub.w	d0,zooly

		lea	posposes,a0		Move Ball position when on.
		move.w	any(a6),d3		Save last ball position.
		move.w	d3,d7
		move.w	d3,d0
		tst.w	d0
		bpl	.itspos
       		neg.w	d0
.itspos		cmp.b	#$80,(a0,d0.w)
		bne	.nores
		clr.w	any+4(a6)

		st.b	mustup
		clr.b	ducking
		clr.b	punching
		clr.b	slideing
		st.b	jumping
		move.w	#2,d0
		jsr	playsample

		move.l	#-$a0000,ymom
		move.b	#20,uptime
		move.w	#-3,any+8(a6)
		jmp	.notonend
.nores
		moveq	#0,d1
		move.b	(a0,d0.w),d1
		
		tst.w	d3
		bne	.well
     		tst.b	zoold
		bne	.ok
		jmp	.negitleft
.well		tst.w	d3
		bgt	.ok
.negitleft	neg.w	d1
.ok		add.w	d1,any(a6)

		moveq	#0,d1
		lea	ds1+4,a0
		move.w	any(a6),d0		Calc zool movement for the -
		tst.w	d0
		bpl	.okpos
		neg.w	d0
		mulu	#ds2-ds1,d0
		move.l	(a0,d0.w),d1		X changes.
      		neg.l	d1
		sub.l	#14*2,d1
		jmp	.c2
.okpos	      	mulu	#ds2-ds1,d0
		move.l	(a0,d0.w),d1		X changes.
.c2		
		tst.w	d3
		bpl	.okpos2
		neg.w	d3
		mulu	#ds2-ds1,d3
		move.l	(a0,d3.w),d5
      		neg.l	d1
		sub.l	#14*2,d1
		jmp	.c3
.okpos2		mulu	#ds2-ds1,d3

.c3		move.l	(a0,d3.w),d5
		sub.l	d5,d1

		move.l	d1,d2
		lsr.l	#1,d1
		sub.w	d1,d2
		move.w	d1,xlift2
		move.w	d2,xlift

		cmp.w	any(a6),d7
		bpl	.fc
		add.w	any(a6),d7
		tst.w	d7
		bmi	.neg
		jmp	.nsw

.fc		tst.w	d7
		bpl	.nsw
		tst.w	any(a6)
		bpl	.nsw
.neg		neg.w	xlift
		neg.w	xlift2
.nsw
		moveq	#0,d1
		move.l	4(a0,d0.w),d1		Y changes.
		sub.l	4(a0,d3.w),d1
		move.l	d1,d2
		asr.l	#1,d1
		sub.l	d1,d2
		move.w	d1,ylift2
		move.w	d2,ylift
.nochange	clr.l	ymom
		jmp	.sk
.notonend	move.w	#0,any+4(a6)

		tst.w	any+8(a6)
		bne	.nostop
		tst.w	d3
		beq	.moveing
.nostop		move.w	any(a6),d2			Settle after bent.
		tst.w	d2
		bpl	.okspos
       		neg.w	d2
.okspos		cmp.w	#1,d2
		bgt	.notsettled
		move.w	any+8(a6),d2
		tst.w	d2
		bpl	.okspos2
		neg.w	d2
.okspos2	cmp.w	#1,d2
		bgt	.notsettled
		clr.l	any+8(a6)
		clr.w	any(a6)
		jmp	.moveing
.notsettled	
		tst.w	any(a6)				Settle after bent.
		bpl	.left
		tst.w	any+8(a6)
		bgt	.o
     		add.l	#$e000,any+8(a6)
		jmp	.swing
.o     		add.l	#$6000,any+8(a6)
		jmp	.swing
.left		tst.w	any+8(a6)
		bmi	.o2
		sub.l	#$e000,any+8(a6)
		jmp	.swing
.o2		sub.l	#$6000,any+8(a6)

.swing	       	move.w	any+8(a6),d0
		add.w	d0,any(a6)
		cmp.w	#32,any(a6)
		bmi	.lc
		neg.w	any+8(a6)
		move.w	#31,any(a6)
.lc
		cmp.w	#-31,any(a6)
		bpl	.rc
		neg.w	any+8(a6)
		move.w	#-31,any(a6)
		jmp	.swing
.rc
.moveing
.sk		jmp	bigoffandend

posposes	dc.b	1,1,1,1,1,1,1,1
		dc.b	2,2,2,2,2,2,2,2
		dc.b	3,3,3,3,3,3,3,2
		dc.b	2,2,2,1,1,1,1,$80

;posposes	dc.w	0,1,2,3,4
		dc.w	6,8,10,12,14
		dc.w	17,20,23,25,27,28,29,29,30,30,30
		dc.w	31,31,31,31,$8000
		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien56		; Trumpet blaster ...

		tst.b	any(a6)			Pause stuff.
		beq	.nopause
		clr.b	trumpeton
		subq.b	#1,any(a6)
		bne	.afterpause
		jsr	random
		andi.b	#$3f,d0
		add.b	#$30,d0
		move.b	d0,any+1(a6)
.nopause
		subq.b	#1,any+1(a6)		On stuff.
		bne	.stillgoing
		jsr	random
		andi.b	#$1f,d0
		add.b	#$20,d0
		move.b	d0,any(a6)
		jmp	nextalien

.stillgoing	st.b	trumpeton
		tst.b	any+4(a6)
		beq	.right
      		move.b	#$37,trumpeton
.right
		subq.b	#1,any+2(a6)		Create a note.
		bpl	.nonote
       		move.b	#3,any+2(a6)
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.fin
		jsr	random	   		Choose note type.
		andi.b	#1,d0
		mulu	#3,d0
		move.b	d0,fr+1(a6)
		move.b	#57,t(a6)
		move.b	any+4(a4),any+8(a6)
		move.b	#30,any+9(a6)
		move.l	x(a4),x(a6)
		move.l	#$30000,any(a6)
		clr.l	any+4(a6)
		move.l	#notes,blbset(a6)
		jsr	random
		andi.w	#$3f,d0
		add.w	d0,x(a6)
.fin		move.l	a4,a6		
.nonote
.afterpause	
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien57		; Floating note ...

		move.w	x(a6),d0		Ceiling check.
		move.w	y(a6),d1
		sub.w	#40,d1
		tst.b	any+8(a6)
		beq	.noc
    		add.w	#40*2,d1
.noc		jsr	checkblock
		btst.b	#0,1(a1,d3.l)
		beq	.onone2
		jsr	freesprite
		jmp	nextalien		
.onone2
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.frok
		clr.b	spining
		clr.b	clinging
		clr.b	ducking
		clr.b	slideing
		st.b	jumping
		tst.b	any+8(a6)
		bne	.add
		cmp.w	#-5,ymom
		bmi	.frok
		subq.w	#2,ymom
		jmp	.frok
.add		cmp.w	#6,ymom
		bpl	.frok
		addq.w	#2,ymom
.frok
		cmp.w	#15,any(a6)
		beq	.noaccel
		add.l	#$7000,any(a6)
.noaccel	move.w	any(a6),d0
		tst.b	any+8(a6)
		beq	.sun
    		neg.w	d0
.sun		sub.w	d0,y(a6)

		addq.w	#2,any+4(a6)
		move.w	any+4(a6),d0
		lea	notexmoms,a0
		cmp.w	#$8000,(a0,d0.w)
		bne	.itsallok
		clr.w	any+4(a6)
		moveq	#0,d0
.itsallok	move.w	(a0,d0.w),d0
		add.w	d0,x(a6)

		subq.b	#1,any+9(a6)
		bpl	.onone
		jsr	freesprite
.onone		jmp	nextalien		

notexmoms	dc.w	0,1,2,3,2,1,0,-1,-2,-3,-2,-1,$8000

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien58		; Cybil ...

		eori.b	#1,any+11(a6)

		tst.b	any(a6)
		bne	.allclear 
	 	st.b	any(a6)
		move.w	x(a6),any+2(a6)
		add.w	#$10*2,any+2(a6)
		move.w	x(a6),any+4(a6)
	 	sub.w	#$10*2,any+4(a6)
		move.l	#-$4000,any+6(a6)
		move.w	y(a6),any+20(a6)
.allclear
		tst.b	any+15(a6)		Pulling out of ground ?
		beq	.nopulling
		subq.b	#1,any+15(a6)
		tst.b	any+15(a6)
		bne	.noout
		move.l	#-$50000,any+16(a6)
		move.b	#2,any+14(a6)
		subq.b	#2,fr+1(a6)
		jmp	.nopulling
.noout	  	lea	pullfrs,a0
		moveq	#0,d0
		move.b	any+15(a6),d0
		move.b	(a0,d0.w),fr+1(a6)
		tst.w	any+6(a6)
		bmi	.frok
		addq.b	#4,fr+1(a6)		
.frok		jmp	.skipmovement
.nopulling
		cmp.b	#2,any+14(a6)		Swooping up ?
		bne	.noswoopup
		cmp.w	#-6,any+16(a6)
		beq	.nomorejim
		sub.l	#$c000,any+16(a6)
.nomorejim	move.w	any+16(a6),d0
		add.w	d0,y(a6)
		move.w	any+6(a6),d0
		add.w	d0,x(a6)
		move.w	any+20(a6),d0
		cmp.w	y(a6),d0
		bmi	.skipmovement
		clr.b	any+14(a6)
		move.w	#25*2,any+12(a6)
		jmp	.skipmovement
.noswoopup
		cmp.b	#1,any+14(a6)		Swooping down ?
		bne	.noswoop
		add.l	#$c000,any+16(a6)
		move.w	any+16(a6),d0
		add.w	d0,y(a6)
		move.w	any+6(a6),d0
		add.w	d0,x(a6)
		move.w	x(a6),d0
		move.w	y(a6),d1
		add.w	#7,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.w)
		beq	.skipmovement
		add.w	#7,y(a6)
		andi.w	#$fff0,y(a6)
		sub.w	#7,y(a6)
		addq.b	#2,fr+1(a6)	
		move.b	#20,any+15(a6)
		move.b	#2,any+14(a6)
		jmp	.skipmovement
.noswoop	
		move.w	x(a6),d0 		Which dir ?
		tst.b	any+10(a6)
		beq	.goingleft
		cmp.w	any+2(a6),d0		Right accel.
		bmi	.notatborder
	    	clr.b	any+10(a6)
.notatborder	cmp.w	#$4,any+6(a6)
		beq	.move
	 	add.l	#$4000,any+6(a6)
		jmp	.move
.goingleft	cmp.w	any+4(a6),d0		Left Accel.
		bpl	.notatlborder
	     	st.b	any+10(a6)
.notatlborder	cmp.w	#-4,any+6(a6)
		beq	.move
     		sub.l	#$4000,any+6(a6)
.move		
		move.w	any+6(a6),d0		Move X.
		add.w	d0,x(a6)
		clr.b	fr+1(a6)		Animate.
		move.b	any+11(a6),d1
		add.b	d1,fr+1(a6)
		tst.w	d0
		bmi	.more
     		addq.b	#4,fr+1(a6)
.more		tst.w	d0
		bne	.notxstill
	  	move.b	#8,fr+1(a6)
.notxstill	lea	cybilys,a0		Move Y (off table).
		addq.w	#2,any+12(a6)
		move.w	any+12(a6),d0
		cmp.w	#$8000,(a0,d0.w)
		bne	.itsallok
		clr.w	any+12(a6)
		cmp.w	#4,any+6(a6)
		beq	.tryit
      		cmp.w	#-4,any+6(a6)
		bne	.noswoopit
.tryit		jsr	random
		btst	#0,d0
		beq	.noswoopit
		move.b	#1,any+14(a6)
		clr.l	any+16(a6)
.noswoopit	moveq	#0,d0
.itsallok	move.w	(a0,d0.w),d0
		add.w	d0,y(a6)

.skipmovement	move.l	#200,scoid
		move.b	#$81,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien

		jmp	bigoffandend 

cybilys		dc.w	0,0,1,1,2,2,3,3,4,4,3,3,2,2,1,1,0,0,-1,-1,-2,-2,-3,-3,-4,-4,-3,-3,-2,-2,-1,-1,$8000		
pullfrs		dc.b	2,2,3,3,2,2,3,3,3,2,2,2,3,3,3,3,2,2,2,2,3,3,3
		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien59		; Cello ...

		move.l	#340,scoid
		move.b	#8,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111001,detecm
		jsr	collision
		bne	nextalien

		move.b	#%00000001,d7		Fall,No home.
		move.w	#$a,yoff
		move.l	#-$80000,bmom		Bouncing
		move.b	#10,pauseifb
		jsr	walkabout

		cmp.b	#5,any+8(a6)		Fire a bow ?
		bne	.nofirebow
		jsr	random
		btst	#0,d0
		beq	.nofirebow
		move.l	a6,a4
		jsr	getlowersprite		Get sprite ...
		cmp.l	#-1,a6
		beq	.didntgetone
		move.w	#$f0,sam3+4	
		move.w	#3,d0
		jsr	playsample

		move.l	x(a4),x(a6)
		add.w	#25,y(a6)
		add.w	#10,x(a6)
		move.l	#cello,blbset(a6)
		move.b	#3,fr+1(a6)
		move.w	any(a4),d0
		add.w	d0,d0
		add.w	any(a4),d0
		move.w	d0,any(a6)
		tst.w	any(a4)
		bpl	.frok
		sub.w	#20,x(a6)
     		move.b	#7,fr+1(a6)
.frok		move.b	#60,t(a6)
.didntgetone	move.l	a4,a6
.nofirebow	
		tst.b	any+8(a6)		Animation ...
		beq	.nw
		move.b	#2,fr+1(a6)
		jmp	.set
.nw		clr.b	fr+1(a6)
		tst.w	any+4(a6)
		bmi	.set
		move.b	#1,fr+1(a6)
.set		tst.w	any(a6)
		bmi	.leave
      		addq.b	#4,fr+1(a6)
.leave
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien60		; Bow from cello.

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#2,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.ok
		jsr	hurtzool
.ok
		move.w	any(a6),d0
		add.w	d0,x(a6)

		move.w	x(a6),d0  		X check with background.
	 	add.w	#$d,d0
		tst.w	any(a6)
		bpl	.rightdir
	 	sub.w	#22,d0
.rightdir	move.w	y(a6),d1
		sub.w	#$10,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.nowall
    		btst.b	#0,1(a1,d3.w)
		beq	.nowall
		jsr	freesprite
		jmp	nextalien
.nowall		
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
			      
alien61		; Drum...

		tst.b	ht(a6)
		bne	.ok
   		st.b	ht(a6)
		move.w	#3,any(a6)		Starting speed.
		move.w	zoolwx,d0
		cmp.w	x(a6),d0
		bpl	.ok
		neg.w	any(a6)
.ok
		subq.b	#1,any+9(a6)
		bpl	.no
		move.b	#$f,any+9(a6)
.no		moveq	#0,d0
		move.b	any+9(a6),d0
		eori.b	#$f,d0
		lsr.b	#2,d0
		move.b	d0,fr+1(a6)
		tst.w	any(a6)
		bmi	.no2
   		addq.b	#4,fr+1(a6)
.no2			    	

		move.b	#%00000010,d7		No fall,Do home.
		move.w	#$6,yoff
		clr.l	bmom
		move.b	#6,pauseifb
		jsr	walkabout
		btst	#0,d6			Stand frame if at wall.
		beq	.nowall
		clr.b	any+9(a6)
.nowall		tst.b	any+8(a6)		Pause frame.
		beq	.nopause
		move.b	#8,fr+1(a6)
.nopause
		
		st.b	drumset
		move.l	#340,scoid
		move.b	#$81,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		clr.b	drumset
		tst.b	d7
		bne	nextalien

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien62		; Bass wobble ...

		eori.b	#$ff,ht(a6)

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#31,d2
		move.w	#32,d3
		jsr	alwithzool
		beq	.ok
		tst.b	spining
		beq	.ok
		add.l	#$00080008,x(a6)
		jsr	bonusexplo
		jsr	freeplease
		jmp	nextalien
.ok
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien63		; Note lift...

		eori.b	#$1,fr+1(a6)

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		add.w	#55,d1
		move.w	#8,d2
		move.w	#8,d3
		st.b	liftcheck
		jsr	norangecheck
		beq	.ok

		st.b	onnl
		sub.w	#2,y(a6)
		clr.w	splitpower
		st.b	onrails
		move.w	y(a6),zoolwy
		add.w	#$61,zoolwy
		sub.w	#60,zoolwy
		move.w	zoolwy,zooly
		add.w	#44,zooly
		move.w	scrwy,d0
		sub.w	d0,zooly
		move.w	zooly,zooly1
	
		move.w	x(a6),zoolwx
		add.w	#56,zoolwx
		move.w	zoolwx,zoolx
		add.w	#44,zoolx
		move.w	scrwx,d0
		sub.w	d0,zoolx
		move.w	zoolx,zoolx1

		clr.l	ymom

		move.w	#-1,ylift
.ok	
		clr.b	liftcheck

		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien64		; Pipe note maker.

		tst.b	any(a6)
		bne	.donethis
	 	jsr	random
		andi.b	#$3f,d0
		add.b	#$30,d0	
		move.b	d0,any(a6)
.donethis
		subq.b	#1,any(a6)	
		bne	.notyet
		jsr	random
		andi.b	#$7f,d0
		add.b	#$30,d0	
		move.b	d0,any(a6)
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.didntget
	 	move.l	#notes,blbset(a6)
		move.l	x(a4),x(a6)
		sub.w	#$10,y(a6)
		clr.b	fr+1(a6)
		jsr	random
		btst	#0,d0
		beq	.okokok
		move.b	#3,fr+1(a6)
.okokok		move.b	#65,t(a6)
		move.b	#10,any(a6)
.didntget	move.l	a4,a6
.notyet
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien65		; Pipe note.

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.frok
		clr.b	spining
		clr.b	clinging
		clr.b	ducking
		clr.b	slideing
		st.b	jumping
		move.w	#2,d0
		jsr	playsample

		move.w	#-6,ymom		Move zool ...
		move.w	#-2,xmom
		tst.b	fr+1(a6)
		beq	.frok
		move.w	#2,xmom
.frok
		subq.w	#5,y(a6)		Move note ...

		tst.b	any(a6)
		bne	.notyet
		addq.b	#1,fr+1(a6)
		cmp.b	#3,fr+1(a6)
		beq	.kill
		cmp.b	#6,fr+1(a6)
		bne	nextalien
.kill		jsr	freesprite
		jmp	nextalien		
.notyet		subq.b	#1,any(a6)
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien66		; Record deck overlay ...

		tst.w	recordmom		Give bonus if constant speed.
		beq	nozeros
		move.w	recordmom,d0
		cmp.w	any+10(a6),d0
		beq	same9
nozeros		clr.b	any+12(a6)
		move.w	recordmom,any+10(a6)
		jmp	not
same9		addq.b	#1,any+12(a6)
		cmp.b	#$c0,any+12(a6)
		bne	not
		move.b	#$ba,any+12(a6)
		tst.b	onealevel
		beq	not
		subq.b	#1,onealevel

		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.didnt
		move.b	#21,any+10(a6)
		move.l	x(a4),x(a6)
		sub.w	#10,y(a6)
		sub.w	#16,x(a6)
		move.l	#lev1stuf,blbset(a6)
		move.b	#6,t(a6)
		clr.b	any+30(a6)
		move.l	#$e000,any+12(a6)
		jsr	random		X
		andi.w	#$3,d0
		sub.w	#$a,d0
		move.w	d0,any+2(a6)
.n		jsr	random		Y
		andi.w	#$7,d0
		cmp.b	#7,d0
		beq	.n
		sub.w	#$3,d0
		move.w	d0,any(a6)
		clr.w	any+4(a6)
.ag		jsr	random
		andi.b	#7,d0
		cmp.b	#5,d0
		bpl	.ag
		add.b	#17,d0
		move.b	d0,fr+1(a6)
    		sub.w	#4,any+2(a6)
    		sub.w	#1,any(a6)

.didnt		move.l	a4,a6
not		

		tst.b	any(a6)
		bne	.already
		st.b	any(a6)
		lea	recxoffs,a0
		move.l	a6,a3
		move.l	a6,a4
		moveq	#5-1,d3
		moveq	#0,d4
.morerecparts	jsr	getsprite
		cmp.l	#-1,a6
		beq	.notenough
		move.l	x(a4),x(a6)
		move.w	(a0)+,d0
		add.w	d0,x(a6)
		move.l	#deck,blbset(a6)
		move.l	a6,jsp(a3)
		move.l	a6,a3
		clr.b	t(a6)
		move.b	d4,fr+1(a6)
		addq.b	#1,d4			Next frame...
		cmp.b	#3,d4
		bne	.nam
		addq.b	#1,d4
.nam		clr.b	ht(a6)
		dbra	d3,.morerecparts
		move.l	a4,a6
		jmp	.already
.notenough	move.l	a4,a6			Give up if not enough sprs.
		jmp	nextdeac
.already
		
		tst.w	recordmom		Spin in time.
		bne	.spinit
   		clr.b	any+1(a6)
		jmp	.nyet
.spinit		subq.b	#1,any+2(a6)
		bpl	.nyet
     		eori.b	#$ff,any+1(a6)
		move.w	recordmom,d0
		tst.w	d0
		bpl	.nneg
     		neg.w	d0
.nneg		moveq	#3,d1
		sub.w	d0,d1
		move.b	d1,any+2(a6)
.nyet

		move.l	a6,a5			Flip it.
.next		move.b	any+1(a6),ht(a5)
		tst.l	jsp(a5)
		beq	.nomore
		move.l	jsp(a5),a5
		jmp	.next
.nomore
		jmp	offandend

recxoffs	dc.w	-$60,-$40,-$20,$10,$30,$50

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien67		; Tune controller.

		cmp.b	#$80,currentnote
		beq	.fincheck
		move.l	any+16(a6),a0
		cmp.b	#$ff,(a0)
		beq	.doneit
		move.b	currentnote,d0
		cmp.b	(a0),d0
		bne	.back
		addq.l	#1,any+16(a6)
		jmp	.fincheck
.back		cmp.b	-1(a0),d0
		beq	.fincheck
.goaway		move.l	any+12(a6),any+16(a6)
.fincheck
		jmp	bigoffandend	       

.doneit
		cmp.l	#tune4,any+12(a6)
		bne	.momo
		tst.b	donetune4
		bne	.goaway

		st.b	donetune4
		move.b	worldno+3,tw
		move.b	areano+3,ta
		move.b	#6,worldno+3			Set Bonus place ...
		move.b	#2,areano+3
		move.l	#((16*6)*($4b-$a)),restarthereb		Set restart ...
		move.l	#($56-8),restarthereb+4
		move.l	#$01100090,restarthereb+8
		move.l	skypl,restarthereb+12
		move.l	collected,collectedst

		move.l	#bye,inaframe+2

		st.b	fromtitle
	 	move.l	#blankcop,$dff080
		clr.l	musicplace
		move.w	#$f,$dff096
		clr.l	ch1timer
		clr.l	forcesample1
		jsr	synchup
		jmp	loadnextlevel

.momo		move.b	any+20(a6),t(a6)
		st.b	ht(a6)
		jmp	nextalien

donetune4	dc.w	0
				 
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien68		; Extra zool note ...
		
		tst.b	any(a6)
		beq	.itsanote

		subq.w	#1,y(a6)
		cmp.b	#1,any(a6)
		beq	.itsazoolfly
		subq.b	#1,any(a6)
		jmp	.ok
.itsazoolfly	move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.ok
		addq.l	#1,lives
		jsr	freeplease
		jmp	nextalien 

.itsanote	move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	bulhutal
		tst.b	d7
		bne	.split
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.ok
		tst.b	spining
		beq	.ok
.split		move.w  #$81,d4
		jsr	createexplo2
		move.l	#lev1stuf,blbset(a6)
		move.b	#6,fr+1(a6)
		move.b	#10,any(a6)	
.ok
      		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien69		; Spark gen.

		subq.b	#1,any(a6)
		bpl	.notyet
		move.b	#$40,any(a6)
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.notgot			    	
		move.b	#70,t(a6)
		move.l	#spark,blbset(a6)
		clr.b	fr+1(a6)
		move.w	any+2(a4),any(a6)
		move.l	x(a4),x(a6)
.notgot		move.l	a4,a6
.notyet		
		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien70		; Spark on climbing wires.

		subq.b	#1,fr+1(a6)
		bpl	.nofr
		move.b	#3,fr+1(a6)
.nofr
		move.w	any(a6),d0
		add.w	d0,x(a6)

		move.w	x(a6),d0  		Check with background.
		move.w	y(a6),d1
		jsr	checkblock
		btst.b	#$7,1(a1,d3.w)		Check for monkey bars.
		bne	.on
		jsr	freesprite
		jmp	nextalien
.on
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#20,d3
		jsr	alwithzool
		beq	.ok
   		jsr	hurtzool
.ok
		jmp	offandend		

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien71		; Bell shield ...

		tst.b	any+1(a6)		Once on the floor.
		beq	.notonfloor

		tst.b	fire
		beq	.notbreakout
		clr.b	bellm
		clr.b	onrails                
		add.w	#$10,x(a6)
		moveq	#2,d4
		jsr	createexplo2
		jsr	freesprite
		move.l	jsp(a6),a6
		jsr	freesprite
		jmp	nextalien
.notbreakout
		move.w	zoolwx,d0
		move.w	zoolwy,d1
		add.w	#$30,d1
		tst.b	left
		beq	.nl
		jsr	checkblock
		btst.b	#0,1(a1,d3.w)
		beq	.nr
		subq.w	#2,x(a6)
   		move.w	#-1,xlift
.nl		tst.b	right
		beq	.nr
		add.w	#$30,d0
		jsr	checkblock
		btst.b	#0,1(a1,d3.w)
		beq	.nr
		addq.w	#2,x(a6)
   		move.w	#1,xlift
.nr		jmp	.keepzool

.notonfloor	tst.b	any(a6)			Falling ?
		beq	.no
		cmp.b	#1,any(a6)
		beq	.doit
		subq.b	#1,any(a6)
		jmp	nextalien		
.doit		add.l	#$c000,any+2(a6)

		move.w	zoolwx,d0  		Zool spr hut ground ?
		move.w	zoolwy,d1
		add.w	#$26,d1
		jsr	checkblock
		move.w	any+2(a6),d0
		btst.b	#0,1(a1,d3.w)
		beq	.both
		move.l	a6,a4
		move.l	jsp(a6),a5
		tst.l	jsp(a5)
		bne	.ng
		move.b	#$20,bellm
		jsr	getlowersprite
		cmp.l	#-1,a6
		beq	.ng
		move.l	zoolwx,x(a6)
		add.l	#$00180006,x(a6)
		move.l	#bell,blbset(a6)
		clr.b	t(a6)
		move.b	#7,fr+1(a6)
		tst.b	zoold
		bne	.nozd
     		addq.b	#1,fr+1(a6)
		sub.w	#4,x(a6)
.nozd		move.l	jsp(a4),a5
		move.l	a6,jsp(a5)
.ng		move.l	a4,a6
     		jmp	.ksd
.both		add.w	d0,zooly
.ksd		add.w	d0,y(a6)
		
		move.w	x(a6),d0  		Bell hut ground ?
		move.w	y(a6),d1
		add.w	#$20,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.w)
		beq	.keepzool
		st.b	any+1(a6)
		move.b	#48,ht(a6)
		move.l	jsp(a6),a5
		move.b	#48,ht(a5)
		andi.w	#$fff0,y(a6)
		subq.w	#1,y(a6)
		move.l	a6,-(sp)
		move.l	jsp(a6),a6
		move.l	jsp(a6),a6
		cmp.l	#0,a6
		beq	.nnnnn
		jsr	freesprite
.nnnnn		move.l	(sp)+,a6
		jmp	.keepzool

.no		tst.w	ymom
		bgt	.ok
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		add.w	#$10,d0
		add.w	#55,d1
		move.w	#1,d2
		move.w	#8,d3
		st.b	liftcheck
		jsr	norangecheck
		beq	.ok
		move.b	#10,any(a6)
		clr.l	any+2(a6)
		move.w	y(a6),zoolwy
		add.w	#$5a,zoolwy
		sub.w	#60,zoolwy
		move.w	zoolwy,zooly
		add.w	#64,zooly
		move.w	scrwy,d0
		sub.w	d0,zooly
		move.w	zooly,zooly1

		move.w	x(a6),zoolwx
		add.w	#68,zoolwx
		move.w	zoolwx,zoolx
		add.w	#44,zoolx
		move.w	scrwx,d0
		sub.w	d0,zoolx
		move.w	zoolx,zoolx1

		clr.l	ymom
		st.b	bellm
.keepzool	st.b	onrails
.ok		clr.b	liftcheck

		move.l	jsp(a6),a5
		move.l	x(a6),x(a5)
		add.w	#$20,x(a5)

		jmp	bigoffandend

bye		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien72		; Little keyboard ...

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		sub.w	#10,d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	alwithzool
		beq	offandend
		move.l	any(a6),a0
		move.l	a6,a4
		add.w	#$18,x(a6)
.morenotes	jsr	getsprite
		cmp.l	#-1,a6
		beq	.ohwell
		move.l	x(a4),x(a6)
		jsr	random
		andi.w	#$f,d0
		lsl.w	#3,d0
		add.w	d0,y(a6)
		add.w	#$100,y(a6)
		add.w	#$10,x(a4)
		moveq	#0,d0
		move.b	(a0)+,d0
		move.b	d0,fr+1(a6)
		lsl.w	#2,d0
		add.w	#$10,d0
		move.w	y(a4),any(a6)
		add.w	d0,any(a6)
		clr.l	any+2(a6)
		move.b	#73,t(a6)
		move.l	#smnotes,blbset(a6)
		cmp.b	#$ff,(a0)
		bne	.morenotes
.ohwell		move.l	a4,a6
		jsr	freesprite
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien73		; Note up ...

		tst.b	any+2(a6)
		beq	.noteyet
		subq.b	#1,any+2(a6)
		bne	nextalien
		jsr	freesprite
		jmp	nextalien
.noteyet	sub.w	#6,y(a6)
		move.w	y(a6),d0
		cmp.w	any(a6),d0
		bpl	nextalien
		st.b	any+2(a6)
		move.w	any(a6),y(a6)
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien74		; Amp ...

		move.l	jsp(a6),a5
		move.w	y(a6),y(a5)
		move.b	ht(a6),ht(a5)
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)


		add.w	#25,y(a6)
		jsr	ampcheck
		sub.w	#25,y(a6)

		tst.b	any+8(a6)
		beq	.nw
   		subq.b	#1,any+8(a6)
		jmp	nextalien
.nw
		tst.b	any+4(a6)
		bne	.goingup
		addq.w	#2,y(a6)		Going down ...
		subq.b	#2,ht(a6)
		tst.b	ht(a6)
		bne	.notall
		move.b	#30,any+8(a6)
		eori.b	#1,any+4(a6)
		jmp	.notall
.goingup	subq.w	#2,y(a6)		Move up ...
		addq.b	#2,ht(a6)
		cmp.b	#64,ht(a6)		Full up ?
		bne	.notall
       		eori.b	#1,any+4(a6)
		move.b	#30,any+8(a6)
.notall
		move.l	jsp(a6),a5
		move.w	y(a6),y(a5)
		move.b	ht(a6),ht(a5)
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

AMPCHECK	move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#10,d1
		move.w	#48,d2
		moveq	#0,d3
		move.b	ht(a6),d3
		jsr	alwithzool
		beq	bye
		jmp	hurtzool

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien75		; Top amp ...


		move.l	jsp(a6),a5
		move.w	y(a6),y(a5)
		move.b	ht(a6),ht(a5)
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)


		cmp.b	#1,fr+1(a6)
		beq	nextalien

		move.w	y(a6),-(sp)		With added Y check ...
		moveq	#0,d6
		move.b	ht(a6),d6
		sub.w	#32,y(a6)
		add.w	d6,y(a6)
		jsr	ampcheck
		move.w	(sp)+,y(a6)

		tst.b	any+8(a6)
		beq	.nw
   		subq.b	#1,any+8(a6)
		jmp	nextalien
.nw
		move.l	jsp(a6),a5
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)

		tst.b	any+4(a6)
		beq	.up

		addq.b	#2,ht(a6)
		addq.b	#2,ht(a5)
		sub.l	#4*2,any(a6)
		sub.l	#2*2,any(a5)
		cmp.b	#64,ht(a6)
		bmi	nextalien
		jmp	.hmm

.up		add.l	#4*2,any(a6)
		add.l	#2*2,any(a5)
		subq.b	#2,ht(a6)
		subq.b	#2,ht(a5)
		bne	nextalien
		move.l	#4*64,any(a6)
		move.l	#2*64,any(a5)
.hmm		eori.b	#1,any+4(a6)
		move.b	#30,any+8(a6)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien76		; Music boss (guitar) ...

		tst.b	any(a6)			Set up other spr stuff.
		bne	.alrsetitup
		st.b	any(a6)
		tst.b	aldone
		beq	.doit
     		jsr	freesprite
		bra	nextalien
.doit		move.b	#15,htg(a6)
		move.b	#70,any+23(a6)
		sub.w	#240,x(a6)
		move.l	a6,a4
		jsr	getsprite		Get other 2 guitar parts.
		move.l	#mboss,blbset(a6)
		move.b	#2,fr+1(a6)
		move.l	a6,jsp(a4)
		move.l	a6,a3
		jsr	getsprite
		move.l	#mboss,blbset(a6)
		move.b	#3,fr+1(a6)
		move.l	a6,jsp(a3)
		move.l	a4,a6
		move.l	x(a6),any+2(a6)	    	Mid point of guitar moms.
		add.l	#$00300020,any+2(a6)
		sub.w	#$30,any+2(a6)
		sub.w	#$a0,y(a6)
		add.w	#$50,x(a6) 
		move.w	x(a6),any+14(a6)
		move.w	y(a6),any+18(a6)
.alrsetitup

		tst.b	any+24(a6)
		beq	.notdead
		move.w	#120,shield
		st.b	aldone
		st.b	stophorz
		add.l	#$a000,any+10(a6)
		move.w	any+10(a6),d0
		add.w	d0,y(a6)
		move.w	any+6(a6),d0
		add.w	d0,x(a6)
		move.l	jsp(a6),a5
		move.l	jsp(a5),a4
		move.l	x(a6),x(a5)
		add.w	#16,x(a5)
		move.l	x(a6),x(a4)
		add.w	#16+32,x(a4)
		subq.b	#1,any+25(a6)
		bpl	.nn
  		move.b	#2,any+25(a6)
		move.w	#$81,d4
		add.w	#$0020,x(a6)
		add.w	#$0020,y(a6)
		move.w	#$80,d4
		jsr	createexplo2
		sub.w	#$0020,x(a6)
		sub.w	#$0020,y(a6)
.nn		jmp	offandend
.notdead		
		st.b	stophorz

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#48,d2
		moveq	#64,d3
		jsr	alwithzool
		beq	.n
		jsr	hurtzool
.n
  		move.l	#340,scoid
		move.b	#$80,sefid
		move.w	#32,offitx
		move.w	#48,sizex 
		move.w	#10,offity
		move.w	#48,sizey
		st.b	detecn
		move.b	#%11100000,detecm
		jsr	collision
		clr.b	drumset
		tst.b	d7
		bne	nextalien

		clr.b	fr+1(a6)		Blink stuff.
		subq.b	#1,any+1(a6)
		bpl	.nr
   		move.b	#$80,any+1(a6)
.nr		cmp.b	#$30,any+1(a6)
		bgt	.notopen
		move.b	#1,fr+1(a6)
		move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	bulhutal
		tst.b	d7
		beq	.notopen
		move.b	#2,flal(a6)
		move.l	jsp(a6),a5
		move.b	#2,flal(a5)
		move.l	jsp(a5),a5
		move.b	#2,flal(a5)
		subq.b	#1,htg(a6)
		bne	.notopen
		st.b	any+24(a6)
		move.b	#1,any+25(a6)
.notopen
		move.w	x(a6),d0
		cmp.w	any+2(a6),d0
		bmi	.addx
		sub.l	#$7000,any+6(a6)
		jmp	.doy
.addx		add.l	#$7000,any+6(a6)
.doy		move.w	y(a6),d0
		cmp.w	any+4(a6),d0
		bmi	.addy
		sub.l	#$7000,any+10(a6)
		jmp	.domove
.addy		cmp.w	#$7,any+10(a6)
		beq	.domove
		add.l	#$7000,any+10(a6)
.domove		move.l	any+6(a6),d0
		add.l	d0,any+14(a6)
		move.w	any+14(a6),x(a6)
		move.l	any+10(a6),d0
		add.l	d0,any+18(a6)
		move.w	any+18(a6),y(a6)

		move.l	jsp(a6),a5
		move.l	jsp(a5),a4
		move.l	x(a6),x(a5)
		add.w	#16,x(a5)
		move.l	x(a6),x(a4)
		add.w	#16+32,x(a4)

		subq.b	#1,any+22(a6)
		bpl	.nfire
		jsr	random
		and.b	#$1f,d0
		add.b	#10,d0
		move.b	d0,any+22(a6)
		move.l	a6,a4
		jsr	getsprite 		Set up the plectrum.
		move.l	#mboss,blbset(a6)
		move.b	#4,fr+1(a6)
		move.b	#77,t(a6)
		move.l	x(a4),x(a6)
		add.w	#100,x(a6)
		sub.w	#100,y(a6)
		move.w	#4,any+2(a6)
		move.w	#-5,any+6(a6)
		clr.b	any+8(a6)
		move.w	#$d0,sam3+4	
		move.w	#3,d0
		jsr	playsample

		move.l	a6,a4
.nfire
		move.l	x(a6),mbossxy

		jmp	nextalien

*----------------------------------------------------------------------------*
nextalien	jmp	nextalien2

*----------------------------------------------------------------------------*

alien77		; Mboss plectrum ...

		move.l	#340,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111001,detecm
		jsr	collision
		clr.b	drumset
		tst.b	d7
		bne	nextalien

		tst.b	any+8(a6)
		bne	.doneit
		move.l	zoolwx,-(sp)		Hut guitar strings.
		move.l	mbossxy,zoolwx
		move.w	x(a6),d0		
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	alwithzool		
		beq	.nohit
		st.b	any+8(a6)
		move.l	a6,a0
		lea	krang,a3
.notlistend	jsr	getsprite
		cmp.l	#-1,a6
		beq	.nogot
		move.l	x(a0),x(a6)
		sub.w	#$8,x(a6)
		sub.w	#$8,y(a6)
		move.l	#mboss,blbset(a6)
		move.b	#33,t(a6)
		move.w	(a3)+,any(a6)
		move.w	(a3)+,any+2(a6)
		move.w	(a3)+,fr(a6)
		cmp.w	#$8000,(a3)
		bne	.notlistend
.nogot		move.l	a0,a6
.nohit		move.l	(sp)+,zoolwx
.doneit
		move.w	any+6(a6),d0	
		add.w	d0,x(a6)
		move.w	any+2(a6),d0		Move Y.
		add.w	d0,y(a6)

		tst.l	any+2(a6)		Up or down detection ?
		bmi	.grav
		move.w	x(a6),d0
		move.w	y(a6),d1
		add.w	#$5,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.l)
		beq	.grav
		move.w	#-5,any+2(a6)		Hut ground
		jsr	random
		andi.w	#$7,d0
		sub.w	d0,any+2(a6)
		add.w	#5,y(a6)
		andi.w	#$fff0,y(a6)
		sub.w	#5,y(a6)
		jmp	.cont
.grav		cmp.w	#$8,any+2(a6)		Do gravity.
		beq	.cont
		add.l	#$11000,any+2(a6)
.cont
		jmp	offandend

krang		dc.w	8,8,7
		dc.w	-8,-8,7
		dc.w	-8,8,6
		dc.w	8,-8,6,$8000

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien78		; Control grape ...

		jsr	freesprite
		jmp	nextalien
				 
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien79		; Grape marker ...

		subq.b	#1,any(a6)
		bpl	.no
		clr.b	any(a6)
		move.w	zoolwx,d0
		sub.w	#$20,d0
		move.w	x(a6),d1
		cmp.w	d0,d1
		bmi	.no
   		add.w	#$80,d0
		cmp.w	d0,d1
		bpl	.no
		move.w	zoolwy,d0
		move.b	#20,any(a6)
		cmp.w	y(a6),d0
		bpl	genagrape
.no		jmp	offandend

genagrape	moveq	#0,d0			To much grapes ?
		move.l	firstsprite,a0
.ne		cmp.l	#-1,a0
		beq	.counted
 		cmp.b	#80,t(a0)
		bne	.nextc
   		addq.b	#1,d0
.nextc		move.l	nsp(a0),a0
		jmp	.ne
.counted	cmp.b	#5,d0
		bpl	notjumpedon	

		move.l	firstsprite,a0		Create a falling grape.
.nextcheck	cmp.l	#-1,a0
		beq	notjumpedon
		cmp.b	#79,t(a0)
		bne	.next
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.n
		move.b	#80,t(a6)
		move.l	#mana,blbset(a6)
		move.b	#10,fr+1(a6)
		move.l	x(a0),x(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.l	any+8(a6)
.n		move.l	a4,a6
		jmp	notjumpedon
.next		move.l	nsp(a0),a0
		jmp	.nextcheck
notjumpedon	jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien80		; Grape ...

		move.l	#21,scoid
		move.b	#$81,sefid
 		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien

		move.b	#%00000001,d7		Fall,No home.
		move.w	#-$e,yoff
		move.l	#-1,bmom		Bouncing
		move.b	#10,pauseifb
	
		jsr	walkabout
		move.b	#10,fr+1(a6)
		tst.w	any(a6)
		bmi	offandend
		addq.b	#1,fr+1(a6)

		jmp	offandend		Are we still on the screen.

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien81		; Big grape ...

		move.l	#21,scoid
		move.b	#$81,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien

		jmp	offandend		Are we still on the screen.

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien82		; Transporter note ...

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#32,d2
		moveq	#32,d3
		jsr	alwithzool
		beq	.nothut

		move.l	any(a6),restarthere
		move.l	any+4(a6),restarthere+4
		move.l	#$01100090,restarthere+8
		move.l	skypl,restarthere+12

		move.l	#blankcop,$dff080
		jsr	synchup
		move.l	firstsprite,a0
.more		cmp.l	#-1,a0
		beq	.nomore
		move.l	snp(a0),a1
		bclr.b	#7,(a1)
		move.l	nsp(a0),a0
		jmp	.more
.nomore		jsr	resetvars
		jmp	startfromzen
.nothut
		jmp	nextalien	

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien83		; Volcano.

		subq.b	#1,any(a6)
		bpl	.nores
		jsr	random
		andi.b	#$1f,d0
		add.b	#$40,d0
		move.b	d0,any(a6)
.nores
		move.b	any(a6),d0
		cmp.b	#$30,d0
		bpl	.nospit
       		andi.b	#3,d0
		tst.b	d0
		bne	.nospit
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.didnt		
		moveq	#3,d0
		jsr	playsample
      		move.l	#mana,blbset(a6)
		move.b	#16,fr+1(a6)
		move.l	x(a4),x(a6)
		move.b	#$ff,any+10(a6)
		move.b	#6,t(a6)
		st.b	any+30(a6)
		move.l	#$e000,any+12(a6)
		jsr	random		X
		andi.w	#$3,d0
		sub.w	#$7,d0
		move.w	d0,any+2(a6)
.n		jsr	random		Y
		andi.w	#$3,d0
		move.w	d0,any(a6)
		jsr	random
		btst	#0,d0
		beq	.nn
   		neg.w	any(a6)
.nn		clr.w	any+4(a6)
		cmp.b	#sln6,worldno+3
		bne	.nag
		move.l	#fair1,blbset(a6)
		move.b	#9,fr+1(a6)
.nag		cmp.b	#sln,worldno+3
		bne	.skip
     		move.l	#cells2,blbset(a6)
		move.b	#42,fr+1(a6)
		add.w	#8,x(a6)
		sub.w	#4,any+2(a6)
.skip		sub.w	#4,any+2(a6)
.didnt		move.l	a4,a6
.nospit		
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien84		; Squeezy jif thing ... (froot can too) ...

		tst.b	any(a6)
		beq	.nokeepfr
		subq.b	#1,any(a6)
		jmp	.nothut
.nokeepfr
		clr.b	ht(a6)
		tst.w	ymom
		ble	.nothut
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#16,d1
		move.w	#32,d2
		tst.b	any+1(a6)		Jif ?
		beq	.yes
    		move.w	#64,d2
		sub.w	#14,d1
.yes		moveq	#32,d3
		tst.b	spining
		beq	.nnn
		add.w	#20,d1 
.nnn		jsr	alwithzool
		beq	.nothut
       		st.b	ht(a6)
		move.b	#4,any(a6)
		st.b	mustup
		clr.b	ducking
		clr.b	punching
		clr.b	slideing
		st.b	jumping
		moveq	#$a,d0
		jsr	playsample
		move.w	#2,d0
		jsr	playsample
		move.l	#-$b0000,ymom
		tst.b	any+1(a6)		Jif ?
		bne	.maybe
		move.l	#-$60000,ymom
.maybe		move.b	#20,uptime
		tst.b	any+1(a6)		Really a jif thing ?
		bne	.nothut

		pushall
		move.l	a6,a4
		move.l	zoolwx,-(sp)
		move.w	zoold,-(sp)
		move.l	x(a6),zoolwx
		sub.w	#$8,zoolwy
		move.b	any+2(a6),zoold
		jsr	fireb
		move.w	(sp)+,zoold
		move.l	(sp)+,zoolwx
		move.l	bulspr,a5
		move.l	#jif,blbset(a5)
		move.b	#5,fr+1(a5)
		tst.b	any+2(a4)
		bne	.nosw
		sub.w	#$30,x(a5)
		subq.b	#1,fr+1(a5)
.nosw		move.l	a4,a6
		pullall		 
.nothut
		move.l	jsp(a6),a5
		move.b	ht(a6),ht(a5)
		move.b	fr+1(a6),fr+1(a5)
		addq.b	#1,fr+1(a5)
		move.l	blbset(a6),blbset(a5)
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien85		; Lemon squirt ...

		sub.w	#10,x(a6)
		cmp.b	#4,fr+1(a6)
		beq	.done
     		add.w	#20,x(a6)
.done
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien86		; Falling fruit generator ...

		subq.b	#1,any(a6)		Fall yet ?
		bpl	offandend
		jsr	random			Delay for next fall.
		andi.b	#$f,d0
		add.b	#$10,d0
		move.b	d0,any(a6)

		move.w	#2,d0
		jsr	playsample
		move.l	a6,a4			Get the first half.
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.didnt
		move.l	a6,a3
		jsr	getsprite		Try to get a second one.
		cmp.l	#-1,a6
		bne	.allright
	 	move.l	a3,a6			Deactivate first one.
		jsr	freesprite
		jmp	offandend
.allright	move.l	x(a4),x(a3)
		sub.w	#$60,y(a3)
		move.b	#87,t(a3)
		move.l	a6,jsp(a3)
		move.l	#mana,blbset(a3)
		move.l	#mana,blbset(a6)
		clr.l	any(a3)
		clr.w	any+4(a3)
		jsr	random 			Add X offset ...
		andi.w	#$ff,d0
		sub.w	#$80,d0
		add.w	d0,x(a3)
		jsr	random
		btst	#0,d0
		beq	.nodir
      		sub.w	d0,x(a3)
      		sub.w	d0,x(a3)
.nodir		clr.b	fr+1(a3)   		Set looks (orange or apple) ?
		btst	#1,d0
		beq	.didnt
		move.b	#2,fr+1(a3)
.didnt		move.l	a4,a6

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien87		; Falling fruit ...

		add.l	#$a000,any(a6)
		move.w	any+4(a6),d0
		add.w	d0,x(a6)
		move.w	any(a6),d0
		add.w	d0,y(a6)
		cmp.b	#sln,worldno+3
		beq	.skipthis
	 	move.l	jsp(a6),a5
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)
		move.b	fr+1(a6),fr+1(a5)
		addq.b	#1,fr+1(a5)
.skipthis
		tst.w	any+4(a6)
		bne	.clear
  		
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#48,d2
		moveq	#32,d3
		jsr	alwithzool
		beq	.clear
		move.l	#-$50000,any(a6)
		jsr	hurtzool
		move.w	#3,any+4(a6)
		jsr	random
		btst	#0,d0
		beq	.clear
		neg.w	any+4(a6)
.clear
		move.w	scrwy,d0
		add.w	#256,d0
		move.w	y(a6),d1
		sub.w	#100,d1
		cmp.w	d1,d0
		bpl	nextalien
		move.l	jsp(a6),a3
		jsr	freesprite
		cmp.b	#sln,worldno+3
		beq	nextalien
		move.l	a3,a6
		jsr	freesprite
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien88		; Radish ...

		tst.b	any+1(a6)		On ground ?
		beq	.onground
		add.l	#$8000,any+2(a6)	Fall till ymom = 3.
		move.w	any+2(a6),d0
		add.w	d0,y(a6)
		st.b	ht(a6)
		cmp.w	#5,any+2(a6)
		bne	.clear
		move.b	any(a6),radno
		moveq	#6,d0
		jsr	playsample
		jsr	bonusexplo
		jsr	freeplease
		jmp	nextalien
.onground	move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#16,d2
		moveq	#16,d3
		jsr	alwithzool
		beq	.clear
      		tst.b	any+2(a6)		Ducked last time ?
		beq	.waittillduck
		tst.b	ducking	     		Still ducked ?
		bne	.clear
		st.b	any+1(a6)		Set pull out stuff.
		sub.w	#$10,y(a6)
		move.l	#-$90000,any+2(a6)
		jmp	.clear
.waittillduck	tst.b	ducking			Ducking yet ?
		beq	.clear
		st.b	any+2(a6)
.clear
   		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien89		; Radish control ...

		clr.b	ht(a6)
		move.b	#$ff,fr+1(a6)

		jsr	offscreen		Are we still on the screen.
		tst.b	d0
		bne	nextalien
		
		tst.b	radno
		beq	nextalien
		move.b	radno,d0
		clr.b	radno
		move.l	any+6(a6),a0
		cmp.b	(a0),d0
		bne	nextalien
		addq.l	#1,any+6(a6)
		move.l	any+6(a6),a0
		cmp.b	#-1,(a0)
		bne	nextalien

		move.l	a6,a4			Get the first half.
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.didnt
		move.l	a6,a3
		jsr	getsprite		Try to get a second one.
		cmp.l	#-1,a6
		bne	.allright
	 	move.l	a3,a6			Deactivate first one.
		jsr	freesprite
		jmp	.didnt
.allright	move.l	x(a4),x(a3)
		sub.w	#$60,y(a3)
		move.b	#90,t(a3)
		move.l	a6,jsp(a3)
		move.l	#lev1stuf,blbset(a3)
		move.b	#29,fr+1(a3)
		st.b	ht(a3)
		move.l	#lev1stuf,blbset(a6)
		move.b	#30,fr+1(a6)
		st.b	ht(a6)
		clr.l	any(a3)
.didnt		move.l	a4,a6

		jsr	freesprite
		jmp	nextalien		

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien90		; Big falling pinapple ...

		add.l	#$b000,any(a6)
		move.w	any(a6),d0
		add.w	d0,y(a6)

		move.w	x(a6),d0
		move.w	y(a6),d1
		add.w	#$32,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.l)
		beq	.nog
		add.w	#2,y(a6)
		andi.w	#$fff0,y(a6)
		sub.w	#2,y(a6)
		move.w	any(a6),d0
		lsr.w	#1,d0
		neg.w	d0	
		move.w	d0,any(a6)
		tst.w	any(a6)
		bne	.nog
		move.b	#15,t(a6)
		jmp	nextalien
.nog		move.l	jsp(a6),a5
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)

		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien91		; Pea bomber ...

		tst.b	any+7(a6)		Check its the correct dir ...
		bne	.checked
		st.b	any+7(a6)
		move.w	x(a6),d0
		cmp.w	zoolwx,d0
		bpl	.right
		tst.w	any+4(a6)
		bpl	.checked
		clr.w	x(a6)
		jmp	offandend
.right		tst.w	any+4(a6)
		bmi	.checked
		clr.w	x(a6)
		jmp	offandend
.checked
		move.l	#250,scoid
		move.b	#$81,sefid
		move.w	#5,offitx
		move.w	#40,sizex 
		move.w	#-5,offity
		move.w	#12,sizey
		st.b	detecn
		move.b	#%11111111,detecm	Clear snp mode.
		jsr	collision
		bne	nextalien

		move.w	any+4(a6),d0		Move pod.
		add.w	d0,x(a6)
		move.w	any(a6),d0
		add.w	d0,y(a6)
		sub.l	#$1800,any(a6)

		move.l	jsp(a6),a5		Update second sprite.
		st.b	ht(a5)
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)
		subq.b	#1,any+10(a6)
		bpl	.b
  		move.b	#1,any+10(a6)
		move.b	any+13(a6),d0
		move.b	fr+1(a6),any+13(a6)
		move.b	d0,fr+1(a6)
.b		move.b	fr+1(a6),fr+1(a5)
		addq.b	#1,fr+1(a5)
		move.l	blbset(a6),blbset(a5)

		subq.b	#1,any+6(a6)		Fire a pea ?
		bpl	.nofire
		move.b	#18,any+6(a6)
		move.l	a6,a4
		jsr	getlowersprite
		cmp.l	#-1,a6
		beq	.didnt		
		move.w	#$200,sam3+4	
		move.w	#3,d0
		jsr	playsample
      		move.l	#mana,blbset(a6)
		move.b	#9,fr+1(a6)
		cmp.b	#sln5,worldno+3
		bne	.no
   		move.l	#toys,blbset(a6)
		move.b	#66,fr+1(a6)
		tst.w	any+4(a4)
		bpl	.no
		move.b	#51,fr+1(a6)
.no		move.l	x(a4),x(a6)
		add.w	#$10,x(a6)
		move.b	#$ff,any+10(a6)
		st.b	any+30(a6)
		move.b	#6,t(a6)
		move.l	#$e000,any+12(a6)
		clr.w	any(a6)
		move.l	#8000,any+2(a6)
.didnt		move.l	a4,a6
.nofire
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien92		; Banana ...

		cmp.b	#20,fr+1(a6)
		bmi	.nnn
    		add.w	#$10,y(a6)
.nnn
		move.l	#125,scoid
		move.b	#$81,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111001,detecm
		jsr	collision
		bne	nextalien

		move.b	#%00000001,d7		Fall,No home.
		move.w	#$10,yoff
		move.l	#-1,bmom		Bouncing
		move.b	#10,pauseifb
		jsr	walkabout

		move.b	#12,fr+1(a6)		Animation ...
		tst.w	any(a6)
		bmi	.dirok
		addq.b	#1,fr+1(a6)
.dirok		tst.b	any+8(a6)
		bne	.nw
   		add.b	#15,fr+1(a6)
.nw
		cmp.b	#20,fr+1(a6)
		bmi	.nnnn
    		sub.w	#$10,y(a6)
.nnnn

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien93		; Carrot ...

		cmp.b	#$ff,ht(a6)
		beq	.done

		move.w	x(a6),d0
		sub.w	#$80,d0
		cmp.w	zoolwx,d0
		bpl	offandend
		add.w	#$80+$50,d0
		cmp.w	zoolwx,d0
		bmi	offandend
		move.w	#-$a,any+4(a6)
		st.b	ht(a6)
		sub.w	#$14,y(a6)
		move.w	#3,any(a6)
		move.w	zoolwx,d0
		cmp.w	x(a6),d0
		bpl	.done
		neg.w	any(a6)
.done
		move.l	#125,scoid
		move.b	#$81,sefid
		move.w	#3,offitx
		move.w	#10,sizex 
		move.w	#5,offity
		move.w	#32,sizey
		st.b	detecn
		move.b	#%11111001,detecm
		jsr	collision
		bne	nextalien

		move.b	#%00000011,d7		Fall,No home.
		move.w	#$11,yoff
		move.l	#-1,bmom		Bouncing
		move.b	#10,pauseifb
		jsr	walkabout

		move.b	#17,fr+1(a6)		Animation ...
		tst.w	any(a6)
		bmi	.dirok
		addq.b	#1,fr+1(a6)
.dirok		
		cmp.b	#5,any+8(a6)		Fire a bow ?
		bne	.nofirebow
		jsr	random
		btst	#0,d0
		beq	.nofirebow
		move.l	a6,a4
		jsr	getlowersprite		Get sprite ...
		cmp.l	#-1,a6
		beq	.didntgetone
		move.w	#$200,sam3+4	
		move.w	#3,d0
		jsr	playsample
		move.l	x(a4),x(a6)
		add.w	#25,y(a6)
		add.w	#10,x(a6)
		move.l	#mana,blbset(a6)
		move.b	#15,fr+1(a6)
		move.w	any(a4),d0
		add.w	d0,d0
		add.w	any(a4),d0
		move.w	d0,any(a6)
		tst.w	any(a4)
		bpl	.frok
		sub.w	#20,x(a6)
     		move.b	#14,fr+1(a6)
.frok		move.b	#60,t(a6)
.didntgetone	move.l	a4,a6
.nofirebow	
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien94		; Cells ...

		move.l	any+10(a6),a0
		jsr	animateal
		move.l	a0,any+10(a6)

		tst.w	any+6(a6)		Past the point of no return ?
		bne	.nop
		jsr	blockmovement
.nop
     		jsr	defmov

		move.w	x(a6),any+28(a6)
		move.w	y(a6),any+32(a6)

		move.l	#127,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#28,sizey
		st.b	detecn
		move.b	#%11111011,detecm
		jsr	collision
		bne	nextalien

		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien95		; Wave generator ...

		tst.l	any(a6)
		beq	offandend

		move.w	x(a6),d6
		move.w	any+8(a6),d7
		move.l	any(a6),a0
		clr.l	any(a6)
		move.w	x(a6),-(sp)
		jsr	gen
		move.w	(sp)+,x(a6)
		jmp	nextalien

gen		move.l	a6,a4
.next		jsr	getsprite
		cmp.l	#-1,a6
		beq	.outofsprs
		move.l	4(a0),blbset(a6)
		move.b	8(a0),t(a6)
		move.b	9(a0),fr+1(a6)
		move.b	10(a0),htg(a6)
		move.l	16(a0),any+0(a6)
		move.l	20(a0),any+4(a6)
		move.l	24(a0),any+8(a6)
		move.l	28(a0),any+12(a6)
		move.l	32(a0),any+16(a6)
		move.l	x(a4),x(a6)
		move.w	any+4(a4),d0
		add.w	d0,x(a4)
		move.w	d6,any+6(a6)
		move.w	any+6(a4),any+8(a6)
		dbra	d7,.next
.outofsprs	move.l	a4,a6
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien96		; Green cell (spliter)...

		move.w	any+16(a6),d0
		sub.w	d0,x(a6)

		move.l	any+10(a6),a0
		jsr	animateal
		move.l	a0,any+10(a6)
		jsr	defmov

		move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#20,d3
		jsr	bulhutal
		tst.b	d7
		beq	notahit
		move.b	#2,flal(a6)
		subq.b	#1,htg(a6)
		bne	notahit
		move.b	#2,htg(a6)
		clr.b	flal(a6)
		addq.w	#4,x(a6)
		addq.w	#4,y(a6)
		cmp.l	#grani3,any+10(a6)
		bmi	.no3
		move.w	#$80,d4
		jsr	createexplo2
		jsr	freesprite
		jmp	nextalien
.no3		move.l	#grani2,d0
		cmp.l	#grani2,any+10(a6)		Already ?
		bmi	.yesitis
		move.l	#grani3,d0
.yesitis	move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.justasec
	     	move.l	x(a4),x(a6)
		move.b	t(a4),t(a6)
		move.l	#cells,blbset(a6)
		move.b	#2,htg(a6)
		move.l	#green2a,any(a6)
		clr.l	any+4(a6)
		clr.w	any+8(a6)
		move.l	d0,any+10(a6)
		move.l	d0,-(sp)
		jsr	random
		andi.w	#3,d0
		move.w	d0,any+16(a6)
		move.l	(sp)+,d0
.justasec	move.l	a4,a6
		move.l	#green2b,any(a6)
		move.l	d0,any+10(a6)
		jsr	random
		andi.w	#3,d0
		move.w	d0,any+16(a6)
notahit
		jmp	offandend		

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien97		; Bloblet ...

		move.l	any+10(a6),a0
		jsr	animateal
		move.l	a0,any+10(a6)

		jsr	defmov

		move.l	#127,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111001,detecm
		jsr	collision
		bne	nextalien

		subq.b	#1,any+14(a6)
		bpl	.nofire
		move.b	#$40,any+14(a6)
		jsr	firebullet
.nofire
		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien98		; White blob thing ...

		tst.b	any+14(a6)
		beq	.cl
		addq.b	#1,dragfactor		
   		move.w	zoolwx,d0
		sub.w	any+10(a6),d0
		move.w	d0,x(a6)
   		move.w	zoolwy,d0
		sub.w	any+12(a6),d0
		move.w	d0,y(a6)
		subq.b	#1,any+14(a6)
		bne	nextalien
		move.w	#$80,d4
		jsr	createexplo2
		jsr	freesprite
		jmp	nextalien
.cl
		subq.w	#1,x(a6)

		move.w	y(a6),d0
		sub.w	#$1c,d0
		sub.w	zoolwy,d0
		bmi	.fare
     		cmp.w	#-4,any(a6)
		beq	.add
		sub.l	#$6000*2,any(a6)
		jmp	.add		
.fare		cmp.w	#4,any(a6)
		beq	.add
     		add.l	#$6000,any(a6)
.add		move.w	any(a6),d0
		add.w	d0,y(a6)

		move.l	#127,scoid
		move.b	#$80,sefid
		move.w	#0,offitx
		move.w	#32,sizex 
		move.w	#0,offity
		move.w	#20,sizey
		st.b	detecn
		move.b	#%10111001,detecm
		jsr	collision
		bne	nextalien

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#8,d0
		addq.w	#4,d1
		move.w	#8,d2
		move.w	#8,d3
		jsr	alwithzool
		beq	.clear
      		move.w	zoolwx,d0
		sub.w	x(a6),d0
		move.w	d0,any+10(a6)
		move.w	zoolwy,d0
		sub.w	y(a6),d0
		move.w	d0,any+12(a6)
		move.b	#100,any+14(a6)
.clear
		moveq	#0,d0
		move.b	htg(a6),d0
		lea	whitefrs,a0
		move.b	(a0,d0.w),fr+1(a6)		
			
		jmp	bigoffandend

whitefrs	dc.b	14,14,13,13,13,12,12,12,12

		even

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*
		
alien99		; Alien direction bullet control ...

		move.l	firstsprite,a0
.check		cmp.l	#-1,a0
		beq	.fin
    		cmp.b	#98,t(a0)
		bne	.no
		tst.b	any+14(a0)
		beq	.no
		move.w	x(a0),d0
		move.w	y(a0),d1
		move.w	x(a6),d2
		move.w	y(a6),d3
		add.w	#32,d0
		cmp.w	d0,d2
		bpl	.no
   		sub.w	#32,d0
		add.w	#16,d2
		cmp.w	d2,d0
		bpl	.no

		add.w	#32,d1
		cmp.w	d1,d3
		bpl	.no
   		sub.w	#32,d1
		add.w	#16,d3
		cmp.w	d3,d1
		bpl	.no
		jsr	freesprite
		jmp	nextalien
.no		move.l	nsp(a0),a0
		jmp	.check
.fin

		move.l	any+20(a6),a0
		jsr	animateal
		move.l	a0,any+20(a6)

		add.w	#2,x(a6)
		
		move.l	any(a6),d0
		add.l	d0,any+8(a6)
		move.w	any+8(a6),x(a6)
		move.l	any+4(a6),d0
		add.l	d0,any+12(a6)
		move.w	any+12(a6),y(a6)

		subq.w	#1,any+24(a6)		Time limit...
		bpl	.ok
.freedom	jsr	freesprite	
		jmp	nextalien
.ok

		cmp.w	#190,any+24(a6)
		bpl	.sk
		move.w	x(a6),d0		Check zool head.
		move.w	y(a6),d1
		jsr	checkblock
		btst.b	#$0,1(a1,d3.w)		Check for monkey bars.
		bne	.freedom
.sk
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.maybenot
		jsr	hurtzool
.maybenot
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien100	; Set scroll speed ...

		move.w	scrollspeed,d0
		add.w	d0,x(a6)

		subq.b	#1,any+2(a6)
		bpl	nextalien
		move.b	#5,any+2(a6)

		cmp.w	any(a6),d0
		beq	.fin
		bpl	.more
		addq.w	#1,scrollspeed
		jmp	nextalien
.more		subq.w	#1,scrollspeed
		jmp	nextalien
.fin		jsr	freesprite
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien101	; Tounge thing ...

		tst.w	any+6(a6)		Past the point of no return ?
		bne	.nop
		jsr	blockmovement
.nop
;     		jsr	defmov

		move.w	x(a6),any+28(a6)
		move.w	y(a6),any+32(a6)

		move.l	#127,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111011,detecm
		jsr	collision
		bne	nextalien

		tst.b	any+11(a6)
		beq	.notopen
		move.b	any+11(a6),d0
		andi.b	#1,d0
		add.b	#15,d0
		move.b	d0,fr+1(a6)
		subq.b	#1,any+11(a6)
		cmp.b	#5,any+11(a6)
		bne	.noyet
		move.w	#$d0,sam3+4	
		move.w	#3,d0
		jsr	playsample
		jsr	firebullet
		tst.l	lastbullet
		beq	.noyet
		move.l	lastbullet,a5
		move.l	#$fff90000,any(a5)
		move.l	#$00000000,any+4(a5)
		add.w	#$10,any+12(a5)
		move.l	#cells2,blbset(a5)
		move.l	#greenbullani,any+20(a5)
		jsr	firebullet
		tst.l	lastbullet
		beq	.noyet
		move.l	lastbullet,a5
		move.l	#$fffa0000,any(a5)
		move.l	#$00020000,any+4(a5)
		add.w	#$10,any+12(a5)
		move.l	#cells2,blbset(a5)
		move.l	#greenbullani,any+20(a5)
      		jsr	firebullet
		tst.l	lastbullet
		beq	.noyet
		move.l	lastbullet,a5
		move.l	#$fffa0000,any(a5)
		move.l	#$fffe0000,any+4(a5)
		add.w	#$10,any+12(a5)
		move.l	#cells2,blbset(a5)
		move.l	#greenbullani,any+20(a5)
		jmp	.noyet
.notopen
		tst.b	any+10(a6)
		bne	.no
		move.b	#14,fr+1(a6)
		move.b	#25,any+10(a6)
.no			   
		subq.b	#1,any+10(a6)
		bne	.noyet
		move.b	#15,fr+1(a6)
		move.b	#10,any+11(a6)
.noyet
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien102	; Nerve ...

		addq.b	#1,fr+1(a6)
		eori.b	#1,fr+1(a6)
		subq.b	#1,fr+1(a6)

		jsr	defmov

		move.l	#127,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111011,detecm
		jsr	collision
		bne	nextalien

		subq.b	#1,any+10(a6)
		bpl	.nofire
		move.b	#$20,any+10(a6)
		jsr	random
		andi.b	#$1f,d0
		add.b	d0,any+10(a6)
		jsr	firebullet
		tst.l	lastbullet
		beq	.nofire
		move.l	lastbullet,a0
		add.w	#$18,any+8(a0)
		move.l	#cells2,blbset(a0)
		move.l	#sparkani,any+20(a0)
.nofire
		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien103	; Splinter ...

		tst.b	ht(a6)
		bne	.done
		st.b	ht(a6)
		jsr	random
		andi.l	#%11100,d0
		add.l	d0,any(a6)
.done
		move.l	any+10(a6),a0
		jsr	animateal
		move.l	a0,any+10(a6)

		jsr	defmov

		move.l	#127,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111011,detecm
		jsr	collision
		bne	nextalien

		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien104	; Bottom set (teeth) ...

		add.w	#25,y(a6)
		jsr	ampcheck
		sub.w	#25,y(a6)

		tst.b	any+8(a6)
		beq	.nw
   		subq.b	#1,any+8(a6)
		jmp	nextalien
.nw
		tst.b	any+4(a6)
		bne	.goingup
		addq.w	#2,y(a6)		Going down ...
		subq.b	#2,ht(a6)
		tst.b	ht(a6)
		bne	.notall
		move.b	#30,any+8(a6)
		eori.b	#1,any+4(a6)
		jmp	.notall
.goingup	subq.w	#2,y(a6)		Move up ...
		addq.b	#2,ht(a6)
		cmp.b	#32,ht(a6)		Full up ?
		bne	.notall
       		eori.b	#1,any+4(a6)
		move.b	#30,any+8(a6)
.notall
		move.l	jsp(a6),a5
		move.l	jsp(a5),a4
		move.w	y(a6),y(a5)
		move.w	y(a5),y(a4)
		move.b	ht(a6),ht(a5)
		move.b	ht(a5),ht(a4)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien105	; Top set (teeth) ...

		tst.b	fr+1(a6)
		bne	nextalien

		move.w	y(a6),-(sp)		With added Y check ...
		moveq	#0,d6
		move.b	ht(a6),d6
		sub.w	#32,y(a6)
		add.w	d6,y(a6)
		jsr	ampcheck
		move.w	(sp)+,y(a6)

		tst.b	any+8(a6)
		beq	.nw
   		subq.b	#1,any+8(a6)
		jmp	nextalien
.nw
		move.l	jsp(a6),a5
		move.l	jsp(a5),a4

		tst.b	any+4(a6)
		beq	.up

		addq.b	#2,ht(a6)
		addq.b	#2,ht(a5)
		addq.b	#2,ht(a4)
		sub.l	#4*2,any(a6)
		sub.l	#4*2,any(a5)
		sub.l	#2*2,any(a4)
		cmp.b	#32,ht(a6)
		bmi	nextalien
		jmp	.hmm

.up		add.l	#4*2,any(a6)
		add.l	#4*2,any(a5)
		add.l	#2*2,any(a4)
		subq.b	#2,ht(a6)
		subq.b	#2,ht(a5)
		subq.b	#2,ht(a4)
		bne	nextalien
		move.l	#4*32,any(a6)
		move.l	#4*32,any(a5)
		move.l	#2*32,any(a4)
.hmm		eori.b	#1,any+4(a6)
		move.b	#30,any+8(a6)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien106	; Snake ...

		tst.b	ht(a6)			Setup stuff...
		bne	.donethis
		sub.w	#$40,y(a6)
		st.b	ht(a6)
		move.w	x(a6),any(a6)	 		
		move.w	y(a6),any+4(a6)	 		
		move.l	#zoolmems,d4
		move.l	d4,any+10(a6)
		move.l	a6,a3
		move.l	a6,a4
		moveq	#6,d7
.nextbodypart	jsr	getlowersprite
		cmp.l	#-1,a6
		beq	.fin
		move.l	x(a4),x(a6)
		move.l	#cells2,blbset(a6)
		move.l	a6,jsp(a3)
		add.l	#6*3,d4
		move.l	d4,any+8(a6)
		move.l	a6,a3
		clr.l	jsp(a3)
		dbra	d7,.nextbodypart
.fin		move.l	a4,a6
.donethis

		jsr	move
		jsr	move

		subq.l	#6,any+10(a6)
		cmp.l	#zoolmems,any+10(a6)	Update table.
		bpl	.notyet
       		add.l	#6*4*22,any+10(a6)
.notyet		move.l	any+10(a6),a0
		move.l	x(a6),(a0)
		add.l	#$00080008,(a0)
		move.b	any+16(a6),4(a0)

		move.l	a6,a5			Move body parts.
.nextbody	move.l	jsp(a5),a5
		cmp.l	#0,a5
		beq	.finished

		pushall

		move.l	a5,a6
		move.l	#127,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%01111001,detecm
		jsr	collision
		bne	nextalien
		pullall


		subq.l	#6,any+8(a5)
		cmp.l	#zoolmems,any+8(a5)
		bpl	.notyet2
       		add.l	#6*4*22,any+8(a5)
.notyet2	move.l	any+8(a5),a0
		move.l	(a0),x(a5)
		move.b	4(a0),fr+1(a5)
		jmp	.nextbody
.finished
		jmp	nextalien

move		subq.b	#1,any+25(a6)
		bpl	.okay
		move.b	#1,any+25(a6)
		addq.w	#1,any+8(a6)		Move head part.
		cmp.w	#64,any+8(a6)
		bmi	.okay
     		clr.w	any+8(a6)
.okay		
		move.w	any+8(a6),d0
		move.w	d0,d1
		move.w	d0,d2
		andi.w	#$1f,d2
		eori.b	#$1f,d2
		lea	roundani,a0
		move.b	(a0,d2.w),any+16(a6)
		andi.w	#$f,d0
		lsl.w	#3,d0
		lea	roundtable,a0
		move.l	(a0,d0.w),d3
		move.l	4(a0,d0.w),d2
		btst	#5,d1
		bne	.otherhalf
	  	btst	#4,d1
		bne	.likethis
		neg.l	d3
		exg.l	d2,d3
		jmp	.likethis
.otherhalf	btst	#4,d1
		beq	.topleft
		neg.l	d2
		neg.l	d3
		jmp	.likethis
.topleft	neg.l	d2
		exg.l	d2,d3
.likethis	add.l	d2,any(a6)
		add.l	d3,any+4(a6)
		move.w	any(a6),x(a6)
		move.w	any+4(a6),y(a6)
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien107	; The claw ...

		tst.b	any+4(a6)
		beq	.nowaiting
		subq.b	#1,any+4(a6)
		bne	offandend
		move.l	#clawani,any(a6)
.nowaiting	
		move.l	any(a6),a0
		jsr	animateal
		move.l	a0,any(a6)

		cmp.b	#8,fr+1(a6)
		bne	.nohide
		clr.b	ht(a6)
		jsr	random
		andi.b	#$3f,d0
		add.b	#$20,d0
		move.b	d0,any+4(a6)	
		jmp	offandend
.nohide		
		st.b	ht(a6)
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien108	; Brain splat ...

		tst.l	any(a6)
		bne	.mn
		move.w	scrollspeed,d0
		lsl.w	#5,d0
		add.w	zoolwx,d0
		add.w	#$70,d0
		cmp.w	x(a6),d0
		bmi	bigoffandend
		move.l	#$20000,any(a6)
.mn
		move.l	#127,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111000,detecm
		jsr	collision
		bne	nextalien

		move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		add.w	offitx,d0
		add.w	offity,d1
		move.w	sizex,d2
		move.w	sizey,d3
		jsr	bulhutal
		tst.b	d7
		beq	.noex

		move.w	#$80,d4
		jsr	createexplo2

		moveq	#7,d3			Fire spack ...
.next		move.w	d3,-(sp)
		jsr	firebullet
		tst.l	lastbullet
		beq	.noyet
		move.l	lastbullet,a5
		jsr	random
		move.w	d0,d1
		andi.w	#3,d0
		addq.w	#2,d0
		btst	#6,d1
		beq	.ne
   		neg.w	d0
		move.w	d0,any(a5)
.ne		jsr	random
		andi.w	#3,d0
		addq.w	#2,d0
		btst	#6,d1
		beq	.ne2
   		neg.w	d0
		move.w	d0,any+4(a5)
.ne2		add.w	#$10,any+12(a5)
		move.l	#cells2,blbset(a5)
		move.l	#spackani,any+20(a5)
.noyet		move.w	(sp)+,d3
		dbra	d3,.next

     		jsr	freesprite
		jmp	nextalien
.noex
		add.l	#$4000,any(a6)
		move.w	any(a6),d0
		add.w	d0,y(a6)

		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien109	; Bullet gen ...

		clr.b	ht(a6)

		tst.l	any(a6)
		bne	.ok

		subq.b	#1,any+4(a6)
		bpl	bigoffandend
		move.b	#15,any+4(a6)
		jmp	.fire

.ok		move.l	any(a6),a0
		jsr	animateal
		move.l	a0,any(a6)

		cmp.b	#53,fr+1(a6)
		bne	offandend
   		st.b	ht(a6)
.fire		jsr	firebullet
		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien110	; Red movement ...

		move.l	any+10(a6),a0
		jsr	animateal
		move.l	a0,any+10(a6)

		tst.w	any+6(a6)		Past the point of no return ?
		bne	.nop
		jsr	blockmovement
.nop
		jsr	defmov

		move.w	x(a6),any+28(a6)
		move.w	y(a6),any+32(a6)

		move.l	#127,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111011,detecm
		jsr	collision
		bne	nextalien

		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien111	; Cells generator ...

		subq.b	#1,any+4(a6)
		bpl	offandend
		move.b	any+5(a6),any+4(a6)

		moveq	#0,d7
		move.w	x(a6),d6
		move.l	any(a6),a0

		move.w	x(a6),-(sp)
		jsr	gen
		move.w	(sp)+,x(a6)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien112	; Drip going down ...


		move.w	any(a6),d0
		add.w	d0,y(a6)
		cmp.w	#10,any(a6)
		beq	.no
   		add.l	#$8000,any(a6)
.no
		move.w	x(a6),d0
		move.w	y(a6),d1
		jsr	checkblock
		btst.b	#$0,1(a1,d3.w)
		beq	.sp
		jsr	freesprite
		jmp	nextalien
.sp
		move.l	#0,scoid
		move.b	#0,sefid
		move.w	#0,offitx
		move.w	#16,sizex 
		move.w	#0,offity
		move.w	#16,sizey
		st.b	detecn
		move.b	#%11111000,detecm
		jsr	collision
		bne	nextalien

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien113	; Falling rock generator ... (Took it out 'cause it was to hard ...)

		jsr	freesprite
		bra	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien114	; Weapon collection ...

      		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#48,d2
		moveq	#32,d3
		jsr	alwithzool
		beq	.clear
		move.b	fr+1(a6),weapon
		jsr	freesprite
		jmp	nextalien
.clear
		jmp	bigoffandend			

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien115	; Launcher blaster ...

		subq.b	#1,any(a6)
		bpl	nextalien
		move.b	#0,any(a6)
		subq.b	#1,any+1(a6)
		bpl	.no
		move.l	any+12(a6),a0
		clr.l	(a0)
		jsr	freesprite
		jmp	nextalien
.no		move.l	a6,a3
		move.w	#20-1,d0		Find a space.
		clr.l	fired
		lea	bulletsprs+8,a0
		jsr	fireit2
		tst.l	fired
		beq	nextalien
		move.w	#1,d0
		jsr	playsample
		move.l	fired,a6
		move.l	x(a3),x(a6)
		jsr	random
		andi.w	#$3f,d0
		sub.w	#$20,d0
		add.w	d0,x(a6)
		jsr	random
		andi.w	#$3f,d0
		sub.w	#$20,d0
		add.w	d0,y(a6)
		move.l	#still,any(a6)
		move.l	#bombani,any+20(a6)
		move.l	#bangs,blbset(a6)
		move.b	#16,fr+1(a6)
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien116	; Tank ...

		move.b	#%00000010,d7		No fall,Do home.
		move.w	#-4,yoff
		clr.l	bmom
		move.b	#20,pauseifb
		jsr	walkabout

		move.l	#340,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111001,detecm
		jsr	collision
		clr.b	drumset
		tst.b	d7
		bne	nextalien

		tst.b	any+8(a6)
		beq	.nofire
		move.b	#0,fr+1(a6)
		cmp.b	#14,any+8(a6)
		bpl	.setfr
		cmp.b	#7,any+8(a6)
		bmi	.setfr
   		move.b	#6,fr+1(a6)
		cmp.b	#10,any+8(a6)
		bmi	.no
   		move.b	#4,fr+1(a6)
.no
		cmp.b	#9,any+8(a6)
		bne	.setfr
		jsr	mallsortb 		Yes.
		cmp.l	#-1,a5
		beq	.nobombyet
		move.b	#51,fr+1(a5)
		sub.w	#9,y(a5)
		tst.w	any(a6)
		bmi	.xok
    		add.w	#25,x(a5)
.xok		move.l	#toys,blbset(a5)
		move.w	#-6,any+2(a5)
		move.w	#4,any(a5)
		tst.w	any(a6)
		bpl	.b
  		neg.w	any(a5)
.b
.nobombyet
		jmp	.setfr
.nofire
		subq.b	#2,any+9(a6)		Animate walk.
		bpl	.no4
		move.b	#2,any+9(a6)
.no4		move.b	any+9(a6),fr+1(a6)
.setfr		tst.w	any(a6)
		bpl	.no2
   		addq.b	#8,fr+1(a6)
.no2		   
		move.l	jsp(a6),a5
		move.l	x(a6),x(a5)
		move.b	fr+1(a6),fr+1(a5)
		addq.b	#1,fr+1(a5)
		add.w	#32,x(a5)

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien117	; Bouncing ball demo ...

		move.l	#340,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111001,detecm
		jsr	collision
		bne	nextalien

		move.b	#%00000001,d7		Fall,No home.
		move.w	#$4,yoff
		move.l	#-$d0000,bmom		Bouncing
		move.b	#0,pauseifb
		jsr	walkabout

		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien118	; Yo-Yo ...

		tst.b	ht(a6)			Setup.
		bne	.done
     		st.b	ht(a6)
		add.w	#$10,y(a6)
		move.w	y(a6),any+8(a6)
		move.w	y(a6),any(a6)
		add.w	#$50,any(a6)
		st.b	any+6(a6)
.done		
		subq.b	#1,fr+1(a6)		Animation.
		eori.b	#1,fr+1(a6)
		addq.b	#1,fr+1(a6)

		tst.b	any+6(a6)
		bne	.down
		cmp.w	#-8,any+2(a6)
		beq	.no
   		sub.l	#$7000,any+2(a6)
.no		move.w	any+2(a6),d0
		add.w	d0,y(a6)
		move.w	any+8(a6),d0
		cmp.w	y(a6),d0
		bmi	.notover
		st.b	any+6(a6)
		jmp	.notover
		
.down		add.w	#8,y(a6)
		move.w	any(a6),d0
		cmp.w	y(a6),d0
		bpl	.notover
		clr.b	any+6(a6)
		move.l	#$00080000,any+2(a6)
.notover
		move.l	jsp(a6),a5		Lenght of string ...
;		move.w	tempp,fr(a5)
		move.w	any+8(a6),d0
		sub.w	#$10,d0
		move.w	y(a6),d1
		sub.w	d0,d1
		move.b	d1,ht(a5)

		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		move.w	#32,d2
		moveq	#32,d3
		jsr	alwithzool
		beq	.clear
		jsr	hurtzool
.clear
		jmp	bigoffandend

;tempp		dc.w	0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien119	; Spining top ...

		move.l	any+4(a6),a0
		jsr	animateal
		move.l	a0,any+4(a6)

		move.l	#275,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien

		move.w	zoolwx,d0
		cmp.w	x(a6),d0
		bpl	.right
		cmp.w	#-7,any(a6)
		beq	.add
		sub.l	#$6000,any(a6)
		jmp	.add
.right		cmp.w	#7,any(a6)
		beq	.add
		add.l	#$6000,any(a6)
.add		move.w	any(a6),d0
		add.w	d0,x(a6)

		move.w	x(a6),d0  		Check with background.
		tst.b	any+1(a6)
		bpl	.rightdir
	 	sub.w	#18,d0
.rightdir	move.w	y(a6),d1
okay		add.w	#$e,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.swap
		btst.b	#3,1(a1,d3.w)
		bne	.swap
    		btst.b	#0,1(a1,d3.w)
		bne	.swap
	 	add.w	#16,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.swap
    		btst.b	#3,1(a1,d3.w)
		bne	.noswap
    		btst.b	#0,1(a1,d3.w)
		bne	.noswap
.swap  		tst.w	any(a6)
		bpl	.pos
		move.l	#$40000,any(a6)
		jmp	.noswap
.pos		move.l	#-$40000,any(a6)
.noswap
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien120	; Bear ...

		move.b	#%00000001,d7		No fall,Do home.
		move.w	any+20(a6),yoff
		clr.l	bmom
		clr.b	pauseifb
		jsr	walkabout

		tst.l	any+4(a6)   		Random jump ...
		bne	.nojump
       		subq.b	#1,any+14(a6)
		cmp.b	#15,any+14(a6)
		beq	.change
		tst.w	any+14(a6)
		bpl	.nojump
		move.b	#20,any+14(a6)
		jsr	random
		andi.w	#$1f,d0
		add.w	d0,any+14(a6)
		andi.w	#$f,d0
		sub.w	#$15,d0
		move.w	d0,any+4(a6)
		jmp	.nojump

.change		move.w	zoolwx,d0		Change dir ...
		cmp.w	x(a6),d0
		bmi	.right
		move.w	#3,any(a6)
		jmp	.nojump
.right		move.w	#-3,any(a6)
.nojump

		move.l	#340,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		move.b	#%11111111,detecn
		move.b	#%11111111,detecm
		jsr	collision
		clr.b	drumset
		tst.b	d7
		bne	nextalien

		lea	leftani2,a0
		tst.w	any(a6)
		bpl	.sk
		lea	rightani2,a0
.sk		subq.b	#1,any+10(a6)
		bpl	.ok
   		move.b	#11,any+10(a6)
.ok		tst.l	any+4(a6)
		beq	.cl
   		clr.b	any+10(a6)
.cl		moveq	#0,d0
		move.b	any+10(a6),d0
		move.b	(a0,d0.w),fr+1(a6)
		cmp.w	#-13,any+20(a6)
		bne	.sk3
   		add.b	#8,fr+1(a6)
.sk3

		jmp	bigoffandend

 
leftani2 	dc.b	20,20,20,21,21,21,22,22,22,23,23,23
rightani2	dc.b	24,24,24,25,25,25,26,26,26,27,27,27

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien121	; Sad ...

		move.b	#%00000000,d7		No fall,Do home.
		move.w	any+20(a6),yoff
		clr.l	bmom
		clr.b	pauseifb
		jsr	walkabout

		lea	leftani,a0
		tst.w	any(a6)
		bpl	.sk
		lea	rightani,a0
.sk		subq.b	#1,any+10(a6)
		bpl	.ok
   		move.b	#11,any+10(a6)
.ok		tst.b	any+8(a6)
		beq	.cl
   		clr.b	any+10(a6)
.cl		moveq	#0,d0
		move.b	any+10(a6),d0
		move.b	(a0,d0.w),fr+1(a6)

		move.l	#340,scoid
		move.b	#$80,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		move.b	#%11111111,detecn
		move.b	#%11111111,detecm
		jsr	collision
		clr.b	drumset
		tst.b	d7
		bne	nextalien

		move.l	jsp(a6),a5
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)
		move.b	fr+1(a6),fr+1(a5)
		addq.b	#1,fr+1(a5)

		jmp	offandend

leftani		dc.b	47,47,47,45,45,45,43,43,43,41,41,41
rightani	dc.b	57,57,57,59,59,59,61,61,61,63,63,63

		even

gamedata	dc.b	26,30		spike
		dc.l	fair1
		dc.w	0

gd2		dc.b	25,30		greenie
		dc.l	fair1
		dc.w	0

		dc.b	26,10		spike
		dc.l	fair1
		dc.w	0

		dc.b	13,3		Med bush ...
		dc.l	fair2
		dc.w	0
		dc.b	14,14
		dc.l	fair2
		dc.w	0

		dc.b	11,3		Big bush ...
		dc.l	fair2
		dc.w	-16
		dc.b	12,10
		dc.l	fair2
		dc.w	-16

		dc.b	15,20		Small bush ...
		dc.l	fair2
		dc.w	0

		dc.b	26,30		spike
		dc.l	fair1
		dc.w	0

		dc.b	26,10		spike
		dc.l	fair1
		dc.w	0

		dc.b	15,20		Small bush ...
		dc.l	fair2
		dc.w	0

		dc.b	11,3		Big bush ...
		dc.l	fair2
		dc.w	-16
		dc.b	12,30
		dc.l	fair2
		dc.w	-16

		dc.b	25,20		greenie
		dc.l	fair1
		dc.w	0

		dc.b	11,3		Big bush ...
		dc.l	fair2
		dc.w	-16
		dc.b	12,10
		dc.l	fair2
		dc.w	-16

		dc.b	25,10		greenie
		dc.l	fair1
		dc.w	0

		dc.b	15,20		Small bush ...
		dc.l	fair2
		dc.w	0

		dc.b	25,30		greenie
		dc.l	fair1
		dc.w	0

		dc.b	15,20		Small bush ...
		dc.l	fair2
		dc.w	0

     		dc.b	11,3		Big bush ...
		dc.l	fair2
		dc.w	-16
		dc.b	12,30
		dc.l	fair2
		dc.w	-16

		dc.b	26,20		spike
		dc.l	fair1
		dc.w	0

		dc.b	13,3		Med bush ...
		dc.l	fair2
		dc.w	0
		dc.b	14,14
		dc.l	fair2
		dc.w	0

		dc.b	25,10		greenie
		dc.l	fair1
		dc.w	0

		dc.b	13,3		Med bush ...
		dc.l	fair2
		dc.w	0
		dc.b	14,20
		dc.l	fair2
		dc.w	0

		dc.b	25,15		greenie
		dc.l	fair1
		dc.w	0

		dc.b	11,3		Big bush ...
		dc.l	fair2
		dc.w	-16
		dc.b	12,10
		dc.l	fair2
		dc.w	-16

     		dc.b	11,3		Big bush ...
		dc.l	fair2
		dc.w	-16
		dc.b	12,30
		dc.l	fair2
		dc.w	-16

		dc.b	26,20		spike
		dc.l	fair1
		dc.w	0

		dc.b	11,3		Big bush ...
		dc.l	fair2
		dc.w	-16
		dc.b	12,30
		dc.l	fair2
		dc.w	-16

		dc.b	25,10		greenie
		dc.l	fair1
		dc.w	0

		dc.b	25,10		greenie
		dc.l	fair1
		dc.w	0

		dc.b	25,5		greenie
		dc.l	fair1
		dc.w	0

		dc.b	13,3		Med bush ...
		dc.l	fair2
		dc.w	0
		dc.b	14,20
		dc.l	fair2
		dc.w	0


		dc.b	26,50		spike
		dc.l	fair1
		dc.w	0
					
one1		dc.b	6,10		1 up ...
		dc.l	lev1stuf
		dc.w	0

one2		dc.b	6,10		1 up ...
		dc.l	lev1stuf
		dc.w	0

one3		dc.b	6,10		1 up ...
		dc.l	lev1stuf
		dc.w	0

		dc.b	26,6		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,6		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,6		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,6		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,6		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,6		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,6		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,76		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,76		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,76		spike
		dc.l	fair1
		dc.w	0
		dc.b	26,76		spike
		dc.l	fair1
		dc.w	0

		dc.b	$fe

		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien122	; Arcade buttons ...

		clr.b	ht(a6)
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		sub.w	#20,d1
		move.w	#22,d2
		move.w	#32,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.no
		st.b	ht(a6)
		move.b	fr+1(a6),pressed	Set button number ... (one at a time) ...
		jmp	nextalien
.no
		cmp.b	#4,fr+1(a6)		If fire button is clear then let fire in future ...
		bne	nextalien
		clr.b	minifired
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien123	; Zool (the second) ...

		move.l	a6,minizool		Set minizool place variable for other routines ...
		tst.w	any+6(a6)      		Set zool starting Y position ... (only once) ...
		bne	.set
    		move.w	y(a6),any+6(a6)
.set
		tst.b	gamemode
		beq	.readytostart
    		cmp.b	#$ff,gamemode
		beq	.playingit
		subq.b	#1,gamemode		Wait for stuff to get off the screen ...
		bra	nextalien

.readytostart	cmp.b	#6,pressed		Start button ?
		bne	nextalien
		move.l	#weezoolani,any(a6)
		move.l	jsp(a6),a5
      		move.l	#gamedata,any(a5)
		move.w	any+6(a6),y(a6)		Set the starting Y pos ...
		clr.l	any+8(a6)
		clr.l	minibullet
		st.b	ht(a6)
		st.b	fr+1(a6)
		st.b	gamemode
		bra	nextalien

.playingit	; Zool II by George Allan ... 1992 ...

		; Jumper ...

		add.l	#$d002,any+8(a6)	Do gravity ...
		move.w	any+8(a6),d0
		add.w	d0,y(a6)
		move.w	any+6(a6),d0
		cmp.w	y(a6),d0		Still in the air ...	
		bpl	.cantjump
		move.w	any+6(a6),y(a6)		Get on the ground ...
		clr.l	any+8(a6)
		cmp.b	#5,pressed		Jump button pressed ?
		bne	.cantjump
		moveq	#$a,d0			Sound ...
		jsr	playsample
		move.l	#-$a0000,any+8(a6)	Start Y mom ...
.cantjump	
		; Bullet control ...

		tst.b	minifired 		Am I allowed to fire yet ...
		bne	.nofire
		tst.l	minibullet    		Sprite used already ...
		bne	.nofire
		cmp.b	#4,pressed		Fire button pressed ?
		bne	.nofire
		move.l	a6,a3			
		jsr	getsprite		Get a sprite, any sprite ...
		cmp.l	#-1,a6
		beq	.didntfire
		move.l	a6,minibullet		Save bullet sprite ...
		moveq	#$f,d0			Sound ...
		jsr	playsample
		move.b	#29,fr+1(a6)		Set look of bullet ...
		move.l	#fair1,blbset(a6)		
		move.l	jsp(a3),a4		Set end X pos from other sprite ...
		move.w	x(a4),any(a6)
		move.l	x(a3),x(a6)		Start X,Y pos ...
		add.w	#12,x(a6)
		move.b	#124,t(a6)		Finish off ...
		st.b	minifired
.didntfire	move.l	a3,a6
.nofire
		; Animate little zool ...

		move.l	any(a6),a0		Running animation ...
		jsr	animateal
		move.l	a0,any(a6)
		tst.w	any+8(a6)		Jump animation ...
		beq	.notjumping
		move.b	#23,fr+1(a6)
		tst.l	any+8(a6)
		bmi	.notjumping
		move.b	#24,fr+1(a6)
.notjumping
		; Generate game aliens ...

		subq.b	#1,any+5(a6)		Ready to create another alien ...		
		bpl	.nogame
		move.l	jsp(a6),a4		Move on one position in game data ...
		move.l	any(a4),a0
		addq.l	#gd2-gamedata,any(a4)
		cmp.b	#$fe,(a0)		Reached the end yet (should never happen) ...
		bne	.nores
      		move.l	#gamedata,any(a4)	Reset ...
      		lea	gamedata,a0
.nores		move.b	1(a0),any+5(a6)		Set the time to wait for next one ...
		move.l	a6,a3
		jsr	getsprite		Get a sprite ...
		cmp.l	#-1,a6			Did we get it ... (probably) ...
		beq	.didnt
		move.l	a6,a1			Clear all of any mem (just in case) ...
		add.w	#any,a1
       		moveq	#30,d0
.morecl		clr.b	(a1)+
		dbra	d0,.morecl
		move.l	a0,any+20(a6)		"Where did I come from" said (a6) ...
		move.b	(a0),fr+1(a6)		Set look ...
		move.l	2(a0),blbset(a6)
		move.l	jsp(a3),a4		Set X,Y start pos ...
		move.l	x(a4),x(a6)
		move.w	6(a0),d0		Adjust Y pos from a0 data ...
		add.w	d0,y(a6)
		move.w	x(a3),any(a6)		Set end X pos for alien ...
		sub.w	#$50,any(a6)
		move.b	#124,t(a6)		Set alien number ...
.didnt		move.l	a3,a6
.nogame		
		clr.b	pressed			Finished game pass so clear the button number ...

		jmp	nextalien

weezoolani	dc.w	19,19,19,20,20,20,21,21,21,22,22,22,$ffff
		dc.l	weezoolani
bymom		dc.w	0,0,0,-7-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,0,$8000

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien124	; Zool 2 aliens ...

		; Bullet ...

		cmp.b	#29,fr+1(a6)		Bullet control ...
		bne	.notbullet
	  	add.w	#9,x(a6)
		move.w	any(a6),d0		At edge yet ?
		cmp.w	x(a6),d0
		bpl	nextalien
		jsr	freeplease		Fire the bullet sprite ...
		clr.l	minibullet
		jmp	nextalien
.notbullet

		; Green Meanie ...

		cmp.b	#25,fr+1(a6)		Greenie ?
		bne	.nogreenie
		subq.w	#4,x(a6)		Move X pos ...
		addq.w	#2,any+10(a6)		Move Y pos ... (Using a table) ...
		move.w	any+10(a6),d0
		lea	bymom,a0
		move.w	(a0,d0.w),d0
		cmp.w	#$8000,d0    		End of table ?
		bne	.ok
   		clr.w	any+10(a6)		Reset table pos ...
		bra	.skip
.ok		add.w	d0,y(a6)		
		tst.w	d0			If in the air then move extra X ...
		beq	.skip
		subq.w	#1,x(a6)
.skip		bsr	hurtminizool
		eori.b	#1,any+26(a6)
		tst.b	any+26(a6)
		beq	checkoffit
		bsr	alwithbull
		jmp	nextalien
.nogreenie

		; Spikes ...

		cmp.b	#26,fr+1(a6)		Spike ?
		bne	.nothere
		subq.w	#4,x(a6)
		jsr	hurtminizool
		jmp	checkoffit
.nothere	
		; 1ups ...

		cmp.b	#6,fr+1(a6)		Is it a 1up ?
		bne	.not1up
		cmp.b	#$ff,gamemode		Still playing ?
		bne	.not1up
		move.w	x(a6),d0		Set 1up's pos ...
		move.w	y(a6),d1
		move.l	minizool,a0		Set minizool's pos ...
		move.w	x(a0),d2
		move.w	y(a0),d3
		add.w	#$10,d3			X.1
		add.w	#$10,d0		
		cmp.w	d0,d2
		bpl	.not1up
		sub.w	#$10,d0			X.2
		add.w	#$10,d2
		cmp.w	d2,d0
		bpl	.not1up
		add.w	#$10,d1	 		Y.1
		cmp.w	d1,d3
		bpl	.not1up
		sub.w	#$10,d1			Y.2
		add.w	#$10,d3
		cmp.w	d3,d1
		bpl	.not1up
		move.l	any+20(a6),a0		Back to its roots ...
		st.b	(a0)			Set the root so that minizool can't collect it again ...
		addq.l	#1,lives		Inc lives ...
		moveq	#6,d0	   		Sound the power up sound ...
		jsr	playsample
		jsr	freeplease		Free and off ...
		bra	nextalien
.not1up
		; Bush's etc ...

     		subq.w	#4,x(a6)		Just move ...
		jmp	checkoffit

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

ALWITHBULL	; Has this little alien been killed by a bullet ?

		moveq	#0,d7
		tst.l	minibullet		Any bullet to detec with ?
		beq	bye
		move.w	x(a6),d0		Get alien's pos ...
		move.w	y(a6),d1
		move.l	minibullet,a0		Get the bullet's pos ...
		move.w	x(a0),d2
		move.w	y(a0),d3
		add.w	#$10,d3			X.1
		add.w	#$10,d0
		cmp.w	d0,d2
		bpl	bye
		sub.w	#$10,d0			X.2
		add.w	#$10,d2
		cmp.w	d2,d0
		bpl	bye
		add.w	#$30,d1	 		Y.1
		cmp.w	d1,d3
		bpl	bye
		sub.w	#$40,d1			Y.2
		add.w	#$40,d3
		cmp.w	d3,d1
		bpl	bye
		jsr	freeplease
		move.l	#$80,d4			Kill the bastard ...
		jsr	createexplo2
		move.l	minibullet,a6		Free the bullet sprite ...
		jsr	freeplease
		clr.l	minibullet
		st.b	d7
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

HURTMINIZOOL	; Test the alien's pos with minizool's pos ...

		cmp.b	#$ff,gamemode		Still playing ?
		bne	bye
		move.w	x(a6),d0		Set alien's pos ...
		move.w	y(a6),d1
		move.l	minizool,a0		Set minizool's pos ...
		move.w	x(a0),d2
		move.w	y(a0),d3
		add.w	#$10,d3			X.1
		add.w	#$10,d0	
		cmp.w	d0,d2
		bpl	bye
		sub.w	#$10,d0			X.2
		add.w	#$10,d2
		cmp.w	d2,d0
		bpl	bye
		add.w	#$10,d1	 		Y.1
		cmp.w	d1,d3
		bpl	bye
		sub.w	#$10,d1			Y.2
		add.w	#$10,d3
		cmp.w	d3,d1
		bpl	bye
		pushall
		move.l	minizool,a6		Create an explosion ...
		move.l	#$80,d4
		jsr	createexplo2
		clr.b	ht(a6)			Disapear minizool ...
		move.b	#$50,gamemode		Set Game Over message ...
		moveq	#0,d0			Sound ...
		jsr	playsample
		pullall
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CHECKOFFIT	; Is this little alien off the screen ... (if so then free) ...

		move.w	any(a6),d0
		cmp.w	x(a6),d0
		bmi	nextalien
		jsr	freeplease
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien125	; Message ...

		move.l	jsp(a6),a5		Which message ?
		tst.b	gamemode
		beq	.start
		cmp.b	#$ff,gamemode
		beq	.off
      		move.b	#9,fr+1(a6)		Game over ...
		move.b	#10,fr+1(a5)
		jmp	nextalien
.start		move.b	#7,fr+1(a6)		Get ready ...
		move.b	#8,fr+1(a5)
		jmp	nextalien
.off		move.b	#$ff,fr+1(a6)		Nothing ...
		move.b	#$ff,fr+1(a5)
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien126	; Up and down ...

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.no
		jsr	hurtzool
.no
		move.l	any+2(a6),a0
		jsr	animateal
		move.l	a0,any+2(a6)

		tst.b	ht(a6)
		bne	.n
  		st.b	ht(a6)
		jsr	random
		andi.b	#1,d0
		move.b	d0,any(a6)
.n
		tst.b	any(a6)
		beq	.up

		addq.w	#7,y(a6)
		move.w	x(a6),d0		Check zool head.
		move.w	y(a6),d1
		add.w	#16,d1
		jsr	checkblock
		btst.b	#$0,1(a1,d3.w)
		beq	.sp
		eori.b	#1,any(a6)
		jmp	.sp

.up		subq.w	#7,y(a6)
		move.w	x(a6),d0		Check zool head.
		move.w	y(a6),d1
		sub.w	#16,d1
		jsr	checkblock
		btst.b	#$0,1(a1,d3.w)
		beq	.sp
		eori.b	#1,any(a6)
.sp
		jmp	bigoffandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien127	; Candy floss ...

		subq.b	#1,any+10(a6)
		bpl	.nospit
		jsr	random
		andi.b	#$1f,d0
		add.b	#$10,d0
		move.b	d0,any+10(a6)
		move.w	#$130,sam3+4	
		move.w	#3,d0
		jsr	playsample
	
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.didnt		
      		move.l	#fair1,blbset(a6)
		move.b	#11,fr+1(a6)
		move.l	x(a4),x(a6)
		add.l	#$00040005,x(a6)
		move.b	#$ff,any+10(a6)
		st.b	any+30(a6)
		move.b	#6,t(a6)
		move.l	#$e000,any+12(a6)
		jsr	random	
		andi.w	#$3,d0
		sub.w	#$7,d0
		move.w	d0,any+2(a6)
.n		jsr	random
		andi.w	#$3,d0
		move.w	d0,any(a6)
		jsr	random
		btst	#0,d0
		beq	.nn
   		neg.w	any(a6)
.nn		clr.w	any+4(a6)
		sub.w	#4,any+2(a6)
.didnt		move.l	a4,a6
.nospit
		move.l	#340,scoid
		move.b	#$81,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111001,detecm
		jsr	collision
		bne	nextalien

		move.b	#%00000001,d7		Fall,No home.
		move.w	#$10,yoff
		move.l	#-1,bmom		Bouncing
		move.b	#10,pauseifb
		jsr	walkabout

		move.b	#13,fr+1(a6)		Animation ...
		tst.b	any+8(a6)   
		beq	.nw
		cmp.b	#8,any+8(a6)
		bne	.n33
  		jsr	random
		btst	#0,d0
		beq	.n33
		neg.w	any(a6)
.n33		moveq	#0,d0
		move.b	any+8(a6),d0
		lea	candyani,a0
		move.b	(a0,d0.w),fr+1(a6)
.nw		tst.w	any(a6)
		bmi	.k
  		addq.b	#3,fr+1(a6)
.k
		jmp	offandend

candyani	dc.b	14,14,15,15,15,15,15,15,15,15,14

		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien128	; Apple thing ...

		subq.b	#1,any+13(a6)		Jump ?
		bpl	.no
 		move.l	#-$80000,any+8(a6)
		jsr	random
		andi.b	#$1f,d0
		add.b	#$20,d0
		move.b	d0,any+13(a6)
		sub.w	#$a,y(a6)
.no
		subq.b	#1,any+12(a6)		Fire ?
		bpl	.nospit
		move.b	#2,any+12(a6)
		tst.l	any+8(a6)
		bpl	.nospit
		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.didnt		
      		move.l	#fair2,blbset(a6)
.mn		jsr	random
		andi.b	#3,d0
		cmp.b	#3,d0
		bpl	.mn
		add.b	#16,d0
		move.b	d0,fr+1(a6)
		move.l	x(a4),x(a6)
		add.w	#5,x(a6)
		sub.w	#12,y(a6)
		add.l	#$00040005,x(a6)
		move.b	#$ff,any+10(a6)
		st.b	any+30(a6)
		move.b	#6,t(a6)
		move.l	#$e000,any+12(a6)
		jsr	random	
		andi.w	#$7,d0
		sub.w	#$f,d0
		move.w	d0,any+2(a6)
.n		jsr	random
		andi.w	#$3,d0
		add.w	#4,d0
		move.w	d0,any(a6)
		jsr	random
		btst	#0,d0
		beq	.nn
   		neg.w	any(a6)
.nn		clr.w	any+4(a6)
.didnt		move.l	a4,a6
.nospit
		move.l	any+4(a6),a0
		jsr	animateal
		move.l	a0,any+4(a6)

		move.l	#275,scoid
		move.b	#$81,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		st.b	detecm
		jsr	collision
		bne	nextalien

		move.w	zoolwx,d0
		cmp.w	x(a6),d0
		bpl	.right
		cmp.w	#-4,any(a6)
		beq	.add
		sub.l	#$6000,any(a6)
		jmp	.add
.right		cmp.w	#4,any(a6)
		beq	.add
		add.l	#$6000,any(a6)
.add		move.w	any(a6),d0
		add.w	d0,x(a6)

		move.w	x(a6),d0  		Check with background.
		add.w	#16,d0
		tst.b	any+1(a6)
		bpl	.rightdir
	 	sub.w	#32,d0
.rightdir	move.w	y(a6),d1
		add.w	#$14-7,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.swap
		btst.b	#3,1(a1,d3.w)
		bne	.swap
    		btst.b	#0,1(a1,d3.w)
		beq	.noswap
.swap		tst.w	any(a6)
		bpl	.pos
		move.l	#$30000,any(a6)
		jmp	.noswap
.pos		move.l	#-$30000,any(a6)
.noswap	
		move.w	x(a6),d0  		Check with background.
	 	add.w	#16,d1
		jsr	checkblock
		btst.b	#3,1(a1,d3.w)
		bne	.gr
    		btst.b	#0,1(a1,d3.w)
		beq	.falling
.gr		andi.w	#$fff0,y(a6)
		add.w	#7,y(a6)
		move.l	#$10000,any+8(a6)
		jmp	.nofalling
.falling	cmp.w	#$e,any+8(a6)
		beq	.s
		add.l	#$c000,any+8(a6)
.s		move.w	any+8(a6),d0
		add.w	d0,y(a6)	
.nofalling
		jmp	offandend

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien129	; Hammer ...

		tst.b	ht(a6)
		bne	.done
     		st.b	ht(a6)
		move.l	x(a6),any+12(a6)
.done
		move.l	#340,scoid
		move.b	#$81,sefid
		move.w	#5,offitx
		move.w	#22,sizex 
		move.w	#5,offity
		move.w	#22,sizey
		st.b	detecn
		move.b	#%11111001,detecm
		jsr	collision
		bne	nextalien

		move.l	x(a6),-(sp)		Move ...
		move.l	any+12(a6),x(a6)
		move.b	#%00000001,d7		Fall,No home.
		move.w	#$2,yoff
		move.l	#-$d0000,bmom		Bouncing
		move.b	#20,pauseifb
		move.l	#$14000,gravit
		jsr	walkabout
		move.l	#$e000,gravit
		move.l	x(a6),any+12(a6)
		move.l	(sp)+,x(a6)

		move.w	x(a6),d0  		Check with background.
		move.w	y(a6),d1
		add.w	#$14-7,d1
		jsr	checkblock
		cmp.b	#144,(a0,d2.l)
		beq	.ping
       		cmp.b	#145,(a0,d2.l)
		beq	.ping
		cmp.b	#146,(a0,d2.l)
		beq	.ping
		cmp.b	#147,(a0,d2.l)
		bne	.noping
.ping		st.b	ping
.noping

		; Animate ...

		clr.w	any+10(a6)
		tst.b	any+8(a6)
		bne	.ongr
		cmp.w	#4,any+4(a6)
		bmi	.fine
		move.b	#1,any+11(a6)
		cmp.w	#7,any+4(a6)
		bmi	.fine
		move.b	#2,any+11(a6)
		jmp	.fine
.ongr		cmp.b	#10,any+8(a6)
		bmi	.fine
     		move.b	#3,any+11(a6)
		cmp.b	#15,any+8(a6)
		bmi	.fine
		move.b	#2,any+11(a6)
.fine		tst.w	any(a6)
		bpl	.kn
   		addq.b	#4,any+11(a6)
.kn
		move.l	jsp(a6),a5		Place hammer in right place.
		move.l	#fair1,blbset(a5)
		move.w	any+10(a6),d0
		add.w	d0,d0
		add.w	d0,d0
		add.w	d0,d0
		lea	hammerfrs,a0
		move.l	any+12(a6),x(a6)
		move.w	(a0,d0.w),d1
		add.w	d1,x(a6)
		move.w	2(a0,d0.w),d1
		add.w	d1,y(a6)
		move.w	4(a0,d0.w),fr(a6)
		move.b	fr+1(a6),fr+1(a5)
		addq.b	#1,fr+1(a5)
		move.l	x(a6),x(a5)
		add.w	#32,x(a5)
		tst.w	6(a0,d0.w)
		bne	.w32
    		clr.l	x(a5)
.w32
		jmp	offandend

		even

hammerfrs	dc.w	0,0,36,0
		dc.w	7,$fffd,37,1
		dc.w	8,5,39,0
		dc.w	7,$fffd,40,1
		dc.w	3,0,42,0
		dc.w	$fff1,$fffe,43,1
		dc.w	3,5,45,0
		dc.w	$fff1,$fffe,46,1

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien130	; Thing ...

		tst.b	ht(a6)
		bne	.okok
		clr.b	ping
		st.b	ht(a6)
.okok

		clr.b	dope
		tst.w	ymom			Lift detec.
		bmi	.ok
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		sub.w	#16,d1
		move.w	#32,d2
		move.w	#32,d3
		st.b	liftcheck
		jsr	alwithzool
		beq	.ok
		st.b	dope
		move.w	y(a6),zoolwy		Position zool x and y ...
		sub.w	#60,zoolwy
		move.w	zoolwy,zooly
		add.w	#44,zooly
		move.w	scrwy,d0
		sub.w	d0,zooly
		move.w	zooly,zooly1
		clr.l	ymom
.ok		clr.b	liftcheck
	
		tst.w	any(a6)			Pinged yet ...
		beq	.check

		move.w	any(a6),d0
		tst.w	d0
		bmi	.up
		add.w	d0,y(a6)
		move.w	x(a6),d0  		Check with background.
		move.w	y(a6),d1
		sub.w	#$10,d1
		jsr	checkblock
		cmp.b	#144,(a0,d2.l)
		beq	.ping2
       		cmp.b	#145,(a0,d2.l)
		beq	.ping2
		cmp.b	#146,(a0,d2.l)
		beq	.ping2
		cmp.b	#147,(a0,d2.l)
		bne	.noping2
.ping2		clr.w	any(a6)
		andi.w	#$fff0,y(a6)
.noping2	jmp	nextalien

.up		add.w	d0,y(a6)
		move.w	#2,d0
		jsr	playsample
		move.w	x(a6),d0  		Check with background.
		move.w	y(a6),d1
		sub.w	#16,d1
		jsr	checkblock
		cmp.b	#124,(a0,d2.l)
		beq	.ping
       		cmp.b	#125,(a0,d2.l)
		beq	.ping
		cmp.b	#126,(a0,d2.l)
		beq	.ping
		cmp.b	#127,(a0,d2.l)
		bne	.noping
.ping		move.w	#10,any(a6)
		tst.b	dope
		beq	.noping
		st.b	mustup
		clr.b	ducking
		clr.b	punching
		clr.b	slideing
		st.b	jumping
		move.w	#2,d0
		jsr	playsample
		tst.b	dope
		beq	.oko333
		move.b	#30,uptime
		move.l	#-$a0000,ymom
.oko333		moveq	#$a,d0
		jsr	playsample

.noping	
		clr.b	ping
		jmp	nextalien

.check		tst.b	ping
		beq	offandend
		move.w	#-16,any(a6)
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien131	; Ballon lift...

		jsr	defmov

		clr.b	onnl

		cmp.w	#1,ymom
		bpl	.ok
		
		move.w	x(a6),d0		Alien hut zool ?
		move.w	y(a6),d1
		add.w	#65,d1
		move.w	#16,d2
		move.w	#16,d3
		st.b	liftcheck
		jsr	norangecheck
		beq	.ok

		st.b	onnl
		clr.w	splitpower
		st.b	onrails
		move.w	y(a6),zoolwy
		add.w	#$61,zoolwy
		sub.w	#60-16,zoolwy
		move.w	zoolwy,zooly
		add.w	#44,zooly
		move.w	scrwy,d0
		sub.w	d0,zooly
		move.w	zooly,zooly1
	
		move.w	x(a6),zoolwx
		add.w	#61,zoolwx
		move.w	zoolwx,zoolx
		add.w	#44,zoolx
		move.w	scrwx,d0
		sub.w	d0,zoolx
		move.w	zoolx,zoolx1

		clr.l	ymom

		move.w	#-1,ylift
.ok	
		clr.b	liftcheck

		move.w	scrwx,d0
		move.w	x(a6),d1
		add.w	#100,d1
		cmp.w	d1,d0
		bhi	.offit
		add.w	#320,d0
		sub.w	#200,d1
 		bmi	.okay
		cmp.w	d1,d0
		bls	.offit
.okay
		jmp	nextalien

.offit		jsr	freesprite
		jmp	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien132	; Target ...

		move.l	any(a6),a0
		jsr	animateal
		move.l	a0,any(a6)

		cmp.l	#.hurt,any(a6)
		bmi	.nm2    
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#48,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.nm2
		jsr	hurtzool
.nm2
		cmp.b	#$ff,fr+1(a6)
		bne	.n
  		jsr	freesprite
		jmp	nextalien
.n
.bno		cmp.b	#20,bcount
		bpl	.pl

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#48,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.nm
		move.l	#.falling,any(a6)
.nm		
   		move.b	bcount,d0
		btst	#0,d0
		beq	.pl
		move.l	a6,a4
		jsr	getlowersprite
		cmp.l	#-1,a6
		beq	.ok
		move.l	x(a4),x(a6)
		jsr	random
		andi.w	#$ff,d0
		sub.w	#$80,d0
		add.w	d0,x(a6)
		jsr	random
		andi.w	#$7f,d0
		sub.w	#$40,d0
		add.w	d0,y(a6)
		move.l	#fair1,blbset(a6)
		move.b	#133,t(a6)
		move.b	#31,fr+1(a6)
		move.l	#bullls,any(a6)
.ok		move.l	a4,a6
.pl
		jmp	offandend

.falling	dc.w	20,20,20,20,20,20,20,20,20,20,20
		dc.w	20,20,20,20,20
		dc.w	19,20,19,20,19,20,19,20,19,20,19
		dc.w	19,20,19,20,19,20,19,20,19,20,19
.hurt		dc.w	20,20,$ff

bullls		dc.w	31,32,33,34,$ff

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien133	; Target ...

		move.l	a6,a4
		move.l	firstsprite,a6
.next		cmp.l	#-1,a6
		beq	.out
		cmp.b	#132,t(a6)
		bne	.no
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#48,d3
		jsr	alwithzool		Zool on this alien ?
		bne	.outok
.no		move.l	nsp(a6),a6
		jmp	.next
.out		move.l	a4,a6
		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.outok2
		jsr	hurtzool
.outok		move.l	a4,a6
.outok2
	
		move.l	any(a6),a0
		jsr	animateal
		move.l	a0,any(a6)
		cmp.b	#$ff,fr+1(a6)
		bne	offandend
		jsr	freesprite
		jmp	nextalien
	
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien134	; Boss 7 ...

		tst.b	ht(a6) 			All initial setup ...
		bne	.doneit
		tst.b	aldone
		beq	.doit
     		jsr	freesprite
		bra	nextalien
.doit		st.b	ht(a6)
		clr.b	any+24(a6)
		move.b	#50,htg(a6)
		move.w	#30,any(a6)
		sub.w	#$28,y(a6)
		move.l	x(a6),any+2(a6)
		sub.w	#$b8,any+2(a6)
		add.w	#$48,any+4(a6)
		add.l	#$00250020,x(a6)
		move.l	#.armdat,any+14(a6)
		clr.b	any+1(a6)
		clr.w	any+26(a6)
		clr.l	any+32(a6)
		move.l	a6,a4
		move.l	a6,a3
		lea	.startoffs,a0
.nextsprite	jsr	getsprite
		cmp.l	#-1,a6
		bne	.ok
.noenough	move.w	d0,$dff180
		addq.w	#1,d0
		jmp	.noenough
.ok		move.l	#boss7,blbset(a6)
		move.l	x(a4),x(a6)
		move.w	(a0)+,d0
		add.w	d0,x(a6)
		move.w	(a0)+,d0
		add.w	d0,y(a6)
		move.w	(a0)+,d0
		move.w	d0,fr(a6)
		move.w	(a0)+,d0
		tst.w	d0
		beq	.sk
   		move.l	a6,(a4,d0.w)
.sk		move.w	x(a6),any(a6)
		move.w	y(a6),any+4(a6)
		clr.w	any+2(a6)
		clr.w	any+6(a6)
		move.l	a6,jsp(a3)
		move.l	a6,a3
		cmp.w	#$8000,(a0)
		bne	.nextsprite

		addq.w	#2,a0
.nextsprite2	jsr	getlowersprite
		cmp.l	#-1,a6
		beq	.noenough
		move.l	#boss7,blbset(a6)
		move.l	x(a4),x(a6)
		move.w	(a0)+,d0
		add.w	d0,x(a6)
		move.w	(a0)+,d0
		add.w	d0,y(a6)
		move.w	(a0)+,d0
		move.w	d0,fr(a6)
		move.w	(a0)+,d0
		tst.w	d0
		beq	.sk2
   		move.l	a6,(a4,d0.w)
.sk2		move.w	x(a6),any(a6)
		move.w	y(a6),any+4(a6)
		clr.w	any+2(a6)
		clr.w	any+6(a6)
		move.l	a6,jsp(a3)
		move.l	a6,a3
		cmp.w	#$8000,(a0)
		bne	.nextsprite2
		move.l	#-1,jsp(a3)
		move.l	a4,a6
.doneit
		tst.b	any+40(a6)
		beq	.notdead
		st.b	stophorz
		move.w	#120,shield
		st.b	aldone
		move.w	#-1,any+6(a6)
		add.l	#$8000,any+10(a6)
		subq.b	#1,any+41(a6)
		bpl	.nn
  		move.b	#2,any+41(a6)
		add.w	#$0010,x(a6)
		move.w	#$80,d4
		jsr	createexplo2
		sub.w	#$0010,x(a6)
.nn		jsr	offscreen
		beq	.morveall
.mo		jsr	freesprite
		move.l	jsp(a6),a6
		cmp.l	#-1,a6
		bne	.mo
		bra	nextalien
.notdead		
		st.b	stophorz

		move.l	any+18(a6),a5
		move.w	x(a5),d0
		move.w	y(a5),d1
		add.w	#32,d1
		moveq	#32,d2
		moveq	#16,d3
		bsr	bulhutal
		move.l	any+22(a6),a5
		move.w	x(a5),d0
		move.w	y(a5),d1
		add.w	#32,d1
		moveq	#32,d2
		moveq	#16,d3
		bsr	bulhutal
		move.w	x(a6),d0
		move.w	y(a6),d1
		moveq	#48,d2
		moveq	#64,d3
		bsr	bulhutal
		move.l	any+28(a6),a5		Skip second arm part 2.
		move.l	jsp(a5),a5
		move.l	jsp(a5),a5
		move.l	jsp(a5),a5
		move.w	x(a5),d0
		move.w	y(a5),d1
		add.w	#16,d1
		moveq	#16,d2
		moveq	#16,d3
		bsr	bulhutal
		tst.b	d7
		beq	.m
  		move.b	#2,flal(a6)
		subq.b	#1,htg(a6)
		bne	.m
		st.b	any+40(a6)
.m
		move.w	x(a6),d0
		add.w	#8,d0
		move.w	y(a6),d1
		moveq	#32,d2
		moveq	#64,d3
		bsr	alwithzool
		beq	.kme
		jsr	hurtzool
.kme

		; Tounge movement ...
	
		tst.b	any+34(a6)
		beq	.nofire

		bra	.nothanks
		subq.b	#1,any+34(a6)
		cmp.b	#6,any+34(a6)
		bpl	.nothanks
		btst.b	#0,any+34(a6)
		beq	.nothanks
		move.l	a6,-(sp)
		move.l	any+28(a6),a6		Skip second arm part 2.
		move.l	jsp(a6),a6
		move.l	jsp(a6),a6
		move.l	jsp(a6),a6
		jsr	firebullet
		move.l	(sp)+,a6
		tst.l	lastbullet
		beq	.nothanks
		move.l	lastbullet,a5
		move.l	#boss7,blbset(a5)
		move.l	#.sparkle,any+20(a5)
		bra	.nothanks

.nofire		move.l	any+28(a6),a5		Skip second arm part 2.

		tst.b	any+33(a6)
		beq	.nothanks
		tst.b	any+32(a6)
		beq	.goingfar2
		subq.b	#1,any+33(a6)
		bra	.noreset2
.goingfar2	addq.b	#1,any+33(a6)
		cmp.b	#$1f-14,any+33(a6)
		bne	.mmm
		move.b	#20,any+34(a6)
.mmm		cmp.b	#$20-14,any+33(a6)
		bne	.noreset2
		move.b	#$1d-14,any+33(a6)
		st.b	any+32(a6)
.noreset2	
		move.l	#$fff0,d0
		tst.b	any+32(a6)
		bne	.cl2
		neg.l	d0	
.cl2		move.l	d0,d2
		moveq	#4-1,d7
.nextpart2	add.l	d2,any(a5)
		add.l	d0,d2
		move.l	jsp(a5),a5
		dbra	d7,.nextpart2
.nothanks
		; Arm movement ...
	
		move.l	jsp(a6),a4		
		move.l	jsp(a4),a4		Skip second half. 
		move.l	jsp(a4),a4		Skip first arm ...
		move.l	jsp(a4),a4
		move.l	jsp(a4),a4
		move.l	jsp(a4),a4
		move.l	jsp(a4),a4
		move.l	jsp(a4),a4
		move.l	jsp(a4),a4
		move.l	jsp(a4),a4
		move.l	jsp(a4),a4		Skip second arm part 2.
	
		move.l	jsp(a6),a5		
		move.l	jsp(a5),a5		Skip second half. 
		move.l	jsp(a5),a5		Skip first arm part.
		tst.b	any+1(a6)
		beq	.goingfar
		sub.l	#8,any+14(a6)
		cmp.l	#.armdat,any+14(a6)
		bne	.noreset
		eori.b	#1,any+1(a6)
		st.b	any+26(a6)
		bra	.noreset
.goingfar	add.l	#8,any+14(a6)
		move.l	any+14(a6),a0
		cmp.l	#-1,(a0)
		bne	.noreset
;		st.b	any+26(a6)
;		st.b	any+27(a6)
		move.w	#$0001,any+32(a6)
		sub.l	#16,any+14(a6)
		eori.b	#1,any+1(a6)
.noreset	move.l	any+14(a6),a0
		move.l	(a0),d0
		move.l	4(a0),d1
		tst.b	any+1(a6)
		beq	.cl
		neg.l	d0	
		neg.l	d1	
.cl		move.l	d0,d2
		move.l	d1,d3
		moveq	#7-1,d7
.nextpart	add.l	d2,any(a5)
		add.l	d3,any+4(a5)
		add.l	d2,any(a4)
		sub.l	d3,any+4(a4)
		add.l	d0,d2
		add.l	d1,d3
		move.l	jsp(a5),a5
		move.l	jsp(a4),a4
		dbra	d7,.nextpart

		; Global movement ...

		cmp.w	#-9,any+6(a6)
		beq	.noadd
		sub.l	#$8000,any+6(a6)	X mom.
.noadd		move.w	any+2(a6),d0
		cmp.w	x(a6),d0
		bmi	.lessx
		add.l	#$8000*2,any+6(a6)
.lessx		sub.l	#$8000,any+10(a6)	Y mom.
		move.w	any+4(a6),d0
		cmp.w	y(a6),d0
		bmi	.lessy
		add.l	#$8000*2,any+10(a6)
.lessy	
.morveall	move.w	any+6(a6),d0		Update X and Y for all parts.
    		move.w	any+10(a6),d1
		add.w	d0,x(a6)
		add.w	d1,y(a6)
		move.l	jsp(a6),a5
.nexthandle	cmp.l	#-1,a5
		beq	.fin
		add.w	d0,any(a5)
		move.w	any(a5),x(a5)
		add.w	d1,any+4(a5)
		move.w	any+4(a5),y(a5)
		move.b	flal(a6),flal(a5)
		move.l	jsp(a5),a5
		jmp	.nexthandle
.fin
		bsr	.fireyet
		bsr	firelaz

		jmp	nextalien

.sparkle	dc.w	4,4,5,5,$ffff
		dc.l	.sparkle

.startoffs	dc.w	32,0,1,0

		dc.w	16-5,0+2,2,0
		dc.w	15-5,-7+2,2,0
		dc.w	13-5,-14+2,2,0
		dc.w	10-5,-20+2,2,0
		dc.w	7-5,-25+2,2,0
		dc.w	4-5,-28+2,2,0
		dc.w	1-5,-29+2,2,0
		dc.w	-22-5,-45+2,3,any+18

		dc.w	16,550+65,2,0
		dc.w	15,557+65,2,0
		dc.w	13,5514+65,2,0
		dc.w	10,5520+65,2,0
		dc.w	7,5525+65,2,0
		dc.w	4,5528+65,2,0
		dc.w	1,5529+65,2,0
		dc.w	-22,5520+65,3,any+22
		dc.w	$8000

		dc.w	32-16,30,11,any+28		
		dc.w	32-16,30,11,0
		dc.w	32-16,30,11,0
		dc.w	32-32,30-6,8,0
		dc.w	$8000

.armdat		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	-$1000,$0000
		dc.l	-$2000,$0000
		dc.l	-$3000,$0000
		dc.l	-$4000,$0000
		dc.l	-$4000,$0000
		dc.l	-$5000,$0000
		dc.l	-$5000,$2000
		dc.l	-$5000,$4000
		dc.l	-$5000,$6000
		dc.l	-$5000,$8000
		dc.l	-$5000,$a000
		dc.l	-$5000,$c000
		dc.l	-$4000,$e000
		dc.l	-$4000,$e000
		dc.l	-$3000,$c000
		dc.l	-$2000,$a000
		dc.l	-$1000,$8000
		dc.l	 $0000,$6000
		dc.l	 $0000,$4000
		dc.l	 $0000,$2000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	 $0000,$0000
		dc.l	-1

.fireyet	tst.b	any+26(a6)
		beq	bye
		clr.b	any+26(a6)
		move.l	a6,-(sp)
		move.l	any+18(a6),a6
		bsr	.fire3way
		move.l	(sp),a6
		move.l	any+22(a6),a6
		bsr	.fire3way
		move.l	(sp)+,a6
		rts

.fire3way	jsr	firebullet
		tst.l	lastbullet
		beq	.notopen
		move.l	lastbullet,a5
		move.l	#$fff70000,any(a5)
		move.l	#$00000000,any+4(a5)
		add.w	#$10,any+12(a5)
		move.l	#boss7,blbset(a5)
		clr.l	any+20(a5)
		move.b	#6,fr+1(a5)
		sub.w	#20,any+8(a6)
		jsr	firebullet
		tst.l	lastbullet
		beq	.notopen
		move.l	lastbullet,a5
		move.l	#$fff90000,any(a5)
		move.l	#$00040000,any+4(a5)
		add.w	#$10,any+12(a5)
		move.l	#boss7,blbset(a5)
		clr.l	any+20(a5)
		move.b	#6,fr+1(a5)
		sub.w	#20,any+8(a6)
      		jsr	firebullet
		tst.l	lastbullet
		beq	.notopen
		move.l	lastbullet,a5
		move.l	#$fff90000,any(a5)
		move.l	#$fffc0000,any+4(a5)
		add.w	#$10,any+12(a5)
		move.l	#boss7,blbset(a5)
		clr.l	any+20(a5)
		move.b	#6,fr+1(a5)
		sub.w	#20,any+8(a6)
.notopen	rts



firelaz		subq.b	#1,any+27(a6)
		bpl	bye
		move.b	#45,any+27(a6)
		move.l	a6,-(sp)
		move.l	any+18(a6),a6
		bsr	.fire3way
		move.l	(sp),a6
		move.l	any+22(a6),a6
		bsr	.fire3way
		move.l	(sp)+,a6
		rts

.fire3way	jsr	firebullet
		tst.l	lastbullet
		beq	bye
		move.l	lastbullet,a5
		move.l	#$fff50000,any(a5)
		move.l	#$00000000,any+4(a5)
		add.w	#$10,any+12(a5)
		move.l	#boss7,blbset(a5)
		clr.l	any+20(a5)
		move.b	#9,fr+1(a5)
		sub.w	#20,any+8(a6)
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien135	; Boss 6 ...

		tst.b	ht(a6) 			All initial setup ...
		bne	.doneit
		tst.b	aldone
		beq	.doit
     		jsr	freesprite
		bra	nextalien
.doit		st.b	ht(a6)
		move.b	#30,htg(a6)
		clr.w	any+40(a6)
		clr.l	any(a6)
		clr.l	any+4(a6)
		clr.l	any+16(a6)
		move.w	x(a6),any+12(a6)
		sub.w	#$100,any+12(a6)
		move.w	#-3,any+14(a6)
		move.l	a6,a4
		move.l	a6,a3
		lea	.startoffs,a0
.nextsprite	jsr	getlowersprite
		cmp.l	#-1,a6
		bne	.ok
.noenough	move.w	d0,$dff180
		addq.w	#1,d0
		jmp	.noenough
.ok		move.l	#boss6,blbset(a6)
		addq.w	#4,a0
		move.w	(a0)+,d0
		move.w	d0,fr(a6)
		move.w	(a0)+,d0
		move.l	a6,jsp(a3)
		move.l	a6,a3
		cmp.w	#$8000,(a0)
		bne	.nextsprite
		move.l	#-1,jsp(a3)
		move.l	a4,a6
.doneit
		tst.b	any+40(a6)
		beq	.notdead
		st.b	stophorz
		move.w	#120,shield
		st.b	aldone
		subq.w	#1,x(a6)
		add.l	#$8000,any(a6)
		move.w	any(a6),d0
		add.w	d0,y(a6)
		subq.b	#1,any+41(a6)
		bpl	.nn
  		move.b	#2,any+41(a6)
		move.w	#$80,d4
		add.w	#$0010,x(a6)
		jsr	createexplo2
		sub.w	#$0010,x(a6)
.nn		jsr	offscreen
		beq	.morveall
.mo		jsr	freesprite
		move.l	jsp(a6),a6
		cmp.l	#-1,a6
		bne	.mo
		bra	nextalien
.notdead		

		st.b	stophorz

		; Detection ...

		move.w	x(a6),d0		Check alien and body ...
		move.w	y(a6),d1
		sub.w	#15,d0
		move.w	#64,d2
		move.w	#40,d3
		jsr	alwithzool		
		beq	.maybenot
		jsr	hurtzool
.maybenot	move.w	x(a6),d0		Check body and bull...
		move.w	y(a6),d1
		sub.w	#15,d0
		add.w	#14,d1
		move.w	#64,d2
		move.w	#40,d3
		bsr	bulhutal

		move.w	x(a6),d0		Check head ...
		move.w	y(a6),d1
		sub.w	#32,d1
		moveq	#32,d2
		moveq	#32,d3
		bsr	bulhutal
		tst.b	d7
		beq	.m
  		move.b	#2,flal(a6)
		subq.b	#1,htg(a6)
		bne	.m
		st.b	any+40(a6)
.m
		; Jumping ...

		tst.b	any+4(a6)
		beq	.jumping
		move.b	#$ff,.startoffs+7
		move.l	any+8(a6),a0
		jsr	animateal
		move.l	a0,any+8(a6)

		cmp.b	#$fe,fr+1(a6)
		bne	.nofire
		move.b	#6,fr+1(a6)
		sub.w	#40,zoolwx
		jsr	okaybul
		add.w	#40,zoolwx
		tst.l	lastbullet
		beq	.nofire
		move.l	lastbullet,a5
		add.w	#8,any+8(a5)
		move.b	#18,fr+1(a5)
		move.l	#boss6,blbset(a5)
		move.l	#.sparkle,any+20(a5)
.nofire
		move.b	#$ff,.eyes+7
		move.w	x(a6),d0
		cmp.w	zoolwx,d0
		bpl	.rm
		add.w	#$28,d0
		cmp.w	zoolwx,d0
		bpl	.thisone
		move.b	#4,.eyes+7
		add.w	#$40,d0
		cmp.w	zoolwx,d0
		bpl	.thisone
		move.b	#5,.eyes+7
		bra	.thisone
.rm		sub.w	#$28,d0
		cmp.w	zoolwx,d0
		bmi	.thisone
		move.b	#3,.eyes+7
		sub.w	#$40,d0
		cmp.w	zoolwx,d0
		bmi	.thisone
		move.b	#2,.eyes+7
.thisone	move.l	jsp(a6),a5
		move.l	jsp(a5),a5
		move.b	.eyes+7,fr+1(a5)		

		cmp.b	#$ff,fr+1(a6)
		beq	.no
		move.b	fr+1(a6),.startoffs+7
		addq.b	#1,.startoffs+7
.no		tst.w	any+6(a6)
		beq	.mnv
    		subq.w	#1,any+6(a6)
		subq.w	#1,y(a6)
.mnv		subq.b	#1,any+4(a6)
		bne	.nojumpnow
		jsr	random				New jump ...
		andi.w	#$7,d0
		sub.w	#$f,d0
		move.w	d0,any(a6)
		clr.l	any+8(a6)
		move.w	any+12(a6),d0
		tst.w	any+14(a6)
		bmi	.left
    		add.w	#$50,d0
		cmp.w	x(a6),d0
		bmi	.neg
		bra	.noground
.left		sub.w	#$50,d0
		cmp.w	x(a6),d0
		bmi	.noground
.neg		neg.w	any+14(a6)
.nojumpnow	bra	.noground
.jumping	move.w	any+14(a6),d0
		add.w	d0,x(a6)
		cmp.w	#$e,any(a6)
		beq	.l
		add.l	#$e000,any(a6)
.l		move.w	any(a6),d0
		add.w	d0,y(a6)
		move.w	x(a6),d0  		Check with background.
		move.w	y(a6),d1
		add.w	#38,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.w)
		beq	.noground
		move.w	y(a6),d0
		add.w	#6,d0
		andi.w	#$fff0,d0
		sub.w	#6,d0
		move.w	y(a6),d1
		sub.w	d0,d1
		move.w	d1,any+6(a6)
		move.b	#60,any+4(a6)
		move.l	#.doorani,any+8(a6)
		clr.l	any(a6)
.noground	
		cmp.w	#1,any(a6)
		bmi	.nofiremiss
	   	subq.b	#1,any+17(a6)
		bpl	.nofiremiss
		move.b	#35,any+17(a6)
		move.l	a6,a4
		bsr	getsprite
		cmp.l	#-1,a6
		beq	.nogot
		move.l	x(a4),x(a6)
		move.l	#boss6,blbset(a6)
		clr.b	any+30(a6)
		move.b	#136,t(a6)
		move.w	#45,any+4(a6)
		move.l	#$fffc,any(a6)
		move.b	#10,fr+1(a6)
		add.w	#14,x(a6)
		sub.w	#50,y(a6)
.nogot		move.l	a4,a6
.nofiremiss	

		tst.w	any(a6)
		bpl	.nofirelaz
     		subq.b	#1,any+16(a6)
		bpl	.nofirelaz
		move.w	#$0300,any+16(a6)

		moveq	#0,d0
		move.l	jsp(a6),a5
		move.l	jsp(a5),a5
		move.b	fr+1(a5),d0
		cmp.b	#$ff,d0	 
		bne	.norm
		moveq	#1,d0
.norm		subq.w	#1,d0
		lsl.w	#3,d0
		lea	.table,a0
		add.w	d0,a0

		jsr	okaybul
		tst.l	lastbullet
		beq	.nofirelaz
		move.l	lastbullet,a5
		move.l	#boss6,blbset(a5)
		clr.l	any+20(a5)
		move.w	(a0),any(a5)
		move.w	2(a0),any+4(a5)
		sub.w	#$20,any+12(a5)
		sub.w	#$20,y(a5)
		move.w	4(a0),d0
		add.w	#16+8,d0
		add.w	d0,any+8(a5)
		add.w	d0,x(a5)
		move.w	6(a0),fr(a5)
	  
		jsr	okaybul
		tst.l	lastbullet
		beq	.nofirelaz
		move.l	lastbullet,a5
		move.l	#boss6,blbset(a5)
		clr.l	any+20(a5)
		move.w	(a0),any(a5)
		move.w	2(a0),any+4(a5)
		sub.w	#$20,any+12(a5)
		sub.w	#$20,y(a5)
		move.w	4(a0),d0
		add.w	#8,d0
		add.w	d0,any+8(a5)
		add.w	d0,x(a5)
		move.w	6(a0),fr(a5)
.nofirelaz

		; Global movement ...

.morveall	lea	.startoffs,a0
		move.l	jsp(a6),a5		Update X and Y for all parts.
   		move.w	any+6(a6),d6
.nexthandle	cmp.l	#-1,a5
		beq	.fin
		move.l	x(a6),x(a5)
		move.w	(a0)+,d0
		add.w	d0,x(a5)
		move.w	(a0)+,d1
		add.w	d1,y(a5)
		cmp.l	#.stof,a0
		bmi	.sk2
		sub.w	d6,y(a5)
.sk2		move.b	flal(a6),flal(a5)
		addq.w	#4,a0
		move.l	jsp(a5),a5
		jmp	.nexthandle
.fin
		jmp	nextalien

.table		dc.w	0,8,0,25
		dc.w	-10,10,-$d,27
		dc.w	-5,9,-$d,26
		dc.w	5,9,0,24
		dc.w	10,10,0,23

.sparkle	dc.w	18,18,18,19,19,19,$ffff
		dc.l	.sparkle
.doorani	dc.w	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
		dc.w	8,8,8,6,6,6,6,6,6,6,6,6,6,6,6,$fe
		dc.w	6,6,6,6,6,6,$fe,6,6,6,6,6,6,6,8,8,8
.nmn		dc.w	$ff,$ffff
		dc.l	.nmn

.startoffs	dc.w	32,0,$ff,0		Panel p2 ...
.eyes		dc.w	3,-29,2,0		Eyes ...
		dc.w	-16+3,-37,0,0		Robot ...
		dc.w	16+3,-37,1,0
.stof		dc.w	-13+3,30,28,0		Legs ...
		dc.w	11+3,30,28,0
		dc.w	$8000

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien136	; Homing missile ...

		move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	alwithzool		Zool on this alien ?
		beq	.maybenot
		jsr	hurtzool
.maybenot
		subq.w	#1,any+4(a6)		Timer...
		bpl	.k
		move.l	#$80,d4
		sub.w	#8,x(a6)
		sub.w	#8,y(a6)
  		bsr	createexplo2
		bsr	freesprite
		bra	nextalien
.k
		move.l	zoolwx,-(sp)
		add.l	#$00200030,zoolwx
		lea.l	zoolwx,a2		X.
		move.w	x(a6),d0
		cmp.w	x(a2),d0
		bpl	.incx
.decx		cmp.w	#4,any(a6)
		beq	.homy
		addq.w	#1,any(a6)
		bra	.homy
.incx		cmp.w	#-4,any(a6)
		beq	.homy
		subq.w	#1,any(a6)
.homy		
		move.w	y(a6),d0		Y.
		cmp.w	y(a2),d0
		beq	.justmove
		bpl	.incy
		cmp.w	#4,any+2(a6)
		beq	.justmove
		addq.w	#1,any+2(a6)
		bra	.justmove
.incy		cmp.w	#-4,any+2(a6)
		beq	.justmove
		subq.w	#1,any+2(a6)

.justmove	move.w	any(a6),d0		Move.
		add.w	d0,x(a6)
		move.w	any+2(a6),d0
		add.w	d0,y(a6)
		
		moveq	#0,d0			Choose right frs ...
		cmp.w	#-2,any+2(a6)
		bpl	.notup
		bset	#0,d0
.notup		cmp.w	#2,any+2(a6)
		bmi	.notdown
		bset	#1,d0
.notdown	cmp.w	#2,any(a6)
		bmi	.notright
		bset	#2,d0
.notright	cmp.w	#-2,any(a6)
		bpl	.notleft
		bset	#3,d0
.notleft	lea	.misslefrs,a1
		tst.b	(a1,d0.w)
		beq	.nofr
		move.b	(a1,d0.w),fr+1(a6)
.nofr	
		move.l	(sp)+,zoolwx

		bra	offandend	
		
.misslefrs	dc.b	0,17-7
		dc.b	21-7,0
		dc.b	19-7,18-7
		dc.b	20-7,0
		dc.b	23-7,24-7
		dc.b	22-7,0
		dc.b	0,0
		dc.b	0,0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien137	; Clock ...

		move.w	x(a6),d0		Check alien and body ...
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	alwithzool		
		beq	.maybenot
		jsr	hurtzool
.maybenot	
	
		tst.b	ht(a6)
		bne	.done
		st.b	ht(a6)
		move.w	#-4,any+12(a6)
     		move.w	x(a6),any+10(a6)
		clr.w	any+8(a6)
.done
		move.w	any+12(a6),d0
		add.w	d0,x(a6)
		move.w	any+10(a6),d0
		cmp.w	x(a6),d0
		bpl	.ok2
		move.w	#-4,any+12(a6)
.ok2		sub.w	#22*$10,d0
		cmp.w	x(a6),d0
		bmi	.ok
		move.w	#4,any+12(a6)
.ok

		subq.b	#1,any+8(a6)
		bpl	.ok4
   		move.b	#60,any+8(a6)
.ok4		move.b	any+8(a6),d0
		cmp.b	#25,d0
		bpl	.nospit
       		andi.b	#3,d0
		tst.b	d0
		bne	.nospit

		move.l	a6,a4
		jsr	getsprite
		cmp.l	#-1,a6
		beq	.didnt		
      		move.l	#boss4,blbset(a6)
		move.b	#3,fr+1(a6)
		move.l	x(a4),x(a6)
		move.b	#$ff,any+10(a6)
		move.b	#6,t(a6)
		clr.b	any+30(a6)
		move.l	#$e000,any+12(a6)
		jsr	random		X
		andi.w	#$3,d0
		sub.w	#$9,d0
		move.w	d0,any+2(a6)
.n		jsr	random		Y
		andi.w	#$3,d0
		move.w	d0,any(a6)
		jsr	random
		btst	#0,d0
		beq	.nn
   		neg.w	any(a6)
.nn		clr.w	any+4(a6)
.didnt		move.l	a4,a6
.nospit		

		move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	bulhutal
		tst.b	d7
		beq	.nothut
		move.w	#300,any(a6)
		tst.l	any+2(a6)
		bne	.nothut
		move.l	#.animation,any+2(a6)
.nothut
		tst.w	any(a6)
		beq	.still
		st.b	alarm
		subq.w	#1,any(a6)
		move.l	any+2(a6),a0
		jsr	animateal
		move.l	a0,any+2(a6)
		bra	nextalien
.still		clr.b	alarm	    	
		move.b	#1,fr+1(a6)
		clr.l	any+2(a6)
		bra	nextalien

.animation	dc.w	0,1,2,1,$ffff
		dc.l	.animation

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien138	; Banana...

		tst.b	ht(a6) 			All initial setup ...
		bne	.doneit
		clr.w	any+40(a6)
		move.b	#30,htg(a6)
		tst.b	aldone
		beq	.doit
     		jsr	freesprite
		bra	nextalien
.doit		st.b	ht(a6)
 		jsr	random
		andi.w	#$3f,d0
		add.w	#$80,d0
		sub.w	#$50,y(a6)
       		move.w	d0,any+12(a6)
		move.l	x(a6),any(a6)
		sub.w	#$e0,any(a6)
		add.l	#$00000020,x(a6)
		move.l	a6,a4
		move.l	a6,a3
		moveq	#4-1,d7
.nextsprite	jsr	getsprite
		cmp.l	#-1,a6
		bne	.ok
.noenough	move.w	d0,$dff180
		addq.w	#1,d0
		jmp	.noenough
.ok		move.l	#boss4,blbset(a6)
		move.l	a6,jsp(a3)
		move.l	a6,a3
		dbra	d7,.nextsprite
		move.l	#-1,jsp(a3)
		move.l	a4,a6
.doneit
		st.b	stophorz

		tst.b	any+40(a6)
		beq	.notdead
		st.b	stophorz
		move.w	#120,shield
		st.b	aldone
		sub.w	#-1,x(a6)
		add.l	#$8000,any+8(a6)
		move.w	any+8(a6),d0
		add.w	d0,y(a6)
		subq.b	#1,any+41(a6)
		bpl	.nn
  		move.b	#2,any+41(a6)
		move.w	#$80,d4
		add.w	#$0010,x(a6)
		jsr	createexplo2
		sub.w	#$0010,x(a6)
.nn		jsr	offscreen
		beq	.morveall
.mo		jsr	freesprite
		move.l	jsp(a6),a6
		cmp.l	#-1,a6
		bne	.mo
		bra	nextalien
.notdead		
		move.b	#$ff,.eye+5  		Asleep yet ?
		cmp.b	#$ff,.blade+5
		bne	.cant
		tst.b	alarm
		bne	.cant
      		move.b	#25,any+16(a6)
		move.b	#9,.eye+5		Asleep ...
		tst.w	any+4(a6)		X slow.
		beq	.doy
		tst.l	any+4(a6)
		bpl	.s
		add.l	#$6000,any+4(a6)
		bra	.doy
.s		sub.l	#$6000,any+4(a6)		
.doy		tst.w	any+8(a6)		Y slow.
		beq	.lessx
		tst.l	any+8(a6)
		bpl	.s2
		add.l	#$6000,any+8(a6)
		bra	.lessx
.s2		sub.l	#$6000,any+8(a6)		
		bra	.lessx
.cant

		cmp.b	#$ff,.blade+5		Fire a banana splip ...
		bne	.nofire
		subq.b	#1,any+16(a6)
		bpl	.nores
      		move.b	#60,any+16(a6)
.nores		cmp.b	#5,any+16(a6)
		beq	.fire
     		cmp.b	#10,any+16(a6)
		beq	.fire
		cmp.b	#15,any+16(a6)
		bne	.nofire
.fire		jsr	okaybul
		tst.l	lastbullet
		beq	.nofire
		move.l	lastbullet,a5
		move.l	#boss4,blbset(a5)
		move.b	#8,fr+1(a5)
		move.l	#-$50000,any(a5)
		move.l	#$50000,any+4(a5)
		clr.l	any+20(a5)
		sub.w	#10,any+8(a5)
		add.w	#100,any+12(a5)
		sub.w	#10,x(a5)
		add.w	#100,y(a5)
.nofire

		subq.w	#1,any+12(a6)
		bne	.no
   		jsr	random
		andi.w	#$3f,d0
		add.w	#$60,d0
       		move.w	d0,any+12(a6)
		bra	.noduck
.no		cmp.w	#30,any+12(a6)
		bpl	.noduck
      		move.b	#25,any+16(a6)
		move.b	#10,.blade+5
		cmp.w	#6,any+8(a6)
		beq	.sk
   		add.l	#$8000,any+8(a6)
.sk		move.w	any+2(a6),d0
		add.w	#$30,d0
		cmp.w	y(a6),d0
		bpl	.doney
		move.b	#1,any+12(a6)
		bra	.doney
.noduck
		move.w	any+2(a6),d0
		cmp.w	y(a6),d0
		bmi	.lessy
		add.l	#$5000,any+8(a6)
		move.b	#$ff,.blade+5
		cmp.w	#5,any+8(a6)		MOM check ...
		bmi	.doney
		move.w	#5,any+8(a6)
		bra	.doney
.lessy		sub.l	#$5000,any+8(a6)	Y mom.
		cmp.w	#-5,any+8(a6)
		bgt	.doney
		move.w	#-5,any+8(a6)
.doney		
		tst.b	any+14(a6)
		beq	.left
		cmp.w	#5,any+4(a6)
		beq	.n2
  		add.l	#$5000,any+4(a6)
.n2   		move.w	any(a6),d0
		add.w	#$40,d0
		cmp.w	x(a6),d0
		bpl	.lessx
		eori.b	#1,any+14(a6)
		bra	.lessx
.left		cmp.w	#-5,any+4(a6)
		beq	.n
  		sub.l	#$5000,any+4(a6)
.n   		move.w	any(a6),d0
		sub.w	#$40,d0
		cmp.w	x(a6),d0
		bmi	.lessx
		eori.b	#1,any+14(a6)
.lessx		move.w	any+4(a6),d0
		add.w	d0,x(a6)
		move.w	any+8(a6),d0
		add.w	d0,y(a6)

		move.b	#6,.boble+5     	Boble animation.
		cmp.w	#2,any+4(a6)
		bpl	.pos
		cmp.w	#-2,any+4(a6)
		bpl	.done
		move.b	#7,.boble+5      
		bra	.done
.pos		move.b	#5,.boble+5
.done	
		; Detection ...

		cmp.b	#$ff,.eye+5		No if asleep ...
		bne	.m
		move.w	x(a6),d0		Check alien and body ...
		move.w	y(a6),d1
		add.w	#16,d0
		move.w	#20,d2
		move.w	#100,d3
		jsr	alwithzool		
		beq	.maybenot
		jsr	hurtzool
.maybenot	move.w	x(a6),d0		Check body and bull...
		move.w	y(a6),d1
		add.w	#30,d0
		move.w	#20,d2
		move.w	#100,d3
		bsr	bulhutal
		move.w	x(a6),d0		Check head ...
		move.w	y(a6),d1
		add.w	#90,d1
		moveq	#20,d2
		moveq	#8,d3
		bsr	bulhutal
		tst.b	d7
		beq	.m
  		move.b	#2,flal(a6)
		subq.b	#1,htg(a6)
		bne	.m
		st.b	any+40(a6)
.m
		; Global movement ...

.morveall	lea	.startoffs,a0
		move.l	jsp(a6),a5		Update X and Y for all parts.
   		move.w	any+6(a6),d6
.nexthandle	cmp.l	#-1,a5
		beq	.fin
		move.l	x(a6),x(a5)
		move.w	(a0)+,d0
		add.w	d0,x(a5)
		move.w	(a0)+,d0
		add.w	d0,y(a5)
		move.w	(a0)+,d0
		move.w	d0,fr(a5)
		move.b	flal(a6),flal(a5)
		move.l	jsp(a5),a5
		jmp	.nexthandle
.fin


		bra	nextalien

.startoffs	dc.w	16,0,12
.eye		dc.w	12,70,9		
.blade		dc.w	-7,100,10		
.boble		dc.w	-16,5,5		

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien139	; Side Drill ...

		tst.b	ht(a6) 			All initial setup ...
		bne	.doneit
		tst.b	aldone
		beq	.d3oit
     		jsr	freesprite
		bra	nextalien
.d3oit		move.l	#.xtable,any+8(a6)
		move.w	y(a6),any(a6)
		clr.w	any+40(a6)
		move.b	#30,htg(a6)
		sub.w	#$50,any(a6)
		sub.w	#$e0,x(a6)
		sub.w	#$130,y(a6)
		andi.w	#$fff0,x(a6)
		andi.w	#$fff0,scrwx
		st.b	ht(a6)

		move.b	#2,.startoffs+5+6
		move.b	#1,.startoffs+5

		move.l	a6,a4
		move.l	a6,a3
		moveq	#6-1,d7
.nextsprite	jsr	getlowersprite
		cmp.l	#-1,a6
		bne	.ok
.noenough	move.w	d0,$dff180
		addq.w	#1,d0
		jmp	.noenough
.ok		move.l	#boss2,blbset(a6)
		move.l	a6,jsp(a3)
		move.l	a6,a3
		dbra	d7,.nextsprite
		move.l	#-1,jsp(a3)
		move.l	a4,a6
		move.w	#$404,any+16(a6)
.doneit

		tst.b	any+40(a6)
		beq	.notdead
		st.b	stophorz
		move.w	#120,shield
		st.b	aldone
		move.w	#-1,any+6(a6)
		add.l	#$8000,any+2(a6)
		move.w	any+2(a6),d0
		add.w	d0,y(a6)

		subq.b	#1,any+41(a6)
		bpl	.nn
  		move.b	#2,any+41(a6)
		move.w	#$80,d4
		add.w	#32,y(a6)
		add.w	#$008,x(a6)
		jsr	createexplo2
		sub.w	#$008,x(a6)
		sub.w	#32,y(a6)
.nn		jsr	offscreen
		beq	.morveall
.mo		jsr	freesprite
		move.l	jsp(a6),a6
		cmp.l	#-1,a6
		bne	.mo
		bra	nextalien
.notdead		
		st.b	stophorz


		; Y movement ...

		tst.w	any+12(a6)
		beq	.propery
		cmp.w	#$e,any+2(a6)
		beq	.doney
		add.l	#$f000,any+2(a6)
		bra	.doney

.propery	move.w	any(a6),d0		Y mom stuff.
		cmp.w	y(a6),d0
		bmi	.lessy
.gotofloor	cmp.w	#5,any+2(a6)
		beq	.doney
		add.l	#$8000,any+2(a6)
		bra	.doney
.lessy		cmp.w	#-8,any+2(a6)
		beq	.doney
		sub.l	#$ffff,any+2(a6)
.doney		move.w	any+2(a6),d0
		add.w	d0,y(a6)	



		move.w	#111+16,.block+2

		; Drilling ...

		tst.w	any+12(a6)
		beq	.notdrilling
	
		move.w	any(a6),d0
		add.w	#$50,d0
		cmp.w	y(a6),d0
		bpl	.more
		clr.w	any+12(a6)
		bra	.notdrilling
     	
.more		move.w	x(a6),d0		Drill bit hut ground ?
		move.w	y(a6),d1
		add.w	#111,d1
		jsr	checkblock
    		btst.b	#0,1(a1,d3.w)
		beq	.nogr
		clr.l	any+2(a6)
		andi.w	#$fff0,y(a6)
		move.w	any+12(a6),d0
		sub.w	#5,d0
		bmi	.skip
		cmp.w	#$f,d0
		bmi	.yes
    		moveq	#$f,d0
.yes		add.w	d0,y(a6)
		sub.w	d0,.block+2
.skip		addq.w	#1,any+12(a6)
.nogr		
		cmp.w	#5,any+12(a6)		Animate ground ...
		bne	.n1
  		move.b	#7,.block+5
.n1		cmp.w	#9,any+12(a6)
		bne	.n2
  		move.b	#8,.block+5
.n2		cmp.w	#12,any+12(a6)
		bne	.n3
  		move.b	#9,.block+5
.n3
		cmp.w	#16,any+12(a6)		Clear block ...
		bne	.noplotblock
  		move.b	#17,.block+5
		pushall
    		moveq	#0,d0
		move.w	x(a6),d0
		sub.w	scrwx,d0
		lsr.w	#4,d0
		addq.w	#1,d0
		moveq	#0,d1
		move.l	mapy,d2
		move.w	y(a6),d1
		add.w	#111,d1
		lsr.w	#4,d1
		sub.w	d2,d1
		lea	blankblock,a6
		lea.l	scrplanes,a0
		add.l	scrollpos,a0
		jsr	createblock

		pullall
.noplotblock
		cmp.w	#17,any+12(a6)		Go up from here ...
		bne	.noend
   		clr.w	any+12(a6)
  		move.b	#$ff,.block+5
		move.w	#-$6,any+2(a6)
    		move.b	#$ff,(a0,d2.w)
.noend
		bra	.nomovement
.notdrilling

		; X movement ...

		tst.w	any+6(a6)
		beq	.newx
		move.w	any+6(a6),d0
		cmp.w	x(a6),d0
		bmi	.less
		addq.w	#4,x(a6)
		cmp.w	x(a6),d0
		bgt	.nomovement
.doit		move.w	#1,any+12(a6)
		move.w	any+6(a6),x(a6)
		clr.w	any+6(a6)
		andi.w	#$fff0,x(a6)
		add.w	#8,x(a6)
		bra	.nomovement
.less		subq.w	#4,x(a6)
		cmp.w	x(a6),d0
		bmi	.nomovement
     		bra	.doit
.newx		move.l	any+8(a6),a0
		cmp.w	#-1,(a0)
		bne	.ook
		move.l	#.xtable,any+8(a6)
		lea	.xtable,a0
.ook		addq.l	#2,any+8(a6)
		move.w	(a0),d0
		add.w	scrwx,d0
		sub.w	#$f,d0
		move.w	d0,any+6(a6)
.nomovement	
		
		; Handle firing ...

		subq.b	#1,any+14(a6)
		bpl	.nores
      		move.b	#40,any+14(a6)
.nores		cmp.b	#11,any+14(a6)
		beq	.fire
		cmp.b	#8,any+14(a6)
		beq	.fire
		cmp.b	#5,any+14(a6)
		beq	.fire
		cmp.b	#4,any+14(a6)
		bne	.nofire
		eori.b	#1,any+15(a6)
		bra	.nofire
.fire		tst.b	any+15(a6)		Handle destroyed ...
		beq	.lef
		cmp.b	#2,.startoffs+5+6
		bne	.nofire
		bra	.firee
.lef		cmp.b	#1,.startoffs+5
		bne	.nofire
.firee		jsr	okaybul			Fire it then ...
		tst.l	lastbullet
		beq	.nofire
		move.l	lastbullet,a5
		move.l	#boss2,blbset(a5)
		move.b	#14,fr+1(a5)
		clr.l	any+20(a5)
		sub.w	#32,x(a5)
		move.l	#-$40000,any(a5)
		move.l	#$60000,any+4(a5)
		tst.b	any+15(a6)
		beq	.nox
		add.w	#32+32+16,x(a5)
		move.l	#$40000,any(a5)
		move.l	#$60000,any+4(a5)
.nox		move.w	x(a5),any+8(a5)
.nofire


		; Detection ...

		move.w	x(a6),d0		Check alien and body ...
		move.w	y(a6),d1
		add.w	#8,d0
		move.w	#16,d2
		move.w	#100,d3
		jsr	alwithzool		
		beq	.maybenot
		jsr	hurtzool
.maybenot	move.w	x(a6),d0		Check body and bull...
		move.w	y(a6),d1
		add.w	#8,d0
		move.w	#16,d2
		move.w	#100,d3
		bsr	bulhutal
		move.w	x(a6),d0		Check head ...
		move.w	y(a6),d1
		sub.w	#20,d0
		add.w	#55,d1
		moveq	#72,d2
		moveq	#8,d3
		bsr	bulhutal
		tst.b	d7
		beq	.m
		move.l	a6,a5
.this  		move.b	#2,flal(a5)
		move.l	jsp(a5),a5
		cmp.l	#-1,a5
		bne	.this
		subq.b	#1,htg(a6)
		bne	.m
		st.b	any+40(a6)
.m
		cmp.b	#3,.startoffs+5
		beq	.m44
		move.l	jsp(a6),a5
		move.w	x(a5),d0		Check head ...
		move.w	y(a5),d1
		moveq	#32,d2
		moveq	#16,d3
		bsr	bulhutal
		tst.b	d7
		beq	.m44
		move.b	#2,flal(a5)
		subq.b	#1,any+16(a6)
		bpl	.m44
		move.b	#3,.startoffs+5
.m44
		cmp.b	#4,.startoffs+5+6
		beq	.m2
		move.l	jsp(a6),a5
		move.l	jsp(a5),a5
		move.w	x(a5),d0		Check head ...
		move.w	y(a5),d1
		moveq	#32,d2
		moveq	#16,d3
		bsr	bulhutal
		tst.b	d7
		beq	.m2
		move.b	#2,flal(a5)
		subq.b	#1,any+17(a6)
		bpl	.m2
		move.b	#4,.startoffs+5+6
.m2

	
		; Global movement ...

.morveall	lea	.startoffs,a0
		move.l	jsp(a6),a5		Update X and Y for all parts.
.nexthandle	cmp.l	#-1,a5
		beq	.fin
		move.l	x(a6),x(a5)
		move.w	(a0)+,d0
		add.w	d0,x(a5)
		move.w	(a0)+,d0
		add.w	d0,y(a5)
		move.w	(a0)+,d0
		move.w	d0,fr(a5)
		move.l	jsp(a5),a5
		jmp	.nexthandle
.fin
		eori.b	#1,.bit+3

		bra	nextalien

.startoffs	dc.w	-32,0,1
		dc.w	32,0,2		
.bit		dc.w	8,111,5
.block		dc.w	8,111+16,$ff
		dc.w	-16,50,10
		dc.w	32,50,11

.xtable		dc.w	$70,$110,$30,$d0,$50,$90,$40,$a0,$120
		dc.w	$c0,$e0,$60,$100,$f0,$80,$b0,-1

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien141	;  Extra bit at the end (I hope) ...

		move.w	x(a6),d0		
		move.w	y(a6),d1
		move.w	#16,d2
		move.w	#16,d3
		jsr	alwithzool		
		beq	offandend

		cmp.b	#32,fr+1(a6)
		bne	.noextra
		add.l	#150,timeleft
		jsr	freeplease
		bra	nextalien
.noextra
		cmp.b	#6,fr+1(a6)
		bne	.noextra3
		addq.l	#1,lives
		jsr	freeplease
		move.w	#$f,d5
		bra	.flashoff
.noextra3
		cmp.b	#5,fr+1(a6)
		bne	.nojumppower
	    	move.w	#$200,jumppower
		jsr	freeplease
		move.w	#$f0f,d5
		bra	.flashoff
.nojumppower
		cmp.b	#9,fr+1(a6)
		bne	.nosmart
		move.b	#5,smartnow
		jsr	freeplease
		move.w	#$fff,d5
		bra	.flashoff
.nosmart	
		cmp.b	#7,fr+1(a6)
		bne	.notsplit
	 	move.w	#800,splitpower
		lea	zoolmems,a0
		moveq	#40,d0
.moreset	move.l	zoolwx,(a0)+
		move.w	zooll,(a0)+
		move.b	zoold,(a0)
		addq.w	#2,a0
		dbra	d0,.moreset
 		jsr	freeplease
		move.w	#$f0,d5
		bra	.flashoff
.notsplit
	    	move.w	#$100,shield
		clr.w	noblink
 		jsr	freeplease
		move.w	#$f00,d5

.flashoff	move.b	#20,fadeback
		lea	gamecols+2,a0
		move.w	#15-1,d0
.moreflash	move.w	d5,(a0)+
		addq.w	#2,a0
		dbra	d0,.moreflash
		moveq	#6,d0
		jsr	playsample

		bra	nextalien

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien142	; Play the tune ...

		addq.w	#2,any+2(a6)
		move.w	any+2(a6),d0
		lea	.tunedat,a0
		tst.w	(a0,d0.w)
		bne	.tok
    		jsr	freesprite
		jmp	nextalien
.tok		move.w	(a0,d0.w),sam5+4
		moveq	#5,d0
		jsr	playsample
.nopl		bra	nextalien

.tunedat	dc.w	$180,$170,$160,$150,$140,$130,$120,$110,$100,$f0,$e0,$d0,$c0,$b0,$a0,$90,$80,$0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

alien143	; Bonus level stuff ...

		tst.b	donebonus
		bne	nextalien

		move.w	x(a6),d0		
		move.w	y(a6),d1
		move.w	#32,d2
		move.w	#32,d3
		jsr	alwithzool		
		beq	offandend
		move.b	worldno+3,tw
		move.b	areano+3,ta
		move.b	#6,worldno+3			Set Bonus place ...
		move.b	any(a6),areano+3
		move.l	any+2(a6),restarthereb		Set restart ...
		move.l	any+6(a6),restarthereb+4
		move.l	#$01100090,restarthereb+8
		move.l	skypl,restarthereb+12
		move.l	collected,collectedst

		move.l	#bye,inaframe+2

		st.b	fromtitle
	 	move.l	#blankcop,$dff080
		clr.l	musicplace
		move.w	#$f,$dff096
		clr.l	ch1timer
		clr.l	forcesample1
		jsr	synchup
		jmp	loadnextlevel

collectedst	dc.l	0
tw		dc.b	0
ta		dc.b	0
restarthereb	dc.l	0,0,0,0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

BLOCKMOVEMENT	; Block movement ...

		; any+14 = place.
		; any+18 = counter.
		; any+20 = xmom.
		; any+24 = ymom.
		; any+28 = x.
		; any+32 = y.

		tst.b	any+18(a6)
		beq	.setnext

		subq.b	#1,any+18(a6)		Move.
.next		move.l	any+20(a6),d0
		sub.l	d0,any+28(a6)
		move.w	any+28(a6),x(a6)
		move.l	any+24(a6),d0
		sub.l	d0,any+32(a6)
		move.w	any+32(a6),y(a6)
		rts

.setnext	move.l	any+14(a6),a0

		moveq	#0,d0			X.
		move.w	(a0),d0
		sub.w	4(a0),d0
		swap	d0
		asr.l	#4,d0
		move.l	d0,any+20(a6)
		clr.l	any+28(a6)
		move.w	x(a6),any+28(a6)

		moveq	#0,d1			Y.
		move.w	2(a0),d1
		sub.w	6(a0),d1
		swap	d1
		asr.l	#4,d1
		move.l	d1,any+24(a6)
		clr.l	any+32(a6)
		move.w	y(a6),any+32(a6)

		move.b	#15,any+18(a6)

		addq.l	#4,any+14(a6)

		jmp	.next

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

FIREBULLET	; Make the current alien in a6 fire a directional
		; bullet (with speed control too !!!).


		clr.l	lastbullet

		move.w	scrwx,d0
		move.w	x(a6),d1
		cmp.w	d1,d0
		bhi	bye
		add.w	#320,d0
 		bmi	okaybul
		cmp.w	d1,d0
		bls	bye
okaybul
		clr.l	lastbullet

		move.w	#$e0,sam3+4	
		move.w	#3,d0
		jsr	playsample

		move.l	a6,a4
		cmp.b	#135,t(a4)
		bne	.mmm
		jsr	getsprite
		bra	.mmmn
.mmm		jsr	getlowersprite
.mmmn		cmp.l	#$ffffffff,a6
		beq	.end

		move.l	a6,lastbullet
		
		move.l	#normbulani,any+20(a6)

		move.l	#cells,blbset(a6)
		move.b	#11,fr+1(a6)
		move.l	x(a4),x(a6)
		move.b	#99,t(a6)

		moveq	#0,d0
		moveq	#0,d1
		move.w	zoolwx,d0		Homeing bullet.
		add.w	#80,d0			16
		move.w	zoolwy,d1
		add.w	#32,d1
		sub.w	x(a6),d0
		sub.w	y(a6),d1

		move.w	d0,d2			Add x and y diffs to
		move.w	d1,d3			calculate rough distance.
		btst	#15,d2			Make d2 positive.
		beq	.pos1
		neg.w	d2
.pos1		btst	#15,d3			Make d3 positive.
		beq	.pos2
		neg.w	d3
.pos2		add.w	d3,d2			d2 = distance.
		
		swap	d0
		swap	d1

		moveq	#1,d3			Set start ASR value.
		move.w	#16,d4			Speed (1 is very slow).
.decreasespeed	cmp.w	d4,d2			Reached correct speed ?
		bmi	.thatsit
		asl.w	#1,d4			Multipy speed by two.
		addq.b	#1,d3			Increase ASR value.
		jmp	.decreasespeed
.thatsit	asr.l	d3,d0			ASR speed values.
		asr.l	d3,d1

		move.l	d0,any(a6)		Set bullet values.
		move.l	d1,any+4(a6)
		clr.w	any+10(a6)
		clr.w	any+14(a6)
		move.w	x(a6),any+8(a6)
		move.w	y(a6),any+12(a6)
		move.w	#120,any+24(a6)

.end		move.l	a4,a6
		
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

ANIMATEAL	; a0 = animation table ...

		cmp.l	#0,a0
		beq	bye
		cmp.w	#$ffff,(a0)
		beq	.goto
		move.w	(a0),fr(a6)
		addq.w	#2,a0
		rts
.goto		move.l	2(a0),a0
		jmp	animateal

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

WALKABOUT	; Give -    a6 = all alien info stored here.
		;	    any	 = X dir and speed of alien (.l for fraction).
		;   	    any+4= Y dir and speed of alien (.l for fraction).
		;           d7.0 = can an alien fall off platforms ?
		;           d7.1 = home in on zool ?
		;           yoff = y offset to y(a6) for detection.
		;	    bmom = .l off mom when bounceing 0=nobounce
		; 				            -1=random

		; Returns - d6.0 = hut a wall.
		; 	    d6.1 = hut ground.
		
		moveq	#0,d6

		tst.b	any+8(a6)
		beq	.nopause
     		subq.b	#1,any+8(a6)
		rts
.nopause
		tst.w	any+4(a6)
		bne	.nothoming
		tst.w	any(a6)			Home in on zool stuff ...
		beq	.nothoming
		btst	#1,d7
		beq	.nothoming
		move.w	zoolwx,d0
		cmp.w	x(a6),d0
		bpl	.morethan
		tst.w	any(a6)
		bpl	.negit
		jmp	.nothoming
.morethan	tst.w	any(a6)
		bpl	.nothoming
.negit		neg.w	any(a6)
		move.b	pauseifb,any+8(a6)
.nothoming	
		move.w	x(a6),d0  		Check for wall.
		sub.w	#$10,d0
		tst.w	any(a6)
		bmi	.leftdir
     		add.w	#$20,d0
.leftdir	move.w	y(a6),d1
		add.w	yoff,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.nowall1
    		btst.b	#0,1(a1,d3.w)
		bne	.wall
.nowall1	btst	#0,d7  			Check for edge.
		bne	.nowall
		add.w	#$10,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.wall
		btst.b	#0,1(a1,d3.w)
		bne	.nowall
.wall		bset	#0,d6	     		Set hut wall.
		btst	#1,d7			Do nothing when wall is hut.
		bne	.skipxmove
		neg.w	any(a6)			Change dir (not homing).
		jmp	.skipxmove
.nowall		move.w	any(a6),d0	
		add.w	d0,x(a6)
.skipxmove
		move.w	any+4(a6),d0		Move in Y.
		add.w	d0,y(a6)
		move.w	x(a6),d0  		Check Ground ...
		move.w	y(a6),d1
		add.w	yoff,d1
		add.w	#$10,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.noground
		btst.b	#3,1(a1,d3.w)
		bne	.gg
    		btst.b	#0,1(a1,d3.w)
		beq	.noground
.gg		move.w	yoff,d0	     		We have hut the ground.
		add.w	d0,y(a6)		Snap to ground
	    	andi.w	#$fff0,y(a6)
		sub.w	d0,y(a6)
		bset	#1,d6
		tst.w	any+4(a6)		Set pause if just hut ground.
		beq	.nop
		move.b	pauseifb,any+8(a6)
.nop		clr.l	any+4(a6)
		tst.w	any(a6)			Set a dir if not got one.
		bne	.set
		move.w	#3,any(a6)
		move.w	zoolwx,d0
		cmp.w	x(a6),d0
		bpl	.set
		neg.w	any(a6)
.set		tst.l	bmom			In a bouncing mood ?
		beq	.nobounce
	 	cmp.l	#-1,bmom
		beq	.randomb
		move.l	bmom,any+4(a6)
		jmp	.ong
.randomb	jsr	random
		andi.w	#$3,d0
		sub.w	#$c,d0
		move.w	d0,any+4(a6)
		jmp	.ong
.nobounce	clr.l	any+4(a6)
		jmp	.ong
.noground	cmp.w	#$e,any+4(a6)  		Add to gravity.
		bpl	.terminalvel
		move.l	gravit,d0
		add.l	d0,any+4(a6)
.terminalvel	
.ong
		rts

gravit		dc.l	$e000
yoff		dc.w	0
bmom		dc.l	0
pauseifb	dc.b	0
		even


*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

COLLISION	; General collision handing all types...
		; scoid = score if dead.
		; sefid = start explosion frame if dead.
		; offitx = x offset.
		; sizex = x size. 
		; offity = y offset.
		; sizey	= y size.
		; detecm = %1 = Do anything with the bullet detec ?
		;          %1 = Do Zool with alien collision ?
		;          %1 = Reset snp(a6)? (1 = dont apear again).
		;          %1 = Kicking and punching ?
		;          %1 = Kill if on head ?
		;          %1 = Make bonuses if dead ?
		;          %1 = Make heart if dead ?
		;          %1 = Bullet detection ?

		; detecn = %1 = 
		;          %1 = 
		;          %1 = 
		;          %1 = 
		;          %1 = 
		;          %1 = 
		;          %1 = 
		;          %1 = Sliding tackle and spin ? 1 = yes ...

		btst.b	#0,detecm
		beq	.nobulldet
		move.w	x(a6),d0		Has a bullet hut this alien.
		move.w	y(a6),d1
		add.w	offitx,d0
		add.w	offity,d1
		move.w	sizex,d2
		move.w	sizey,d3
		jsr	bulhutal
		btst.b	#7,detecm
		beq	.nobulldet
		tst.b	d7
		bne	.ex
.nobulldet	move.w	x(a6),d0		Check alien and zool.
		move.w	y(a6),d1
		add.w	offitx,d0
		add.w	offity,d1
		move.w	sizex,d2
		move.w	sizey,d3
		jsr	alwithzool2		Zool on this alien ?
		bne	.hutalien
		tst.b	onhead
		beq	.noex
		st.b	mustup			Just bounce ...
		clr.b	ducking
		clr.b	punching
		clr.b	slideing
		st.b	jumping
		move.w	#2,d0
		jsr	playsample
		move.l	#-$50000,ymom
		move.b	#20,uptime
		btst.b	#3,detecm		Allowed to jump on head ?
		beq	.noex
;		jmp	.noex

.normhead	st.b	jun
		jmp	.ex
.noex	     	tst.b	d6			Zool close to this alien ?
		beq	.ok
		btst.b	#4,detecm		Allowed to kick and punch ?
		beq	.ok
		move.w	x(a6),d0		Zool punched alien ?
		move.w	y(a6),d1
		move.w	sizex,d2
		add.w	offitx,d2
		add.w	offitx,d2
		move.w	sizey,d3
		add.w	offity,d3
		add.w	offity,d3
		jsr	checkpunch
		bne	.ex	
 		jmp	.ok
.hutalien	btst.b	#0,detecn		Allowed tackle and spin ?
		beq	.hurtzool
		cmp.b	#3,slideing
		beq	.ex
		tst.b	spining
		beq	.hurtzool
.ex		move.b	#2,flal(a6)
		cmp.b	#$ff,htg(a6)
		beq	.ok
		tst.b	htg(a6)
		beq	.kill
		subq.b	#1,htg(a6)
		jmp	.ok
.kill		add.l	#$00080008,x(a6)
		btst.b	#1,detecm		Allowed to have a heart ?
		beq	.sk
		jsr	makeheart
.sk		moveq	#0,d4
		move.b	sefid,d4
		btst.b	#2,detecm		Allowed to have bonuses ?
		bne	.bc
		jsr	createexplo2
		jmp	.sk2
.bc		jsr	createexplo
		btst.b	#5,detecm
		bne	.sk2
		move.l	snp(a6),a0
		bclr.b	#7,(a0)
.sk2		jsr	freesprite
		move.l	jsp(a6),a6
		cmp.l	#0,a6
		bne	.sk2
		move.l	scoid,d0
		add.l	d0,score
		st.b	d7
		rts
.hurtzool	btst	#6,detecm		Able to hurt zool ?
		beq	.ok
		jsr	hurtzool
.ok		clr.b	d7
		rts

scoid 		dc.l	0
sefid 		dc.b	0
offitx		dc.w	0
sizex 		dc.w	0 
offity		dc.w	0
sizey		dc.w	0
detecn		dc.b	0
detecm		dc.b	0

		even

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

MAKEHEART	; Make heart appear.

		jsr	random
 		andi.b	#3,d0	
 		tst.b	d0
 		bne	bye

		move.l	a6,a2
		jsr	getlowersprite
		cmp.l	#-1,a6
		beq	.end
		move.l	x(a2),x(a6)
		sub.w	#4,x(a6)
		move.b	#23,t(a6)
		move.l	#lev1stuf,blbset(a6)
		move.b	#24,fr+1(a6)
      		move.w	#0,any(a6)

.end		move.l	a2,a6
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

HURTZOOL	; Push zool and take energy or initiate kill sequence.

		pushall
		jsr	.hurt
		pullall
		rts

.hurt		tst.b	zoolisdead
		bne	bye
		tst.b	bellm
		bne	bye
		tst.w	shield
		bne	bye
		tst.b	zoolhut
		bne	bye
		clr.b	spining
		clr.b	clinging
		clr.b	ducking
		clr.b	slideing
		tst.b	energylevel
		beq	byebyezool
	   	subq.b	#1,energylevel
		move.w	#30,shield
		cmp.b	#sln,worldno+3
		beq	bye
		move.w	#10,shield
		move.w	#-5,ymom
		move.w	#4,d0
		jsr	playsample
		tst.b	onrails
		beq	.noton
		clr.b	onrails
		move.w	#3,ymom 
		move.w	#2,d0
		jsr	playsample
.noton		st.b	jumping
		move.b	#25,zoolhut
		move.w	#$c000,ymom+2
		rts
byebyezool	move.b	#$7f,gamefadetmr
		move.l	a6,-(sp)
		lea.l	zdsp,a6
		move.w	zoolwx,x(a6)
		move.w	zoolwy,y(a6)
		add.l	#$001c001c,x(a6)
		clr.b	t(a6)
		move.l	#lev1stuf,blbset(a6)
		move.w	#$81,d4
		jsr	createexplo2
		move.w	#0,d0
		jsr	playsample
		move.w	#0,d0
		jsr	playsample
		move.w	#0,d0
		jsr	playsample
		move.w	#0,d0
		jsr	playsample
		move.l	(sp)+,a6
		clr.l	zoolx
		move.b	#50,zoolisdead
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CREATEEXPLO	; Create a five sprite explosion.

      		jsr	random
		andi.b	#$7f,d0
		cmp.b	#$78,d0
		bmi	createexplo2
bonusexplo	moveq	#17,d4
      		move.l	#lev1stuf,blbset(a6)
		move.b	#21,t(a6)

createexplo2	move.l	a6,a0

		cmp.b	#$80,d4
		beq	.no5

		jsr	getsprite
		cmp.l	#-1,a6
		beq	.end
		move.l	x(a0),x(a6)
		cmp.b	#$81,d4
		bne	.n1
   		move.l	#lev1stuf,blbset(a6)
		move.b	#15,fr+1(a6)
		jmp	.n2
.n1		move.l	blbset(a0),blbset(a6)
		move.b	d4,fr+1(a6)
		cmp.b	#68,d4
		beq	.n2
		addq.b	#1,d4
.n2		move.b	t(a0),any+10(a6)
		move.b	#6,t(a6)
		clr.b	any+30(a6)
		move.l	#$e000,any+12(a6)
		move.l	#$fffdfffa,any(a6)
		clr.w	any+4(a6)
		move.w	#$ffff,any+6(a6)
		cmp.b	#21,any+10(a6)
		bne	.no1
.ag		jsr	random
		andi.b	#7,d0
		cmp.b	#5,d0
		bpl	.ag
		add.b	#17,d0
		move.b	d0,fr+1(a6)
    		sub.w	#4,any+2(a6)
    		sub.w	#1,any(a6)
.no1
  		jsr	getsprite
		cmp.l	#-1,a6
		beq	.end
   		move.l	x(a0),x(a6)
		cmp.b	#$81,d4
		bne	.n3
		move.l	#lev1stuf,blbset(a6)
		move.b	#15,fr+1(a6)
		jmp	.n4
.n3		move.l	blbset(a0),blbset(a6)
		move.b	d4,fr+1(a6)
		cmp.b	#68,d4
		beq	.n4
		addq.b	#1,d4
.n4		move.b	t(a0),any+10(a6)
		move.b	#6,t(a6)
		clr.b	any+30(a6)
		move.l	#$e000,any+12(a6)
		move.l	#$0002fff8,any(a6)
		clr.w	any+4(a6)
		move.w	#$ffff,any+6(a6)
		cmp.b	#21,any+10(a6)
		bne	.no2
.ag2		jsr	random
		andi.b	#7,d0
		cmp.b	#5,d0
		bpl	.ag2
		add.b	#17,d0
		move.b	d0,fr+1(a6)
 
    		sub.w	#4,any+2(a6)
.no2
  		jsr	getsprite
		cmp.l	#-1,a6
		beq	.end
   		move.l	x(a0),x(a6)
		cmp.b	#$81,d4
		bne	.n5
		move.l	#lev1stuf,blbset(a6)
		move.b	#15,fr+1(a6)
		jmp	.n6
.n5		move.l	blbset(a0),blbset(a6)
		move.b	d4,fr+1(a6)
		cmp.b	#68,d4
		beq	.n6
		addq.b	#1,d4
.n6		move.b	t(a0),any+10(a6)
		move.b	#6,t(a6)
		clr.b	any+30(a6)
		move.l	#$e000,any+12(a6)
		move.l	#$0000fff7,any(a6)
		clr.w	any+4(a6)
		move.w	#$ffff,any+6(a6)
    		cmp.b	#21,any+10(a6)
		bne	.no3
.ag3		jsr	random
		andi.b	#7,d0
		cmp.b	#5,d0
		bpl	.ag3
		add.b	#17,d0
		move.b	d0,fr+1(a6)
     
		sub.w	#4,any+2(a6)
.no3
      		jsr	getsprite
		cmp.l	#-1,a6
		beq	.end
		move.l	x(a0),x(a6)
		cmp.b	#$81,d4
		bne	.n7
		move.l	#lev1stuf,blbset(a6)
		move.b	#15,fr+1(a6)
		jmp	.n8
.n7		move.l	blbset(a0),blbset(a6)
		move.b	d4,fr+1(a6)
		cmp.b	#68,d4
		beq	.n8
		addq.b	#1,d4
.n8		move.b	t(a0),any+10(a6)
		move.b	#6,t(a6)
		clr.b	any+30(a6)
		move.l	#$e000,any+12(a6)
		move.l	#$fffefff8,any(a6)
		clr.w	any+4(a6)
		move.w	#$ffff,any+6(a6)
    		cmp.b	#21,any+10(a6)
		bne	.no4
.ag4		jsr	random
		andi.b	#7,d0
		cmp.b	#5,d0
		bpl	.ag4
		add.b	#17,d0
		move.b	d0,fr+1(a6)
 
    		sub.w	#4,any+2(a6)
.no4
      		jsr	getsprite
		cmp.l	#-1,a6
		beq	.end
		move.l	x(a0),x(a6)
		cmp.b	#$81,d4
		bne	.n9
		move.l	#lev1stuf,blbset(a6)
		move.b	#15,fr+1(a6)
		jmp	.n10
.n9		move.l	blbset(a0),blbset(a6)
		move.b	d4,fr+1(a6)
		cmp.b	#68,d4
		beq	.n10
		addq.b	#1,d4
.n10		move.b	t(a0),any+10(a6)
		move.b	#6,t(a6)
		clr.b	any+30(a6)
		move.l	#$e000,any+12(a6)
		move.l	#$0003fffa,any(a6)
		clr.w	any+4(a6)
		move.w	#$ffff,any+6(a6)
    		cmp.b	#21,any+10(a6)
		bne	.no5
.ag5		jsr	random
		andi.b	#7,d0
		cmp.b	#5,d0
		bpl	.ag5
		add.b	#17,d0
		move.b	d0,fr+1(a6)
 
    		sub.w	#4,any+2(a6)
    		addq.w	#1,any(a6)
.no5
      		jsr	getsprite
		cmp.l	#-1,a6
		beq	.end
		move.l	x(a0),x(a6)
		sub.l	#$00080008,x(a6)
		move.l	#lev1stuf,blbset(a6)
		move.b	#33,fr+1(a6)
		clr.b	any(a6)
		move.b	#8,t(a6)
		move.w	#1,d0
		jsr	playsample

.end		move.l	a0,a6

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

NORANGECHECK	clr.b	onhead
		move.w	zoolwx,d4
		add.w	#8+18,d4
		move.w	zoolwy,d5
		add.w	#8,d5
		jmp	skiprange

ALWITHZOOL	; Check alien in a6 against zool.

		st.b	liftcheck
		jsr	alwithzool2
		clr.b	liftcheck
		tst.b	d7
		rts

ALWITHZOOL2	; Check alien in a6 against zool.
		; d0 = X place.
		; d1 = Y place.
		; d2 = X size.
		; d3 = Y size.	  
 	
		clr.b	onhead
		move.w	zoolwx,d4		Set Zool stuff.
		add.w	#8+18,d4
alwith2		move.w	zoolwy,d5
		add.w	#8,d5
		pushall				Hut it range ...
	 	add.w	d2,d0
		sub.w	#40,d4
		cmp.w	d0,d4
		bpl	out
		add.w	#40+40+32,d4
		sub.w	d2,d0
		cmp.w	d4,d0
		bpl	out
		add.w	d3,d1
		sub.w	#40,d5
		cmp.w	d1,d5
		bpl	out
		add.w	#40+40+26,d5
		sub.w	d3,d1
		cmp.w	d5,d1
		bpl	out
		st.b	closenow
		pullall

skiprange	tst.b	ducking
		beq	.noadd
      		add.w	#32,d5
.noadd	
		pushall				Hut it close ...
	 	add.w	d2,d0
		cmp.w	d0,d4
		bpl	out
		sub.w	d2,d0
		add.w	#26,d4
		cmp.w	d4,d0
		bpl	out
		add.w	d3,d1
		cmp.w	d1,d5
		bpl	out
		add.w	#48,d5		48 ******
		sub.w	d3,d1
		cmp.w	d5,d1
		bpl	out
		pullall

		tst.b	liftcheck		Can I do on head check ...
		bne	.get
		tst.b	spining
		bne	.get	

     		cmp.w	#2,ymom
		bmi	.get

;		add.w	#1,d5			On head... ? 20 ******
		cmp.w	d1,d5
		bpl	.get

		clr.b	d7	 		Kill al ( on head )...
		st.b	onhead
		add.w	#20,shield
		add.w	#25,noblink
		tst.b	d7
		rts

.get		st.b	d7
		tst.b	d7
		rts

out		pullall
		clr.b	liftcheck
		clr.b	d7
		tst.b	d7
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

BULHUTAL	; Check alien in a6 against zool.
		; d0 = X place.
		; d1 = Y place.
		; d2 = X size.
		; d3 = Y size.

		tst.b	smartnow
		beq	.nosmart
		st.b	d7
		move.b	smartnow,smartnow
		rts
.nosmart
		move.w	#4-1,d7
		lea	bulletsprs,a0
.more		tst.l	(a0)
		bne	.checkbul
.nextbul	addq.w	#4,a0
		dbra	d7,.more
		clr.b	d7
		rts

.checkbul	movem.l	d0-d3,-(sp)
		move.l	(a0),a1
		move.w	x(a1),d4		Set Zool stuff.
		move.w	y(a1),d5
		add.w	d2,d0			X check.
		cmp.w	d4,d0
		bmi	.no
		sub.w	d2,d0
		add.w	#18,d4
		cmp.w	d4,d0
		bpl	.no
		add.w	d3,d1			Y check.
		cmp.w	d5,d1
		bmi	.no
		sub.w	d3,d1
		add.w	#28+28,d5
		cmp.w	d5,d1
		bpl	.no
		movem.l	(sp)+,d0-d3
		tst.b	drumset			Is it a drum (bounce).
		beq	.no2
		st.b	any+10(a1)
		clr.b	d7
		moveq	#$a,d0
		jsr	playsample

		rts
.no2		st.b	any+16(a1)
		st.b	d7			Set.
		rts
.no		movem.l	(sp)+,d0-d3
		jmp	.nextbul

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CHECKPUNCH	move.w	#40,d4		25
		cmp.b	#72,zooll+1
		beq	.hitch
		cmp.b	#$38,zooll+1
		beq	.hitch
		cmp.b	#72+4,zooll+1
		beq	.hitch2
		cmp.b	#$3c,zooll+1
		bne	.nohit
.hitch2		move.w	#0,d4		-20
.hitch	  	add.w	zoolwx,d4		Set Zool stuff.
		jsr	alwith2
		beq	.nohit
		st.b	d0
		rts
.nohit		clr.b	d0
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

OFFSCREEN	; Check to see if an alien is off the screen.
		; Alien in a6 , d0 = result.

		move.w	scrwx,d0
		move.w	x(a6),d1
smadd		add.w	#100,d1
		cmp.w	d1,d0
		bhi	offit
		
		add.w	#320,d0
bigadd		sub.w	#200,d1
 		bmi	.okay
		cmp.w	d1,d0
		bls	offit
.okay
		cmp.b	#sln,worldno+3
		beq	freeit

		move.w	scrwy,d0
		move.w	y(a6),d1
smadd2		add.w	#150,d1
		cmp.w	d1,d0
		bhi	offit

		add.w	#256,d0
bigadd2		sub.w	#300,d1
		bpl	.noset
      		clr.l	d1
.noset
		cmp.w	d1,d0
		bls	offit
		
freeit		moveq	#0,d0
		rts
offit		st	d0
		rts	

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CREATECOORDS	; Make world coordinates.
	
		move.l	mapx,d0
smod7		divu	#mh,d0
		andi.l	#$ffff,d0
		lsl.w	#4,d0
		moveq	#0,d1
		move.b	scrx,d1
		eori.w	#$f,d1
		andi.w	#$f,d1
		add.w	d1,d0
		move.w	d0,scrwx
		add.w	zoolx,d0
		sub.w	#64+$40,d0
		move.w	d0,zoolwx
createycoords
		move.l	mapy,d2
		addq.w	#1,d2
		lsl.w	#4,d2
		move.w	scry,d1
		andi.w	#$f,d1
		eori.w	#$f,d1
		add.w	d1,d2
		move.w	d2,scrwy
		add.w	zooly,d2
		sub.w	#44,d2
		move.w	d2,zoolwy
		
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CHECKCOLLECT	; Over a collectable ...

		clr.b	dc
		clr.b	d6
		move.w	zoolwy,-(sp)
		jsr	.temp
		st.b	d6
		sub.w	#$10,zoolwy
		jsr	.temp
		sub.w	#$10,zoolwy
		jsr	.temp
		move.w	(sp)+,zoolwy
		rts

.temp		move.w	zoolwy,d1
		add.w	#$2d-8,d1
		move.w	zoolwx,d0
		add.w	#18+4,d0
		tst.b	zoold
		bne	.kmd
    		sub.w	#14,d0
.kmd		jsr	checkit
		tst.b	(a1,d1.w)
		bne	bye

		cmp.b	#sln2,worldno+3		World 3...
		bne	.n2
		cmp.b	#120,1(a0)		Hit a drum ?
		blt	.n3
		cmp.b	#123,1(a0)
		bgt	.n3
		st.b	mustup
		clr.b	ducking
		clr.b	punching
		clr.b	slideing
		st.b	jumping
		move.w	#2,d0
		jsr	playsample
		move.l	#-$48000,ymom
		move.b	#20,uptime
		jmp	detecclear
.n3    		tst.b	d6
		bne	.n2
		cmp.b	#200,1(a0)		On record deck ?
		blt	.slow
		cmp.b	#210,1(a0)
		bgt	.slow
		move.w	recordmom,d0
		add.w	d0,zoolx
		tst.w	xmom
		beq	.slow
		bpl	.negit
.posit		cmp.w	#3,recordmom
		beq	.n2
		add.l	#$1000,recordmom
		jmp	.n2
.negit		cmp.w	#-3,recordmom
		beq	.n2
		sub.l	#$1000,recordmom
		jmp	.n2
.slow		tst.w	recordmom
		beq	.n2
		bpl	.negit
		jmp	.posit
.n2


		move.l	detecplace,a1
		moveq	#0,d1
		move.b	(a0),d1			Find place in detec list.
		add.w	d1,d1
		btst.b	#2,1(a1,d1.w)
	 	beq	bye

		addq.l	#1,collected
		move.b	#$ff,(a0)		Clear mapdata block.
		add.l	#100,score
		moveq	#0,d0			Get pos ...
		move.w	zoolwx,d0
		add.w	#18+16+4,d0
		tst.b	zoold
		bne	.kmd3
    		sub.w	#14,d0
.kmd3	
		lsr.w	#4,d0
		move.l	scrollpos,d2
		lsr.l	#1,d2
		sub.l	d2,d0
		moveq	#0,d1
		move.w	zoolwy,d1
		add.w	#$2d-8,d1
		lsr.w	#4,d1
		sub.l	mapy,d1
		lea	blankblock,a6		Clear it from screen .
		lea.l	scrplanes,a0
		add.l	scrollpos,a0
		jsr	createblock
		move.l	wrkplanes,a0
		add.l	scrollpos,a0
		jsr	createblock
		move.l	displanes,a0
		add.l	scrollpos,a0
		jsr	createblock
		cmp.b	#10,hunds
		beq	bye
    		addq.b	#1,hunds
		jsr	getsprite
		cmp.l	#-1,a6
		beq	bye
		move.l	#lev1stuf,blbset(a6)
		move.b	#8,fr+1(a6)
		move.w	zoolwx,d0
		add.w	#18+16+4,d0
		move.w	d0,x(a6)
		move.w	zoolwy,d1
		add.w	#$2d-8,d1
		move.w	d1,y(a6)
		move.b	#20,any(a6)
		move.b	#2,t(a6)
		moveq	#5,d0
		jsr	playsample
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CHECKDOWN	; Check Hiros feet.

		clr.w	convx

		tst.b	jumping
		bne	.cont

		jsr	.cont
		bne	.set
		move.w	leftfoot,ftmem
		addq.w	#5,zooly
		addq.w	#5,zoolwy
		jsr	.cont
		bne	bye
		move.w	ftmem,leftfoot
		subq.w	#5,zooly
		subq.w	#5,zoolwy

		clr.b	d0
		rts
.set		move.w	leftfoot,ftmem
		subq.w	#5,zooly
		subq.w	#5,zoolwy
		jsr	.cont
		bne	bye
		move.w	ftmem,leftfoot
		addq.w	#5,zooly
		addq.w	#5,zoolwy
		st.b	d0
		rts

.cont		clr.b	onslope
		
		move.w	zoolwx,d0
		add.w	#12,d0
		move.w	zoolwy,d1
		add.w	#$2d,d1
		move.b	#2,dc
		jsr	checkit
	 	tst.b	onslope
		beq	.standard
	 	st.b	leftfoot
	 	st.b	rightfoot
		st.b	d0
		rts
.standard	move.w	d0,-(sp)
		move.b	d0,leftfoot

		add.w	#16,zoolwx
		move.w	zoolwx,d0
		add.w	#12,d0
		move.w	zoolwy,d1
		add.w	#$2d,d1
		move.b	#1,dc
		jsr	checkit	
		sub.w	#16,zoolwx
		move.b	d0,rightfoot

		move.w	(sp)+,d1
		add.b	d1,d0
		tst.b	d0
		rts

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*

CHECKLEFT	; Check Hiros left side.

		move.w	zoolwx,d0
		addq.w	#2,d0
checkside	tst.b	onslope			Do side detec if on slope.
		bne	detecclear
		move.w	d0,-(sp)
		move.w	zoolwy,d1
		tst.w	d1
		bpl	.ok
   		move.w	#$10,d1
.ok		add.w	#$1c+$10-3,d1
		jsr	checkit
		move.b	d0,bottomch
		
		move.w	(sp)+,d0
		move.w	zoolwy,d1
		add.w	#$10-4,d1
		jsr	checkit
		move.b	d0,topch
		add.b	bottomch,d0    
		tst.b	d0

		rts

*-------------------------------------------------------------------------*
*-------------------------------------------------------------------------*
		
CHECKRIGHT	; Check Hiros left side.

		move.w	zoolwx,d0
		add.w	#38,d0
		jmp	checkside

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
		
CHECKBLOCK	; General background check for zool.
		; d0 = x.
		; d1 = y.

		andi.l	#$ffff,d0
		andi.l	#$ffff,d1
		move.l	d0,d2		
		move.l	d1,d3	     
		lsr.l	#4,d2
smod8		mulu	#mh/2,d2
		lsl.l	#1,d2
		lsr.l	#4,d3
		add.l	d3,d2
		move.l	mapdata,a0
		cmp.b	#242,(a0)
		bne	.g
		moveq	#0,d2
		lea	blankblock,a0
.g		moveq	#0,d3
		move.b	(a0,d2.l),d3
		add.w	d3,d3
		move.l	detecplace,a1
		rts		

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CHECKIT		; General background check for zool.
		; d0 = x.
		; d1 = y.
		
		andi.l	#$ffff,d0
		andi.l	#$ffff,d1
		lsr.l	#4,d0
smod9		mulu	#mh/2,d0
		lsl.l	#1,d0
		lsr.l	#4,d1
		add.l	d1,d0
		move.l	mapdata,a0
		add.l	d0,a0
		
		tst.b	dc			Special lift detection.
		beq	.notlift
		tst.w	xlift
		beq	.notxlift
		st.b	d0
		rts
.notxlift	cmp.b	#$fd,(a0)
		bne	.notlift2
		tst.w	onlift
		beq	.notlift2
		move.w	onlift,zooly
		st.b	stonlift
		st.b	d0
		rts
.notlift 	cmp.b	#242,(a0)
		beq	detecclear   
.notlift2 
		moveq	#0,d1
		move.b	(a0),d1			Find place in detec list.
		add.w	d1,d1
		move.l	detecplace,a1
		
		tst.b	(a1,d1.w)		On a slope ?
		beq	noslope
		
		tst.b	dc			Testing down ?
		beq	detecclear
		cmp.b	#1,dc			Do only left foot on slopes. 
		bne	.doch
		move.b	leftfoot,d0
		clr.b	dc
		rts
.doch		clr.b	dc			On a slope stuff.
		move.w	(a1,d1.w),d2		Set correct height.
		move.w	zoolwx,d3
		add.w	#$c,d3
		andi.w	#$f,d3
		add.w	d3,d2
		move.l	detecplace,a2
		move.b	(a2,d2.w),d3
		move.w	zoolwy,d0
		subq.w	#3,d0
		andi.w	#$fff0,d0
		sub.w	scrwy,d0
		add.w	#44+3,d0
		add.w	d3,d0
		move.w	zooly,d3		Actually hut ground hut ?
		addq.w	#4,d3
 		cmp.w	d3,d0
  		bgt	.detecclear2
		move.w	d0,zooly		Set zools Y place.
		st.b	onslope
		st.b	d0
		cmp.l	#snowhr1-dett,d2	Snow stuff ?
		bmi	bye
		tst.b	almove			Snow stuff ...
		beq	.notalien
		move.w	(a1,d1.w),d2
		move.b	$13(a2,d2.w),alslopefr
		move.b	$11(a2,d2.w),almust
		move.b	$12(a2,d2.w),almust+1
		subq.b	#1,ss
		bpl	bye
		move.b	#10,ss
		moveq	#9,d0
		jsr	playsample
		rts
.notalien	tst.b	jumping			Just hut snow ?
		beq	.nojump
		clr.l	xmom
		move.b	#6,tilljump
.nojump		move.w	(a1,d1.w),d2		Set correct looks and x.
		move.b	$10(a2,d2.w),icing
		st.b	flatice
		move.b	$11(a2,d2.w),lrmust
		move.b	$12(a2,d2.w),lrmust+1
		rts
.detecclear2	clr.w	leftfoot
		st.b	onslope
		move.l	(sp)+,d0
		moveq.l	#0,d0
		rts

noslope		tst.b	dc
		beq	.noch

		tst.b	actualzool
		beq	.nobridge

		cmp.b	#sln3,worldno+3		Fruit + Veg world ?
		beq	.nocon2
		cmp.b	#sln5,worldno+3		Toys ?
		beq	.nocon2
       
		cmp.b	#sln6,worldno+3		Fair ?
		bne	.nocon3
		cmp.b	#226,(a0)
		beq	.foo
    		cmp.b	#227,(a0)
		bne	.nocon2
.foo		st.b	mustup			Just bounce ...
		clr.b	ducking
		clr.b	punching
		clr.b	slideing
		st.b	jumping
		move.l	#-$a0000,ymom
		move.w	#$a,d0
		jsr	playsample
		bra	.nocon2
.nocon3		cmp.b	#132,(a0)		Rolling block ?
		bne	.nocon1
		move.w	#-1,convx
		jmp	.animatethisbl
.nocon1		cmp.b	#sln1,worldno+3
		bne	.n1
		cmp.b	#64,(a0)
		bne	.n1
		move.w	#-1,convx
.n1		cmp.b	#110,(a0)
		bne	.nocon2
		move.w	#1,convx
.animatethisbl	pushall
		lea	firstroll,a0
		cmp.b	#1,dc
		beq	.ok
   		lea	secondroll,a0
.ok		tst.l	(a0)
		beq	.getone
       		move.l	(a0),a6
		jmp	.gotone
.getone		jsr	getsprite
		cmp.l	#-1,a6
		beq	.fin
		move.l	#lev1stuf,blbset(a6)
		move.b	#23,fr+1(a6)
		move.b	#36,t(a6)
		move.l	a0,any+2(a6)
		move.l	a6,(a0)
.gotone		move.w	zoolwx,d0		Find sprite position.
		add.w	#12+16,d0
		andi.w	#$fff0,d0
		move.w	zoolwy,d1
		add.w	#$2d+8+16,d1
		andi.w	#$fff0,d1
		subq.w	#1,d1
		move.w	d0,x(a6)
		move.w	d1,y(a6)
		move.b	#10,any(a6)
.fin		pullall
.nocon2
		tst.b	actualzool
		beq	.nobridge
		tst.w	ymom
		bmi	.nobridge
		btst.b	#5,1(a1,d1.w)
		beq	.nobridge
		pushall
		jsr	getsprite
		cmp.l	#-1,a6
		bne	.gotoneagain
		pullall
		jmp	.nobridge
.gotoneagain	move.b	#254,(a0)
		move.l	a0,any+10(a6)
		moveq	#0,d0
		move.w	zoolwx,d0
		add.w	#12+16,d0	
		lsr.w	#4,d0
		move.l	scrollpos,d2
		lsr.l	#1,d2
		sub.l	d2,d0
		moveq	#0,d1
		move.w	zoolwy,d1
		add.w	#$2d-16+16,d1		-12+16
		lsr.w	#4,d1
		sub.l	mapy,d1
		move.l	a6,-(sp)
		lea	blankblock,a6
		lea.l	scrplanes,a0
		add.l	scrollpos,a0
		jsr	createblock
		move.l	wrkplanes,a0
		add.l	scrollpos,a0
		jsr	createblock
		move.l	(sp)+,a6		Do falling bit ...
		move.w	zoolwx,d0
		add.w	#12+16,d0	
		andi.w	#$fff0,d0
		move.w	zoolwy,d1
		add.w	#$2d+16,d1		4+16
		andi.w	#$fff0,d1
		subq.w	#1,d1
		move.l	#lev1stuf,blbset(a6)
		move.b	#22,fr+1(a6)
		move.w	d0,x(a6)
		move.w	d1,y(a6)
		move.b	#35,t(a6)
		move.b	#8,any(a6)
		move.l	#$10000,any+2(a6)
		pullall
		st.b	d0
		rts
.nobridge
		btst.b	#3,1(a1,d1.w)
		bne	.sk

.noch		btst.b	#4,1(a1,d1.w)		On a spike.
		beq	.nothurtable
		tst.b	almove
		bne	.nothurtable
		jsr	hurtzool
		btst.b	#0,1(a1,d1.w)
		beq	detecclear
		st.b	d0
		rts
.nothurtable
		btst.b	#0,1(a1,d1.w)		Anything there ?
		beq	detecclear
   
.sk		btst.b	#2,1(a1,d1.w)		On ice ?
		beq	.noflatice
      		st.b	flatice
.noflatice
		tst.b	dc			Testing down ?
		beq	.notdownch

		cmp.b	#2,dc
		bne	.jc
		btst.b	#1,1(a1,d1.w)
		beq	.jc
		subq.w	#4,zoolwy
		move.w	zoolwx,d0
		add.w	#12,d0
		move.w	zoolwy,d1
		add.w	#$2d,d1
		move.b	#2,dc
		jmp	checkit		
.jc
		clr.b	dc

.snap16		tst.b	stonlift
		bne	.notdownch
		move.w	zoolwy,d0
		andi.w	#$fff0,d0		On the ground.
		sub.w	scrwy,d0
		add.w	#44+3,d0
		move.w	d0,zooly
.notdownch	st.b	d0
		rts
detecclear	moveq	#0,d0		
		clr.b	dc
		rts		
	
ss		dc.w	0
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

ANIMATEZOOL	tst.b	magic
		beq	nomagic
		move.b	#77,zooll+1
		tst.b	zoold
		bne	.nodir
		move.b	#78,zooll+1
.nodir		subq.b	#1,magic
		rts
nomagic

		tst.l	xmom
		bne	.res
     		tst.l	ymom
		beq	.nres
.res		clr.b	closeness
.nres
		tst.b	clinging
		beq	.noclingani
		subq.b	#1,cfc
		bpl	.noresc
		move.b	#4,cfc
		subq.w	#1,clingfr
		bpl	.noresc
		move.w	#20,clingfr
.noresc		lea	clingfrs,a0
		move.w	clingfr,d0
		move.b	(a0,d0.w),zooll+1
		tst.b	zoold
		bne	bye
		addq.b	#3,zooll+1
		rts
.noclingani	move.w	#30,clingfr

		tst.b	zoolhut
		beq	.skip
		move.w	#60,shield
		clr.w	noblink
		move.b	#24,zooll+1
		tst.b	zoold
		beq	bye
		addq.b	#3,zooll+1
     		rts
.skip
		tst.w	punching		Attack moves ...
		beq	notpunch
	 	subq.w	#1,punching
punchani	move.l	#punchfrs,a0
		move.w	punching,d0
		move.b	(a0,d0.w),zooll+1
		tst.b	zoold
		bne	bye
		addq.b	#4,zooll+1
		rts
notpunch
		tst.b	slideing	
		beq	noslideing
   	 	move.b	#$45,zooll+1
		cmp.b	#3,slideing
		bne	alld2
slid2		move.b	#$34,zooll+1
alld2		tst.b	zoold
		bne	bye
		sub.b	#3,zooll+1
		rts
noslideing

		tst.b	ducking			Ducking ...
		beq	noducked
	 	move.b	#69,zooll+1
		cmp.b	#3,ducking
		bne	alld
		move.b	#70,zooll+1
alld		tst.b	zoold
		bne	bye
		addq.b	#4,zooll+1
		rts
noducked
		tst.b	icing
		beq	.noincing
		subq.b	#1,slidesound
		bpl	.nss
		move.b	#$a,slidesound
		moveq	#9,d0
		jsr	playsample
.nss		move.b	icing,zooll+1
		tst.b	zoold
		bne	bye
		addq.b	#4,zooll+1
		rts
.noincing	clr.b	slidesound

		tst.b	jumping			Jumping animation.
		bne	.nosk

		tst.b	flatice
		beq	.noflatice
		tst.w	left
		beq	.trystance
     		tst.b	left
		beq	.lc
		tst.l	xmom
		bmi	.noflatice
		jmp	.trystance
.lc		tst.l	xmom
		bpl	.noflatice
.doit2		jmp	.trystance	
.noflatice	
	
		tst.b	flatice
		bne	.nosk
		tst.l	xmom			Skiding animation.
		beq	.nosk
		tst.b	skid
		beq	.nosk
		move.b	#23,zooll+1
		tst.b	zoold
		beq	.nodirch
		move.b	#22,zooll+1
.nodirch
		subq.b	#1,skidcnt
		bpl	bye
		tst.b	sks
		bne	.done
		moveq	#8,d0
		jsr	playsample
		st.b	sks
.done		move.b	#5,skidcnt
		jsr	getsprite
		cmp.l	#-1,a6
		beq	bye
		move.l	#lev1stuf,blbset(a6)
		move.b	#30,t(a6)
		move.l	#$06000000,any(a6)
		move.l	zoolwx,x(a6)
		add.w	#48+8,y(a6)
		add.w	#12,x(a6)
		move.b	#14,fr+1(a6)
		tst.b	zoold
		beq	bye
		st.b	any+1(a6)
		add.w	#26,x(a6)
		
		rts
.nosk
		tst.b	jumping			Determine zool dir.
		beq	.nojset
		tst.w	left
		beq	.no
		clr.b	zoold
		tst.b	left
		bne	.no
		st.b	zoold
		jmp	.no
.nojset		tst.l	xmom		
		beq	.no
		bpl	.yr
		clr.b	zoold
		jmp	.no
.yr		st.b	zoold
.no

		tst.b	jumping			Jumping animation.
		beq	.notinair
		tst.b	spining			Spining animation.
		beq	.nospining
       		subq.w	#1,spinfr
		bpl	.nores
      		move.w	#7,spinfr
.nores		move.w	spinfr,d0
     		lea	spinfrs2,a0
.skcd		move.b	(a0,d0.w),zooll+1
		rts
.nospining	move.b	#25,zooll+1		Normal jump animation.
		cmp.w	#1,ymom
		bpl	.d
		cmp.w	#-1,ymom
		bpl	.cd
		move.b	#26,zooll+1
		jmp	.cd
.d		move.b	#24,zooll+1
		tst.b	upjump			Falling ? (not jumping).
		beq	.cd
		cmp.w	#4,xmom
		beq	.change
       		cmp.w	#-4,xmom
		bne	.cd
.change		move.b	#24,zooll+1
.cd		tst.b	zoold
		beq	bye
		addq.b	#3,zooll+1
		rts
.notinair	
		tst.l	xmom			Standing animation.
		bne	.nostand

		cmp.w	#$ffff,leftfoot
		bne	.doit

.trystance	move.b	#$8,zooll+1
		tst.b	zoold
		bne	.dir
		move.b	#$13,zooll+1
.dir
		tst.b	closenow
		bne	.addit
		tst.b	closeness
		beq	bye
		subq.b	#1,closeness
		addq.b	#1,zooll+1
		rts
.addit		cmp.b	#4,closeness
		beq	.add2
	     	addq.b	#1,closeness
		addq.b	#1,zooll+1
		rts
.add2		addq.b	#2,zooll+1
		rts


.doit		moveq	#0,d0
		tst.b	leftfoot
		beq	.sk1
    		addq.b	#1,d0
.sk1		tst.b	rightfoot
		beq	.sk2
    		addq.b	#2,d0
.sk2
		tst.b	zoold
		bne	.norev
		addq.b	#4,d0
.norev		lea	standingfrs2,a0
		subq.b	#1,zwc
		bpl	.noreszw
		move.b	#4,zwc
		subq.w	#1,zoolwobble
		bpl	.noreszw
      		move.w	#20,zoolwobble
.noreszw	move.w	zoolwobble,d1
		lea	wobs,a1
		tst.b	(a1,d1.w)
		beq	.noswap
		lea	standingfrs,a0
.noswap		move.b	(a0,d0.w),zooll+1

		rts
.nostand	
		clr.w	zoolwobble

		subq.b	#1,walkc
		bpl	bye
      		move.w	xmom,d0
		tst.w	d0
		bpl	.noneg
      		neg.w	d0
.noneg		
		move.b	#7,walkc
		sub.b	d0,walkc
		subq.w	#1,zoolwf
		bpl	.nor
    		move.w	#7,zoolwf
.nor		move.w	zoolwf,d0		

		tst.l	xmom
		bpl	.right
		lea	walkleft,a0
		move.b	(a0,d0.w),zooll+1
		rts
.right		lea	walkright,a0
		move.b	(a0,d0.w),zooll+1
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

MOMSCROLL	; X routine ...

		tst.b	onrails
		bne	bye

		cmp.b	#sln,worldno+3
		bne	.normalmomscr

		tst.w	xhitmom
		beq	.nox
  		move.w	xhitmom,d0
		add.w	d0,zoolx
		tst.w	xhitmom
		bpl	.more
		add.l	#$8000,xhitmom
		jmp	.nox
.more		sub.l	#$8000,xhitmom
.nox
    		tst.w	yhitmom
		beq	.noy
  		move.w	yhitmom,d0
		add.w	d0,zooly
		tst.w	yhitmom
		bpl	.more2
		add.l	#$8000,yhitmom
		jmp	.noy
.more2		sub.l	#$8000,yhitmom
.noy
		move.w	zoolwx,d0		Current stuff ...
		add.w	#$1c+$10,d0
		move.w	zoolwy,d1
		add.w	#$1c,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.w)
		beq	.ok1
		move.l	#-$60000,xhitmom
		jsr	hurtzool
.ok1	
		move.w	zoolwx,d0		Current stuff ...
		add.w	#$1c-$10,d0
		move.w	zoolwy,d1
		add.w	#$1c,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.w)
		beq	.ok2	
		move.l	#$60000,xhitmom
		jsr	hurtzool
.ok2	
		clr.b	d7
		move.w	zoolwx,d0		Current stuff ...
		add.w	#$1c,d0
		move.w	zoolwy,d1
		add.w	#$1c+$10,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.w)
		beq	.ok3
		st.b	d7
.ok3	
		move.w	zoolwx,d0		Current stuff ...
		add.w	#$1c,d0
		move.w	zoolwy,d1
		add.w	#$1c-$10,d1
		jsr	checkblock
		btst.b	#0,1(a1,d3.w)
		beq	.ok4
		tst.b	d7
		bne	.cant
		move.l	#$60000,yhitmom
		jsr	hurtzool
		jmp	.cant
.ok4
     		tst.b	d7
		beq	.cant
		move.l	#-$60000,yhitmom
		jsr	hurtzool
.cant
		move.w	speed,d5
		move.b	dragfactor,d1
		sub.b	d1,d5
		cmp.b	#1,d5
		bpl	.speedok
		moveq	#1,d5
.speedok	tst.b	left
		beq	.nl2
		sub.w	d5,zoolx
.nl2		tst.b	right
		beq	.nr2
		add.w	d5,zoolx
.nr2		tst.b	up
		beq	.nu2
		sub.w	d5,zooly
.nu2		tst.b	down
		beq	.nd2
		add.w	d5,zooly
.nd2
		rts

.normalmomscr	
		tst.b	onrails			Hanging from bars.
		bne	bye

		tst.w	outofit
		beq	.nooutofit
		subq.w	#1,outofit
		bne	.noyet

		; Finished Zen stuff.

		move.l	#bye,inaframe+2
		lea	gamecols+2,a0
		lea	blackpal,a1
		move.w	#32-1,a4
		moveq	#$0,d6
		jsr	fadeacross
		lea	stacktop,sp
		jsr	resetvars
		jmp	startfromzen
.noyet      
		tst.l	up
		beq	.no
   		move.l	up,zenup
.no
		move.w	zoolwx,d0  		X check with background.
	 	add.w	#$00,d0
		tst.l	xooim
		bmi	.rightdir
		add.w	#$20,d0
.rightdir	move.w	zoolwy,d1
	 	add.w	#$08,d1
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.noswap
    		btst.b	#0,1(a1,d3.w)
		beq	.noswap
		tst.l	xooim
		bpl	.n2
		move.l	#$40000,xooim
		jmp	.noswap
.n2		move.l	#-$40000,xooim
.noswap
		move.w	zoolwy,d1  		Y check with background.
	 	add.w	#$00,d1
		tst.l	yooim
		bmi	.rightdiry
		add.w	#$20,d1
.rightdiry	move.w	zoolwx,d0
	 	add.w	#$08,d0
		jsr	checkblock
		tst.b	(a1,d3.w)
		bne	.noswapy
    		btst.b	#0,1(a1,d3.w)
		beq	.noswapy
		tst.l	yooim
		bpl	.n
		move.l	#$40000,yooim
		jmp	.noswapy
.n		move.l	#-$40000,yooim
.noswapy
		tst.b	zenright
		beq	.nl
		cmp.w	#$5,xooim
		beq	.doy
		add.l	#$2000,xooim
		jmp	.doy
.nl		tst.b	zenleft
		beq	.slox
		cmp.w	#-5,xooim
		beq	.doy
		sub.l	#$2000,xooim
		jmp	.doy
.slox		tst.l	xooim
		beq	.doy
		bmi	.less
    		sub.l	#$2000,xooim
		bpl	.doy
		jmp	.cldoy
.less		add.l	#$2000,xooim
		bmi	.doy
.cldoy		clr.l	xooim
.doy

		tst.b	zendown
		beq	.nu
		cmp.w	#$5,yooim
		beq	.end
		add.l	#$2000,yooim
		jmp	.end
.nu		tst.b	zenup
		beq	.sloy
		cmp.w	#-5,yooim
		beq	.end
		sub.l	#$2000,yooim
		jmp	.end
.sloy		tst.l	yooim
		beq	.end
		bmi	.yless
    		sub.l	#$2000,yooim
		bpl	.end
		jmp	.clend
.yless		add.l	#$2000,yooim
		bmi	.end
.clend		clr.l	yooim
.end
		move.w	xooim,d0
		add.w	d0,zoolx
		move.w	yooim,d1
		add.w	d1,zooly

		
		subq.b	#1,mindtrail
		bpl	bye
		move.b	#2,mindtrail

		jsr	getsprite
		cmp.l	#-1,a6
		beq	.end
		move.l	zoolwx,x(a6)
		add.l	#$00200010,x(a6)
		move.l	#lev1stuf,blbset(a6)
		move.b	#13,fr+1(a6)
		clr.b	any(a6)
		move.b	#32,t(a6)
		clr.b	any(a6)
		st.b	any+1(a6)
		jmp	nextalien
.notrail
     		rts
.nooutofit
		tst.b	magic
		bne	animatezool

		tst.b	clinging		Is zool on a wall.
		bne	animatezool

		tst.w	xlift
		bne	.nowall	
		move.w	xmom,d0
		add.w	convx,d0
		tst.w	d0			Wall detection.
		beq	.skchecks
		bpl	.rc
		clr.b	d7
	 	jsr	checkleft
		beq	.nowall
		jmp	.cling
.rc		st.b	d7
		jsr	checkright
		beq	.nowall
.cling 		clr.l	xmom	
		tst.b	jumping
		beq	.skchecks

		tst.b	zoolhut	 		If zool hut do cling.
		bne	.skchecks

		tst.b	topch			Both hands and feet on wall ?
		beq	.skchecks
		tst.b	bottomch
		beq	.skchecks

		move.b	d7,zoold		

		subq.w	#4,zoolwx
		move.w	zoolwx,d0
		andi.w	#$fff0,d0
		sub.w	scrwx,d0
		add.w	#64+$40,d0
		move.w	d0,zoolx

		tst.b	zoold
		beq	.add
		add.w	#10,zoolx
		jmp	.cont
.add		add.w	#13,zoolx
.cont		st.b	clinging
		clr.l	ymom
		jmp	animatezool

.nowall		move.w	xmom,d0
		add.w	convx,d0
		add.w	d0,zoolx
.skchecks		
		jsr	animatezool
		clr.b	skid

		tst.w	punching
		bne	bye
		tst.b	ducking
		bne	.zero

		tst.w	lrmust
		beq	.nomust
       		move.w	lrmust,left
.nomust
 
		tst.b	zoolhut
		beq	.nohut
      		subq.b	#1,zoolhut
		cmp.b	#19,zoolhut
		bmi	.slow
		move.w	#$8000,xmom+2   
		move.w	#4,xmom   
		tst.b	zoold
		beq	bye
		move.w	#-4,xmom
		rts
.nohut

		tst.b	down		Pull down to slide attack.
		beq	.noslide
		tst.b	icing
		bne	.noslide
      		tst.b	slideing
		bne	.already
		cmp.w	#3,xmom
		bpl	.already
		cmp.w	#-2,xmom
		bpl 	.noslide
.already	tst.l	xmom
		beq	.noslide
		tst.b	jumping
		bne	.noslide
		tst.b	ducking
		bne	.noslide
		cmp.b	#3,slideing
		beq	.nomoreadd
	  	addq.b	#1,slideing
.nomoreadd	tst.l	xmom
		bpl	.right4
		add.l	#$1200,xmom
		bmi	bye
		clr.b	slideing
		move.b	#3,ducking
	 	jmp	.zero
.right4		sub.l	#$1200,xmom
		bpl	bye
		clr.b	slideing
		move.b	#3,ducking
	 	jmp	.zero
.noslide	clr.b	slideing

		tst.w	left
		beq	.slow
   
		tst.b	left
		beq	.mr

.ml		tst.b	jumping			Move left...
		beq	.ong
    		cmp.w	#-5,xmom		In air.
		beq	bye
		sub.l	#$ff00,xmom
.ch		tst.b	intt
		beq	bye
		move.w	#-5,xmom
		rts
.ong   		tst.b	skidable
		beq	.noskid3
		tst.w	xmom			Skiding ?
		bmi	.noskid2
       		st.b	skid
		sub.l	#$4000,xmom		3000
		bra	.ch
.noskid2	clr.b	skidable
		clr.b	sks
.noskid3	cmp.w	#-1,xmom		On ground at first.
		bmi	.ok
		sub.l	#$4000,xmom		5000
		bra	.ch
.ok		cmp.w	#-5,xmom		On ground after a bit.
		beq	.sk
		sub.l	#$2000,xmom		2000
		bra	.ch
.sk		move.b	#1,skidable
		rts

.mr		tst.b	jumping			Move left...
		beq	.ong2
    		cmp.w	#5,xmom			In air.
		beq	bye
		add.l	#$ff00,xmom
.ch2		tst.b	intt
		beq	bye
		move.w	#5,xmom
		rts
.ong2		tst.b	skidable
		beq	.noskid5
		tst.w	xmom			Skiding ?
		bpl	.noskid
       		st.b	skid
		add.l	#$4000,xmom
		bra	.ch2
.noskid		clr.b	skidable
		clr.b	sks
.noskid5	cmp.w	#2,xmom			Fast accel.
		bpl	.ok33
		add.l	#$4000,xmom
		bra	.ch2
.ok33		cmp.w	#5,xmom			Slow accel.
		beq	.sk6
		add.l	#$2000,xmom		
		bra	.ch2
.sk6		move.b	#2,skidable
		rts

.slow		
		tst.b	flatice
		beq	.normslow
		tst.l	xmom
		bpl	.right3
		add.l	#$1000,xmom
		bpl	.zero
		rts
.right3		sub.l	#$1000,xmom
		bpl	bye
	 	jmp	.zero

.normslow	tst.l	xmom
		bpl	.right
		add.l	#$7000,xmom		4000
		bpl	.zero
		rts
.right		sub.l	#$7000,xmom
		bpl	bye
.zero		clr.l	xmom
		tst.b	jumping
		bne	bye
		cmp.w	#$ffff,leftfoot
		bne	bye
		tst.b	fire
		beq	bye
		tst.b	ducking
		beq	.standattack
		cmp.b	#3,ducking
		bne	bye
		move.w	#13,punching
		move.l	#footsweep,punchani+2
		rts
.standattack	move.w	#13,punching
		move.l	#punchfrs,punchani+2
		rts

******************************************************************************
*						                             *
*	                     SCROLLING ROUTINES	 		     	     *
*						                             *
******************************************************************************

SCROLLER	; Moves the screen so that zoolx and zooly goes
		; towards the middle ...

		tst.b	gamefadetmr
		bne	bye

		cmp.w	#124,zoolx
		bgt	.lxok
     		move.w	#124,zoolx
.lxok		cmp.w	#$19d,zoolx
		bmi	.rxok
     		move.w	#$19d,zoolx
.rxok
		cmp.b	#sln,worldno+3
		bne	.norm
		cmp.w	#$90,zooly
		beq	.noxx2
		bmi	.zl90w2
		moveq	#0,d0
		move.w	zooly,d0
		sub.w	#$90,d0
		lsl.l 	#5,d0
		lsl.l 	#7,d0
		clr.w	scrollfrac
		add.l	d0,scrollfrac
		move.w	scrollfrac,d0		
		jsr	scrolldown
		jmp	.noxx2
.zl90w2   	move.l	#$90,d0
		sub.w	zooly,d0
		lsl.l 	#6,d0
		lsl.l 	#7,d0
		clr.w	scrollfrac
		add.l	d0,scrollfrac
		move.w	scrollfrac,d0		
		jsr	scrollup
.noxx2		move.w	scrollspeed,d0

		cmp.l	#$98d0-(16*3*5),mapx
		bmi	.scok
		move.w	scrollspeed,d0
		add.w	d0,zoolx
		cmp.w	#$19d,zoolx
		bmi	bye
		st.b	endlevel
.scok		jmp	rightscroll
.norm
 		cmp.w	#$90,zooly
		beq	.noxx
		bmi	.zl90w
		moveq	#0,d0
		move.w	zooly,d0
		sub.w	#$90,d0
		lsl.l 	#7,d0
		lsl.l 	#7,d0
		clr.w	scrollfrac
		add.l	d0,scrollfrac
		move.w	scrollfrac,d0		
		jsr	scrolldown
		jmp	.noxx
.zl90w   	move.l	#$90,d0
		sub.w	zooly,d0
		lsl.l 	#7,d0
		lsl.l 	#7,d0
		clr.w	scrollfrac
		add.l	d0,scrollfrac
		move.w	scrollfrac,d0		
		jsr	scrollup
.noxx
		tst.b	stophorz
		bne	bye

 		move.l	#$110,d3
		tst.b	zoold
		beq	.ns
   		move.l	#$110,d3
.ns 		move.w	zoolx,d4
		cmp.w	d3,d4
		beq	bye3
		ble	.zl90x
		moveq	#0,d0
		move.w	zoolx,d0
		sub.w	d3,d0
		lsl.l 	#7,d0
		lsl.l 	#7,d0
		clr.w	scrollfrac2
		add.l	d0,scrollfrac2
		move.w	scrollfrac2,d0		
		jmp	scrollright
.zl90x   	move.l	d3,d0
		sub.w	zoolx,d0
		lsl.l 	#7,d0
		lsl.l 	#7,d0
		clr.w	scrollfrac2
		add.l	d0,scrollfrac2
		move.w	scrollfrac2,d0		
		jmp	scrollleft

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
		
SCROLLLEFT	; Scroll left d0 pixels ...

		tst.l	mapx			At edge of map.
		beq	bye3

		cmp.w	#$f,d0			Check speed limit.
		ble	.notover
   		moveq	#$f,d0
.notover	add.w	d0,zoolx		Move zool on screen.		
	
		move.w	d0,d1	  		Pixel scroll.
		lsl.w	#$4,d1
		add.w	d0,d1
		add.w	d1,tscrx
		tst.b	tscrx
		beq	.notover16
		sub.w	#$110,tscrx		Chr scroll.
		subq.l	#2,scrollpos
		move.l	mht,d0
		sub.l	d0,mapx	  
		jsr	placeleftedge
		jsr	createleftals
.notover16	move.b	tscrx+1,scrx		Set copper xoff.
		rts
		
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

SCROLLRIGHT	; Scroll right d0 pixels ...

arealenght	cmp.l	#$7bc0,mapx		At edge of map.
		bpl	bye3

		cmp.w	#$f,d0			Check speed limit.
		ble	.notover
   		moveq	#$f,d0
.notover	sub.w	d0,zoolx		Move zool on screen.
	
rightscroll	move.w	d0,d1			Pixel scroll.
		lsl.w	#$4,d1
		add.w	d0,d1
		sub.w	d1,tscrx
		tst.b	tscrx
		beq	.notover16
		jsr	placerightedge
		add.w	#$110,tscrx		Chr scroll.
		addq.l	#2,scrollpos		
		move.l	mht,d0
		add.l	d0,mapx			
		jsr	createrightals
.notover16	move.b	tscrx+1,scrx		Set copper xoff.
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

SCROLLDOWN	; Scroll down d0 pixels.

		cmp.w	#$f,d0			Check speed limit.
		ble	.notover
		moveq	#$f,d0
.notover	sub.w	d0,zooly		Move zool to middle

		sub.w	d0,skyy
		bpl	.skyyok
		add.w	#$30,skyy
		addq.l	#2,skypl
.skyyok
		move.w	scry,d1			Pixel scroll.
		sub.w	d0,scry
		bpl	.norap
      		add.w	#287,scry
.norap		andi.w	#$f,d1
		sub.w	d0,d1
		bpl	.notover16
		addq.l	#1,mapy			Chr scroll.
		jsr	placedownedge
		jsr	createdownals
.notover16
bye3		rts
							  		
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
		
SCROLLUP	; Scroll up d0 pixels.
		
		tst.l	mapy			At top of map ?
		beq	bye3

		cmp.w	#$f,d0			Check speed limit.
		ble	.notover
		moveq	#$f,d0
.notover	add.w	d0,zooly		Move zool to middle

		add.w	d0,skyy
		cmp.w	#$30,skyy
		bmi	.skyyok
		sub.w	#$30,skyy
		cmp.l	#sky,skypl
		beq	.skyyok
		cmp.l	#sky3,skypl
		beq	.skyyok
		subq.l	#2,skypl
.skyyok
		move.w	scry,d1			Pixel scroll.
		move.w	d1,d2
		add.w	d0,scry
		cmp.w	#288,scry
		bmi	.norapround
		sub.w	#288,scry
.norapround	andi.w	#$f,d1
		add.w	d0,d1
		btst	#4,d1
		beq	.notover16
		move.w	scry,-(sp)
		move.w	d2,scry
		jsr	placeupedge
		move.w	(sp)+,scry
		subq.l	#1,mapy			Chr scroll.
		jsr	createupals
.notover16	
		rts
	    
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PLACELEFTEDGE	; Set left edge.

		moveq	#0,d0			Set X off.
		moveq	#0,d1			Set Y off.
		jsr	calcblock
			
		move.l	maps,a6			a6 = map pos.
		add.w	#12,a6
		add.l	mapx,a6
		add.l	mapy,a6

		moveq	#18-1,d7

.nextblk	jsr	createblock2

		addq.w	#1,a6
		add.l	#44*4*16,a0
		add.w	#$10,d6
		cmp.w	#$120,d6
		bmi	.notover
		moveq	#0,d6
		sub.l	#44*4*16*18,a0
.notover
		dbra	d7,.nextblk

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PLACERIGHTEDGE	; Set left edge.

		moveq	#21,d0			Set X.
		moveq	#0,d1			Set Y.
		jsr	calcblock
			
		move.l	maps,a6			a6 = map pos.
		add.w	#12,a6
		add.l	mapx,a6
		add.l	mapy,a6
smods1		add.l	#mh*21,a6

		moveq	#18-1,d7

.nextblk	jsr	createblock2

		addq.w	#1,a6
		add.l	#44*4*16,a0
		add.w	#$10,d6
		cmp.w	#$120,d6
		bmi	.notover
		moveq	#0,d6
		sub.l	#44*4*16*18,a0
.notover
		dbra	d7,.nextblk

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PLACEDOWNEDGE	; Set down edge.

		move.l	#scrplanes,a0		a0 = screen address.
		add.l	scrollpos,a0
		moveq	#0,d0			Set X.
		moveq	#17,d1			Set Y.
		jsr	calcblock
			
		move.l	maps,a6			a6 = map pos.
		add.w	#12,a6
		add.l	mapx,a6
		add.l	mapy,a6
		add.w	#17,a6

		move.l	mht,d4
		moveq	#22-1,d7

.nextblk	jsr	createblock2

		addq.w	#2,a0
		add.w	d4,a6

		dbra	d7,.nextblk

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PLACEUPEDGE	; Set down edge.

		moveq	#0,d0			Set X.
		moveq	#0,d1			Set Y.
		jsr	calcblock
			
		move.l	maps,a6			a6 = map pos.
		add.w	#12,a6
		add.l	mapx,a6
		add.l	mapy,a6

		move.l	mht,d4
		moveq	#22-1,d7

.nextblk	jsr	createblock2

		addq.w	#2,a0
		add.w	d4,a6

		dbra	d7,.nextblk

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CALCBLOCK	; Calculate initial block stuff.

		move.l	#scrplanes,a0		a0 = screen address.
		add.l	scrollpos,a0
		move.w	#$11f,d2		Find Y position pixel pos.
		sub.w	scry,d2
		andi.l	#$ff0,d2
		lsl.w	#4,d1
		add.w	d1,d2
		cmp.w	#$120,d2
		bmi	.alright
		sub.w	#$120,d2
.alright	move.w	d2,d6
		mulu	#44*4,d2
		lsl.w	#1,d0			Find X word position.
		add.w	d0,d2
		add.l	d2,a0			Screen position.

		move.w	#$ffff,$dff044	        First word mask.
		move.w	#$ffff,$dff046	        Last word mask.
		move.w	#42,$dff066	        Destination mod.
		move.w	#00,$dff064	        Source A mod.
		move.w	#$9f0,$dff040	        Control reg 1.
		move.w	#$000,$dff042	        Control reg 2.

 		rts

CREATEBLOCK2	; Block blitter on all 3 screens.(Try straight copy after setup)
		; d0 = x chr .w
		; d1 = y chr .w
		; a6 = pointer to block number.
		; a0 = top left hand corner address (0,0).

		move.l	a0,-(sp)

		moveq	#0,d3			Calculate block's address.
		move.b	(a6),d3
		lsl.w	#7,d3
		add.l	#blocks+32,d3		d4 = the block's address.
		move.l	d3,$dff050	        Set source.
		move.l	a0,$dff054	        Set destination.
		bset.b	#6,$dff002
		move.w	#$8400,$dff096		Our DMA on.
		move.w	#$1001,$dff058	        Size and start.
		nop
		nop
		nop
		jsr	waitblit

		sub.l	#scrplanes,a0		Screen 2.
		add.l	wrkplanes,a0
		move.l	d3,$dff050	        Set source.
		move.l	a0,$dff054	        Set destination.
		bset.b	#6,$dff002
		move.w	#$8400,$dff096		Our DMA on.
		move.w	#$1001,$dff058	        Size and start.
		nop
		nop
		nop
		jsr	waitblit

		sub.l	wrkplanes,a0		Screen 3.
		add.l	displanes,a0
		move.l	d3,$dff050	        Set source.
		move.l	a0,$dff054	        Set destination.
		bset.b	#6,$dff002
		move.w	#$8400,$dff096		Our DMA on.
		move.w	#$1001,$dff058	        Size and start.
		nop
		nop
		nop
		jsr	waitblit

		move.l	(sp)+,a0

		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CREATEBLOCK	; Block blitter.
		; d0 = x chr .w
		; d1 = y chr .w
		; a6 = pointer to block number.
		; a0 = top left hand corner address (0,0).

		pushall

		subq.w	#2,a0

		move.w	#$11f,d2		Find Y position pixel pos.
		sub.w	scry,d2
		andi.l	#$ff0,d2
		lsl.w	#4,d1
		add.w	d1,d2
		cmp.w	#$120,d2
		bmi	.alright
		sub.w	#$120,d2
.alright	mulu	#44*4,d2
		
		lsl.w	#1,d0			Find X word position.
		add.w	d0,d2
		add.l	d2,a0			Screen position.

		moveq	#0,d4			Calculate block's address.
		move.b	(a6),d4
		lsl.w	#7,d4
		add.l	#blocks+32,d4		d4 = the block's address.
	
		move.w	#$ffff,$dff044	        First word mask.
		move.w	#$ffff,$dff046	        Last word mask.
		move.w	#42,$dff066	        Destination mod.
		move.w	#00,$dff064	        Source A mod.
		move.w	#$9f0,$dff040	        Control reg 1.
		move.w	#$000,$dff042	        Control reg 2.
		move.l	d4,$dff050	        Set source.
		move.l	a0,$dff054	        Set destination.
		bset.b	#6,$dff002
		move.w	#$8400,$dff096		Our DMA on.
		move.w	#$1001,$dff058	        Size and start.
		nop
		nop
		nop
		jsr	waitblit

		pullall
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

SETSCROLLSTUFF	; Set the copper to point to the correct area -
		; and set the x hardware offset.

		move.l	#lttsl1,lttsl
		move.l	#gttsl1,gttsl
		move.l	#ltbsl1,ltbsl
		move.l	#gtbsl1,gtbsl

		move.w	scry,d7
		
		sub.w	#$11,d7
		bpl	.skipadd
		add.w	#288,d7
.skipadd
		move.b	scrx,scrollx+3		Set the x offset.

		move.w	d7,d0
		add.w	#$2c,d0
		move.b	d0,copwait1
		move.w	d0,d6
		
		lea	scrollbpls,a0
		move.l	displanes,d1
		add.l	scrollpos,d1		Set plane 1 copper bpls.
		addq.w	#1,d1
		move.w	#287,d2
		sub.w	d7,d2
		andi.l	#$ffff,d2
		mulu	#44*4,d2
		add.l	d2,d1
		move.l	d1,d0
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		move.l	d1,d0			Set plane 2 copper bpls.
		add.l	#44,d0
		move.w	d0,14(a0)
		swap	d0
		move.w	d0,10(a0)
		move.l	d1,d0			Set plane 3 copper bpls.
		add.l	#44*2,d0
		move.w	d0,22(a0)
		swap	d0
		move.w	d0,18(a0)
		move.l	d1,d0			Set plane 4 copper bpls.
		add.l	#44*3,d0
		move.w	d0,30(a0)
		swap	d0
		move.w	d0,26(a0)
		move.l	d1,d0			Set plane 5 copper bpls.
		add.l	#44*4,d0
		move.w	d0,38(a0)
		swap	d0
		move.w	d0,34(a0)
		
		lea	scrollbpls2,a0
		move.l	displanes,d1
		add.l	scrollpos,d1		Set plane 1 copper bpls.
		addq.w	#1,d1
		move.l	d1,d0
		move.w	d0,2(a0)
		add.l	#44,d0
		move.w	d0,6(a0)
		add.l	#44,d0
		move.w	d0,10(a0)
		add.l	#44,d0
		move.w	d0,14(a0)
		add.l	#44,d0
		move.w	d0,18(a0)
		lea	scrollbpls,a2
		move.w	2(a2),22(a0)

		move.l	skypl,a0
		cmp.b	#sln,worldno+3
		bne	.n
   		move.w	#$900,skycol2+2
		jmp	.nn
.n		move.w	-2(a0),skycol2+2
.nn
		lea	clearcopmem,a2		Restore copper.
.moremem	cmp.l	#-1,(a2)
		beq	.nomemory
		move.l	(a2)+,a0		
		moveq	#8-1,d0
.moreremem	move.l	#$01800000,(a0)+
		dbra	d0,.moreremem
		jmp	.moremem
.nomemory
		move.l	#$ffddfffe,ffwait
		lea	clearcopmem,a2
		st.b	d5
		move.w	d6,d4

		move.w	skyy,d3
		lsr.w	#1,d3
		add.w	#$2c,d3
		move.l	skypl,a3
.next		move.w	d3,d6
		move.w	(a3)+,skycol+4
		tst.b	d5
		beq	.done     
		cmp.w	d4,d6
		ble	.done
		clr.b	d5
		move.w	d4,d6
		jsr	placebplline
		move.w	d3,d6
.done		jsr	placeskyline
		add.w	#$1a,d3
		cmp.w	#$12c,d3
		bmi	.next

 		tst.b	d5
		beq	.already
		move.w	d4,d6
		jsr	placebplline
.already
		move.l	#-1,(a2)

		rts	

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PLACESKYLINE	cmp.w	#$33,d6
		bpl	.notlttsl
		move.l	lttsl,a0
		add.l	#lttsl2-lttsl1,lttsl
		jmp	.putskycol
.notlttsl
		cmp.w	#$3b+4,d6
		bpl	.notbetweentop
		sub.w	#$33,d6
		mulu	#tl2-topscolines,d6
		add.l	#topscolines+136+16,d6
		move.l	d6,a0
		move.l	a0,(a2)
		addq.l	#4,(a2)+
		add.w	#8*4,a0
		jmp	skythink
.notbetweentop		
		cmp.w	#$100,d6		
		bpl	.notgttsl
		cmp.w	#$ff,d6
		bne	.nocancel
		move.l	#$01800000,ffwait
.nocancel	move.l	gttsl,a0
		add.l	#gttsl2-gttsl1,gttsl
		jmp	.putskycol
.notgttsl
		cmp.w	#$11d-4,d6
		bpl	.notltbsl
		move.l	ltbsl,a0
		add.l	#ltbsl2-ltbsl1,ltbsl
		jmp	.putskycol
.notltbsl
		cmp.w	#$125,d6
		bpl	.itsgtbsl
		sub.w	#$11d-4,d6
		mulu	#bl2-botscolines,d6
		add.l	#botscolines+96,d6
		move.l	d6,a0
		move.l	d6,a0
		move.l	a0,(a2)
		add.l	#$4,(a2)+
		add.w	#8*4,a0
		jmp	skythink

.itsgtbsl	move.l	gtbsl,a0
		add.l	#gtbsl2-gtbsl1,gtbsl
		jmp	.putskycol

.putskycol	move.l	a0,(a2)+
		add.w	#4*6,a0
		andi.b	#$ff,d6
		move.b	d6,(a0)+
		move.b	#$df,(a0)+
		move.w	#$fffe,(a0)+
skythink	cmp.b	#sln,worldno+3
		bne	skycol
		move.l	#$01920900,(a0)+
		rts
skycol		move.l	#$01920f00,(a0)+
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PLACEBPLLINE	cmp.w	#$33,d6
		bpl	.notlttsl
		move.l	lttsl,a0
		add.l	#lttsl2-lttsl1,lttsl
		jmp	copycopwait
.notlttsl
		cmp.w	#$3b+4,d6
		bpl	.notbetweentop
		sub.w	#$33,d6
		mulu	#tl2-topscolines,d6
		add.l	#topscolines+136+16,d6
		move.l	d6,a0
		jmp	copycopwait
.notbetweentop		
		cmp.w	#$100,d6		
		bpl	.notgttsl
		cmp.w	#$ff,d6
		bne	.nocancel
		move.l	#$01800000,ffwait
.nocancel	move.l	gttsl,a0
		add.l	#gttsl2-gttsl1,gttsl
		jmp	copycopwait
.notgttsl
		cmp.w	#$11d-4,d6
		bpl	.notltbsl
		move.l	ltbsl,a0
		add.l	#ltbsl2-ltbsl1,ltbsl
		jmp	copycopwait
.notltbsl
		cmp.w	#$125,d6
		bpl	.itsgtbsl
		sub.w	#$11d-4,d6
		mulu	#bl2-botscolines,d6
		add.l	#botscolines+96,d6
		move.l	d6,a0
		jmp	copycopwait
		
.itsgtbsl	move.l	gtbsl,a0
		add.l	#gtbsl2-gtbsl1,gtbsl
		jmp	copycopwait

copycopwait	move.l	a0,(a2)+

		move.l	a0,copplcmem
		move.w	#(28/4)-1,d0
		lea	copwait1,a1
.morecopy	move.l	(a1)+,(a0)+
		dbra	d0,.morecopy
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CREATESCREEN	; Create a full screen.
		; d7 = address to create to.

		moveq	#0,d0
		moveq	#0,d1

		move.w	#18-1,d1		Set y.
morey14		move.w	#22-1,d0		Set x.
morex14		move.l	d7,a0			a0 = screen address.
		add.l	scrollpos,a0
		move.l	d0,d2			Calculate map position.
smod14		mulu	#mh,d2
		sub.l	mht,d2
		add.l	d1,d2
		add.l	mapdata,d2
		add.l	mapx,d2
		add.l	mapy,d2
		move.l	d2,a6			a6 = map position.
		jsr	createblock		Blit the block on.
		dbra	 d0,morex14		Do all the x's.
		dbra	d1,morey14		Do all the y's.
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

CONVERTNO	; Simple BCD to string conversion

		lea	result,a0
		lea	tens,a1

.nextdigit	moveq	#0,d1
.moresubs	addq.w	#1,d1		
		sub.l	(a1),d0
		bpl	.moresubs

		subq.w	#1,d1

		move.b	d1,(a0)+
		add.l	(a1)+,d0

		cmp.l	#0,(a1)
		bne	.nextdigit

		rts
	
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
	
PRINTSCO	; 99999900078bc.

		move.l	collected,d0
		tst.l	d0
		beq	.sk
		addq.b	#1,d0
.sk		move.l	collectables,d1
		cmp.l	d1,d0
		bmi	.kmf
    		move.l	#99,d0
		bra	ok
.kmf		mulu	#100,d0
		divu	d1,d0
		andi.l	#$ff,d0
		cmp.w	#100,d0
		bmi	ok
		move.w	#99,d0
ok	;	cmp.l	lastpercent,d0
	 ;	beq	.nopercent
		move.l	d0,lastpercent
		jsr	convertno
		move.l	#botscolines,var1
		move.l	#percentoffs,var2
		move.w	#bl2-botscolines,var3
		move.w	#2-1,d7
		lea	result+4,a0
		jsr	printnumbers
.nopercent  
		move.l	lives,d0			Print lives.
		cmp.l	lastlives,d0
		beq	.nolives
		move.l	d0,lastlives
		jsr	convertno
		move.l	#botscolines,var1
		move.l	#livesoffs,var2
		move.w	#bl2-botscolines,var3
		move.w	#2-1,d7
		lea	result+4,a0
		jsr	printnumbers
.nolives   
		move.l	timeleft,d0
		cmp.l	timeleftlast,d0
		beq	.notime
		move.w	#3-1,d7
		move.l	timeleft,timeleftlast
		jsr	convertno
		move.l	#botscolines,var1
		move.l	#magicoffs+6,var2
		move.w	#bl2-botscolines,var3
		lea	result+3,a0
		jsr	printnumbers
.notime
		move.l	score,d0
		cmp.l	lastscore,d0
		beq	.nohi
		move.w	#6-1,d7
		move.l	score,lastscore
		move.l	score,d0
		jsr	convertno
		move.l	#topscolines,var1
		move.l	#tscoloffs,var2	     
		move.w	#tl2-topscolines,var3
		lea	result,a0
		jsr	printnumbers
		move.l	hiscore,d5	Test for high score.
		cmp.l	score,d5
		bpl	.nohi
		move.l	score,hiscore
		move.l	#hiscoffs,var2
		move.w	#6-1,d7
		lea	result,a0
		jsr	printnumbers
.nohi
		move.b	lastep,d0
		cmp.b	energylevel,d0
		beq	.noenergy
		move.b	energylevel,lastep
		move.l	#$02030203,result
		move.w	#$0203,result+4
		moveq	#0,d0
		move.b	energylevel,d0
		lsl.w	#1,d0
		lea	result,a0
		move.l	#$27272727,(a0,d0.w)
		move.w	#$2727,4(a0,d0.w)
		move.l	#topscolines,var1
		move.l	#baroffs,var2
		move.w	#tl2-topscolines,var3
		move.w	#6-1,d7
		lea	result,a0
		lea	scoline,a1
		move.l	#scoline,var4
		jsr	printnumbers
		move.l	#scoline+10,var4
.noenergy	
		rts

var1		dc.l	0
var2		dc.l	0
var3		dc.w	0
var4		dc.l	scoline+10
		
PRINTNUMBERS	move.l	var1,a5
		move.l	var2,a6
		move.w	d7,d6
		add.w	d6,d6
		add.w	(a6,d6.w),a5
		moveq	#0,d0
		move.b	(a0,d7.w),d0
		move.l	var4,a1
		add.w	d0,a1
		move.w	#12-1,d0
morelines37	move.b	(a1),(a5)
		move.b	480(a1),4(a5)
		move.w	var3,d1
		add.w	d1,a5
		add.w	#40,a1
		dbra	d0,morelines37
		dbra	d7,printnumbers
		rts

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

PLAYSAMPLE	; Play the sample.

		tst.l	musicplace
		bne	bye44

		movem.l	d0-d7/a0-a6,-(sp)
		jsr	.doit
		movem.l	(sp)+,d0-d7/a0-a6
		rts
		
.doit		move.l	d0,d1
		mulu	#10,d1
		lea	sam0,a4
		tst.l	(a4,d1.w)
		beq	byeit

		subq.b	#1,sampleno
		bpl	.nores
		move.b	#3,sampleno
.nores		
		tst.b	sampleno
		bne	.otherone
		move.w	#$8,$dff096
		move.b	#1,forcesample1
		move.l	d1,channel1data
		move.b	#4,ch1timer
		rts
.otherone	cmp.b	#1,sampleno
		bne	.noother2
		move.w	#$4,$dff096
		move.b	#1,forcesample2
		move.l	d1,channel2data
		move.b	#4,ch2timer
		rts
.noother2	cmp.b	#2,sampleno
		bne	.noother3
		move.w	#$2,$dff096
		move.b	#1,forcesample3
		move.l	d1,channel3data
		move.b	#4,ch3timer
		rts
.noother3	cmp.b	#3,sampleno
		bne	.noother4
		move.w	#$1,$dff096
		move.b	#1,forcesample4
		move.l	d1,channel4data
		move.b	#4,ch4timer
.noother4	rts

		
PLAYSAMPLE2	; Play the sample.

		tst.l	musicplace
		bne	bye44

		movem.l	d0-d7/a0-a6,-(sp)
		jsr	doit
		movem.l	(sp)+,d0-d7/a0-a6
byeit		rts
		
doit		move.l	d0,d1
		mulu	#10,d1
		lea	sam0,a4
		tst.l	(a4,d1.w)
		beq	byeit

		tst.b	ch1timer
		bne	notinchannel1
		move.l	(a4,d1.w),$dff0d0	Set start address.
		move.w	4(a4,d1.w),$dff0d6	Set lenght.
		move.w	6(a4,d1.w),$dff0d8	Set volume.
		move.b	8(a4,d1.w),ch1timer
		move.w	#%111111,$dff09e
		move.w	#$f000,$dff0d4		Set sample rate.
		move.w	#$8208,$dff096		Play it.
		rts
		
notinchannel1	tst.b	ch2timer
		bne	.notinchannel2
		move.l	(a4,d1.w),$dff0c0	Set start address.
		move.w	4(a4,d1.w),$dff0c6	Set lenght.
		move.w	6(a4,d1.w),$dff0c8	Set volume.
		move.b	8(a4,d1.w),ch2timer
		move.w	#%111111,$dff09e
		move.w	#$f000,$dff0c4		Set sample rate.
		move.w	#$8204,$dff096		Play it.
		rts
.notinchannel2	
		tst.b	ch3timer
		bne	.notinchannel3
		move.l	(a4,d1.w),$dff0b0	Set start address.
		move.w	4(a4,d1.w),$dff0b6	Set lenght.
		move.w	6(a4,d1.w),$dff0b8	Set volume.
		move.b	8(a4,d1.w),ch3timer
		move.w	#%111111,$dff09e
		move.w	#$f000,$dff0b4		Set sample rate.
		move.w	#$8202,$dff096		Play it.
		rts
.notinchannel3
		tst.b	ch4timer
		bne	.notinchannel4
		move.l	(a4,d1.w),$dff0a0	Set start address.
		move.w	4(a4,d1.w),$dff0a6	Set lenght.
		move.w	6(a4,d1.w),$dff0a8	Set volume.
		move.b	8(a4,d1.w),ch4timer
		move.w	#%111111,$dff09e
		move.w	#$f000,$dff0a4		Set sample rate.
		move.w	#$8201,$dff096		Play it.
.notinchannel4
		rts

		even
ch1timer	dc.b	0
ch2timer	dc.b	0
ch3timer	dc.b	0
ch4timer	dc.b	0
		even
forcesample1	dc.b	0
forcesample2	dc.b	0
forcesample3	dc.b	0
forcesample4	dc.b	0
		even
channel1data	dc.l	0
channel2data	dc.l	0
channel3data	dc.l	0
channel4data	dc.l	0
sampleno	dc.b	0
		even

PLAYSAMPLES

		tst.l	musicplace
		bne	bye44


		tst.b	ch1timer
		beq	.ch1clear
		subq.b	#1,ch1timer
		tst.b	ch1timer
		bne	.ch1clear
		move.w	#$8,$dff096
.ch1clear	
		tst.b	ch2timer
		beq	.ch2clear
		subq.b	#1,ch2timer
		tst.b	ch2timer
		bne	.ch2clear
		move.w	#$4,$dff096
.ch2clear
		tst.b	ch3timer
		beq	.ch3clear
		subq.b	#1,ch3timer
		tst.b	ch3timer
		bne	.ch3clear
		move.w	#$2,$dff096
.ch3clear
		tst.b	ch4timer
		beq	.ch4clear
		subq.b	#1,ch4timer
		tst.b	ch4timer
		bne	.ch4clear
		move.w	#$1,$dff096
.ch4clear
bye44		rts

FORCESAMPLES	; Play the sample.

		tst.l	musicplace
		bne	bye44


		lea	sam0,a4

		tst.b	forcesample1
		beq	.notinchannel1
		clr.b	forcesample1
		move.l	channel1data,d1
		move.l	(a4,d1.w),$dff0d0	Set start address.
		move.w	4(a4,d1.w),$dff0d6	Set lenght.
		move.w	6(a4,d1.w),$dff0d8	Set volume.
		move.b	8(a4,d1.w),ch1timer
		move.w	#%111111,$dff09e
		move.w	#$f000,$dff0d4		Set sample rate.
		move.w	#$8208,$dff096		Play it.
		rts
		
.notinchannel1	tst.b	forcesample2
		beq	notinchannel2
		clr.b	forcesample2
		move.l	channel2data,d1
		move.l	(a4,d1.w),$dff0c0	Set start address.
		move.w	4(a4,d1.w),$dff0c6	Set lenght.
		move.w	6(a4,d1.w),$dff0c8	Set volume.
		move.b	8(a4,d1.w),ch2timer
		move.w	#%111111,$dff09e
		move.w	#$f000,$dff0c4		Set sample rate.
		move.w	#$8204,$dff096		Play it.
		rts
notinchannel2	
		tst.b	forcesample3
		beq	notinchannel3
		clr.b	forcesample3
		move.l	channel3data,d1
		move.l	(a4,d1.w),$dff0b0	Set start address.
		move.w	4(a4,d1.w),$dff0b6	Set lenght.
		move.w	6(a4,d1.w),$dff0b8	Set volume.
		move.b	8(a4,d1.w),ch3timer
		move.w	#%111111,$dff09e
		move.w	#$f000,$dff0b4		Set sample rate.
		move.w	#$8202,$dff096		Play it.
		rts
notinchannel3
		tst.b	forcesample4
		beq	notinchannel4
		clr.b	forcesample4
		move.l	channel4data,d1
		move.l	(a4,d1.w),$dff0a0	Set start address.
		move.w	4(a4,d1.w),$dff0a6	Set lenght.
		move.w	6(a4,d1.w),$dff0a8	Set volume.
		move.b	8(a4,d1.w),ch4timer
 
		move.w	#%111111,$dff09e
		move.w	#$f000,$dff0a4		Set sample rate.
		move.w	#$8201,$dff096		Play it.
notinchannel4
		rts
		
*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*

COPYSCOLINES	; Copy scolines file into copper.

		lea	scoline,a0
		lea	topscolines,a1

		move.w	#12-1,d0
.more		
		move.w	(a0),10(a1)
		move.w	480(a0),10+4(a1)

		move.w	2(a0),62(a1)
		move.w	480+2(a0),62+4(a1)
		move.w	2(a0),74(a1)
		move.w	480+2(a0),74+4(a1)
		move.w	2(a0),90(a1)
		move.w	480+2(a0),90+4(a1)

		move.w	4(a0),106(a1)
		move.w	480+4(a0),106+4(a1)

		add.l	#40,a0
		add.l	#tl2-topscolines,a1
		dbra	d0,.more

		lea	scoline,a0
		lea	botscolines,a1

		move.w	#12-1,d0
.more2		
		move.w	18(a0),10(a1)
		move.w	480+18(a0),14(a1)
		move.w	6(a0),22(a1)
		move.w	480+6(a0),26(a1)

		move.w	22(a0),38(a1)
		move.w	480+22(a0),38+4(a1)
		move.w	16(a0),62(a1)
		move.w	480+16(a0),62+4(a1)

		move.w	8(a0),78(a1)
		move.w	480+8(a0),78+4(a1)
		move.w	10(a0),90(a1)
		move.w	480+10(a0),90+4(a1)

		add.l	#40,a0
		add.l	#bl2-botscolines,a1
		dbra	d0,.more2
		
		rts

***************************************************************************
*						                          *
*	                        COPPERS				     	  *
*						                          *
***************************************************************************

detecs		dc.l	detectcodes1,detectcodes2,detectcodes3,detectcodes4
		dc.l	detectcodes5,detectcodes6,detectcodes7
detecplace	dc.l	0
detectcodes1	include	d:\source\detec1.s
detectcodes2	include	d:\source\detec2.s
detectcodes3	include	d:\source\detec3.s
detectcodes4	include	d:\source\detec4.s
detectcodes5	include	d:\source\detec5.s
detectcodes6	include	d:\source\detec6.s
detectcodes7	include	d:\source\detec7.s
		even

xmom		dc.l	0
ymom		dc.l	0
scrxmom		dc.l	0
scrymom		dc.l	0
tscrx		dc.w	0

zoold		dc.b	1
zoolwf		dc.w	0

walkright	dc.b	7,6,5,4,3,2,1,0
walkleft	dc.b	18,17,16,15,14,13,12,11

walkc		dc.b	0
jumping		dc.b	0
uptime		dc.b	0
upheld		dc.b	0
skid		dc.b	0
skidable	dc.b	0
gamefadetmr	dc.b	0

standingfrs	dc.b	$8,$1f,$21,$8
		dc.b	$13,$25,$23,$13
standingfrs2	dc.b	$8,$20,$22,$8
		dc.b	$13,$26,$24,$13

zwc		dc.b	0
wobs		dc.b	0,0,1,1,1,0,0,0,0,1,1,0,0,1,1,1,1,0,0,1,1,1,0,0,0,0,0
zoolwobble	dc.w	0
spinfr		dc.w	0
spining		dc.b	0
spinfrs2	dc.b	$2b,$2b,$2c,$2c,$2d,$2d,$2e,$2e
upjump		dc.b	0
clinging	dc.b	0

topch		dc.b	0
bottomch	dc.b	0

cfc		dc.b	0
clingfr		dc.w	0
clingfrs	dc.b	$30,$30,$30,$30,$30,$30,$30,$30,$30,$30
		dc.b	$30,$30,$30,$30,$30,$30,$30,$30,$30,$30
		dc.b	$30,$30,$2f,$2f,$2f,$2f,$2f,$2f,$2f,$2f,$2f
dc		dc.b	0
onslope		dc.b	0
blankblock	dc.b	$ff

joinsp		dc.l	0
joinsp2		dc.l	0


bouncefrs	dc.b	-1,-1,-1,0,0,1,1,2,2,1,1,0,0,-1,-1,$80

gamepal		ds.w	15
sppl		ds.w	16

sprpal		dc.w	$0000,$0fff,$0888,$0000,$0666,$0a70,$0520,$0160
		dc.w	$0240,$0b00,$0193,$0222,$0000,$0e52,$0800,$0b80
sprpal2		dc.w	$0000,$0fff,$0888,$0000,$0222,$0444,$0530,$0666
		dc.w	$0750,$0800,$0b00,$0d20,$0888,$0fa1,$0111,$0fff
sillypal	dc.w	$0000,$0fff,$0888,$0000,$0666,$0330,$0770,$0660
		dc.w	$0880,$0990,$0aa0,$0bb0,$0cc0,$0dd0,$0ee0,$0ff0

almappl		dc.l	0
xmappl		dc.l	0
almapxpos	dc.l	0
xmapbit		dc.b	0

lttsl		dc.l	0
gttsl		dc.l	0
ltbsl		dc.l	0
gtbsl		dc.l	0

clearcopmem	dc.l	-1
		ds.l	20

zdsp		ds.w	20

skyy		dc.w	0
skypl		dc.l	sky+(10*2)
skyy1		dc.w	0
skypl1		dc.l	sky+(10*2)
skyplmem	dc.l	0

dnskc		dc.b	0
upskc		dc.b	0
icing		dc.b	0
flatice		dc.b	0
lrmust		dc.w	0
zoold1		dc.b	0
tilljump	dc.b	0
fire		dc.b	0
fireheld	dc.b	0
ducking		dc.b	0
punchfrs	dc.b	$36,$36,$37,$37,$37,$38,$38,$38,$38,$37,$37,$36,$36
footsweep	dc.b	70,70,70,71,71,71,71,71,72,72,72,72,71

lastssspr	dc.l	0

punching	dc.w	0
blink		dc.b	1

allsortani	dc.b	5,5,5,5,5,5,5,5,5,5,8,5,7,5,8
		dc.b	5,7,5,8,5,7,5,8,5,5
		dc.b	5,5,5,5,4,4,4,-1,4,-1,4,-1,4,-1,4,-1,4,-1
		dc.b	4,-1,4,-1,4,4,5,6,6,6,6,5,5,5,5
		dc.b	5,5,5,5,5
allsortmoms	dc.w	4,-5
		dc.w	5,-3
		dc.w	5,-5
		dc.w	4,-3
		dc.w	4,-6
		dc.w	5,-4
		dc.w	4,-2
		dc.w	5,-5
zoolhut		dc.b	0
ftmem		dc.w	0
closenow	dc.b	0
closeness	dc.b	0
almove		dc.b	0
alslopefr	dc.b	0
zoolylift	dc.w	0
xlift		dc.w	0
ylift		dc.w	0
stonlift	dc.b	0
justoff		dc.b	0

tscoloffs	dc.w	22,23,38,39,50,51
hiscoffs	dc.w	118,119,130,131,142,143
baroffs		dc.w	62,63,74,75,90,91
percentoffs	dc.w	10,11
livesoffs	dc.w	90,91
lastscore	dc.l	$ffffffff
score		dc.l	0
percent		dc.l	0
lastpercent	dc.l	0
hiscore		dc.l	$ffffffff
result		ds.b	6
		ds.b	8
tens		dc.l	100000
		dc.l	10000
		dc.l	1000
		dc.l	100
		dc.l	10
		dc.l	1
		dc.l	0

energylevel	dc.b	0
lastep		dc.b	0
magicpointer	dc.b	0
magicpmem	dc.b	-1
magiccols	dc.w	$fe0,$ba0,$320,0
		dc.w	$fff,$aaa,$444,0
		dc.w	$f0f,$a0a,$404,0
		dc.w	$0ff,$0aa,$044,0
magicoffs	dc.w	38,39,50,51,62,63
magicfrs	dc.b	12,14,10,8
magictfrs	dc.b	5,9,7,4
magicflashes	dc.w	$ff0,$fff,$f0f,$0ff
arrows		dc.l	50
bombs		dc.l	50
splits		dc.l	50
shields		dc.l	50

spacewait	dc.b	0
restarthere	dc.l	0,0,0,0
alittlespace	ds.l	18/4
onhead		dc.b	0
slideing	dc.b	0
scaler		dc.l	0
liftcheck	dc.b	0
mustup		dc.b	0
magic		dc.b	0

fadecnt		dc.b	0
heartxs		dc.w	5,5,4,3,2,1,0,0,-1,-2,-3,-4,-5,-5,-5,-5,-4,-3,-2,-1,0,0,1,2,3,4,5,5
hxe
eggyys		dc.w	0,3,2,1,-1,-2,-3,0	
ee
canspin		dc.b	0
spinflash	dc.b	0

controlmode	dc.b	0
jun		dc.b	0
almust		dc.w	0
flfrs		dc.b	13,14,15,14
mapsend		dc.w	0
chainset	dc.b	0
chain1		dc.b	0
chainspr	dc.l	0
zenup		dc.b	0		mbe
zendown		dc.b	0
zenleft		dc.b	0
zenright	dc.b	0
noblink		dc.w	0

roundani	dc.b	33,33,34,34
		dc.b	34,34,35,35
		dc.b	35,35,36,36
		dc.b	36,36,29,29
		dc.b	29,29,30,30
		dc.b	30,30,31,31
		dc.b	31,31,32,32
		dc.b	32,32,33,33
	
roundtable	dc.l	18*$3200,0*$3200
		dc.l	18*$3200,2*$3200
		dc.l	18*$3200,4*$3200
		dc.l	17*$3200,6*$3200
		dc.l	17*$3200,7*$3200
		dc.l	16*$3200,9*$3200
		dc.l	15*$3200,10*$3200
		dc.l	14*$3200,12*$3200
		dc.l	13*$3200,13*$3200
		dc.l	12*$3200,14*$3200
		dc.l	10*$3200,15*$3200
		dc.l	9*$3200,16*$3200
		dc.l	7*$3200,17*$3200
		dc.l	5*$3200,17*$3200
		dc.l	3*$3200,18*$3200
		dc.l	1*$3200,18*$3200




	; All the movement tables for the shoot'em up ...

		; Tounge thing ...

toungemov	dc.w	-1,0
		dc.w	goto
		dc.l	toungemov


jmpindown	dc.w	0,2,goto
		dc.l	jmpindown

		; Green cell that splits up.

green1		dc.w	-2,0,-2,1,-2,2,-2,1,-2,0,-2,-1,-2,-2,-2,-3,-2,-2,-2,-1
		dc.w	goto
		dc.l	green1
green2a		dc.w	-1,-2
		dc.w	goto
		dc.l	green2a
green2b		dc.w	-1,2
		dc.w	goto
		dc.l	green2b
grani 		dc.w	8,8,6,7
granib		dc.w	3,3,4,4,5,5,4,4,$ffff
		dc.l	granib
grani2		dc.w	6,6,7,7,$ffff
		dc.l	grani2
grani3		dc.w	8,$ffff
		dc.l	grani3
clawani		dc.w	45,45,46,46,47,47,48,48,48,48,48,48,47,47,46,46,45,45,$ffff
		dc.l	grani3

greenbullani	dc.w	19,19,20,20,21,21,20,20,$ffff
		dc.l	greenbullani
normbulani	dc.w	11,$ffff
		dc.l	normbulani
sparkani	dc.w	26,27,28,27,$ffff
		dc.l	sparkani

		; MOVEMENTS ...

splinter	dc.w	-3,0,-2,1,-1,2,0,3,1,2,2,1,3,0,2,-1,1,-2,0,-3,-1,-2,-2,-1,goto
		dc.l	splinter
splint		dc.w	22,22,23,23,24,24,25,25,24,24,23,23,$ffff
		dc.l	splint
spackani	dc.w	41,41,42,42,$ffff
		dc.l	spackani

		; Normal red cell.

movrcell1	dc.w	0,0,0,1,0,2,0,3,0,3,0,2,0,1
		dc.w	0,0,0,-1,0,-2,0,-3,0,-3,0,-2,0,-1
		dc.w	goto
		dc.l	movrcell1
redani		dc.w	0,0,1,1,2,2,1,1,$ffff
		dc.l	redani

nervemov	dc.w	-1,0,-1,1,-1,2,-1,3,-1,4,-1,5,-1,6,-1,7,-1,6,-1,5,-1,4,-1,3,-1,2,-1,1
		dc.w	-1,0,-1,-1,-1,-2,-1,-3,-1,-4,-1,-5,-1,-6,-1,-7,-1,-6,-1,-5,-1,-4,-1,-3,-1,-2,-1,-1
		dc.w	goto
		dc.l	nervemov

		; Blob thing stuff ...

bloblet		dc.w	-7,0,-6,1,-5,2,-4,3,-3,4,-2,5,-1,6,0,7,1,6,2,5,3,4,4,3,5,2,6,1
		dc.w	7,0,6,-1,5,-2,4,-3,3,-4,2,-5,1,-6,0,-7,-1,-6,-2,-5,-3,-4,-4,-3,-5,-2,-6,-1
		dc.w	goto
		dc.l	bloblet
blobani		dc.w	10,10,9,9,$ffff
		dc.l	blobani

prop		dc.w	0

BLUECOP		dc.w	$180,$5
		dc.w	$100,$0
		dc.w	$ffff,$fffe

PROCOP		dc.w	$2801,$fffe		Wait a bit for mainline.
		dc.w	$0096,$0020		Sprite DMA on.
		dc.w	$0092,$0038		Data fetch start.
		dc.w	$0094,$00d0		Data fetch stop.
		dc.w	$008e,$3081		Window start.
		dc.w	$0090,$23c1		Window stop.		
		dc.w	$0108,$0000		Mods.
		dc.w	$010a,$0000		Mods.
		dc.w	$0102,$0000		X shifts.
		dc.w	$0104,$0064		Priorities.

		dc.w	$00e0,$0006		Set top bitplane pointers.
		dc.w	$00e2,0
		dc.w	$00e4,$0006	
		dc.w	$00e6,(256*40)
		dc.w	$00e8,$0006	
		dc.w	$00ea,(256*40)*2
		dc.w	$00ec,$0006	
		dc.w	$00ee,(256*40)*3
		dc.w	$00f0,$0006
		dc.w	$00f2,(256*40)*4
		
		dc.w	$0180,$5		Main game colours.
procols
topy		dc.w	$0182,$f0f
		dc.w	$0184,$0
		dc.w	$0186,$160
		dc.w	$0188,$b00
		dc.w	$018a,$195
		dc.w	$018c,$800
		dc.w	$018e,$b80
		dc.w	$0190,$0
		dc.w	$0192,$0
		dc.w	$0194,$0
		dc.w	$0196,$0
		dc.w	$0198,$0
		dc.w	$019a,$0
		dc.w	$019c,$0
		dc.w	$019e,$0
		dc.w	$01a0,$0
		dc.w	$01a2,$44
		dc.w	$01a4,$66
		dc.w	$01a6,$88
		dc.w	$01a8,$aa
		dc.w	$01aa,$cc
		dc.w	$01ac,$ee
		dc.w	$01ae,$ff
		dc.w	$01b0,$0
		dc.w	$01b2,$0
		dc.w	$01b4,$0
		dc.w	$01b6,$0
		dc.w	$01b8,$0
		dc.w	$01ba,$0
		dc.w	$01bc,$0
		dc.w	$01be,$0

		dc.w	$100,$5000
   	
		dc.w	$7201,$fffe
boty		dc.w	$0182,$f00

		dc.w	$ffff,$fffe

procols7	dc.w	$b00,$a08,$c06,$045,$004,$050,$070,$077,$606,$cc0,$c50,$d30

holeani		dc.w	0,0,0,0,0,53,0,53,0,53,0,53,0,53,0,53
		ds.w	20
		dc.w	$ffff
		dc.l	holeani
holeani2	dc.w	0,0,0,0,0,53,0,0,0,53,0,0,0,53,0,0,0,53,0,0,0,53,0,0,0,53
		ds.w	25
		dc.w	$ffff
		dc.l	holeani

allists		dc.l	alienlist1,alienlist2,alienlist3,alienlist4
		dc.l	alienlist5,alienlist6,alienlist7
alienlist	dc.l	0        

alienlist1	dc.l	0,a1,a2,a3,a4,a5,a6,a7,a8,a9,aa,ab,ac,ad
		dc.l	ae,af,a10,a11,a12,a13,a14,a15,a16,a17,a18
		dc.l	a19,a1a,a1b,a1c,a1d,a1e,a1f,a20,a21,a22,a23
		dc.l	a24,a25,a26,a27,a28,a29,a2a,a2b,a2c,a2d,a2e
		dc.l	a2f,a30,a31,a32,a33,a34,a35,a36,a37,a38
		dc.l	cole1,cole2,cole3,cole4,cole5,cole6,ae

alienlist2	dc.l	0,cole1,cole2,cole3,cole4,cole5,cole6,ae
		dc.l	a8,a9,aa,ab,ac,ad
		dc.l	ae,af,a10,a11,a12,a13,a14,a15,bonus0,a17,a18
		dc.l	a19,a1a,a1b,a1c,a1d,a1e,a1f,a20,a21,a22,a23
		dc.l	a24,a25,a26,a27,a28,a29,a2a,a2b,a2c,a2d,a2e
		dc.l	a2f,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39
		dc.l	a3a,a3b,a2e,a2f,a3e,a3f

bonus0		dc.w	0,0	   		Bonus level ... (16)
		dc.l	lev1stuf   		Blbset.
		dc.b	143,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,0
		dc.l	(200*($26-$a)),$a5-8		Any set.
		even


alienlist3	dc.l	0,b1,b2,b3,b4,b5,b6,b7,b8,b9,ba,bb,bc,bd2,be
		dc.l	bf,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b1a
		dc.l	b1b,b1c,b1d,b1e,b1f,b20,b21,b22,b23,b24
		dc.l	a10,a11,a12,a13,b29,b2a,b2b,b2c,a25	
		;	b25,b26,b27,b28                 b2d
		dc.l	cole1,cole2,cole3,cole4,cole5,cole6,ae2,af2,bonus1,tun4
		;       2e     2f    30   31     32    33   34  35    36    37

tun4		dc.w	0,0			Extra note tune.
		dc.l	fuck			Blbset.
		dc.b	67,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,0,0,0			Any set.
		dc.l	0,0,tune4,tune4
		even


bonus1		dc.w	0,0	   		Bonus level ... (16)
		dc.l	lev1stuf   		Blbset.
		dc.b	143,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	1,0
		dc.l	((16*6)*($83-$a)),$e-8	Any set.
		even



ae2		dc.w	0,-15			Polo (.
		dc.l	lev1stuf		Blbset.
		dc.b	15,29,0,$ff		Type,look and htg ht
		dc.l	$35			Link
		even
af2		dc.w	32,-15			Polo ).
		dc.l	lev1stuf		Blbset.
		dc.b	0,30,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even


alienlist4	dc.l	0,d1,d2,d3,d5,d4,d6,d7,d8,d9,da,db,adc,dd,de,df
		dc.l	d10,d11,d12,d13,d14,d15,d16,d17,a25,d19,d1a
		;   				        d18
		dc.l	cole1,cole2,cole3,cole4,cole5,cole6,ae3,af3
		dc.l	a10,a11,a12,a13
		;  	23  24   25 26

ae3		dc.w	0,-15			Polo (.
		dc.l	lev1stuf		Blbset.
		dc.b	15,29,0,$ff		Type,look and htg ht
		dc.l	$22			Link
		even
af3		dc.w	32,-15			Polo ).
		dc.l	lev1stuf		Blbset.
		dc.b	0,30,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even




cole1		dc.w	0,0			Shield.
		dc.l	lev1stuf		Blbset.
		dc.b	141,4,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	1	   		Any set.
		even
cole2		dc.w	0,0			Big jump.
		dc.l	lev1stuf		Blbset.
		dc.b	141,5,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	2	   		Any set.
		even
cole3		dc.w	0,0			1 up.
		dc.l	lev1stuf		Blbset.
		dc.b	141,6,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	3	   		Any set.
		even
cole4		dc.w	0,0			split.
		dc.l	lev1stuf		Blbset.
		dc.b	141,7,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	4	   		Any set.
		even
cole5		dc.w	0,0			Smart bomb.
		dc.l	lev1stuf		Blbset.
		dc.b	141,9,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	5	   		Any set.
		even
cole6		dc.w	0,0			Extra time.
		dc.l	lev1stuf		Blbset.
		dc.b	141,32,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	6	   		Any set.
		even

a1		dc.w	0,1			32/32 collectable.
		dc.l	lev1stuf		Blbset.
		dc.b	4,8,0,0			Type,look and htg ht
		dc.l	0			Link
a1any		dc.b	4			Any set.
		even
a2		dc.w	0,0			Cloud 1.
		dc.l	lev1stuf			Blbset.
		dc.b	4,0,0,0			Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0			Any set.
		even
a3		dc.w	0,0			Cloud 2.
		dc.l	lev1stuf			Blbset.
		dc.b	4,1,0,0			Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0			Any set.
		even
a4		dc.w	0,-28			Walker.
		dc.l	allsort			Blbset.
		dc.b	5,1,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,-1			Any set.
		dc.l	0,0
		even
a5		dc.w	0,0			Bumble.
		dc.l	bumble			Blbset.
		dc.b	7,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0			Any set.
		dc.b	0,30
		even
a6		dc.w	0,1			32/32 collectable.
		dc.l	lev1stuf		Blbset.
		dc.b	4,9,0,0			Type,look and htg ht
		dc.l	0			Link
		dc.b	4			Any set.
		even
a7		dc.w	0,-8			Sloper.
		dc.l	sloper			Blbset.
		dc.b	10,4,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0			Any set.
		even
a8		dc.w	0,0			Y lift.
		dc.l	lifts			Blbset.
		dc.b	11,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	90,0			Any set.
		dc.w	2
		even
a9		dc.w	0,0			X lift.
		dc.l	lifts			Blbset.
		dc.b	28,1,0,$ff		Type,look and htg ht
		dc.l	$a			Link
		dc.b	50,0			Any set.
		dc.w	-2,0,0,0,0
		even
aa		dc.w	0,0			X lift part 2.
		dc.l	lifts			Blbset.
		dc.b	0,2,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
ab		dc.w	0,-3			Greener.
		dc.l	greener			Blbset.
		dc.b	13,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0			Any set.
		even
ac		dc.w	-11,0			Wallslob >.
		dc.l	wallslob		Blbset.
		dc.b	14,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	40,20,0			Any set.
		even
ad		dc.w	-6,0			Wallslob <.
		dc.l	wallslob		Blbset.
		dc.b	14,2,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	40,20,0			Any set.
		even
ae		dc.w	0,-15			Polo (.
		dc.l	lev1stuf		Blbset.
		dc.b	15,29,0,$ff		Type,look and htg ht
		dc.l	$f			Link
		even
af		dc.w	32,-15			Polo ).
		dc.l	lev1stuf		Blbset.
		dc.b	0,30,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
a10		dc.w	9,3			Restart 1.
		dc.l	lev1stuf		Blbset.
		dc.b	16,31,0,$0		Type,look and htg ht
		dc.l	0			Link
		dc.l	$2800,$24		Any set.
		dc.w	$10c,$b8
		dc.w	0
		even
a11		dc.w	9,3			Restart 2.
		dc.l	lev1stuf		Blbset.
		dc.b	16,31,0,$0		Type,look and htg ht
		dc.l	0			Link
		dc.l	$4cc0,$2e		Any set.
		dc.w	$10c,$b8
		dc.w	0
		even
a12		dc.w	9,3			Restart 3.
		dc.l	lev1stuf		Blbset.
		dc.b	16,31,0,$0		Type,look and htg ht
		dc.l	0			Link
		dc.l	$5b40,$25		Any set.
		dc.w	$10c,$b8
		dc.w	0
		even
a13		dc.w	9,3			Restart 4.
		dc.l	lev1stuf		Blbset.
		dc.b	16,31,0,$0		Type,look and htg ht
		dc.l	0			Link
		dc.l	$7640,$7		Any set.
		dc.w	$10c,$b8
		dc.w	0
		even
a14		dc.w	0,0			Hole L.
		dc.l	holes			Blbset.
		dc.b	17,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,248,249,137,138	Any set.
		even
a15		dc.w	0,0			Hole _.
		dc.l	holes			Blbset.
		dc.b	17,1,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,249,250,137,138	Any set.
		even
a16		dc.w	0,0			Bouncer thing.
		dc.l	bounce			Blbset.
		dc.b	18,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	20,0,0,0,40		Any set.
		even
a17		dc.w	0,5			Bouncer thing.
		dc.l	lev1stuf		Blbset.
		dc.b	4,0,0,0			Type,look and htg ht
		dc.l	0			Link
		dc.b	1,0,0,0,0,0		Any set.
		even
a18		dc.w	7,2			Cherry.
		dc.l	cherry			Blbset.
		dc.b	20,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
a19		dc.w	0,0			Lolly top.
		dc.l	chuptop			Blbset.
		dc.b	21,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
a1a		dc.w	32,0			Eolg1 (bee thing).
		dc.l	eolg1			Blbset.
		dc.b	24,9,0,$ff		Type,look and htg ht
		dc.l	$1b			Link
		ds.b	20			Any set.
		dc.b	2
		even
a1b		dc.w	32+32,0			Eolg1 (bee thing).
		dc.l	eolg1			Blbset.
		dc.b	0,10,0,$ff		Type,look and htg ht
		dc.l	$1c			Link
		dc.l	0			Any set.
		even
a1c		dc.w	32+48,49		Eolg1 (bee thing).
		dc.l	eolg1			Blbset.
		dc.b	0,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
a1d		dc.w	0,-4			Binary.
		dc.l	binary			Blbset.
		dc.b	25,0,0,$ff		Type,look and htg ht
		dc.l	$1e			Link
		dc.b	0,1,0,1,0		Any set.
		even
a1e		dc.w	64,-4			Binary.
		dc.l	binary			Blbset.
		dc.b	0,0,0,$ff		Type,look and htg ht
		dc.l	$1f			Link
		dc.b	0			Any set.
		even
a1f		dc.w	128,-4			Binary.
		dc.l	binary			Blbset.
		dc.b	0,0,0,$ff		Type,look and htg ht
		dc.l	$20			Link
		dc.b	0			Any set.
		even
a20		dc.w	128+64,-4		Binary.
		dc.l	binary			Blbset.
		dc.b	0,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	0			Any set.
		even
a21		
a22		dc.w	0,-102+16		Drill 1.
		dc.l	drills			Blbset.
		dc.b	27,0,0,102		Type,look and htg ht
		dc.l	$23			Link
		dc.l	0			Any set.
		even
a23		dc.w	-8,6			Drill dust.
		dc.l	drills			Blbset.
		dc.b	0,2,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
a24		dc.w	0,8			Wood platform.
		dc.l	woodplat		Blbset.
		dc.b	28,0,0,$ff		Type,look and htg ht
		dc.l	$28			Link
		dc.b	50,0			Any set.
		dc.w	-2,0,0,0,0
		even
a25		dc.w	0,0			Collect to end.
		dc.l	lev1stuf		Blbset.
		dc.b	29,26,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
a26		dc.w	0,0			Hole _.
		dc.l	holes			Blbset.
		dc.b	17,2,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,249,251,137,138	Any set.
		even
a27		dc.w	-8,-20			Saw thing.
		dc.l	saw			Blbset.
		dc.b	37,0,0,36		Type,look and htg ht
		dc.l	0			Link
		dc.b	2,1,0,2			Any set.
		dc.l	0
		even
a28		dc.w	32,8			Wood platform.
		dc.l	woodplat		Blbset.
		dc.b	0,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
a29		dc.w	0,0			Rolling ball.
		dc.l	balls			Blbset.
		dc.b	38,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0,0,0,0,0		Any set.
		even
a2a		dc.w	0,-32			Rolling ball.
		dc.l	balls			Blbset.
		dc.b	38,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,-$50000,0,0,0,0,0,0	Any set.
		even
a2b		dc.w	32,-8			Chainsaw.
		dc.l	chainsaw		Blbset.
		dc.b	39,5,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	20,0			Any set.
		dc.l	0
		even
a2c		dc.w	0,0			Acme.
		dc.l	acme			Blbset.
		dc.b	41,0,0,110		Type,look and htg ht
		dc.l	$2d			Link
		dc.l	0,0			Any set.
		even
a2d		dc.w	32,0			Acme.
		dc.l	acme			Blbset.
		dc.b	0,1,0,110		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0			Any set.
		even
a2e		dc.w	-7,-10			Switch.
		dc.l	switch2			Blbset.
		dc.b	12,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.w	$50,-$60		Any set.
		dc.l	0
		even
a2f		dc.w	-6,-48			Mine.
		dc.l	switch			Blbset.
		dc.b	42,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0			Any set.
		dc.w	0,0,0,0,0,0,0,0,0,0
		even
a30		dc.w	0,-5			Chris.
		dc.l	chris			Blbset.
		dc.b	43,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0			Any set.
		even
a31		dc.w	0,0			Chris.
		dc.l	drills			Blbset.
		dc.b	44,12,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	3,0			Any set.
		dc.l	0,0,0
		even
a32		dc.w	0,0			Chris.
		dc.l	drills			Blbset.
		dc.b	44,12,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	3,1			Any set.
		dc.l	0,0,0
		even
a33		dc.w	-8,-23			Big flame.
		dc.l	flames			Blbset.
		dc.b	45,13,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,0	   		Any set.
		even
a34		dc.w	0,0			Oil generator.
		dc.l	flames			Blbset.
		dc.b	47,12,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	5,0			Any set.
		dc.w	-8
		even
a35		dc.w	0,0			Woodworm.
		dc.l	croc			Blbset.
		dc.b	49,2,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	40,0			Any set.
		dc.l	0,0,0
		even
a36		dc.w	0,-20+8			Croc.
		dc.l	croc			Blbset.
		dc.b	50,0,0,28		Type,look and htg ht
		dc.l	0			Link
		dc.b	3,1,$ff,$fe		Any set.
		dc.l	0,0,0,0
		even
a37		dc.w	0,-20			Swiv.
		dc.l	swiv			Blbset.
		dc.b	51,0,0,36		Type,look and htg ht
		dc.l	0			Link
		dc.l	swiv1			Any set.
		dc.l	0
		dc.b	20
		even
swiv1		dc.w	$3,$0,20
		dc.w	-$3,$3,20
		dc.w	$0,-$3,20
		dc.w	$8000
		even
a38		dc.w	0,0			Left chainsaw setup.
		dc.l	chainsaw		Blbset.
		dc.b	52,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.w	0			Any set.
		dc.w	$10*20		24
		even
a39		dc.w	0,-20			Swiv.
		dc.l	swiv			Blbset.
		dc.b	51,0,0,36		Type,look and htg ht
		dc.l	0			Link
		dc.l	swiv2			Any set.
		dc.l	0
		dc.b	30
		even
swiv2		dc.w	0,$3,30
		dc.w	0,-3,30
		dc.w	$8000
		even
a3a		dc.w	0,-20			Swiv.
		dc.l	swiv			Blbset.
		dc.b	51,0,0,36		Type,look and htg ht
		dc.l	0			Link
		dc.l	swiv3			Any set.
		dc.l	0
		dc.b	30
		even
swiv3		dc.w	$3,0,30
		dc.w	-3,0,30
		dc.w	$8000
		even
a3b		dc.w	0,0			Oil generator.
		dc.l	flames			Blbset.
		dc.b	47,12,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	-5,0			Any set.
		dc.w	8
		even

a3e		dc.w	0,0			Oil generator.
		dc.l	flames			Blbset.
		dc.b	47,12,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	5,0			Any set.
		dc.w	-9

a3f		dc.w	0,0			Drill boss ...
		dc.l	boss2			Blbset.
		dc.b	139,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0,0,0,0,0,0	Any set.
		even




b1		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,1,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$123			Any set.
		dc.w	15,0
		even
b2		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,2,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$100			Any set.
		dc.w	13,1
		even
b3		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,3,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$e4			Any set.
		dc.w	12,2
		even
b4		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,1,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$d8			Any set.
		dc.w	11,3
		even
b5		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,2,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$c0			Any set.
		dc.w	10,4
		even
b6		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,2,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$ac			Any set.
		dc.w	9,5
		even
b7		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,3,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$98			Any set.
		dc.w	8,6
		even
b8		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,1,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$90			Any set.
		dc.w	7,7
		even
b9		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,2,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$80			Any set.
		dc.w	6,8
		even
ba		dc.w	0,10			key 1.
		dc.l	keys			Blbset.
		dc.b	53,3,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	$48			Any set.
		dc.w	5,9
		even
bb		dc.w	0,12			Muzakcat.
		dc.l	muzakcat		Blbset.
		dc.b	54,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	$ff,0			Any set.
		dc.l	0,catys,0,0
		even
bc		dc.w	0,0			Dstick.
		dc.l	dstick			Blbset.
		dc.b	55,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
bd2		dc.w	0,0			Trumpet stuff (1 per scr).
		dc.l	lev1stuf		Blbset.
		dc.b	56,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	30,0			Any set.
		even
be		dc.w	0,0			cybil.
		dc.l	cybil			Blbset.
		dc.b	58,8,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0			Any set.
		even
bf		dc.w	0,0			Cello.
		dc.l	cello			Blbset.
		dc.b	59,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0			Any set.
		even
b10		dc.w	0,5			Drum.
		dc.l	drum			Blbset.
		dc.b	61,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0			Any set.
		even
b11		dc.w	0,4			Basswob.
		dc.l	basswob			Blbset.
		dc.b	62,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0			Any set.
		even
b12		dc.w	0,0			Note lift.
		dc.l	fuck			Blbset.
		dc.b	63,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	40,80,0,2		Any set.
		even
b13		dc.w	0,0			Pipes.
		dc.l	notes			Blbset.
		dc.b	64,0,0,$0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0			Any set.
		even
b14		dc.w	0,0			Record deck.
		dc.l	deck			Blbset.
		dc.b	66,3,0,$0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0			Any set.
		dc.b	0,15
		even
b15		dc.w	0,0			Extra note tune.
		dc.l	fuck			Blbset.
		dc.b	67,2,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	16*6*($ad-$a)		Any set.
		dc.l	$5b-8,0,tune1,tune1
		dc.b	82			Transport. 68 (extra life)
		even
b16		dc.w	0,0			Note lift.
		dc.l	fuck			Blbset.
		dc.b	67,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		dc.l	0,0,tune2,tune2
		dc.b	68
		even
b17		dc.w	0,-4			Spark.
		dc.l	spark			Blbset.
		dc.b	69,0,0,0		Type,look and htg ht
		dc.l	0			Link
 		dc.w	$1000,8			Any set.
		even
b18		dc.w	0,0			Spark.
		dc.l	bell			Blbset.
		dc.b	71,0,0,$ff		Type,look and htg ht
		dc.l	$19			Link
		dc.w	0,0,0			Any set.
		even
b19		dc.w	32,0			Spark.
		dc.l	bell			Blbset.
		dc.b	0,1,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.w	0			Any set.
		even
b1a		dc.w	0,11			Small piano.
		dc.l	smnotes			Blbset.
		dc.b	72,10,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	tune1			Any set.
		even
b1b		dc.w	0,0			Elecbod I>.
		dc.l	elecbod			Blbset.
		dc.b	14,2,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	40,20,0			Any set.
		even
b1c		dc.w	-16,0			Elecbod <I.
		dc.l	elecbod			Blbset.
		dc.b	14,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.b	40,20,0			Any set.
		even
b1d		dc.w	0,0			Trumpet stuff (1 per scr).
		dc.l	lev1stuf		Blbset.
		dc.b	56,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	30,0,0,0,$ff		Any set.
		even
b1e		dc.w	0,0			Amp.
		dc.l	amp			Blbset.
		dc.b	74,0,0,64		Type,look and htg ht
		dc.l	$1f			Link
		dc.l	0			Any set.
		even
b1f		dc.w	32,0			Amp part 2.
		dc.l	amp			Blbset.
		dc.b	0,1,0,64		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
b20		dc.w	0,0			Amp part 2.
		dc.l	amp			Blbset.
		dc.b	75,0,0,64		Type,look and htg ht
		dc.l	$21			Link
		dc.l	0			Any set.
		even
b21		dc.w	32,0			Amp part 2.
		dc.l	amp			Blbset.
		dc.b	75,1,0,64		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
b22		dc.w	0,0			X lift.
		dc.l	mlift			Blbset.
		dc.b	28,0,0,$ff		Type,look and htg ht
		dc.l	$23			Link
		dc.b	50,0			Any set.
		dc.w	-2,0,0,0,0
		even
b23		dc.w	0,0			X lift part 2.
		dc.l	mlift			Blbset.
		dc.b	0,1,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
b24		dc.w	0,0			Music eolg.
		dc.l	mboss			Blbset.
		dc.b	76,0,30,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0,0,0,0,0,0	Any set.
		even

b29		dc.w	0,11			Small piano 2.
		dc.l	smnotes			Blbset.
		dc.b	72,10,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	tune2			Any set.
		even
b2a		dc.w	0,11			Small piano 3.
		dc.l	smnotes			Blbset.
		dc.b	72,10,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	tune3			Any set.
		even
b2b		dc.w	0,0			Extra note tune.
		dc.l	fuck			Blbset.
		dc.b	67,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	40,80,0,2		Any set.
		dc.l	0,0,tune3,tune3
		dc.b	63
		even
b2c		dc.w	0,0			Extra note tune.
		dc.l	fuck			Blbset.
		dc.b	82,2,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	16*6*($dc-$a)		Any set.
		dc.l	$3a-8
		even




d1		dc.w	0,0			Hit for grape thing.
		dc.l	mana			Blbset.
		dc.b	78,10,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
d2		dc.w	0,0			Grape bunch locator.
		dc.l	mana			Blbset.
		dc.b	79,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
d3		dc.w	0,0			Volcano...
		dc.l	lev1stuf		Blbset.
		dc.b	83,1,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	10,0			Any set.
		even
d4		dc.w	0,8			Jif.
		dc.l	jif			Blbset.
		dc.b	84,0,0,0		Type,look and htg ht
		dc.l	6			Link
		dc.b	0,0,0			Any set.
		even
d5		dc.w	-4,8			Jif.
		dc.l	jif			Blbset.
		dc.b	84,0,0,0		Type,look and htg ht
		dc.l	6			Link
		dc.b	0,0,$ff			Any set.
		even
d6		dc.w	0,0			Jif part 2.
		dc.l	jif			Blbset.
		dc.b	0,0,0,0			Type,look and htg ht
		dc.l	0			Link
		even
d7		dc.w	0,8-16			Carrot top.
		dc.l	overlays		Blbset.
		dc.b	21,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
d8		dc.w	0,-1			Froot can.
		dc.l	overlays		Blbset.
		dc.b	84,1,0,0		Type,look and htg ht
		dc.l	6			Link
		dc.b	0,$ff			Any set.
		even
d9		dc.w	0,-1			Froot can.
		dc.l	mana			Blbset.
		dc.b	86,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	20			Any set.
		even
da		dc.w	0,0			Radish 1.
		dc.l	mana			Blbset.
		dc.b	88,4,0,16		Type,look and htg ht
		dc.l	0			Link
		dc.b	1,0,0			Any set.
		even
db		dc.w	0,0			Radish 2.
		dc.l	mana			Blbset.
		dc.b	88,4,0,16		Type,look and htg ht
		dc.l	0			Link
		dc.b	2,0,0			Any set.
		even
adc		dc.w	0,0			Radish 3.
		dc.l	mana			Blbset.
		dc.b	88,4,0,16		Type,look and htg ht
		dc.l	0			Link
		dc.b	3,0,0			Any set.
		even
dd		dc.w	0,0			Radish 4.
		dc.l	mana			Blbset.
		dc.b	88,4,0,16		Type,look and htg ht
		dc.l	0			Link
		dc.b	4,0,0			Any set.
		even
de		dc.w	0,0			Radish control.
		dc.l	lev1stuf		Blbset.
		dc.b	89,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
radseq1		dc.b	1,2,3,4,$ff,0		Any set.
		dc.l	radseq1
		even
df		dc.w	0,0			Standard radish.
		dc.l	mana			Blbset.
		dc.b	88,4,0,16		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,0,0			Any set.
		even
d10		dc.w	0,0			Pea runner.
		dc.l	mana			Blbset.
		dc.b	91,7,0,$ff		Type,look and htg ht
		dc.l	6			Link
		dc.l	$20000			Any set.
		dc.w	-7,0,0,0,5
		even
d11		dc.w	0,0			Pea runner.
		dc.l	mana			Blbset.
		dc.b	91,23,0,$ff		Type,look and htg ht
		dc.l	6			Link
		dc.l	$20000			Any set.
		dc.w	7,0,0,0,25
		even
d12		dc.w	0,0			Banana.
		dc.l	mana			Blbset.
		dc.b	92,12,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0			Any set.
		even
d13		dc.w	0,0			Rolling ball.
		dc.l	mana			Blbset.
		dc.b	38,21,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0,0,0,0,0		Any set.
		even
d14		dc.w	0,-5			Carrot.
		dc.l	mana			Blbset.
		dc.b	93,17,0,$25		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0			Any set.
		even
d15		dc.w	0,0			Hole \.
		dc.l	holes2			Blbset.
		dc.b	17,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,76,77,136,137		Any set.
		even
d16		dc.w	0,0			Hole _.
		dc.l	holes2			Blbset.
		dc.b	17,1,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,77,77,136,137		Any set.
		even
d17		dc.w	0,0			Hole /.
		dc.l	holes2			Blbset.
		dc.b	17,2,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0,77,78,136,137		Any set.
		even

d19		dc.w	0,-1			Alarm clock ...
		dc.l	boss4			Blbset.
		dc.b	137,1,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0			Any set.
		even
d1a		dc.w	0,0			Banana boss ...
		dc.l	boss4			Blbset.
		dc.b	138,11,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0,0,0,0,0,0,0,0	Any set.
		even




e1		dc.w	0,0			Red cell.
		dc.l	cells			Blbset.
		dc.b	94,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	movrcell1,0		Any set.
		dc.w	0
		dc.l	redani,0,0
		even
e2		dc.w	0,0			% cell gen (e1).
		dc.l	cells			Blbset.
		dc.b	95,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e1			Any set.
		dc.w	32,4,7
		even
e3		dc.w	0,0			Green cell.
		dc.l	cells			Blbset.
		dc.b	96,0,2,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	green1,0		Any set.
		dc.w	0
		dc.l	grani,0,0
		even
e4		dc.w	0,0			% cell gen (e6).
		dc.l	cells			Blbset.
		dc.b	95,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e6			Any set.
		dc.w	32,4,3
		even
e5		dc.w	0,0			White cell (hugger).
		dc.l	cells			Blbset.
		dc.b	98,0,14,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0,0,0,0		Any set.
		even
e6		dc.w	0,0			Bloblet.
		dc.l	cells			Blbset.
		dc.b	97,0,6,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	splinter,0		Any set.
		dc.w	0
		dc.l	blobani,0,0
		even
e2e		dc.w	0,0			Set scroll speed.
		dc.l	cells			Blbset.
		dc.b	100,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	4,0			Any set.
		even
e2f		dc.w	0,0			Set scroll speed.
		dc.l	cells			Blbset.
		dc.b	100,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	6,0			Any set.
		even
e30		dc.w	0,0			Set scroll speed.
		dc.l	cells			Blbset.
		dc.b	100,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	8,0			Any set.
		even
e31		dc.w	0,0			Set scroll speed.
		dc.l	cells			Blbset.
		dc.b	100,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	2,0			Any set.
		even
e2a		dc.w	0,0			Nerve.
		dc.l	cells2			Blbset.
		dc.b	102,17,4,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	nervemov,0		Any set.
		dc.w	0
		dc.b	10,0
		even
eb		dc.w	0,0			Splinter.
		dc.l	cells2			Blbset.
		dc.b	103,22,10,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	splinter,0		Any set.
		dc.w	0
		dc.l	splint,0
		even
ec		dc.w	0,0			Amp.
		dc.l	cells2			Blbset.
		dc.b	104,3,0,32		Type,look and htg ht
		dc.l	$d			Link
		dc.l	0			Any set.
		even
ed		dc.w	32,0			Amp part 2.
		dc.l	cells2			Blbset.
		dc.b	0,4,0,32		Type,look and htg ht
		dc.l	$10			Link
		dc.l	0			Any set.
		even
eesec		dc.w	0,0			Amp part 2.
		dc.l	cells2			Blbset.
		dc.b	105,0,0,32		Type,look and htg ht
		dc.l	$f			Link
		dc.l	0			Any set.
		even
ef		dc.w	32,0			Amp part 2.
		dc.l	cells2			Blbset.
		dc.b	105,1,0,32		Type,look and htg ht
		dc.l	$11			Link
		dc.l	0			Any set.
		even
e10		dc.w	64,0			Amp part 2.
		dc.l	cells2			Blbset.
		dc.b	0,5,0,32		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
e11		dc.w	64,0			Amp part 2.
		dc.l	cells2			Blbset.
		dc.b	105,2,0,32		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
e25		dc.w	0,0			Snake.
		dc.l	cells2			Blbset.
		dc.b	106,37,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0,0,0,0,0,0	Any set.
		even
e22		dc.w	-3,3			Claw.
		dc.l	cells2			Blbset.
		dc.b	107,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	grani3,0		Any set.
		even
e23		dc.w	0,0			Brain spack.
		dc.l	cells2			Blbset.
		dc.b	108,40,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0			Any set.
		even

e1f		dc.w	30,0			Bullet hole...
		dc.l	cells2			Blbset.
		dc.b	109,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0			Any set.
		even


e29		dc.w	0,0			Rock gen.
		dc.l	cells2			Blbset.
		dc.b	113,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	20			Any set.
		even


e20		dc.w	2,5			Bullet hole...
		dc.l	cells2			Blbset.
		dc.b	109,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0			Any set.
		even

e21		dc.w	13-16,12-16		Bullet hole...
		dc.l	cells2			Blbset.
		dc.b	109,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	holeani2		Any set.
		even

e26		dc.w	13-16,12-16		Bullet hole...
		dc.l	cells2			Blbset.
		dc.b	109,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	holeani			Any set.
		even

e12		dc.w	0,0			Weapon 0 ...
		dc.l	bangs			Blbset.
		dc.b	114,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
e13		dc.w	0,0			Weapon 1 ...
		dc.l	bangs			Blbset.
		dc.b	114,1,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
e14		dc.w	0,0			Weapon 2 ...
		dc.l	bangs			Blbset.
		dc.b	114,2,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
e15		dc.w	0,0			Weapon 3 ...
		dc.l	bangs			Blbset.
		dc.b	114,3,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
e16		dc.w	0,0			Weapon 4 ...
		dc.l	bangs			Blbset.
		dc.b	114,4,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even

e36		dc.w	0,0			Up and down ...
		dc.l	cells			Blbset.
		dc.b	126,4,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.w	0			Any set.
		dc.l	spinani
		even

e37		dc.w	0,0			Up and down ...
		dc.l	boss7 			Blbset.
		dc.b	134,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	40			Any set.
		even



alienlist5	dc.l	0,e1,e2,e3,e4,e5,e6,0,0,0,0,eb,ec,ed,eesec,ef
		dc.l	e10,e11,e12,e13,e14,e15,e16,0,e18,e19,e1a,e1b,d3,0
		dc.l	e1e,e1f,e20,e21,e22,e23,e24,e25,e26,e27,e1b,e29,e2a
		dc.l	e2b,e2c,e2d,e2e,e2f,e30,e31,e32,e33,e34,e35,e36,e37


e24		dc.w	0,0			3 way tounge thing.
		dc.l	cells2			Blbset.
		dc.b	101,14,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	toungemov,0		Any set.
		dc.w	0
		dc.b	0,20,0,0
		dc.l	way3a,0
		even
way3a		dc.w	$194*$10,$19*$10
		dc.w	$190*$10,$19*$10
		dc.w	$190*$10,$19*$10
		dc.w	$190*$10,$19*$10
		dc.w	$190*$10,$19*$10
		dc.w	$18c*$10,$19*$10
		dc.w	$188*$10,$19*$10
		dc.w	$184*$10,$18*$10
		dc.w	$180*$10,$14*$10
		dc.w	$178,$10,$10*$10
		even



e27		dc.w	0,0			3 way tounge thing.
		dc.l	cells2			Blbset.
		dc.b	101,14,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	toungemov,0		Any set.
		dc.w	0
		dc.b	0,20,0,0
		dc.l	way3b,0
		even
way3b		dc.w	$1b3*$10,$15*$10
		dc.w	$1b0*$10,$15*$10
		dc.w	$1ad*$10,$16*$10
		dc.w	$1aa*$10,$15*$10
		dc.w	$1a7*$10,$15*$10
		dc.w	$1a3*$10,$1a*$10
		dc.w	$19f*$10,$1d*$10
		dc.w	$190*$10,$1e*$10
		dc.w	$187,$10,$20*$10
		even

e2b		dc.w	0,0			3 way tounge thing.
		dc.l	cells2			Blbset.
		dc.b	101,14,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	toungemov,0		Any set.
		dc.w	0
		dc.b	0,20,0,0
		dc.l	way3c,0
		even
way3c		dc.w	$231*$10,$17*$10
		dc.w	$22f*$10,$15*$10
		dc.w	$22d*$10,$13*$10
		dc.w	$22a*$10,$13*$10
		dc.w	$226*$10,$13*$10
		dc.w	$220*$10,$15*$10
		dc.w	$21a*$10,$18*$10
		dc.w	$21d*$10,$1c*$10
		dc.w	$210,$10,$20*$10
		even

e33		dc.w	0,0			3 way tounge thing.
		dc.l	cells2			Blbset.
		dc.b	101,14,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	toungemov,0		Any set.
		dc.w	0
		dc.b	0,20,0,0
		dc.l	way33,0
		even
way33		dc.w	$132*$10,$15*$10
		dc.w	$12e*$10,$13*$10
		dc.w	$12b*$10,$18*$10
		dc.w	$124*$10,$14*$10
		dc.w	$11d*$10,$18*$10
		dc.w	$118*$10,$15*$10
		even

e34		dc.w	0,0			3 way tounge thing.
		dc.l	cells2			Blbset.
		dc.b	101,14,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	toungemov,0		Any set.
		dc.w	0
		dc.b	0,20,0,0
		dc.l	way34,0
		even
way34		dc.w	$139*$10,$16*$10
		dc.w	$135*$10,$18*$10
		dc.w	$132*$10,$13*$10
		dc.w	$12b*$10,$17*$10
		dc.w	$124*$10,$13*$10
		dc.w	$11e*$10,$16*$10
		even

e32		dc.w	0,0			% cell gen (e1).
		dc.l	cells			Blbset.
		dc.b	95,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e32b			Any set.
		dc.w	32,4,1
		even
e32b		dc.w	0,0			Red cell.
		dc.l	cells2			Blbset.
		dc.b	94,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	movrcell1,0		Any set.
		dc.w	0
		dc.l	spinani,coo32,0
		even
coo32		dc.w	$6f*$10,$10*$10
		dc.w	$6b*$10,$c*$10
		dc.w	$65*$10,$c*$10
		dc.w	$61*$10,$10*$10
		dc.w	$5a*$10,$10*$10
		dc.w	$55*$10,$10*$10
		dc.w	$50*$10,$10*$10
		dc.l	0 
		even



e18		dc.w	0,0			% cell gen (e1).
		dc.l	cells			Blbset.
		dc.b	95,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e18al			Any set.
		dc.w	32,4,2
		even
e18al		dc.w	0,0			Red cell.
		dc.l	cells			Blbset.
		dc.b	94,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	movrcell1,0		Any set.
		dc.w	0
		dc.l	redani,coo18,0
		even
coo18		dc.w	$21*$10,$6*$10
		dc.w	$1d*$10,$6*$10
		dc.w	$18*$10,$6*$10
		dc.w	$0e*$10,$6*$10
		dc.l	0
		even

e19		dc.w	0,0			% cell gen (e1).
		dc.l	cells			Blbset.
		dc.b	111,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e19b			Any set.
		dc.b	0,15
		even
e19b		dc.w	0,0			Red cell.
		dc.l	cells			Blbset.
		dc.b	94,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0			Any set.
		dc.w	0
		dc.l	grani,coo19,0
		even
coo19		dc.w	20*$10,20*$10
		dc.w	20*$10,24*$10
		dc.w	18*$10,26*$10
		dc.w	15*$10,27*$10
		dc.w	11*$10,27*$10
		dc.w	6*$10,26*$10
		dc.w	1*$10,24*$10
		dc.l	0
		even



e1a		dc.w	0,0			% cell gen (e1).
		dc.l	cells			Blbset.
		dc.b	95,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e1aal			Any set.
		dc.w	32,4,1
		even
e1aal		dc.w	0,0			Red cell.
		dc.l	cells2			Blbset.
		dc.b	94,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	movrcell1,0		Any set.
		dc.w	0
		dc.l	spinani,coo1a,0
		even
coo1a		dc.w	$68*$10,$12*$10
		dc.w	$65*$10,$10*$10
		dc.w	$62*$10,$12*$10
		dc.w	$5f*$10,$10*$10
		dc.w	$5c*$10,$12*$10
		dc.w	$59*$10,$10*$10
		dc.w	$56*$10,$12*$10
		dc.w	$53*$10,$10*$10
		dc.l	0 
		even
spinani		dc.w	6,6,7,7,8,8,9,9,$ffff
		dc.l	spinani
		even


e2c		dc.w	0,0			% cell gen (e1).
		dc.l	cells			Blbset.
		dc.b	95,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e2cal			Any set.
		dc.w	24,8,3
		even
e2cal		dc.w	0,0			Red cell.
		dc.l	cells2			Blbset.
		dc.b	94,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	movrcell1,0		Any set.
		dc.w	0
		dc.l	spinani,coo2c,0
		even
coo2c		dc.w	$243*$10,$22*$10
		dc.w	$248*$10,$25*$10
		dc.w	$23d*$10,$28*$10
		dc.w	$237*$10,$22*$10
		dc.w	$231*$10,$1c*$10
		dc.w	$22d*$10,$20*$10
		dc.w	$22a*$10,$23*$10
		dc.w	$225*$10,$22*$10
		dc.w	$220*$10,$20*$10
		dc.l	0
		even

e2d		dc.w	0,0			% cell gen (e1).
		dc.l	cells			Blbset.
		dc.b	95,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e2dal			Any set.
		dc.w	24,8,3
		even
e2dal		dc.w	0,0			Red cell.
		dc.l	cells2			Blbset.
		dc.b	94,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	movrcell1,0		Any set.
		dc.w	0
		dc.l	spinani,coo2d,0
		even
coo2d		dc.w	$24f*$10,$1f*$10
		dc.w	$24c*$10,$1c*$10
		dc.w	$249*$10,$19*$10
		dc.w	$233*$10,$22*$10
		dc.w	$23d*$10,$25*$10
		dc.w	$23a*$10,$21*$10
		dc.w	$237*$10,$1e*$10
		dc.w	$234*$10,$20*$10
		dc.w	$230*$10,$22*$10
		dc.l	0
		even

e35		dc.w	0,0			% cell gen (e1).
		dc.l	cells			Blbset.
		dc.b	95,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e35al			Any set.
		dc.w	24,8,3
		even
e35al		dc.w	0,0			Red cell.
		dc.l	cells2			Blbset.
		dc.b	94,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	movrcell1,0		Any set.
		dc.w	0
		dc.l	spinani,coo35,0
		even
coo35		dc.w	$23*$10,$19*$10
		dc.w	$1f*$10,$1a*$10
		dc.w	$1b*$10,$18*$10
		dc.w	$15*$10,$1a*$10
		dc.w	$f*$10,$18*$10
		dc.w	$c*$10,$1b*$10
		dc.l	0
		even





e1b		dc.w	8,5			Drip...gen
		dc.l	cells			Blbset.
		dc.b	111,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	e1bb			Any set.
		dc.b	0,30
		even
e1bb		dc.w	0,0			Drip...
		dc.l	cells2			Blbset.
		dc.b	112,54,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	$20000			Any set.
		even

e1e		dc.w	0,0			% cell gen (e1).
		dc.l	cells			Blbset.
		dc.b	98,0,8,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0			Any set.
		even


alienlist6	dc.l	0,f1,f2,f3,f4,f5,f6,f7,f8,f9,fa,fb,fc,fd
		dc.l	a10,a11,a12,a13,a25,f13
		;	fe  ff  f10 f11 f12
		dc.l	cole1,cole2,cole3,cole4,cole5,cole6,ae4,af4,bonus2
		;      f14   f15    f16   f17   f18   f19   f1a f1b f1c


bonus2		dc.w	0,0	   		Bonus level ... (16)
		dc.l	lev1stuf   		Blbset.
		dc.b	143,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	2,0
		dc.l	((16*4)*($5a-$a)),$1c-8	Any set.
		even


ae4		dc.w	0,-15			Polo (.
		dc.l	lev1stuf		Blbset.
		dc.b	15,29,0,$ff		Type,look and htg ht
		dc.l	$1b			Link
		even
af4		dc.w	32,-15			Polo ).
		dc.l	lev1stuf		Blbset.
		dc.b	0,30,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even



f1		dc.w	0,0			Drum.
		dc.l	toys			Blbset.
		dc.b	116,0,0,$ff		Type,look and htg ht
		dc.l	2			Link
		dc.l	0,0,0			Any set.
		even
f2		dc.w	0,0			Drum.
		dc.l	toys			Blbset.
		dc.b	0,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
f3		dc.w	0,0			Rolling ball.
		dc.l	toys			Blbset.
		dc.b	38,36,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0,0,0,0,0		Any set.
		even
f4		dc.w	0,0			Toy plane.
		dc.l	toys			Blbset.
		dc.b	91,18,0,$ff		Type,look and htg ht
		dc.l	6			Link
		dc.l	$20000			Any set.
		dc.w	-7,0,0,0,18
		even
f5		dc.w	0,0			Toy plane.
		dc.l	toys			Blbset.
		dc.b	91,16,0,$ff		Type,look and htg ht
		dc.l	6			Link
		dc.l	$20000			Any set.
		dc.w	7,0,0,0,16
		even
f6		dc.w	0,0			Jif part 2.
		dc.l	toys			Blbset.
		dc.b	0,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
f7		dc.w	0,0			Bouncing.
		dc.l	toys			Blbset.
		dc.b	117,38,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,0,0,0			Any set.
		even
f8		dc.w	0,0			Yo - Yo ...
		dc.l	toys			Blbset.
		dc.b	118,39,0,0		Type,look and htg ht
		dc.l	9			Link
		even
f9		dc.w	16,0			Yo - Yo ...
		dc.l	toys			Blbset.
		dc.b	0,$33,$78,0		Type,look and htg ht 49
		dc.l	0			Link
		even
fa		dc.w	0,0			Spining.
		dc.l	toys			Blbset.
		dc.b	119,53,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,.spinani		Any set.
.spinani	dc.w	53,54,55,56,$ffff
		dc.l	.spinani
		even
fb		dc.w	0,0			Big bear.
		dc.l	toys			Blbset.
		dc.b	120,20,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	$30000,0,0,0,0		Any set.
		dc.w	5
		even
fc		dc.w	0,0			Small bear.
		dc.l	toys			Blbset.
		dc.b	120,28,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	$30000,0,0,0,0		Any set.
		dc.w	-13
		even
fd		dc.w	0,0			Big bear.
		dc.l	toys			Blbset.
		dc.b	121,0,0,$ff		Type,look and htg ht
		dc.l	6			Link
		dc.l	$30000,0,0,0,0		Any set.
		even

f13		dc.w	0,0			Up and down ...
		dc.l	boss6			Blbset.
		dc.b	135,$ff,0,0		Type,look and htg ht
		dc.l	0			Link
		even


alienlist7	dc.l	0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,ge
		dc.l	ge,g11,g12
		;       g10					    gf

		dc.l	cole1,cole2,cole3,cole4,cole5,cole6,ae5,a10,a11,a12,a13,af5,a25,e37
		;        13    14    15    16    17    18   19 1a  1b  1c  1d  1e   1f  20

ae5		dc.w	0,-15			Polo (.
		dc.l	lev1stuf		Blbset.
		dc.b	15,29,0,$ff		Type,look and htg ht
		dc.l	$1e			Link
		even
af5		dc.w	32,-15			Polo ).
		dc.l	lev1stuf		Blbset.
		dc.b	0,30,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even


g1		dc.w	10,11			White button.
		dc.l	fair2			Blbset.
		dc.b	122,5,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
g2		dc.w	10,11			White button.
		dc.l	fair2			Blbset.
		dc.b	122,4,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
g3		dc.w	10,11			White button.
		dc.l	fair2			Blbset.
		dc.b	122,6,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
g4		dc.w	0,$10			Zool.
		dc.l	fair1			Blbset.
		dc.b	123,20,0,0		Type,look and htg ht
		dc.l	5			Link
		dc.l	0,0,0,0,0,0,0,0,0,0,0	Any set.
		even
g5		dc.w	$b0,$20			Hidden part...
		dc.l	fair1			Blbset.
		dc.b	0,29,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	gamedata		Any set.
		even
g6		dc.w	5,36			Game over message.
		dc.l	fair2			Blbset.
		dc.b	125,0,0,$ff		Type,look and htg ht
		dc.l	7			Link
		dc.b	1
		even
g7		dc.w	38+5,36			message part two ...
		dc.l	fair2			Blbset.
		dc.b	0,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
g8		dc.w	0,0			Candy.
		dc.l	fair1			Blbset.
		dc.b	127,13,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	$30000,0,0,0,0		Any set.
		even
g9		dc.w	0,6			Spining apple.
		dc.l	fair1			Blbset.
		dc.b	128,3,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	0,.apani,0		Any set.
		dc.b	0,20
.apani		dc.w	3,3,4,4,5,5,6,6,$ffff
		dc.l	.apani
		even
ga		dc.w	0,0			Hammer.
		dc.l	fair1			Blbset.
		dc.b	129,34,0,0		Type,look and htg ht
		dc.l	7			Link
		dc.l	0,0,0,0,0		Any set.
		even
gb		dc.w	0,16			Ping plat.
		dc.l	fair1			Blbset.
		dc.b	130,35,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.l	0			Any set.
		even
gc		dc.w	0,5+16			Wood platform.
		dc.l	fair2			Blbset.
		dc.b	28,0,0,$ff		Type,look and htg ht
		dc.l	$d			Link
		dc.b	50,0			Any set.
		dc.w	-2,0,0,0,0
		even
gd		dc.w	32,5+16			Wood platform.
		dc.l	fair2			Blbset.
		dc.b	0,1,0,$ff		Type,look and htg ht
		dc.l	0			Link
		even
ge		dc.w	0,0			Note lift.
		dc.l	fair1			Blbset.
		dc.b	131,0,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	ballon1,0		Any set.
		dc.w	0
		dc.w	90
		even
ballon1		dc.w	0,-4,repeat,95
		dc.l	ballon1
.b2		dc.w	0,4,repeat,95
		dc.l	.b2
		dc.w	goto
		dc.l	ballon1
		even
gf		dc.w	0,0			Note lift.
		dc.l	fair1			Blbset.
		dc.b	131,1,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	ballon2,0		Any set.
		dc.w	0
		dc.w	130
		even
ballon2		dc.w	0,-3,repeat,50
		dc.l	ballon2
.b3		dc.w	0,3,repeat,50
		dc.l	.b3
		dc.w	goto
		dc.l	ballon2
		even
g10		dc.w	0,0			Note lift.
		dc.l	fair1			Blbset.
		dc.b	131,2,0,$ff		Type,look and htg ht
		dc.l	0			Link
		dc.l	ballon3,0		Any set.
		dc.w	0
		dc.w	250
		even
ballon3		dc.w	0,-3,repeat,40
		dc.l	ballon3
.b4		dc.w	0,3,repeat,40
		dc.l	.b4
		dc.w	goto
		dc.l	ballon3
		even
g11		dc.w	0,0			Volcano...
		dc.l	fair1			Blbset.
		dc.b	83,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	10,0			Any set.
		even
g12		dc.w	0,-38			Fall down ...
		dc.l	fair2			Blbset.
		dc.b	132,0,0,0		Type,look and htg ht
		dc.l	0			Link
		dc.b	0			Any set.
		even















areastarts	dc.l	128,31,sky+(30*2)	Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$96c0			Lenght.
		dc.b	'a1'
		dc.w	64
		dc.l	a10,$1880,$15
		dc.l	a11,$4800,$23
		dc.l	a12,$7fc0,$17
		dc.l	0,0,0
		dc.w	$26c,$25

a2s		dc.l	128,34,sky+(30*2)	Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.		
		dc.l	$7b80			Lenght.
		dc.b	'a2'
		dc.w	64
 		dc.l	a10,$2840,$26
		dc.l	a11,$4d00,$30
		dc.l	a12,$5b80,$27
		dc.l	a13,$7680,$9
		dc.w	$1fe,$14

		dc.l	128,19,sky+(30*2)	Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$66c0			Lenght.
		dc.b	'a3'
		dc.w	64
		dc.l	a10,$3500,$17
		dc.l	a11,$4c40,$d
		dc.l	0,0,0
		dc.l	0,0,0
		dc.w	$1a6,$16

areastarts2	dc.l	$6*200,$ba-14,sky+120	 Map x,y and sky start.
	  	dc.w	$110,$90		Zool x,y.
		dc.l	$8bd8			Lenght.
		dc.b	'b1'
		dc.w	200
		dc.l	a10,($a5-$a)*200,$b9-8
		dc.l	a11,($b1-$a)*200,$4b-8
		dc.l	a12,($1e-$a)*200,$5c-8
		dc.l	a13,($4d-$a)*200,$45-8
		dc.w	$c2,$2b

		dc.l	($58-$a)*200,$44-8,sky+100 Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$8bd8			Lenght.
		dc.b	'b2'
		dc.w	200
		dc.l	a10,($50-$a)*200,$8d-8
		dc.l	a11,($76-$a)*200,$8d-8
		dc.l	a12,($1a-$a)*200,$4a-8
		dc.l	a13,($b1-$a)*200,$a1-8
		dc.w	$59,$2d

		dc.l	($c-$a)*200,$c5-9,sky+120		Map x,y and sky start.
;		dc.l	($c-$a)*200,2,sky+120	GUARD ... Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$8bd8			Lenght.
		dc.b	'b3'
		dc.w	200
		dc.l	a10,($59-$a)*200,$90-8
		dc.l	a11,($43-$a)*200,$6e-8
		dc.l	a12,($76-$a)*200,$4c-8
		dc.l	a13,($b6-$a)*200,$39-8
		dc.w	$48,$6

areastarts3	dc.l	16*6,1,sky3+80		 Map x,y and sky start.
		dc.w	$110,$70		Zool x,y.
		dc.l	$6de0			Lenght.
		dc.b	'c1'
		dc.w	16*6
		dc.l	a10,($83-$a)*(16*6),$1b-8
		dc.l	a11,($0c-$a)*(16*6),$1a-8
		dc.l	a12,($48-$a)*(16*6),$42-8
		dc.l	a13,($ba-$a)*(16*6),$4a-8
		dc.w	$12d,$26

;		dc.l	$1d40,$19,sky3+20      for piano end ...
		dc.l	$c0,$46,sky3+90
		dc.w	$110,$90		Zool x,y.
		dc.l	$93c0			Lenght.
		dc.b	'c3'
		dc.w	16*6
		dc.l	a10,($b0-$a)*(16*6),$38-8
		dc.l	a11,($170-$a)*(16*6),$47-8
		dc.l	a12,($171-$a)*(16*6),$35-8
		dc.l	a13,($ec-$a)*(16*6),$8-8
		dc.w	$5c,$0b

		dc.l	16*6,2,sky3+80		Map x,y and sky start.
;		dc.l	16*6*$151,$49,sky3+60	Guard ... Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$93c0			Lenght.
		dc.b	'c2'
		dc.w	16*6
		dc.l	a10,($8e-$a)*(16*6),$17-8
		dc.l	a11,($138-$a)*(16*6),$17-8
		dc.l	a12,($9a-$a)*(16*6),$33-8
		dc.l	a13,($119-$a)*(16*6),$4d-8
		dc.w	$19c,$45

areastarts4	dc.l	16*5*4,$d,sky+40	Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$94c0			Lenght.
		dc.b	'd1'
		dc.w	16*5
		dc.l	a10,($50-$a)*(16*5),$13-8
		dc.l	a11,($9f-$a)*(16*5),$23-8
		dc.l	a12,($f6-$a)*(16*5),$31-8
		dc.l	a13,($16d-$a)*(16*5),$36-8
		dc.w	$1eb,$30

		dc.l	16*5*$1,$15,sky+40	Map x,y and sky start.
		dc.w	$80,$90		Zool x,y.
		dc.l	$94c0			Lenght.
		dc.b	'd2'
		dc.w	16*5
		dc.l	a10,($44-$a)*(16*5),$24-8
		dc.l	a11,($ab-$a)*(16*5),$23-8
		dc.l	a12,($125-$a)*(16*5),$28-8
		dc.l	a13,($171-$a)*(16*5),$2a-8
		dc.w	$1ed,$16

		dc.l	16*5*$a,1,sky+(20*2)	Map x,y and sky start.
;		dc.l	$91a0-(16*5*$a),1,sky+(20*2) Boss ... Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$94c0			Lenght.
		dc.b	'd3'
		dc.w	16*5
		dc.l	a10,($56-$a)*(16*5),$e-8
		dc.l	a11,($ce-$a)*(16*5),$22-8
		dc.l	a12,($122-$a)*(16*5),$32-8
		dc.l	a13,($17a-$a)*(16*5),$1a-8
		dc.w	$1e9,$12

areastarts5	dc.l	16*3*4,$1,0	    	Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$98d0-(16*3*5)		Lenght.
		dc.b	'e1'
		dc.w	16*3
		dc.l	a10,$0,$0
		dc.l	a11,$0,$0
		dc.l	a12,$0,$0
		dc.l	a13,$0,$0
		dc.w	0,0

		dc.l	16*3*4,$1,0		Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$98d0-(16*3*5)		Lenght.
		dc.b	'e2'
		dc.w	16*3
		dc.l	a10,$0,$0
		dc.l	a11,$0,$0
		dc.l	a12,$0,$0
		dc.l	a13,$0,$0
		dc.w	0,0

		dc.l	16*3,15,0		Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$98d0-(16*3*5)		Lenght.
		dc.b	'e3'
		dc.w	16*3
		dc.l	a10,$0,$0
		dc.l	a11,$0,$0
		dc.l	a12,$0,$0
		dc.l	a13,$0,$0
		dc.w	0,0

areastarts6	dc.l	16*4*2,$1f,sky+50    	Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$9680			Lenght.
		dc.b	'f1'
		dc.w	16*4
		dc.l	a10,($89-$a)*(16*4),$2d-8
		dc.l	a11,($11c-$a)*(16*4),$2c-8
		dc.l	a12,($1bd-$a)*(16*4),$1d-8
		dc.l	a13,($20a-$a)*(16*4),$20-8
		dc.w	$26a,$8

		dc.l	16*4*2,$1f+$e,sky+50	Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$9680			Lenght.
		dc.b	'f2'
		dc.w	16*4
		dc.l	a10,($7d-$a)*(16*4),$31-8
		dc.l	a11,($105-$a)*(16*4),$27-8
		dc.l	a12,($160-$a)*(16*4),$21-8
		dc.l	a13,($1da-$a)*(16*4),$2b-8
		dc.w	$26a,$23

		dc.l	16*4*2,2,sky+50		Map x,y and sky start.
;		dc.l	16*4*$23d,$7,sky+50	Guardian ...
		dc.w	$110,$90		Zool x,y.
		dc.l	$9680			Lenght.
		dc.b	'f3'
		dc.w	16*4
		dc.l	a10,($9a-$a)*(16*4),$15-8
		dc.l	a11,($fe-$a)*(16*4),$12-8
		dc.l	a12,($17b-$a)*(16*4),$38-8
		dc.l	a13,($214-$a)*(16*4),$d-8
		dc.w	$261,$2b

areastarts7	dc.l	16*5*2,$1a-$8,sky+40	   	Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$9680-(16*5*4)		Lenght.
		dc.b	'g1'
		dc.w	16*5
		dc.l	a10,($71-$a)*(16*5),$27-8
		dc.l	a11,($9f-$a)*(16*5),$2d-8
		dc.l	a12,($e2-$a)*(16*5),$26-8
		dc.l	a13,($131-$a)*(16*5),$3f-8
		dc.w	$1ee,$2c

		dc.l	16*5*2,$24-$8,sky+40	Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$9680-(16*5*4)		Lenght.
		dc.b	'g2'
		dc.w	16*5
		dc.l	a10,($3c-$a)*(16*5),$25-8
		dc.l	a11,($95-$a)*(16*5),$21-8
		dc.l	a12,($104-$a)*(16*5),$35-8
		dc.l	a13,($185-$a)*(16*5),$24-8
		dc.w	$1ea,$16

		dc.l	16*5*2,$22-$8,sky+40	Map x,y and sky start.
;		dc.l	16*5*$1ae,$d,sky+40	Guard ... Map x,y and sky start.
		dc.w	$110,$90		Zool x,y.
		dc.l	$9680-(16*5*4)		Lenght.
		dc.b	'g3'
		dc.w	16*5
		dc.l	a10,($6b-$a)*(16*5),$18-8
		dc.l	a11,($d6-$a)*(16*5),$1b-8
		dc.l	a12,($126-$a)*(16*5),$c-8
		dc.l	a13,($18d-$a)*(16*5),$1b-8
		dc.w	$1ef,$9	?


blocksname	dc.b	'blocks'
blockno		dc.b	'1.chm',0

mapname		dc.b	'map'
mapno		dc.b	'a1.map',0

jumppower	dc.w	0
endlevel	dc.b	0

bulletsprs	ds.l	29
mapplace	dc.l	prog
blankbit	ds.l	64*6	   
flashpls	dc.b	1,1,1,0		world 4
flashpls2	dc.b	0,1,1,1		world 5

mht		dc.l	0

eolg1dat	incbin	d:\various\eolg1.dat
joymem		;ds.w	500
zoolmems	ds.l	6*20*5
joyplace	dc.l	joymem
gotfirst	dc.l	0
zoolmem		dc.l	0
whichzool	dc.b	0
zoolisdead	dc.b	0
lives		dc.l	0
lastlives	dc.l	-1
startp		dc.l	0
skidcnt		dc.b	1
fadeback	dc.b	0
smartnow	dc.b	0
silly		dc.b	0
sillyani	dc.w	0
sillyfrs	dc.b	0,79,80,80,80,80,81,81,81,81,80,80,80,80
ptest		dc.b	0
outofit		dc.w	0
xooim		dc.l	0
yooim		dc.l	0
mindtrail	dc.b	0
firstroll	dc.l	0
secondroll	dc.l	0
rollcnt		dc.b	0

worldblobs	dc.l	world1blobs
		dc.l	world2blobs
		dc.l	world3blobs
		dc.l	world4blobs
		dc.l	world5blobs
		dc.l	world6blobs
		dc.l	world7blobs

world1blobs	dc.l	bumble,sloper,allsort
		dc.l	wallslob,greener
		dc.l	holes,bounce,eolg1
		dc.l	binary,lifts,lev1stuf,chuptop
		dc.l	sam0,sam1,sam2,sam3,sam4
		dc.l	sam5,sam6,sam7,sam8,sam9
		dc.l	samb,samc,samd,samf,sama,sam11,sam10,-1

world2blobs	dc.l	boss2
		dc.l	saw,drills,woodplat,balls,chainsaw,acme
		dc.l	switch,switch2,chris,flames,swiv,croc,lev1stuf
		dc.l	sam0,sam1,sam2,sam3,sam4
		dc.l	sam5,sam6,sam7,sam8,sam9
		dc.l	samb,samc,samd,samf,sama,sam11,sam10,-1

world3blobs	dc.l	mboss
		dc.l	keys,muzakcat,notes,cybil,cello,dstick
		dc.l	lev1stuf,drum,basswob,fuck,spark,bell,deck,smnotes
		dc.l	elecbod,mlift,amp
		dc.l	sam0,sam1,sam2,sam3,sam4
		dc.l	sam5,sam6,sam7,sam8,sam9
		dc.l	samb,samc,samd,same,samf,-1

world4blobs	dc.l	boss4,jif,overlays,mana,holes2,lev1stuf
		dc.l	sam0,sam1,sam2,sam3,sam4
		dc.l	sam5,sam6,sam7,sam8,sam9
		dc.l	samb,samc,samd,samf,sama,sam11,sam10,-1

world5blobs	dc.l	zoolship,cells,cells2,bangs,lev1stuf
		dc.l	sam0,sam1,sam2,sam3,sam4
		dc.l	sam5,sam6,sam7,sam8,sam9
		dc.l	samb,samc,samd,samf,-1

world6blobs	dc.l	toys,boss6,lev1stuf
		dc.l	sam0,sam1,sam2,sam3,sam4
		dc.l	sam5,sam6,sam7,sam8,sam9
		dc.l	samb,samc,samd,samf,sama,sam11,sam10,-1

world7blobs	dc.l	fair1,fair2,boss7,lev1stuf
		dc.l	sam0,sam1,sam2,sam3,sam4
		dc.l	sam5,sam6,sam7,sam8,sam9
		dc.l	samb,samc,samd,samf,sama,sam11,-1


thingstodo	dc.l	bumble,bumblen,4098
ttd2		dc.l	allsort,allsortn,21604
		dc.l	sloper,slopern,6772
		dc.l	chuptop,chuptopn,1200
		dc.l	lifts,liftsn,1830
		dc.l	wallslob,wallslobn,2676
		dc.l	greener,greenern,1732
		dc.l	cherry,cherryn,590
		dc.l	holes,holesn,6282
		dc.l	bounce,bouncen,1122
		dc.l	heart,heartn,282
		dc.l	zoolblob,zoolblobn,4300
		dc.l	eolg1,eolg1n,6044
		dc.l	binary,binaryn,1150
		dc.l	saw,sawn,2102
		dc.l	drills,drillsn,5428
		dc.l	woodplat,woodplatn,444
		dc.l	balls,ballsn,2404
		dc.l	chainsaw,chainsawn,4210
		dc.l	acme,acmen,4692
		dc.l	switch,switchn,1242
		dc.l	switch2,switch2n,394
		dc.l	chris,chrisn,4392
		dc.l	flames,flamesn,5686
		dc.l	swiv,swivn,2326
		dc.l	keys,keysn,2172
		dc.l	muzakcat,muzakcatn,5000
		dc.l	dstick,dstickn,3664
		dc.l	notes,notesn,1038
		dc.l	cybil,cybiln,6184
		dc.l	cello,cellon,7724
		dc.l	drum,drumn,9348
		dc.l	basswob,basswobn,640
		dc.l	fuck,fuckn,5176
		dc.l	spark,sparkn,996
		dc.l	bell,belln,5792
		dc.l	deck,deckn,2522
		dc.l	smnotes,smnotesn,2684
		dc.l	elecbod,elecbodn,2508
		dc.l	amp,ampn,2718
		dc.l	mlift,mliftn,926
		dc.l	zoolship,zoolshipn,3032	
		dc.l	jif,jifn,1514
		dc.l	overlays,overlaysn,2054
		dc.l	mana,manan,17264
		dc.l	croc,crocn,7016
		dc.l	holes2,holes2n,6282
		dc.l	fair1,fair1n,34694
		dc.l	fair2,fair2n,12722
		dc.l	toys,toysn,39246
		dc.l	boss2,boss2n,7674
		dc.l	mboss,mbossn,7352
		dc.l	boss4,boss4n,9558
		dc.l	boss7,boss7n,6008
		dc.l	boss6,boss6n,11042
		dc.l	cells,cellsn,9334
		dc.l	cells2,cells2n,22872
		dc.l	bangs,bangsn,11938
		dc.l	lev1stuf,lev1stufn,15486

		dc.l	sam0,sn0,8982
		dc.l	sam1,sn1,8674
		dc.l	sam2,sn2,5892
		dc.l	sam3,sn3,1150
		dc.l	sam4,sn4,2846
		dc.l	sam5,sn5,5380
		dc.l	sam6,sn6,7448
		dc.l	sam7,sn7,550
		dc.l	sam8,sn8,5988
		dc.l	sam9,sn9,6588
		dc.l	sama,sna,3138
		dc.l	samb,snb,1474
		dc.l	samc,snc,646
		dc.l	samd,snd,1650
		dc.l	same,sne,5112
		dc.l	samf,snf,3374
		dc.l	sam10,sn10,6048
		dc.l	sam11,sn11,8256

		dc.l	-1

chuptopn	dc.b	'chuptop.chm',0
sawn		dc.b	'saw.chm',0
drillsn		dc.b	'drills.chm',0
woodplatn	dc.b	'woodplat.chm',0
ballsn		dc.b	'balls.chm',0
chainsawn	dc.b	'chainsaw.chm',0
acmen		dc.b	'acme.chm',0
switchn		dc.b	'switch.chm',0
switch2n	dc.b	'switch2.chm',0
chrisn		dc.b	'chris.chm',0
flamesn		dc.b	'flames.chm',0
swivn		dc.b	'swiv.chm',0
allsortn	dc.b	'allsort.chm',0
bumblen		dc.b	'bumble.chm',0
slopern		dc.b	'sloper.chm',0
liftsn		dc.b	'lifts.chm',0
wallslobn	dc.b	'wallslob.chm',0
greenern	dc.b	'greener.chm',0
cherryn		dc.b	'cherry.chm',0
holesn		dc.b	'holes.chm',0
bouncen		dc.b	'bounce.chm',0
heartn		dc.b	'heart.chm',0
zoolblobn	dc.b	'zoolblob.chm',0
eolg1n		dc.b	'eolg1.chm',0
binaryn		dc.b	'binary.chm',0
lev1stufn	dc.b	'lev'
levworldn	dc.b	'1stuf.chm',0
keysn		dc.b	'keys.chm',0
muzakcatn	dc.b	'muzakcat.chm',0
dstickn		dc.b	'dstick.chm',0
notesn		dc.b	'notes.chm',0
cybiln		dc.b	'cybil.chm',0
cellon		dc.b	'cello.chm',0
drumn		dc.b	'drum.chm',0
basswobn	dc.b	'basswob.chm',0
fuckn		dc.b	'fuck.chm',0
sparkn		dc.b	'spark.chm',0
belln		dc.b	'bell.chm',0
deckn		dc.b	'deck.chm',0
smnotesn	dc.b	'smnotes.chm',0
elecbodn	dc.b	'elecbod.chm',0
ampn		dc.b	'amp.chm',0
mliftn		dc.b	'mlift.chm',0
jifn		dc.b	'jif.chm',0
overlaysn	dc.b	'overlays.chm',0
manan		dc.b	'mana.chm',0
crocn		dc.b	'croc.chm',0
holes2n		dc.b	'holes2.chm',0
zoolshipn	dc.b	'zoolship.spr',0
fair1n		dc.b	'fair1.chm',0
fair2n		dc.b	'fair2.chm',0
toysn		dc.b	'toys.chm',0
boss2n		dc.b	'boss2.chm',0
mbossn		dc.b	'mboss.chm',0
boss4n		dc.b	'boss4.chm',0
boss7n		dc.b	'boss7.chm',0
boss6n		dc.b	'boss6.chm',0
cellsn		dc.b	'cells.chm',0
cells2n		dc.b	'cells2.chm',0
bangsn		dc.b	'bangs.chm',0

sn0		dc.b	'zooldyin.sam',0
sn1		dc.b	'aliendie.sam',0
sn2		dc.b	'jump____.sam',0
sn3		dc.b	'alienfir.sam',0
sn4		dc.b	'ouch2___.sam',0
sn5		dc.b	'pickup1_.sam',0
sn6		dc.b	'powerup_.sam',0
sn7		dc.b	'punchbut.sam',0
sn8		dc.b	'skid1___.sam',0
sn9		dc.b	'slide2__.sam',0
sna		dc.b	'spring1_.sam',0
snb		dc.b	'swipe1__.sam',0
snc		dc.b	'footstep.sam',0
snd		dc.b	'heart_be.sam',0
sne		dc.b	'piano.sam',0
snf		dc.b	'laser2__.sam',0
sn10		dc.b	'jellycas.sam',0
sn11		dc.b	'restartm.sam',0

allsort		dc.l	0
bumble		dc.l	0
sloper		dc.l	0
lifts		dc.l	0
wallslob	dc.l	0
greener		dc.l	0
cherry		dc.l	0
holes		dc.l	0
bounce		dc.l	0
heart		dc.l	0
zoolblob	dc.l	0
eolg1		dc.l	0
binary		dc.l	0
lev1stuf	dc.l	0
saw		dc.l	0
drills		dc.l	0
woodplat	dc.l	0
balls		dc.l	0
chainsaw	dc.l	0
acme		dc.l	0
switch		dc.l	0
switch2		dc.l	0
chris		dc.l	0
flames		dc.l	0
swiv		dc.l	0
keys		dc.l	0
muzakcat	dc.l	0
dstick		dc.l	0
notes		dc.l	0
cybil		dc.l	0
cello		dc.l	0
drum		dc.l	0
basswob		dc.l	0
fuck		dc.l	0
spark		dc.l	0
bell		dc.l	0
deck		dc.l	0
smnotes		dc.l	0
elecbod		dc.l	0
amp		dc.l	0
mlift		dc.l	0
jif		dc.l	0
overlays	dc.l	0
mana		dc.l	0
croc		dc.l	0
holes2		dc.l	0
fair1		dc.l	0
fair2		dc.l	0  
toys		dc.l	0
boss2		dc.l	0
mboss		dc.l	0
boss4		dc.l	0
boss7		dc.l	0
boss6		dc.l	0
cells		dc.l	0
cells2		dc.l	0
bangs		dc.l	0
chuptop		dc.l	0

blobs2		dc.l	0

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
		
sam0		dc.l	0
		dc.w	$100,64
		dc.b	$11,0
sam1		dc.l	0
		dc.w	$100,64
		dc.b	$10,0
sam2		dc.l	0
		dc.w	$100,40
		dc.b	$c,0
sam3		dc.l	0			alien fire ...
		dc.w	$200,64
		dc.b	$4,0
sam4		dc.l	0
		dc.w	$100,64
		dc.b	$7,0
sam5		dc.l	0
		dc.w	$100,64
		dc.b	$10,0
sam6		dc.l	0
		dc.w	$100,64
		dc.b	$17,0
sam7		dc.l	0
		dc.w	$100,64
		dc.b	$3,0
sam8		dc.l	0
		dc.w	$100,64
		dc.b	$13,0
sam9		dc.l	0
		dc.w	$100,64
		dc.b	$14,0
sama		dc.l	0
		dc.w	$100,64
		dc.b	$8,0
samb		dc.l	0
		dc.w	$100,44
		dc.b	$8,0
samc		dc.l	0
		dc.w	$100,30
		dc.b	$4,0
samd		dc.l	0
		dc.w	$200,64
		dc.b	$9,0
same		dc.l	0
same23		dc.w	$100,64
		dc.b	$8,0
samf		dc.l	0
		dc.w	$100,64
		dc.b	$8,0
sam10		dc.l	0			jelly cas ...
		dc.w	$200,64
		dc.b	$9,0
sam11		dc.l	0			restart m ...
		dc.w	$100,64
		dc.b	$1d,0
sam12

*----------------------------------------------------------------------------*
*----------------------------------------------------------------------------*
		
scoline		incbin	d:\graphics\scoline.lo2

zoolpicn	dc.b	'zoolpic.chm',0		incbin	d:\graphics\zoolpic.chm
compn		dc.b	'comp.chm',0		incbin	d:\graphics\comp.chm

onrails		dc.b	0

nopm		nop
speed		dc.w	3
onealevel	dc.b	0
currentnote	dc.b	0,$80
tune1		dc.b	0,3,1,2,4,$ff,$80
tune2		dc.b	0,2,4,6,$ff,$80
tune3		dc.b	1,3,5,6,$ff,$80
tune4		dc.b	8,9,7,0,4,$ff,$80
noscr		dc.b	0	
xlift2		dc.w	0		For dstick.	
ylift2		dc.w	0		For dstick.	
onstick		dc.b	0
trumpeton	dc.b	0
drumset		dc.b	0
recordmom	dc.l	0
zambo		dc.b	0
bellm		dc.b	0
mbossxy		dc.l	0
grapeamount	dc.b	0
scrollfrac	dc.l	0
scrollfrac2	dc.l	0
radno		dc.b	0
chrplace	dc.l	0
maps		dc.l	0
pressed		dc.b	0
minibullet	dc.l	0
minizool	dc.l	0
minifired 	dc.l	0
message		dc.b	0
spinc		dc.b	0

arrows2	   	incbin	d:\graphics\arrows.lo1

weapon		dc.b	5

gamemode	dc.b	0
bcount		dc.b	0
fired		dc.l	0
lastbullet	dc.l	0
bulspr		dc.l	0
convx		dc.w	0
sks		dc.b	0
alarm		dc.b	0
slidesound	dc.b	0
dragfactor	dc.b	0
scrollspeed	dc.w	0
catmem		ds.l	5*13*4
paused		dc.b	0
ping		dc.b	0
hunds		dc.b	0
donebonus	dc.b	0
		even	 
sawpls		incbin	d:\various\sawoffs.dat

promess		dc.w	8,10
pronos		dc.b	'         ',0,0,0,0,0,0
		dc.b	' PLEASE ENTER THE',0
		dc.b	'CODE FOR WINDOW '
nopo		dc.b	'34',$ff
codepl		dc.w	132-8,195
		dc.b	'...',$ff
		
pushfire	dc.w	0,108
		dc.b	' PLEASE PRESS FIRE',$ff

xhitmom		dc.l	0
yhitmom		dc.l	0
arowfrs		dc.b	0,6,2,1,0,7,1,1,4,5,3
stophorz	dc.b	0
aldone		dc.b	0

ds1		dc.l	0,-15,-114,1,-6,-83,1,-6,-69,1,-6,-56,1,-6,-42,1,-6,-28,1,-6,-15
ds2		dc.l	0,-12,-114,1,-5,-84,1,-5,-72,1,-6,-57,1,-6,-43,1,-6,-29,1,-6,-15
		dc.l	0,-9,-114,1,-3,-84,1,-4,-71,1,-5,-56,1,-5,-42,1,-6,-29,1,-6,-15
		dc.l	0,-7,-114,1,-2,-83,1,-3,-70,1,-4,-56,1,-5,-42,1,-6,-28,1,-6,-15
		dc.l	0,-4,-113,2,1,-82,2,-1,-70,1,-4,-57,1,-5,-43,1,-6,-29,1,-6,-15
		dc.l	0,-1,-113,2,2,-83,2,-1,-68,1,-4,-54,1,-5,-41,1,-6,-28,1,-6,-15
		dc.l	0,2,-112,2,5,-83,2,2,-72,2,-1,-57,1,-4,-43,1,-5,-29,1,-6,-15
		dc.l	0,5,-111,3,5,-82,2,4,-73,2,1,-58,2,-2,-43,1,-5,-29,1,-6,-15
		dc.l	0,7,-106,3,7,-81,3,3,-71,2,1,-57,2,-2,-43,1,-5,-29,1,-6,-15
		dc.l	0,10,-109,3,10,-81,3,5,-71,2,2,-56,2,-1,-43,2,-3,-28,1,-6,-15
		dc.l	0,12,-108,3,12,-80,3,7,-71,3,3,-59,2,0,-44,2,-3,-29,1,-6,-15
		dc.l	0,15,-107,4,14,-79,3,7,-68,3,2,-55,2,0,-41,2,-3,-27,1,-6,-15
		dc.l	0,17,-106,4,16,-78,3,10,-68,3,5,-55,3,0,-41,2,-2,-27,1,-6,-15
		dc.l	0,20,-104,4,18,-76,4,14,-69,3,7,-58,3,1,-44,2,-1,-30,2,-3,-15
		dc.l	0,22,-103,4,19,-76,4,14,-67,3,7,-55,3,2,-43,2,0,-29,2,-3,-15
		dc.l	0,24,-101,5,19,-76,4,15,-67,3,9,-56,3,2,-43,2,0,-28,2,-3,-15
		dc.l	0,27,-99,5,21,-74,4,14,-64,4,7,-53,3,1,-42,2,0,-29,2,-3,-15
		dc.l	0,29,-97,5,23,-73,5,13,-63,4,7,-52,3,2,-43,2,0,-28,2,-3,-15
		dc.l	0,31,-95,6,24,-73,4,16,-63,4,8,-53,3,2,-44,2,0,-30,2,-3,-15
		dc.l	0,33,-93,6,27,-71,5,17,-63,4,10,-52,3,3,-41,2,1,-29,2,-3,-15
		dc.l	0,35,-91,6,27,-70,5,17,-63,4,11,-51,3,3,-41,2,1,-29,2,-3,-15
		dc.l	0,36,-89,6,28,-67,5,18,-60,5,10,-51,3,3,-41,2,1,-29,2,-3,-15
		dc.l	0,38,-86,6,29,-65,5,20,-59,5,12,-51,4,5,-40,3,0,-30,2,-3,-15
		dc.l	0,40,-84,6,30,-64,5,21,-57,5,12,-49,4,5,-38,3,0,-30,2,-3,-15
		dc.l	0,41,-81,6,33,-61,5,23,-56,5,16,-50,4,8,-39,3,1,-30,2,-3,-15
		dc.l	0,42,-79,7,34,-59,6,25,-54,5,15,-47,4,7,-35,3,0,-26,2,-3,-15
		dc.l	0,44,-76,7,34,-58,6,26,-53,5,16,-47,5,9,-39,3,1,-28,2,-3,-15
		dc.l	0,45,-74,7,35,-56,6,26,-51,5,16,-45,4,9,-35,4,2,-24,2,-3,-15
		dc.l	0,46,-71,8,35,-54,6,28,-52,5,18,-44,4,10,-34,3,1,-24,2,-3,-15
		dc.l	0,46,-69,8,35,-53,7,29,-52,5,20,-46,5,11,-37,4,5,-26,3,-3,-15
		dc.l	0,47,-66,8,34,-52,6,29,-51,5,18,-45,5,11,-36,4,4,-25,3,-3,-15
		dc.l	0,48,-63,8,34,-50,6,27,-49,5,18,-43,5,11,-35,5,4,-27,3,-3,-15  
		even
sky		; Blue.

		dc.w	$0ff
		dc.w	$0ef
		dc.w	$0df
		dc.w	$0cf
		dc.w	$0bf
		dc.w	$0af
		dc.w	$09f
		dc.w	$08f
		dc.w	$07f
		dc.w	$06f
		dc.w	$05f
		dc.w	$04f
		dc.w	$03f
		dc.w	$02f
		dc.w	$01f
		dc.w	$00f
		dc.w	$00e
		dc.w	$00d
		dc.w	$00c
		dc.w	$00b
		dc.w	$00a
		dc.w	$009
		dc.w	$008
		dc.w	$007
		dc.w	$008
		dc.w	$009
		dc.w	$00a
		dc.w	$00b
		dc.w	$00c
		dc.w	$00d
		dc.w	$01e
		dc.w	$02f
		dc.w	$03f
		dc.w	$04f
		dc.w	$05f
		dc.w	$06f
		dc.w	$07f
		dc.w	$08f
		dc.w	$09f
		dc.w	$0af
		dc.w	$0bf
		dc.w	$0cf
		dc.w	$0df
		dc.w	$0ef
		dc.w	$0ff
		dc.w	$0ef
		dc.w	$0df
		dc.w	$0cf
		dc.w	$0bf
		dc.w	$0af
		dc.w	$09f
		dc.w	$08f
		dc.w	$07f
		dc.w	$06f
		dc.w	$05f
		dc.w	$04f
		dc.w	$03f
		dc.w	$02f
		dc.w	$01f
		dc.w	$00f
		dc.w	$00e
		dc.w	$00d
		dc.w	$00c
		dc.w	$00b
		dc.w	$00a
		dc.w	$009
		dc.w	$008
		dc.w	$007
		dc.w	$008
		dc.w	$009
		dc.w	$00a
		dc.w	$00b
		dc.w	$00c
		dc.w	$00d
		dc.w	$01e
		dc.w	$02f
		dc.w	$03f
		dc.w	$04f
		dc.w	$05f
		dc.w	$06f
		dc.w	$07f
		dc.w	$08f
		dc.w	$09f
		dc.w	$0af
		dc.w	$0bf
		dc.w	$0cf
		dc.w	$0df
		dc.w	$0ef
		dc.w	$0ff
		dc.w	$0ef
		dc.w	$0df
		dc.w	$0cf
		dc.w	$0bf
		dc.w	$0af
		dc.w	$09f
		dc.w	$08f
		dc.w	$07f
		dc.w	$06f
		dc.w	$05f
		dc.w	$04f
		dc.w	$03f
		dc.w	$02f
		dc.w	$01f
		dc.w	$00f
		dc.w	$00e
		dc.w	$00d
		dc.w	$00c
		dc.w	$00b
		dc.w	$00a
		dc.w	$009
		dc.w	$008
		dc.w	$007
		dc.w	$008
		dc.w	$009
		dc.w	$00a
		dc.w	$00b
		dc.w	$00c
		dc.w	$00d
		dc.w	-1
sky3		; Red.
		dc.w	00
		dc.w	$0
		dc.w	$f0f
		dc.w	$f0e
		dc.w	$f0d
		dc.w	$f0c
		dc.w	$f0b
		dc.w	$f0a
		dc.w	$f09
		dc.w	$f08
		dc.w	$f07
		dc.w	$f06
		dc.w	$f05
		dc.w	$f04
		dc.w	$f03
		dc.w	$f02
		dc.w	$f01
		dc.w	$f00
		dc.w	$f01
		dc.w	$f02
		dc.w	$f03
		dc.w	$f04
		dc.w	$f05
		dc.w	$f06
		dc.w	$f07
		dc.w	$f08
		dc.w	$f09
		dc.w	$f0a
		dc.w	$f0b
		dc.w	$f0c
		dc.w	$f0d
		dc.w	$f0e
		dc.w	$f0f
		dc.w	$f0e
		dc.w	$f0d
		dc.w	$f0c
		dc.w	$f0b
		dc.w	$f0a
		dc.w	$f09
		dc.w	$f08
		dc.w	$f07
		dc.w	$f06
		dc.w	$f05
		dc.w	$f04
		dc.w	$f03
		dc.w	$f02
		dc.w	$f01
		dc.w	$f00
		dc.w	$f01
		dc.w	$f02
		dc.w	$f03
		dc.w	$f04
		dc.w	$f05
		dc.w	$f06
		dc.w	$f07
		dc.w	$f08
		dc.w	$f09
		dc.w	$f0a
		dc.w	$f0b
		dc.w	$f0c
		dc.w	$f0d
		dc.w	$f0e
		dc.w	$f0f
		dc.w	$f0e
		dc.w	$f0d
		dc.w	$f0c
		dc.w	$f0b
		dc.w	$f0a
		dc.w	$f09
		dc.w	$f08
		dc.w	$f07
		dc.w	$f06
		dc.w	$f05
		dc.w	$f04
		dc.w	$f03
		dc.w	$f02
		dc.w	$f01
		dc.w	$f00
		dc.w	$f01
		dc.w	$f02
		dc.w	$f03
		dc.w	$f04
		dc.w	$f05
		dc.w	$f06
		dc.w	$f07
		dc.w	$f08
		dc.w	$f09
		dc.w	$f0a
		dc.w	$f0b
		dc.w	$f0c
		dc.w	$f0d
		dc.w	$f0e
		dc.w	$f0f
		dc.w	$f0f
		dc.w	-1

blocks		ds.b	32800	incbin	d:\graphics\blocks1.bgx

blobs		dc.l	0		Nothing Beyond this point ...
realend		even

zoolptec	incbin	d:\graphics\zoolptec.lo4

exstart		; Extra mem stuff (copied at start).
mapdat		ds.b	55012	incbin	d:\maps\mapa1.map	Copied at start ...
chrset		incbin	d:\chmd1\chrs.chm
zoolrunspr	incbin	d:\graphics\zoolrun.spr
zoolpic		incbin	d:\graphics\zoolpic.chm
exend 		; End of extra mem stuff.

end		even

