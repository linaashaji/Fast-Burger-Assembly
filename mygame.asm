X          equ     $E020        X coordinate
Y          equ     $E022        Y coordinate
yman       equ     $4000

mouseX     equ     $E026
mouseY     equ     $E028
mouseflags equ     $E02A

dDELAY3     equ    90000000
DELAY      equ     110000
rd1        equ     2500
rd2        equ     6000

SOUND      equ     $E031
plus       equ     $8000

time1      equ     $4010
time2      equ     $4020

score      equ     $4030
score2     equ     $4040

tfries     equ     $4050
ls         equ     $4060

* initialize vectors in lower memory

           org     $0
           dc.l    stack              initial SP
           dc.l    start


           org     $0040
           dc.l    timerproc          timer interrupt routine



           org     $0400

start:
           trap     #15
           dc.w     36

* program begins here
startgame
           move.w #$0000,$E024    to erease background
           move    #$2400,SR

           jsr    form1

           lea    $5001,a1        a1 init
           lea    $6000,a3        a3 init


           move.b #$03,(a3)
           move.b #00,plus


rloop      TST    (a3)
           beq     repeat1
           jsr     rand
           jsr     rdelay
           SUBI.b  #$1,(a3)
           bra     rloop

repeat1
           btst        #0,mouseflags  ; check bit 0 = left button down
           bne.s       left_dwn1
           bra         repeat1

left_dwn1  move.w      mouseX,D5
           move.w      mouseY,d6
           clr.b       mouseflags
           CMP         #$01C0,d5
           bmi         repeat1
           CMP         #$02a0,d5
           bpl         repeat1
           CMP         #$0100,d6
           bmi         repeat1
           CMP         #$0150,d6
           bpl         repeat1
           bra         form2
           bra         repeat1




form2      move.w #$0000,$E024
           CLR    D1
           CLR    D2
           CLR    D3
           CLR    D4
           CLR    D5
           CLR    D6
           move.w #$00A2,yman          Y MAN   (30)
           move.w #$0055,D1            X FOOD
           move.w #$01F4,D3            Y FOOD
           move.b #$04,D6
           move.w #$014E,D2            number of steps (X MAN)




qq         Lea     food1,a0
           move.w  #$21,d7
           jsr     myfood1
           Lea     food,a0
           move.w  #$5,d7
           jsr     myfood
           jsr     rdraw


           jsr     inita


           CMP.b   #01,tfries       draw fries if 1
           BNE     q2
           move.w  #$00A0,D1
           move.w  #$0050,D3
           Lea     fries,a0
           move.w  #$5,d7
           jsr     myfood1


q2         move.w  #$0055,D1
           move.w  #$01F4,D3
           jsr     CMAN




           move.w  #$B547,time1
           move.w  #$B147,time2

           move.w  #$08,$E020
           move.w  #$08,$E022
           move.b  #$80,$E025

           move.w  #$10,$E020
           move.w  #$10,$E022
           move.w  #$d447,$E024   T
           move.w  #$C94F,$E024   i
           move.w  #$CD4F,$E024   m
           move.w  #$C54F,$E024   e
           move.w  #$ba4F,$E024   :
           move.w  time2,$E024
           move.w  time1,$E024

           move.w  #$B047,score
           move.w  #$B047,score2
           move.w  #$150,$E020
           move.w  #$08,$E022
           move.b  #$80,$E025

           move.w  #$200,$E020
           move.w  #$10,$E022
           move.w  #$d34F,$E024   S
           move.w  #$C34F,$E024   c
           move.w  #$CF4F,$E024   o
           move.w  #$d24F,$E024   R
           move.w  #$C54F,$E024   e
           move.w  #$bA4F,$E024   :

           move.w  score,$E024
           move.w  score2,$E024

repeat     move.w  #$08,$E020
           move.w  #$08,$E022
           move.b  #$80,$E025

           move.w  #$10,$E020
           move.w  #$10,$E022
           move.w  #$d447,$E024   T
           move.w  #$C94F,$E024   i
           move.w  #$CD4F,$E024   m
           move.w  #$C54F,$E024   e
           move.w  #$bA4F,$E024   :
           move.w  time2,$E024
           move.w  time1,$E024

           CMP.b  #01,ls
           beq    tt1
           CMP    #$B547,time1
           bne    tt1
           CMP    #$B047,time2
           bne    tt1
           move.l #late,SOUND+1
           move.b #5,SOUND
           move.b #01,ls


tt1        CMP     #$B047,time1
           bne     continu
           CMP     #$B047,time2
           BEQ     loser
continu    TST         (a3)
           beq         next
           btst        #0,mouseflags  ; check bit 0 = left button down
           bne.s       left_dwn
           bra         repeat

left_dwn   move.w      mouseX,D5
           clr.b       mouseflags
           CMP         D5,D1
           BPL         repeat
           move.w      D5,D0
           SUB.w       #$02D0,D0
           CMP         D0,D1
           BMI         repeat



           MOVE.w      D5,D0
           SUB.w       #$0073,D0
           CMP         D0,D1
           BPL         P1

           MOVE.w      D5,D0
           SUB.w       #$00EC,D0
           CMP         D0,D1
           BPL         P2

           MOVE.w      D5,D0
           SUB.w       #$0164,D0
           CMP         D0,D1
           BPL         P3

           MOVE.w      D5,D0
           SUB.w       #$01DC,D0
           CMP         D0,D1
           BPL         P4

           MOVE.w      D5,D0
           SUB.w       #$0254,D0
           CMP         D0,D1
           BPL         P5

           MOVE.w      D5,D0
           SUB.w       #$02D0,D0
           CMP         D0,D1
           BPL         P6








P1         CMP        #$01,D6
           BEQ        same
           move.w     #$00,D5
           jsr        C1
           move.w     #$0001,D6
           move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat




P2         CMP        #$02,D6
           BEQ        same
           move.w     #$007E,D5

           CMP        #$02,D6
           BMI        K2
           jsr        C1
           move.w     #$0002,D6
           move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat
K2         jsr        C2
           move.w     #$0002,D6
           move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat


P3         CMP        #$03,D6
           BEQ        same
           move.w     #$00F1,D5

           CMP        #$03,D6
           BMI        K3
           jsr        C1
           move.w     #$0003,D6
           move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat
K3         jsr        C2
           move.w     #$0003,D6
           move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat

P4         CMP        #$04,D6
           BEQ        same
           move.w     #$0164,D5

           CMP        #$04,D6
           BMI        K4
           jsr        C1
           move.w     #$0004,D6
           move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat
K4         jsr        C2
           move.w     #$0004,D6
           move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat

P5         CMP        #$05,D6
           BEQ        same
           move.w     #$01D7,D5

           CMP        #$05,D6
           BMI        K5
           jsr        C1
           move.w     #$0005,D6
           move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat
K5         jsr        C2
           move.w     #$0005,D6
           move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat

P6         CMP        #$06,D6
           BEQ        same
           move.w     #$024A,D5

           CMP        #$06,D6
           BMI        K6
           jsr        C1
           move.w     #$0006,D6
           move.b     (a1),d0
           jsr        tloser
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat
K6         jsr        C2
           move.w     #$0006,D6
           move.b     (a1),d0
           jsr        tloser
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat







C1         CMP        D2,D5
           BPL        S1
           jsr        DMAN
           jsr        delay1
           SUB.w      #$0008,D2
           jsr        CMAN
           jsr        delay2
           bra        C1
S1         rts

C2         CMP        D2,D5
           BMI        S2
           jsr        DMAN
           jsr        delay1
           add.w      #$0008,D2
           jsr        CMAN
           jsr        delay2
           bra        C2
S2         rts

same       move.b     (a1),d0
           jsr        tloser
           jsr        rand
           move.l     #onemove,SOUND+1
           move.b     #5,SOUND
           bra        repeat

CMAN
*head
           move.w  #$0079,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00A0,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00B5,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00E0,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$2661,$E024
*eyes
           move.w  #$0085,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00A0,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$0092,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00C5,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$1001,$E024
*eyes2
           move.w  #$0098,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00A0,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00A5,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00C5,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$1001,$E024
*casq
*rrec
           move.w  #$0070,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0045,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00C0,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00A0,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$3771,$E024
*cir1
           move.w  #$0060,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0030,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00A0,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0070,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$1771,$E024

*cir2
           move.w  #$0090,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0030,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00D0,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0070,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$1771,$E024
*tyeb1
           move.w  #$0070,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0115,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00C4,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0150,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$2441,$E024
*tyeb2
           move.w  #$0040,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00E0,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$0087,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0115,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$2441,$E024


           move.w  #$00A7,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00E0,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00F5,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0115,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$2441,$E024



*tyeb3
           move.w  #$0087,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00E0,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00A7,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0150,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$2771,$E024

*cir1
           move.w  #$0090,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$00F0,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00A0,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0100,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$1001,$E024

*cir2
           move.w  #$0090,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0110,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00A0,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0120,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$1001,$E024

*cir3
           move.w  #$0090,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0130,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00A0,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0140,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$1001,$E024

           rts


DMAN
           move.w  #$0040,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0030,D0
           add     yman,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$00F5,D0
           add     D2,D0
           move.w  D0,X
           move.w  #$0150,D0
           add     yman,D0
           move.w  D0,Y
           move.w  #$2001,$E024
           rts

delay1     move.l  #DELAY,D0    delay loop 1
del        sub.w   #$012c,D0
           bne     del
           rts


delay2     move.l  #DELAY,D0    delay loop 2
dell       subq.l  #1,D0
           bne     dell
           rts

delay3     move.l  #dDELAY3,D0    delay loop 2
dell3      subq.l  #1,D0
           bne     dell3
           rts

rdelay     move.l  #rd1,D0
rdel       sub.w   #1,d0
           beq     fin
           move.l  #rd2,d5
rdel2      sub.w   #$1,d5
           bne     rdel2
           bra     rdel
fin        rts

next       move.b  #00,ls
           move.l  #thankyou,SOUND+1
           move.b  #5,SOUND
           move.w  #$B547,time1
           move.w  #$B147,time2
           CMP.b   #01,tfries
           BNE     N3
           move.b  tfries,d0
           sub.b   d0,(a3)

N3         CMP     #$B847,score2
           BNE     n2
           move.w  #$B047,score2
           add.w   #$100,score
           sub.w   #$200,score2

n2         add.w   #$200,score2
           move.w  #$150,$E020
           move.w  #$08,$E022
           move.b  #$80,$E025

           move.w  #$200,$E020
           move.w  #$10,$E022
           move.w  #$d34F,$E024   S
           move.w  #$C34F,$E024   c
           move.w  #$CF4F,$E024   o
           move.w  #$d24F,$E024   R
           move.w  #$C54F,$E024   e
           move.w  #$ba4F,$E024   :

           move.w  score,$E024
           move.w  score2,$E024

           Lea     blank2,a0
           move.w  #$0300,D1
           move.w  #$003c,D3
           move.w  #$1,d7
           jsr     myfood1

           Lea     blank,a0
           move.w  #$0370,D1
           move.w  #$003c,D3
           move.w  #$1,d7
           jsr     myfood1
           lea     $5001,a1
           jsr     rdraw
           jsr     inita
           CMP.b   #01,tfries       draw fries if 1
           BNE     q3
           move.w  #$00A0,D1
           move.w  #$0050,D3
           Lea     fries,a0
           move.w  #$5,d7
           jsr     myfood1

q3         CMP.b   #00,tfries
           bne     q4
           move.l  #nofries,SOUND+1
           move.b  #5,SOUND
q4         move.w #$0055,D1            X FOOD
           move.w #$01F4,D3

           bra     repeat

inita      CLR     d5
           CLR     d0
           move.b  plus,d0
           move.b  #$03,(a3)
           add.b   d0,(a3)
           move.b  (a3),d5
           add.b   #$02,(a3)
           lea     $5000,a1
           move.b  #$01,(a1)
           move.b  #$01,$01(a1,d5.w)
           lea     $5000,a1
           jsr     frand
           CMP.b   #01,tfries
           BNE     ff
           CLR     d5
           move.b  (a3),d5
           sub.b   #01,d5
           lea     $5000,a1
           move.b  #$06,$01(a1,d5.w)
           move.b  tfries,d0
           add.b   d0,(a3)
           lea     $5000,a1

ff         CMP.b   #$08,plus
           BEQ     inrts
           add.b   #01,plus
inrts      rts

tloser     CMP.b      d6,d0
           bne        loser
           SUBI.b     #01,(a3)
           rts


loser      move.l  #lost,SOUND+1
           move.b  #5,SOUND
           jsr     delay3
           bra     startgame

myfood1    tst     d7
           beq     l
           move.w  (a0)+,D0
           add     D1,D0
           move.w  D0,X
           move.w  (a0)+,D0
           add     D3,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  (a0)+,D0
           add     D1,D0
           move.w  D0,X
           move.w  (a0)+,D0
           add     D3,D0
           move.w  D0,Y
           move.w  (a0)+,$E024
           sub     #1,d7
           bra     myfood1
l          rts

myfood     tst     d7
           beq     l2
           move.w  (a0)+,D0
           add     D1,D0
           move.w  D0,X
           move.w  #$0000,D0
           add     D3,D0
           move.w  D0,Y
           move.b  #$80,$E025

           move.w  #$0064,D0
           add     D3,D0
           move.w  D0,Y
           move.w  #$0075,$E024

           sub     #1,d7
           bra     myfood
l2         rts



rand       CLR.L       D0
           TRAP        #15
           DC.W        40
           move        d0,d5
           AND.L       #$3FFFF,D0    ;prevent overflow in divu
           DIVU        #4,D0
           SWAP        D0
           ADDQ.W      #2,D0
           MOVE.b      D0,(a1)+
           rts

frand      CLR.L       D0
           TRAP        #15
           DC.W        40
           move        d0,d5
           AND.L       #$1FFFF,D0    ;prevent overflow in divu
           DIVU        #3,D0
           SWAP        D0
           CMP.b       #01,D0
           BNE         tf1
           bra         tf2
tf3        rts
tf1        move.b      #0,tfries
           bra         tf3
tf2        move.b      #01,tfries
           bra         tf3



rdraw      CLR     d0
           move.b  plus,d0
           move.b  #$03,(a3)
           add.b   d0,(a3)
           Lea     bread2,a0
           move.w  #$0380,D1
           move.w  #$003c,D3
           move.w  #$07,d7
           jsr     myfood1
           Lea     bread3,a0
           move.b  (a3),D3
           mulu    #$001E,d3
           add.w   #$003c,D3
           move.w  #$07,d7
           jsr     myfood1
           move.w  #$003c,D3
           lea     $5001,a1
rtest      TST     (a3)
           BEQ     rfin
           CMP.b   #02,(a1)
           bne     t2
           jsr     mycheese
t2         CMP.b   #03,(a1)
           bne     t3
           jsr     mybeef
t3         CMP.b   #04,(a1)
           bne     t4
           jsr     mychicken
t4         CMP.b   #05,(a1)+
           bne     t5
           jsr     mycucumber
t5         SUBI.b  #$1,(a3)
           bra     rtest
rfin       move.w #$0055,D1
           move.w #$01F4,D3

           rts


mycheese   Lea     cheese2,a0
           add.w   #$001E,D3
           move.w  #$06,d7
           jsr     myfood1
           rts

mybeef     Lea     beef2,a0
           add.w   #$001E,D3
           move.w  #$03,d7
           jsr     myfood1
           rts

mychicken  Lea     chicken2,a0
           add.w   #$001E,D3
           move.w  #$04,d7
           jsr     myfood1
           rts

mycucumber Lea     cucumber2,a0
           add.w   #$001E,D3
           move.w  #$04,d7
           jsr     myfood1
           rts


form1      move.l     #back,SOUND+1
           move.b     #5,SOUND
           move.w #$0072,yman          Y MAN   (30)
           move.w #$0035,D1            X FOOD
           move.w #$01F4,D3            Y FOOD
           move.b #$04,D6
           move.w #$005E,D2            number of steps (X MAN)



           move.w  #$150,$E020
           move.w  #$20,$E022
           move.b  #$80,$E025

           move.w  #$190,$E020
           move.w  #$100,$E022
           move.w  #$C64F,$E024   F
           move.w  #$C14F,$E024   A
           move.w  #$d34F,$E024   S
           move.w  #$d44F,$E024   T
           move.w  #$A04F,$E024
           move.w  #$C24F,$E024   B
           move.w  #$d54F,$E024   U
           move.w  #$d24F,$E024   R
           move.w  #$C74F,$E024   G
           move.w  #$C54F,$E024   E
           move.w  #$d24F,$E024   R


           move.w  #$01C0,X
           move.w  #$0100,Y
           move.b  #$80,$E025

           move.w  #$02A0,X
           move.w  #$0150,Y
           move.w  #$204E,$E024

           move.w  #$01F5,X
           move.w  #$010C,Y
           move.b  #$80,$E025

           move.w  #$02A0,X
           move.w  #$0150,Y
           move.w  #$d36c,$E024
           move.w  #$d46C,$E024
           move.w  #$C16C,$E024
           move.w  #$d26C,$E024
           move.w  #$d46C,$E024






           jsr    CMAN


           move.w  #$0050,X
           move.w  #$01C0,Y
           move.b  #$80,$E025

           move.w  #$0370,X
           move.w  #$0600,Y
           move.w  #$2644,$E024

           move.w  #$0100,X
           move.w  #$01F0,Y
           move.b  #$80,$E025

           move.w  #$0150,X
           move.w  #$0230,Y
           move.w  #$1044,$E024


           move.w  #$01A0,X
           move.w  #$01F0,Y
           move.b  #$80,$E025

           move.w  #$01F0,X
           move.w  #$0230,Y
           move.w  #$1044,$E024

           move.w  #$0250,X
           move.w  #$01F0,Y
           move.b  #$80,$E025

           move.w  #$02A0,X
           move.w  #$0230,Y
           move.w  #$1044,$E024



           rts


           org         $3000
food       DC.w        115,236,356,476,596
food1      DC.W        0,0,720,100,8309
bread      DC.W        15,20,100,85,5733
           DC.W        15,63,100,85,8197
           DC.W        30,40,42,43,8193
           DC.W        65,40,80,43,8193
           DC.W        50,32,65,35,8193
           DC.W        35,50,47,53,8193
           DC.W        61,50,76,53,8193
cheese     DC.W        172,17,130,50,105
           DC.W        130,50,172,85,105
           DC.W        172,85,215,50,105
           DC.W        215,50,172,17,105
           DC.W        162,40,170,45,5733
           DC.W        159,59,169,64,5733
           DC.W        182,50,190,55,5733
beef       DC.W        245,10,350,85,6005
           DC.W        252,18,342,78,5189
           DC.W        270,40,290,55,6005
chicken    DC.W        372,20,465,80,5733
           DC.W        390,65,410,30,4
           DC.W        410,65,430,30,4
           DC.W        430,65,450,30,4
cucumber   DC.W        494,19,531,51,4215
           DC.W        495,20,530,50,4645
           DC.W        545,19,585,51,4215
           DC.W        544,20,584,50,4645
           DC.W        510,61,550,90,4215
           DC.W        509,62,549,89,4645
fries      DC.W        630,49,615,25,107
           DC.W        645,49,635,25,107
           DC.W        660,49,665,25,107
           DC.W        678,49,692,25,107
           DC.W        620,50,690,89,9281


bread2     DC.W        0,0,60,60,5733
           DC.W        0,30,60,60,8197
           DC.W        20,10,30,12,8193
           DC.W        40,15,50,17,8193
           DC.W        15,17,25,19,8193
           DC.W        10,22,20,24,8193
           DC.W        45,23,50,25,8193

cheese2    DC.W        5,15,30,1,101
           DC.W        55,15,30,1,101
           DC.W        55,15,30,28,101
           DC.W        5,15,30,28,101
           DC.W        20,10,25,13,5733
           DC.W        30,20,35,22,5733

beef2      DC.W        5,4,55,29,6005
           DC.W        6,7,54,25,5189
           DC.W        15,15,20,18,6005

chicken2   DC.W        5,4,55,29,5733
           DC.W        15,25,20,10,4
           DC.W        30,25,35,10,4
           DC.W        45,25,50,10,4

bread3     DC.W        0,0,60,60,5733
           DC.W        0,0,60,30,8197
           DC.W        20,40,30,42,8193
           DC.W        40,45,50,47,8193
           DC.W        15,47,25,49,8193
           DC.W        10,52,20,54,8193
           DC.W        45,53,50,55,8193

cucumber2  DC.W        5,5,25,15,4215
           DC.W        4,6,26,14,4645
           DC.W        35,18,50,25,4215
           DC.W        34,19,49,26,4645

blank      DC.w        0,0,100,500,8197
blank2     DC.w        0,0,110,145,8197
onemove    dc.b        'onemove.wav',0
thankyou   dc.b        'thankyou.wav',0
nofries    dc.b        'nofries.wav',0
late       dc.b        'late.wav',0
lost       dc.b        'lost.wav',0
back       dc.b        'back.wav',0


* Timer interrupt routine

timerproc  addq.b  #1,d4
           CMP     #$64,d4
           BNE     f
           CLR     d4
           CMP     #$B047,time1
           BNE     f1
           CMP     #$B047,time2
           BEQ     f
           move.w  #$B947,time1
           sub.w   #$100,time2
f          rte
f1         sub.w   #$100,time1
           bra     f

* Stack area

           ds.w    256               stack area, 512 bytes
stack      equ     *