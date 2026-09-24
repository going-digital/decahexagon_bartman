; Disposable-disk diagnostic. No Exec/DOS calls after entry.
        move.l a4,sp
        adda.l #327680,sp
        move.w #$2700,sr
        move.w #$7fff,$dff09a
        move.w #$7fff,$dff096
        move.w #$8200,$dff096   ; master DMA; driver controls disk DMA
        move.w #$007,$dff180
        moveq #0,d0
        lea filename(pc),a0
        move.l a4,a1
        adda.l #45000,a1
        lea 24000(a4),a2
        bsr dosio
        tst.l d0
        bne fail
        cmp.l #242049,d1
        bne fail
        move.l a4,a0
        adda.l #45000,a0
        move.l d1,d2
        moveq #0,d3
sumloop:
        moveq #0,d4
        move.b (a0)+,d4
        add.l d4,d3
        subq.l #1,d2
        bne.s sumloop
        cmp.l #FOCUS_SUM,d3
        bne fail
        ; Write one sector in reserved slot A only, never filesystem metadata.
        move.l a4,a0
        adda.l #45000,a0
        move.w #255,d0
fill:   move.w #$5aa5,(a0)+
        dbf d0,fill
        moveq #0,d0
        move.w #1738,d1
        moveq #1,d2
        move.w #$8001,d3
        move.l a4,a0
        adda.l #45000,a0
        lea 24000(a4),a1
        bsr diskio
        tst.l d0
        bne fail
        move.l a4,a0
        adda.l #45000,a0
        move.w #255,d0
clear:  clr.w (a0)+
        dbf d0,clear
        moveq #0,d0
        move.w #1738,d1
        moveq #1,d2
        move.w #$8000,d3
        move.l a4,a0
        adda.l #45000,a0
        lea 24000(a4),a1
        bsr diskio
        tst.l d0
        bne fail
        move.l a4,a0
        adda.l #45000,a0
        move.w #255,d0
check:  cmp.w #$5aa5,(a0)+
        bne fail
        dbf d0,check
        move.w #$0f0,$dff180
        lea success(pc),a0
        bra.s serial
fail:   move.w #$f00,$dff180
        lea failure(pc),a0
serial: move.w #368,$dff032
next:   moveq #0,d0
        move.b (a0)+,d0
        beq.s halt
        or.w #$100,d0
        move.w d0,$dff030
        move.l #20000,d1
delay:  subq.l #1,d1
        bne.s delay
        bra.s next
halt:   bra.s halt
filename: dc.b 'DF0:focus',0
success: dc.b 'TRACKDISK-PASS',10,0
failure: dc.b 'TRACKDISK-FAIL',10,0
        even
        include "DosIO.s"
