; Disposable-disk diagnostic. No Exec/DOS calls after entry.
        move.l a4,sp
        adda.l #65536,sp
        move.l a2,a3
        move.w #$2700,sr
        move.w #$7fff,$dff09a
        move.w #$7fff,$dff096
        move.w #$8200,$dff096   ; master DMA; driver controls disk DMA
        move.w #$007,$dff180
        lea descriptors(pc),a0
        lea current(pc),a1
        move.l a0,(a1)
load_next:
        move.l current(pc),a0
        move.l (a0),d5
        moveq #0,d0
        lea 24(a0),a0
        move.l a3,a1
        adda.l d5,a1
        lea 24000(a4),a2
        bsr dosio
        tst.l d0
        bne fail
        move.l current(pc),a2
        cmp.l 4(a2),d1
        bne fail
        move.l a3,a0
        adda.l (a2),a0
        move.l d1,d2
        moveq #0,d3
sumloop:
        moveq #0,d4
        move.b (a0)+,d4
        add.l d4,d3
        subq.l #1,d2
        bne.s sumloop
        cmp.l 8(a2),d3
        bne fail
        move.l a4,-(sp)
        move.l a4,a6
        adda.l #52000,a6
        move.l a3,a4
        move.l a3,a5
        adda.l (a2),a5
        bsr inflate
        move.l (sp)+,a4
        ; Bind the existing PCM sequencer only after validating all metadata.
        move.l a4,a0
        adda.l #54000,a0
        move.l a0,-(sp)        ; TrackTuneInfo
        adda.l #64,a0
        move.l a0,-(sp)        ; PcmSong
        move.l #500000,-(sp)
        move.l current(pc),a0
        move.l 12(a0),-(sp)
        move.l a3,-(sp)
        bsr fib_binary
        lea 20(sp),sp
        tst.l d0
        beq fail
        move.l a3,a0
        move.l current(pc),a2
        move.l 16(a2),d2
        moveq #0,d3
final_sum:
        moveq #0,d4
        move.b (a0)+,d4
        add.l d4,d3
        subq.l #1,d2
        bne.s final_sum
        cmp.l 20(a2),d3
        bne fail
        ifd PAULA_PROBE
        move.l a4,a0
        adda.l #56000,a0
        move.l a0,-(sp)
        move.l a4,a0
        adda.l #54000,a0
        move.l a0,-(sp)
        adda.l #64,a0
        move.l a0,-(sp)
        bsr fib_binary+4
        lea 12(sp),sp
        tst.l d0
        beq fail
        endc
        lea current(pc),a0
        add.l #40,(a0)
        move.l (a0),a0
        cmp.l #-1,(a0)
        bne load_next
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
current: dc.l 0
descriptors: include "scratchpad/trackloader/tune_descriptors.i"
success: dc.b 'TRACKLOAD-PASS',10,0
failure: dc.b 'TRACKLOAD-FAIL',10,0
        even
        include "DosIO.s"

        include "scratchpad/trackloader/inflate_core_distance_fixed.s"
fib_binary:
        incbin "scratchpad/trackloader/fib_pic.bin"
