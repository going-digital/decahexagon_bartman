; A500 diagnostic boot: allocate Chip memory and load the reserved second stage
; using the boot IORequest. The second stage permanently takes over the machine.
        dc.b 'DOS',0
        dc.l 0,880
        move.l a1,a3
        move.l 4.w,a6
        move.l #327680,d0
        move.l #$10002,d1
        jsr -198(a6)           ; AllocMem(CHIP|CLEAR), before takeover
        tst.l d0
        beq.s failed
        move.l d0,a4
        move.l a3,a1
        move.w #2,28(a1)       ; CMD_READ
        move.l #22528,36(a1)
        move.l a4,40(a1)
        move.l #850432,44(a1)  ; track 151, sector 1661
        jsr -456(a6)           ; DoIO
        tst.l d0
        bne.s failed
        move.l a4,a5
        jsr -30(a6)            ; Supervisor: does not return
failed:
        move.w #$f00,$dff180
        bra.s failed
