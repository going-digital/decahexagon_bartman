; A500 diagnostic boot: allocate Chip memory and load the reserved second stage
; using the boot IORequest. The second stage permanently takes over the machine.
        dc.b 'DOS',0
        dc.l 0,880
        move.l a1,a3
        move.l 4.w,a6
        moveq #1,d7
        move.l #65536,d0
        move.l #$10002,d1
        jsr -198(a6)           ; AllocMem(CHIP|CLEAR), before takeover
        tst.l d0
        beq.s failed
        move.l d0,a4
        moveq #2,d7
        move.l a3,a1
        move.w #2,28(a1)       ; CMD_READ
        move.l #22528,36(a1)
        move.l a4,40(a1)
        move.l #850432,44(a1)  ; track 151, sector 1661
        jsr -456(a6)           ; DoIO
        tst.l d0
        bne.s failed
        ; A500 diagnostic only: verify expansion RAM before takeover.
        ; Do not touch its OS contents until our private stack is installed.
        moveq #3,d7
        move.l #$c01000,a2
verify_ram:
        move.l a2,a1
        jsr -534(a6)           ; TypeOfMem, read-only presence check
        tst.l d0
        beq.s failed
        adda.l #4096,a2
        cmpa.l #$c7b000,a2
        bne.s verify_ram
        move.l #$c01000,a2
        move.l a4,a5
        jsr -30(a6)            ; Supervisor: does not return
failed:
        move.w #$7fff,$dff09a
        move.w #368,$dff032
        add.w #$130,d7
        move.w d7,$dff030
        move.w #$f00,$dff180
hang:   bra.s hang
