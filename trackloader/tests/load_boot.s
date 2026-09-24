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
        beq.w failed
        move.l d0,a4
        moveq #2,d7
        move.l a3,a1
        move.w #2,28(a1)       ; CMD_READ
        move.l #22528,36(a1)
        move.l a4,40(a1)
        move.l #850432,44(a1)  ; track 151, sector 1661
        jsr -456(a6)           ; DoIO
        tst.l d0
        bne.w failed
        ifd NATIVE_CACHE
        ; Keep the reclaimed A500 slow-RAM arena when present. Additional
        ; banks must be real OS allocations, never guessed physical addresses.
        moveq #0,d4
        moveq #0,d5
        moveq #0,d6
        move.l #$c01000,a2
cache_check_slow:
        move.l a2,a1
        jsr -534(a6)            ; TypeOfMem
        tst.l d0
        beq.s cache_allocate_first
        adda.l #4096,a2
        cmpa.l #$c7c000,a2
        bne.s cache_check_slow
        move.l #$c01000,d4
        bra.s cache_more
cache_allocate_first:
        bsr cache_allocate
        move.l d0,d4
        beq failed
cache_more:
        bsr cache_allocate
        move.l d0,d5
        beq.s cache_allocated
        bsr cache_allocate
        move.l d0,d6
cache_allocated:
        else
        ; A500 diagnostic only: verify expansion RAM before takeover.
        ; Do not touch its OS contents until our private stack is installed.
        moveq #3,d7
        move.l #$c01000,a2
verify_ram:
        move.l a2,a1
        jsr -534(a6)           ; TypeOfMem, read-only presence check
        tst.l d0
        beq.w failed
        adda.l #4096,a2
        cmpa.l #$c7b000,a2
        bne.s verify_ram
        move.l #$c01000,a2
        endif
        move.l a4,a5
        jsr -30(a6)            ; Supervisor: does not return
failed:
        move.w #$7fff,$dff09a
        move.w #368,$dff032
        add.w #$130,d7
        move.w d7,$dff030
        move.w #$f00,$dff180
hang:   bra.s hang

        ifd NATIVE_CACHE
; Prefer non-chip RAM, falling back to Chip RAM on larger machines. Reject an
; allocation overlapping the reclaimed slow-RAM arena, even if Exec offered it.
cache_allocate:
        move.l #500000,d0
        moveq #4,d1             ; MEMF_FAST
        jsr -198(a6)
        tst.l d0
        beq.s cache_chip
        cmp.l #$c7b120,d0
        bcc.s cache_return
        move.l d0,d1
        add.l #500000,d1
        cmp.l #$c01000,d1
        bls.s cache_return
        move.l d0,a1
        move.l #500000,d0
        jsr -210(a6)            ; FreeMem: cannot overlap fixed primary bank
cache_chip:
        move.l #500000,d0
        moveq #2,d1             ; MEMF_CHIP
        jsr -198(a6)
cache_return:
        rts
        endif
