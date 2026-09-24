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
        ; Identify the boot IORequest's unit through public OpenDevice calls.
        ; Do not depend on private trackdisk unit structure offsets.
        suba.w #56,sp
        moveq #0,d7
boot_find_unit:
        move.l sp,a1
        moveq #13,d0
boot_clear_request:
        clr.l (a1)+
        dbf d0,boot_clear_request
        move.l sp,a1
        move.w #56,18(a1)
        lea boot_device(pc),a0
        move.l d7,d0
        moveq #0,d1
        jsr -444(a6)           ; OpenDevice
        tst.l d0
        bne.s boot_next_unit
        move.l 24(sp),a0
        cmpa.l 24(a3),a0
        seq d2
        move.l sp,a1
        jsr -450(a6)           ; CloseDevice (D2 preserved)
        tst.b d2
        bne.s boot_unit_found
boot_next_unit:
        addq.l #1,d7
        cmp.l #4,d7
        bcs.s boot_find_unit
        bra.w failed
boot_unit_found:
        adda.w #56,sp
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

boot_device: dc.b 'trackdisk.device',0
        even
