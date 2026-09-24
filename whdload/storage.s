; The shared resident calls this with a trusted byte count/capacity. Preserve
; all registers except D0; its caller also checks sums and decoded structure.
bounded_load:
        movem.l d1-d7/a0-a6,-(sp)
        move.l d1,d3
        cmp.l d2,d3
        bhi.w load_bad
        addq.l #4,a0             ; shared descriptor's DF0: prefix
        move.l a0,a3
        move.l a1,a2
        move.l resload_base(pc),a6
        jsr resload_GetFileSize(a6)
        cmp.l d3,d0
        bne.w load_bad
        move.l a3,a0
        move.l a2,a1
        jsr resload_LoadFile(a6)
        cmp.l d3,d0
        bne.w load_bad
        moveq #0,d0
        bra.w load_return
load_bad: moveq #-1,d0
load_return:
        movem.l (sp)+,d1-d7/a0-a6
        rts
; Unused floppy callback: file commits have a separate boot-contract entry.
save_transfer:
        moveq #0,d0
        rts
; C ABI: write, slot index 0/1, 512-byte buffer. A missing file is blank;
; malformed files become invalid records, recoverable from a valid peer.
; Unreadable files fail without permitting an overwrite.
save_file_io:
        movem.l d2-d4/a2-a6,-(sp)
        move.l 36(sp),d2
        move.l 40(sp),d3
        move.l 44(sp),a2
        cmp.l #1,d3
        bhi.w file_bad
        lea slot_a(pc),a3
        tst.l d3
        beq.w file_name
        lea slot_b(pc),a3
file_name:
        move.l resload_base(pc),a6
        move.l a3,a0
        tst.l d2
        bne.w file_write
        jsr resload_GetFileSize(a6)
        tst.l d0
        beq.w file_missing
        cmp.l #512,d0
        bne.w file_invalid
        move.l a3,a0
        move.l a2,a1
        jsr resload_LoadFile(a6)
        cmp.l #512,d0
        seq d0
        and.l #1,d0
        bra.w file_return
file_missing:
        ; GetFileSize returns zero for both missing and empty files. Check
        ; IOERR so an empty/truncated existing file is not treated as blank.
        lea io_tags(pc),a0
        jsr resload_Control(a6)
        move.l io_error(pc),d0
        tst.l d0
        beq.w file_invalid
        cmp.l #205,d0           ; ERROR_OBJECT_NOT_FOUND
        bne.w file_bad
        moveq #127,d0
file_clear: clr.l (a2)+
        dbf d0,file_clear
        moveq #1,d0
        bra.w file_return
file_invalid:
        moveq #127,d0
file_poison: move.l #-1,(a2)+
        dbf d0,file_poison
        moveq #1,d0
        bra.w file_return
file_write:
        cmp.l #1,d2
        bne.w file_bad
        move.l #512,d0
        move.l a2,a1
        jsr resload_SaveFile(a6)
        tst.l d0
        sne d0
        and.l #1,d0
        bra.w file_return
file_bad: moveq #0,d0
file_return:
        movem.l (sp)+,d2-d4/a2-a6
        rts
commit_save:
        move.l 4(sp),a0
        movem.l d2-d7/a2-a6,-(sp)
        lea save_file_io(pc),a1
        move.l a1,-(sp)
        move.l resident_base(pc),a1
        adda.l #46800,a1
        move.l a1,-(sp)
        pea save_identity(pc)
        move.l a0,-(sp)
        bsr executable_binary+12
        lea 16(sp),sp
        movem.l (sp)+,d2-d7/a2-a6
        rts
slot_a: dc.b 'save-a',0
slot_b: dc.b 'save-b',0
        even
io_tags: dc.l WHDLTAG_IOERR_GET
io_error: dc.l 0
        dc.l 0
