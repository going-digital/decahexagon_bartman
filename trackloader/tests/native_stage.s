; A500 native-game boot. A4 owns a 360448-byte Chip block.
; stage/workspace/stack [0,65536), image [65536,262144),
; cues [262144,278528), display heap [278528,360448).
; DiskIO workspace [24000,37056), track cache [38000,43632),
; OFS scratch [54000,55244) reused as tune metadata after disk loading.
        lea resident_base(pc),a0
        move.l a4,(a0)
        move.l a4,sp
        adda.l #65536,sp
        move.w #$2700,sr
        move.w #$7fff,$dff09a
        move.w #$7fff,$dff096
        move.w #$8200,$dff096
        ; Detect the live video standard before configuring game timing.
        ; Both standards cross line 256; only PAL reaches line 300.
        lea boot_args(pc),a0
        clr.l 12(a0)
video_wait_low:
        move.l $dff004,d0
        and.l #$1ff00,d0
        cmp.l #$10000,d0
        bcc.s video_wait_low
video_wait_high:
        move.l $dff004,d0
        and.l #$1ff00,d0
        cmp.l #$10000,d0
        bcs.s video_wait_high
video_classify:
        move.l $dff004,d0
        and.l #$1ff00,d0
        cmp.l #$12c00,d0
        bcc.s video_is_pal
        cmp.l #$10000,d0
        bcc.s video_classify
        bra.s video_detected
video_is_pal:
        move.l #1,12(a0)
video_detected:
        move.l a4,a3
        adda.l #65536,a3
        moveq #0,d0
        lea filename(pc),a0
        move.l a3,a1
        adda.l #GAME_SOURCE_OFFSET,a1
        lea 24000(a4),a2
        move.l #GAME_PACKED_BYTES,d1
        move.l #196608-GAME_SOURCE_OFFSET,d2
        bsr bounded_load
        tst.l d0
        bne fail
        cmp.l #GAME_PACKED_BYTES,d1
        bne fail
        move.l a3,a0
        adda.l #GAME_SOURCE_OFFSET,a0
        moveq #0,d2
sum:    moveq #0,d3
        move.b (a0)+,d3
        add.l d3,d2
        subq.l #1,d1
        bne.s sum
        cmp.l #GAME_PACKED_SUM,d2
        bne fail
        move.l a4,-(sp)
        move.l a4,a6
        adda.l #52000,a6
        move.l a3,a4
        move.l a3,a5
        adda.l #GAME_SOURCE_OFFSET,a5
        bsr inflate
        move.l (sp)+,a4
        move.l a4,a0
        adda.l #54000,a0
        move.l a0,-(sp)          ; output info
        move.l a3,-(sp)          ; actual relocation base
        move.l #196608,-(sp)     ; image arena capacity
        move.l #GAME_PACKAGE_BYTES,-(sp)
        move.l a3,-(sp)
        bsr executable_binary
        lea 20(sp),sp
        tst.l d0
        beq fail
        bsr restore_save
        move.l a4,-(sp)
        move.l a4,a6
        adda.l #52000,a6
        lea save_anchors(pc),a5
        adda.l #45000,a4
        bsr inflate
        move.l (sp)+,a4
        ; Native game contract: stop DMA, CIAs and outstanding interrupts.
        move.w #$7fff,$dff096
        move.w #$7fff,$dff09a
        move.w #$7fff,$dff09c
        move.b #$7f,$bfed01
        move.b #$7f,$bfdd00
        move.b $bfed01,d0
        move.b $bfdd00,d0
        lea boot_args(pc),a0
        move.l a4,d0
        add.l #278528,d0
        move.l d0,(a0)
        lea prepare_track(pc),a1
        move.l a1,16(a0)
        lea save_transfer(pc),a1
        move.l a1,24(a0)
        lea save_identity(pc),a1
        move.l a1,28(a0)
        move.l a4,d0
        add.l #45000,d0
        move.l d0,32(a0)
        move.l a4,d0
        add.l #46800,d0
        move.l d0,36(a0)
        move.l a0,-(sp)
        lea entered(pc),a0
        bsr serial
        move.l a4,a0
        adda.l #54000,a0
        move.l (a0),a0
        jsr (a0)
        addq.l #4,sp
        tst.l d0
        bne fail
        lea returned(pc),a0
        bsr serial
halt:   bra.s halt
fail:   move.w #$f00,$dff180
        lea failure(pc),a0
        bsr serial
        bra.s halt
 ; Startup-only raw slot reads; failures leave defaults, never write the disk.
restore_save:
        move.l #1738,d5
        moveq #1,d6
        move.l a4,a2
        adda.l #38000,a2
save_read:
        moveq #0,d0
        move.l d5,d1
        moveq #1,d2
        move.w #$8000,d3
        move.l a2,a0
        lea 24000(a4),a1
        bsr diskio
        tst.l d0
        beq.s save_read_next
        lea save_read_error(pc),a0
        bsr serial
        move.l a2,a0            ; discard partial data from an unreadable track
        moveq #127,d0
save_read_clear:
        clr.l (a0)+
        dbf d0,save_read_clear
save_read_next:
        add.l #11,d5
        adda.l #512,a2
        dbf d6,save_read
        move.l a4,a0
        adda.l #55000,a0
        move.l a0,-(sp)
        lea save_identity(pc),a0
        move.l a0,-(sp)
        move.l a4,a0
        adda.l #38512,a0
        move.l a0,-(sp)
        suba.l #512,a0
        move.l a0,-(sp)
        move.l a4,a0
        adda.l #65536+GAME_SAVE_SELECT,a0
        jsr (a0)
        lea 16(sp),sp
        tst.l d0
        bmi.s save_default
        move.l a4,d0
        add.l #55000,d0
        lea boot_args(pc),a0
        move.l d0,20(a0)
        lea save_restored(pc),a0
        bra serial
save_default:
        lea save_defaults(pc),a0
        bra serial
save_identity: incbin "scratchpad/trackloader/native_game/save_identity.bin"
save_read_error: dc.b 'NATIVE-SAVE-READ-FAIL',10,0
save_restored: dc.b 'NATIVE-SAVE-RESTORED',10,0
save_defaults: dc.b 'NATIVE-SAVE-DEFAULT',10,0
        even
 ; C ABI callback: profiles 0..5 share the three base-stage banks.
prepare_track:
        move.l 4(sp),d0
        and.l #255,d0
        cmp.l #6,d0
        bcc.s not_ready
        cmp.l #3,d0
        bcs.s base_profile
        subq.l #3,d0
base_profile:
        mulu #48,d0
        lea descriptors(pc),a0
        adda.l d0,a0
        cmpa.l loaded_descriptor(pc),a0
        bne.s load_track
        moveq #1,d0
        rts
not_ready:
        moveq #0,d0
        rts
load_track:
        lea current_descriptor(pc),a1
        move.l a0,(a1)
        movem.l d2-d7/a2-a6,-(sp)
        move.w sr,-(sp)
        move.w #$2700,sr
        move.l resident_base(pc),a4
        move.l a4,a5
        adda.l #65536,a5
        lea GAME_STOP(a5),a0
        jsr (a0)
        lea GAME_UNBIND(a5),a0
        jsr (a0)
        tst.l d0
        beq callback_failed_early
        lea loaded_descriptor(pc),a0
        clr.l (a0)
        lea GAME_SFX_STOP(a5),a0
        jsr (a0)
        move.w $dff002,d6
        and.w #$07f0,d6
        move.w $dff01c,d7
        and.w #$7fff,d7
        move.w #$7fff,$dff09a
        move.w #$7fff,$dff096
        move.w #$83a0,$dff096 ; display DMA: static loading list
        moveq #0,d0
        move.l current_descriptor(pc),a0
        move.l #$c01000,a1
        adda.l (a0),a1
        lea 32(a0),a0
        lea 24000(a4),a2
        move.l current_descriptor(pc),a0
        move.l 4(a0),d1
        move.l #500000,d2
        sub.l (a0),d2
        lea 32(a0),a0
        bsr bounded_load
        tst.l d0
        bne callback_failed
        move.l current_descriptor(pc),a2
        cmp.l 4(a2),d1
        bne callback_failed
        move.l #$c01000,a0
        adda.l (a2),a0
        moveq #0,d2
packed_sum:
        moveq #0,d3
        move.b (a0)+,d3
        add.l d3,d2
        subq.l #1,d1
        bne.s packed_sum
        cmp.l 8(a2),d2
        bne callback_failed
        move.l a4,-(sp)
        move.l a4,a6
        adda.l #52000,a6
        move.l #$c01000,a4
        move.l #$c01000,a5
        adda.l (a2),a5
        bsr inflate
        move.l (sp)+,a4
        move.l a4,a0
        adda.l #54000,a0
        move.l a0,-(sp)
        adda.l #64,a0
        move.l a0,-(sp)
        move.l #500000,-(sp)
        move.l current_descriptor(pc),a0
        move.l 12(a0),-(sp)
        move.l #$c01000,-(sp)
        bsr executable_binary+4
        lea 20(sp),sp
        tst.l d0
        beq callback_failed
        ; Immutable compressed cues -> dedicated 16 KiB Chip region.
        move.l current_descriptor(pc),a2
        lea descriptors(pc),a5
        adda.l 24(a2),a5
        move.l a4,-(sp)
        move.l a4,a6
        adda.l #52000,a6
        adda.l #262144,a4
        bsr inflate
        move.l (sp)+,a4
        move.l a4,a0
        adda.l #262144,a0
        move.l 20(a2),d1
        moveq #0,d2
cue_sum:
        moveq #0,d3
        move.b (a0)+,d3
        add.l d3,d2
        subq.l #1,d1
        bne.s cue_sum
        cmp.l 28(a2),d2
        bne callback_failed
        move.l a4,a0
        adda.l #56000,a0
        move.l a0,-(sp)
        move.l current_descriptor(pc),a2
        move.l 20(a2),-(sp)
        move.l a4,a0
        adda.l #262144,a0
        move.l a0,-(sp)
        move.l a4,a0
        adda.l #54000,a0
        move.l 4(a0),-(sp) ; info.samples
        adda.l #64,a0
        move.l a0,-(sp)
        move.l a4,a5
        adda.l #65536,a5
        lea GAME_BIND(a5),a0
        jsr (a0)
        lea 20(sp),sp
        tst.l d0
        beq callback_failed
        move.l current_descriptor(pc),a2
        move.l 16(a2),-(sp)
        lea GAME_CUE_LEAD(a5),a0
        jsr (a0)
        addq.l #4,sp
        tst.l d0
        beq callback_failed
        lea loaded_descriptor(pc),a0
        move.l a2,(a0)
        lea 32(a2),a0
        bsr serial
        lea tune_ready(pc),a0
        bsr serial
        moveq #1,d5
        bra.s restore_hardware
callback_failed:
        lea load_error(pc),a0
        bsr serial
        moveq #0,d5
restore_hardware:
        move.l a4,a5
        adda.l #65536,a5
        lea GAME_SFX_INIT(a5),a0
        jsr (a0)
        move.w #$7fff,$dff096
        move.w #$7fff,$dff09c
        or.w #$8000,d6
        move.w d6,$dff096
        or.w #$8000,d7
        move.w d7,$dff09a
        move.l d5,d0
        bra.s callback_return
callback_failed_early:
        moveq #0,d0
callback_return:
        move.w (sp)+,sr
        movem.l (sp)+,d2-d7/a2-a6
        rts
resident_base: dc.l 0
current_descriptor: dc.l 0
loaded_descriptor: dc.l 0
descriptors: include "scratchpad/trackloader/native_game/tunes.i"
load_error: dc.b 'NATIVE-TUNE-FAIL',10,0
tune_ready: dc.b 'NATIVE-TUNE-READY',10,0
        even
serial: move.w #368,$dff032
next:   moveq #0,d0
        move.b (a0)+,d0
        beq.s done
        or.w #$100,d0
        move.w d0,$dff030
        move.l #20000,d1
delay:  subq.l #1,d1
        bne.s delay
        bra.s next
done:   rts
boot_args: dc.l 0,81920,0,0,0,0,0,0,0,0 ; heap, bytes, VBR, detected PAL flag, prepare
filename: dc.b 'DF0:game',0
entered: dc.b 'NATIVE-GAME-ENTRY',10,0
returned: dc.b 'NATIVE-GAME-RETURN',10,0
failure: dc.b 'NATIVE-GAME-FAIL',10,0
        even
; C adapter: a0=DF0:name, a1=destination, d1=trusted bytes, d2=capacity.
; Scratch is disjoint from disk workspace, destination and player DMA buffers.
bounded_load:
        movem.l d1-d7/a0-a6,-(sp)
        lea cached_track(pc),a3
        move.w #-1,(a3)
        move.l a4,a3
        adda.l #54000,a3
        move.l a3,-(sp)
        move.l d1,-(sp)
        move.l d2,-(sp)
        move.l a1,-(sp)
        addq.l #4,a0
        move.l a0,-(sp)
        move.l a2,-(sp)
        lea read_sector(pc),a0
        move.l a0,-(sp)
        bsr executable_binary+8
        lea 28(sp),sp
        move.l d0,-(sp)
        moveq #0,d0
        moveq #0,d1
        moveq #0,d2
        move.w #$8000,d3
        bsr diskio              ; always stop the motor, including errors
        move.l (sp)+,d0
        subq.l #1,d0            ; C success 1 -> historical success 0
        movem.l (sp)+,d1-d7/a0-a6
        rts
read_sector:
        movem.l d2-d4/a2-a3,-(sp)
        move.l 32(sp),a2        ; destination scratch sector
        move.l 28(sp),d4
        divu #11,d4
        and.l #$ffff,d4         ; physical track
        cmp.w cached_track(pc),d4
        beq.s cached_sector
        move.l resident_base(pc),a0
        adda.l #38000,a0        ; 5632-byte whole-track cache
        move.l 24(sp),a1        ; 13056-byte DiskIO workspace at +24000
        move.l d4,d1
        mulu #11,d1
        moveq #11,d2
        moveq #0,d3
        moveq #0,d0
        bsr diskio
        tst.l d0
        bne.s sector_failed
        lea cached_track(pc),a0
        move.w d4,(a0)          ; publish only after a successful full read
cached_sector:
        move.l 28(sp),d0
        divu #11,d0
        swap d0
        and.l #$ffff,d0
        lsl.l #8,d0
        add.l d0,d0
        move.l resident_base(pc),a0
        adda.l #38000,a0
        adda.l d0,a0
        moveq #127,d1
copy_sector:
        move.l (a0)+,(a2)+
        dbf d1,copy_sector
        moveq #1,d0
        bra.s sector_return
sector_failed:
        lea cached_track(pc),a0
        move.w #-1,(a0)
        moveq #0,d0
sector_return:
        movem.l (sp)+,d2-d4/a2-a3
        rts
 ; C ABI: op 0 read, 1 write reserved slot, 2 begin media sequence.
save_transfer:
        movem.l d2-d4/a2,-(sp)
        move.l 20(sp),d3
        lea save_media_seen(pc),a2
        cmp.l #2,d3
        bne.s transfer_io
        clr.b (a2)
        moveq #1,d0
        bra.w transfer_return
transfer_io:
        move.l 24(sp),d1
        cmp.l #1760,d1
        bcc.s transfer_fail
        tst.l d3
        beq.s transfer_check
        cmp.l #1,d3
        bne.s transfer_fail
        cmp.l #1738,d1
        beq.s transfer_check
        cmp.l #1749,d1
        bne.s transfer_fail
transfer_check:
        tst.b (a2)
        beq.s transfer_read
        move.b $bfd100,d4
        move.b #$f7,$bfd100
        btst #2,$bfe001
        sne d0
        move.b d4,$bfd100
        tst.b d0
        beq.s transfer_fail
transfer_read:
        lea native_disk_guard_enabled(pc),a0
        move.b (a2),(a0)
        move.l 28(sp),a0
        move.l resident_base(pc),a1
        adda.l #24000,a1
        ori.w #$8000,d3
        moveq #0,d0
        moveq #1,d2
        bsr diskio
        lea native_disk_guard_enabled(pc),a0
        clr.b (a0)
        move.b #1,(a2)
        tst.l d0
        seq d0
        and.l #1,d0
        bra.w transfer_return
transfer_fail:
        moveq #0,d0
transfer_return:
        movem.l (sp)+,d2-d4/a2
        rts
save_media_seen: dc.b 0
        even
save_anchors: incbin "scratchpad/trackloader/native_game/save_anchors.deflate"
        even
cached_track: dc.w -1
        include "scratchpad/trackloader/native_game/guarded_diskio.s"
        include "scratchpad/trackloader/inflate_core_distance_fixed.s"
executable_binary:
        incbin "scratchpad/trackloader/native_game/executable_pic.bin"

        even
cues1: incbin "scratchpad/trackloader/native_game/cues1.deflate"
        even
cues2: incbin "scratchpad/trackloader/native_game/cues2.deflate"
        even
cues3: incbin "scratchpad/trackloader/native_game/cues3.deflate"
