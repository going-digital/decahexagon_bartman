*------------------------------------------------------------------------------
* Low Level Read/Write/Format Disk Code for Commodore Amiga Dos Disks
*
* Copyright (c) 1988-92 Rob Northen Computing, U.K. All Rights Reserved.
*
* File: DISKIO.S
*
* Date: 07.09.92
*------------------------------------------------------------------------------


*------------------------------------------------------------------------------
* Low Level Read/Write/Format Disk Code for AmigaDos Disks
* on entry,
*	d0.w = drive number
*		0 = DF0
*		1 = DF1
*		2 = DF2
*		3 = DF3
*		4 = DF0, side 0 only
*		5 = DF1, side 0 only
*		6 = DF2, side 0 only
*		7 = DF3, side 0 only
*	d1.w = start sector no.
*		(0-879 for single sided disk, 0-1759 for double sided disk)
*	d2.w = number of sectors to read/write
*		0 = turn motor off (bit 15 of d3 must be set)
*	d3.w = function
*		0 = read sectors
*		1 = write sectors
*		2 = format track
*			a1.l = address of $3300 byte buffer
*			d1.w = sector no. of track to format
*			       ie. 0=track 0, side 0
*			          11=track 0, side 1 or track 1, side 0
*				  depending upon d0.w
*			d2.w = sectors to format
*			       ie.11=1 track
*			          22=2 tracks
*	bit 15 of d3.w controls whether the drive motor is turned off
*	after use. If bit 15 is clear the drive motor is left on. If
*	bit 15 is set the drive motor is turned off. Leave the drive
*	motor on if another disk operation is to be made soon after
*	the current one.
*
*	a0.l = disk buffer address for transfer
*	a1.l = workspace buffer, ($3300 bytes of CHIPMEM required)
* on exit,
*	d0.l = error no., if d0.l <> 0 then d1.l = sector offset in error
*		00 = no error
*		21 = No Sector Header
*		22 = Bad Sector Preamble
*		23 = Bad Sector ID
*		24 = Bad Sector Header Checksum
*		25 = Bad Sector Sum
*		26 = Too Few Sectors
*		27 = Bad Sector Header
*		28 = Write Protected
*		29 = No Disk in Drive
*		30 = Seek Error
*		34 = Drive In Use
*		-1 = DMA timeout
*
* Note: The disk code uses Timer A of CIA B
*       Master DMA (bit 9, $dff096) must be enabled
*	The blitter is not used
*------------------------------------------------------------------------------

        opt o-                  ; Preserve original instruction encodings.

; Annotated 68000 disassembly. Offsets are relative to this file entry.
; Data remains data; branch widths and absolute address widths are explicit.
; Original copyright and calling convention above are retained.
; Labels ending Lxxxx are address-derived control-flow labels; descriptive
; labels/comments document observable operations, not recovered source names.
; This module contains writable state and must not be placed in ROM.
; opt o- intentionally disables assembler shortening/rewriting for exact bytes.

; Public entry; preserves caller registers as documented above.
diskio:
        movem.l d1-a5,-(sp)                              ; +$0000; register save/restore
        link.w a6,#-36                                   ; +$0004
        move.w d0,d4                                     ; +$0008
        andi.w #3,d4                                     ; +$000a
        move.w d4,-36(a6)                                ; +$000e
        move.w d1,-34(a6)                                ; +$0012
        move.w d2,-32(a6)                                ; +$0016
        move.w d3,-30(a6)                                ; +$001a
        move.l a0,-28(a6)                                ; +$001e
        move.l a1,-24(a6)                                ; +$0022
        ror.w #2,d0                                      ; +$0026
        andi.w #1,d0                                     ; +$0028
        addq.w #1,d0                                     ; +$002c
        move.w d0,-20(a6)                                ; +$002e
        moveq #0,d0                                      ; +$0032
        move.w d2,d3                                     ; +$0034
        beq.w diskio_L00c0                               ; +$0036
        moveq #30,d0                                     ; +$003a
        add.w d1,d3                                      ; +$003c
        cmp.w #1760,d3                                   ; +$003e
        bgt.w diskio_L00ea                               ; +$0042
        andi.l #65535,d1                                 ; +$0046
        divu.w #11,d1                                    ; +$004c
        cmpi.w #1,-20(a6)                                ; +$0050
        beq.s diskio_L005a                               ; +$0056
        add.w d1,d1                                      ; +$0058
diskio_L005a:
        move.w d1,-18(a6)                                ; +$005a
        swap d1                                          ; +$005e
        move.w d1,-16(a6)                                ; +$0060
        bsr.w diskio_L0642                               ; +$0064; call internal helper
        tst.b -29(a6)                                    ; +$0068
        beq.s diskio_L0072                               ; +$006c
        bsr.w diskio_L04c2                               ; +$006e; call internal helper
diskio_L0072:
        move.w -16(a6),d0                                ; +$0072
        moveq #11,d1                                     ; +$0076
        sub.w d0,d1                                      ; +$0078
        cmp.w -32(a6),d1                                 ; +$007a
        ble.s diskio_L0084                               ; +$007e
        move.w -32(a6),d1                                ; +$0080
diskio_L0084:
        move.w d1,-14(a6)                                ; +$0084
        bsr.s diskio_L00f4                               ; +$0088; call internal helper
        bne.s diskio_L00c0                               ; +$008a
        tst.b -29(a6)                                    ; +$008c
        beq.s diskio_L0098                               ; +$0090
        bsr.w diskio_L01e4                               ; +$0092; call internal helper
        bne.s diskio_L00c0                               ; +$0096
diskio_L0098:
        move.w -32(a6),d0                                ; +$0098
        sub.w -14(a6),d0                                 ; +$009c
        beq.s diskio_L00c0                               ; +$00a0
        move.w d0,-32(a6)                                ; +$00a2
        move.w -14(a6),d0                                ; +$00a6
        lsl.l #8,d0                                      ; +$00aa
        add.l d0,d0                                      ; +$00ac
        add.l d0,-28(a6)                                 ; +$00ae
        clr.w -16(a6)                                    ; +$00b2
        move.w -20(a6),d0                                ; +$00b6
        add.w d0,-18(a6)                                 ; +$00ba
        bra.s diskio_L0072                               ; +$00be
diskio_L00c0:
        move.l d0,-(sp)                                  ; +$00c0
        bsr.w diskio_L0614                               ; +$00c2; call internal helper
        move.l (sp)+,d0                                  ; +$00c6
        beq.s diskio_L00ea                               ; +$00c8
        moveq #0,d1                                      ; +$00ca
        move.w -18(a6),d1                                ; +$00cc
        cmpi.w #1,-20(a6)                                ; +$00d0
        beq.s diskio_L00da                               ; +$00d6
        lsr.w #1,d1                                      ; +$00d8
diskio_L00da:
        mulu.w #11,d1                                    ; +$00da
        add.w -16(a6),d1                                 ; +$00de
        add.w -6(a6),d1                                  ; +$00e2
        move.l d1,40(sp)                                 ; +$00e6
diskio_L00ea:
        unlk a6                                          ; +$00ea
        tst.l d0                                         ; +$00ec
        movem.l (sp)+,d1-a5                              ; +$00ee; register save/restore
        rts                                              ; +$00f2; return to caller
diskio_L00f4:
        moveq #2,d4                                      ; +$00f4
diskio_L00f6:
        clr.w -4(a6)                                     ; +$00f6
        clr.w -6(a6)                                     ; +$00fa
        clr.w -8(a6)                                     ; +$00fe
        move.w -18(a6),d2                                ; +$0102
        bsr.w diskio_L065a                               ; +$0106; call internal helper
        bne.w diskio_L01c4                               ; +$010a
        moveq #29,d0                                     ; +$010e
        btst #2,$bfe001.l                                ; +$0110; CIA-A PRA: disk/input status
        beq.w diskio_L01c4                               ; +$0118
        moveq #0,d0                                      ; +$011c
        cmpi.b #2,-29(a6)                                ; +$011e
        beq.w diskio_L01e0                               ; +$0124
        movea.l -24(a6),a5                               ; +$0128
        lea 1024(a5),a5                                  ; +$012c
        move.l #-1431655766,(a5)                         ; +$0130
        move.w #17545,4(a5)                              ; +$0136
        bsr.w diskio_L044c                               ; +$013c; call internal helper
        bsr.w diskio_L02b2                               ; +$0140; call internal helper
        bne.s diskio_L01c4                               ; +$0144
        move.w -12(a6),d0                                ; +$0146
        beq.s diskio_L019a                               ; +$014a
        mulu.w #1088,d0                                  ; +$014c
        lea 6(a5),a0                                     ; +$0150
        bsr.w diskio_start_read_dma                      ; +$0154; call internal helper
        lea $dff01e.l,a4                                 ; +$0158
        bsr.w diskio_L030a                               ; +$015e; call internal helper
        bne.s diskio_L01dc                               ; +$0162
        cmpi.b #1,-29(a6)                                ; +$0164
        beq.s diskio_L0176                               ; +$016a
        move.w -6(a6),d0                                 ; +$016c
        sub.w -14(a6),d0                                 ; +$0170
        beq.s diskio_L01e0                               ; +$0174
diskio_L0176:
        movea.l -24(a6),a5                               ; +$0176
        lea 1024(a5),a5                                  ; +$017a
        move.w -12(a6),d0                                ; +$017e
        mulu.w #1088,d0                                  ; +$0182
        adda.l d0,a5                                     ; +$0186
        move.l #-1431655766,(a5)                         ; +$0188
        move.w #17545,4(a5)                              ; +$018e
        movea.l a5,a0                                    ; +$0194
        bsr.w diskio_L0554                               ; +$0196; call internal helper
diskio_L019a:
        move.w -10(a6),d0                                ; +$019a
        beq.s diskio_L01b8                               ; +$019e
        mulu.w #1088,d0                                  ; +$01a0
        lea 6(a5),a0                                     ; +$01a4
        bsr.w diskio_start_read_dma                      ; +$01a8; call internal helper
        lea -2(a6),a4                                    ; +$01ac
        clr.w (a4)                                       ; +$01b0
        bsr.w diskio_L030a                               ; +$01b2; call internal helper
        bne.s diskio_L01dc                               ; +$01b6
diskio_L01b8:
        move.w -6(a6),d0                                 ; +$01b8
        sub.w -14(a6),d0                                 ; +$01bc
        beq.s diskio_L01e0                               ; +$01c0
        moveq #26,d0                                     ; +$01c2
diskio_L01c4:
        move.l d0,-(sp)                                  ; +$01c4
        moveq #2,d2                                      ; +$01c6
        bsr.w diskio_L065a                               ; +$01c8; call internal helper
        bsr.w diskio_L06a8                               ; +$01cc; call internal helper
        move.l (sp)+,d0                                  ; +$01d0
        btst #2,$bfe001.l                                ; +$01d2; CIA-A PRA: disk/input status
        beq.s diskio_L01e0                               ; +$01da
diskio_L01dc:
        dbf d4,diskio_L00f6                              ; +$01dc
diskio_L01e0:
        bra.w diskio_L0600                               ; +$01e0
diskio_L01e4:
        moveq #2,d4                                      ; +$01e4
        clr.w -6(a6)                                     ; +$01e6
        cmpi.b #2,-29(a6)                                ; +$01ea
        bne.s diskio_L01fc                               ; +$01f0
        move.w -18(a6),d0                                ; +$01f2
        movea.l -24(a6),a0                               ; +$01f6
        bsr.s diskio_L026c                               ; +$01fa; call internal helper
diskio_L01fc:
        bsr.w diskio_L0700                               ; +$01fc; call internal helper
        moveq #100,d0                                    ; +$0200
        bsr.w diskio_L073a                               ; +$0202; call internal helper
        moveq #28,d0                                     ; +$0206
        btst #3,$bfe001.l                                ; +$0208; CIA-A PRA: disk/input status
        beq.s diskio_L0260                               ; +$0210
        lea $dff000.l,a0                                 ; +$0212; custom register base
        move.w #16384,36(a0)                             ; +$0218
        move.l -24(a6),32(a0)                            ; +$021e
        move.w #26112,158(a0)                            ; +$0224
        move.w #-28416,158(a0)                           ; +$022a
        cmpi.w #80,-18(a6)                               ; +$0230
        bcs.s diskio_L023e                               ; +$0236
        move.w #-24576,158(a0)                           ; +$0238
diskio_L023e:
        move.w #-32752,150(a0)                           ; +$023e
        move.w #2,156(a0)                                ; +$0244
        move.w #-9887,36(a0)                             ; +$024a
        move.w #-9887,36(a0)                             ; +$0250
        bsr.w diskio_wait_dma                            ; +$0256; call internal helper
        beq.s diskio_L0260                               ; +$025a
        dbf d4,diskio_L01fc                              ; +$025c
diskio_L0260:
        move.l d0,-(sp)                                  ; +$0260
        moveq #2,d0                                      ; +$0262
        bsr.w diskio_L073a                               ; +$0264; call internal helper
        move.l (sp)+,d0                                  ; +$0268
        rts                                              ; +$026a; return to caller
diskio_L026c:
        move.l d0,d3                                     ; +$026c
        ori.w #-256,d3                                   ; +$026e
        swap d3                                          ; +$0272
        move.w #11,d3                                    ; +$0274
        lea 1024(a0),a0                                  ; +$0278
diskio_L027c:
        addq.w #4,a0                                     ; +$027c
        move.l #1149846665,(a0)+                         ; +$027e
        move.l d3,d0                                     ; +$0284
        bsr.w diskio_L054a                               ; +$0286; call internal helper
        lea -8(a0),a0                                    ; +$028a
        bsr.w diskio_L0554                               ; +$028e; call internal helper
        moveq #40,d1                                     ; +$0292
        bsr.w diskio_L04a8                               ; +$0294; call internal helper
        bsr.w diskio_L054a                               ; +$0298; call internal helper
        lea -8(a0),a0                                    ; +$029c
        bsr.w diskio_L0554                               ; +$02a0; call internal helper
        lea 1040(a0),a0                                  ; +$02a4
        addi.w #256,d3                                   ; +$02a8
        subq.b #1,d3                                     ; +$02ac
        bne.s diskio_L027c                               ; +$02ae
        rts                                              ; +$02b0; return to caller
diskio_L02b2:
        moveq #10,d2                                     ; +$02b2
diskio_L02b4:
        lea 6(a5),a0                                     ; +$02b4
        move.w #64,d0                                    ; +$02b8
        bsr.w diskio_start_read_dma                      ; +$02bc; call internal helper
        bsr.w diskio_wait_dma                            ; +$02c0; call internal helper
        bne.s diskio_L02fc                               ; +$02c4
        bsr.w diskio_L0494                               ; +$02c6; call internal helper
        beq.s diskio_L02d2                               ; +$02ca
        dbf d2,diskio_L02b4                              ; +$02cc
        bra.s diskio_L02fe                               ; +$02d0
diskio_L02d2:
        bsr.w diskio_L045e                               ; +$02d2; call internal helper
        bne.s diskio_L0302                               ; +$02d6
        cmp.w -18(a6),d1                                 ; +$02d8
        bne.s diskio_L0302                               ; +$02dc
        cmp.b #11,d2                                     ; +$02de
        bge.s diskio_L0302                               ; +$02e2
        cmp.b #11,d3                                     ; +$02e4
        bgt.s diskio_L0302                               ; +$02e8
        subq.b #1,d3                                     ; +$02ea
        move.w d3,-12(a6)                                ; +$02ec
        move.w #11,-10(a6)                               ; +$02f0
        sub.w d3,-10(a6)                                 ; +$02f6
        moveq #0,d0                                      ; +$02fa
diskio_L02fc:
        rts                                              ; +$02fc; return to caller
diskio_L02fe:
        moveq #24,d0                                     ; +$02fe
        rts                                              ; +$0300; return to caller
diskio_L0302:
        moveq #27,d0                                     ; +$0302
        rts                                              ; +$0304; return to caller
diskio_L0306:
        moveq #25,d0                                     ; +$0306
        rts                                              ; +$0308; return to caller
diskio_L030a:
        movea.l -24(a6),a5                               ; +$030a
        lea 1024(a5),a5                                  ; +$030e
        move.w -8(a6),d0                                 ; +$0312
        mulu.w #1088,d0                                  ; +$0316
        adda.l d0,a5                                     ; +$031a
        move.l #6000,d0                                  ; +$031c
        bsr.w diskio_start_timer                         ; +$0322; call internal helper
diskio_L0326:
        btst #1,1(a4)                                    ; +$0326
        bne.w diskio_L0424                               ; +$032c
        bsr.w diskio_poll_timer                          ; +$0330; call internal helper
        beq.w diskio_L0428                               ; +$0334
        tst.l 1088(a5)                                   ; +$0338
        beq.s diskio_L0326                               ; +$033c
        bsr.w diskio_L0494                               ; +$033e; call internal helper
        bne.s diskio_L02fe                               ; +$0342
        bsr.w diskio_L045e                               ; +$0344; call internal helper
        bne.s diskio_L0302                               ; +$0348
        cmp.w -18(a6),d1                                 ; +$034a
        bne.s diskio_L0302                               ; +$034e
        move.w d2,d3                                     ; +$0350
        lea 8(a5),a0                                     ; +$0352
        bsr.w diskio_L047e                               ; +$0356; call internal helper
        move.b #11,d0                                    ; +$035a
        sub.b -7(a6),d0                                  ; +$035e
        lea 8(a5),a0                                     ; +$0362
        bsr.w diskio_L054a                               ; +$0366; call internal helper
        bsr.w diskio_L04a2                               ; +$036a; call internal helper
        lea 48(a5),a0                                    ; +$036e
        bsr.w diskio_L054a                               ; +$0372; call internal helper
        cmp.w -16(a6),d3                                 ; +$0376
        blt.w diskio_L0416                               ; +$037a
        move.w -14(a6),d0                                ; +$037e
        add.w -16(a6),d0                                 ; +$0382
        cmp.w d0,d3                                      ; +$0386
        bge.w diskio_L0416                               ; +$0388
        btst #1,1(a4)                                    ; +$038c
        bne.w diskio_L0424                               ; +$0392
        move.w -4(a6),d0                                 ; +$0396
        btst d3,d0                                       ; +$039a
        bne.s diskio_L0416                               ; +$039c
        tst.b -29(a6)                                    ; +$039e
        beq.s diskio_L03d6                               ; +$03a2
        bsr.w diskio_L042c                               ; +$03a4; call internal helper
        movea.l -28(a6),a0                               ; +$03a8
        adda.l d1,a0                                     ; +$03ac
        lea 64(a5),a1                                    ; +$03ae
        bsr.w diskio_L051c                               ; +$03b2; call internal helper
        btst #1,1(a4)                                    ; +$03b6
        bne.s diskio_L0424                               ; +$03bc
        lea 64(a5),a0                                    ; +$03be
        move.w #1024,d1                                  ; +$03c2
        bsr.w diskio_L04a8                               ; +$03c6; call internal helper
        lea 56(a5),a0                                    ; +$03ca
        bsr.w diskio_L054a                               ; +$03ce; call internal helper
        bsr.s diskio_L043c                               ; +$03d2; call internal helper
        bra.s diskio_L0416                               ; +$03d4
diskio_L03d6:
        lea 64(a5),a0                                    ; +$03d6
        move.w #1024,d1                                  ; +$03da
        bsr.w diskio_L04a8                               ; +$03de; call internal helper
        move.l d0,-(sp)                                  ; +$03e2
        lea 56(a5),a0                                    ; +$03e4
        bsr.w diskio_L047e                               ; +$03e8; call internal helper
        cmp.l (sp)+,d0                                   ; +$03ec
        bne.w diskio_L0306                               ; +$03ee
        btst #1,1(a4)                                    ; +$03f2
        bne.s diskio_L0424                               ; +$03f8
        bsr.s diskio_L042c                               ; +$03fa; call internal helper
        lea 64(a5),a0                                    ; +$03fc
        movea.l -28(a6),a1                               ; +$0400
        adda.l d1,a1                                     ; +$0404
        bsr.w diskio_L04f4                               ; +$0406; call internal helper
        bsr.s diskio_L043c                               ; +$040a; call internal helper
        move.w -6(a6),d0                                 ; +$040c
        cmp.w -14(a6),d0                                 ; +$0410
        beq.s diskio_L0424                               ; +$0414
diskio_L0416:
        addq.w #1,-8(a6)                                 ; +$0416
        cmpi.w #11,-8(a6)                                ; +$041a
        bne.w diskio_L030a                               ; +$0420
diskio_L0424:
        moveq #0,d0                                      ; +$0424
        rts                                              ; +$0426; return to caller
diskio_L0428:
        moveq #-1,d0                                     ; +$0428
        rts                                              ; +$042a; return to caller
diskio_L042c:
        move.l d3,d1                                     ; +$042c
        sub.w -16(a6),d1                                 ; +$042e
        move.l #512,d0                                   ; +$0432
        mulu.w d0,d1                                     ; +$0438
        rts                                              ; +$043a; return to caller
diskio_L043c:
        move.w -4(a6),d0                                 ; +$043c
        bset d3,d0                                       ; +$0440
        move.w d0,-4(a6)                                 ; +$0442
        addq.w #1,-6(a6)                                 ; +$0446
        rts                                              ; +$044a; return to caller
diskio_L044c:
        movea.l a5,a0                                    ; +$044c
        moveq #10,d1                                     ; +$044e
        moveq #0,d0                                      ; +$0450
diskio_L0452:
        lea 1088(a0),a0                                  ; +$0452
        move.l d0,(a0)                                   ; +$0456
        dbf d1,diskio_L0452                              ; +$0458
        rts                                              ; +$045c; return to caller
diskio_L045e:
        lea 8(a5),a0                                     ; +$045e
        bsr.s diskio_L047e                               ; +$0462; call internal helper
        move.w d0,d3                                     ; +$0464
        andi.w #255,d3                                   ; +$0466
        move.w d0,d2                                     ; +$046a
        lsr.w #8,d2                                      ; +$046c
        swap d0                                          ; +$046e
        move.w d0,d1                                     ; +$0470
        andi.w #255,d1                                   ; +$0472
        lsr.w #8,d0                                      ; +$0476
        cmp.b #-1,d0                                     ; +$0478
        rts                                              ; +$047c; return to caller
diskio_L047e:
        move.l (a0)+,d0                                  ; +$047e
        move.l (a0)+,d1                                  ; +$0480
        andi.l #1431655765,d0                            ; +$0482
        andi.l #1431655765,d1                            ; +$0488
        add.l d0,d0                                      ; +$048e
        or.l d1,d0                                       ; +$0490
        rts                                              ; +$0492; return to caller
diskio_L0494:
        bsr.s diskio_L04a2                               ; +$0494; call internal helper
        move.l d0,-(sp)                                  ; +$0496
        lea 48(a5),a0                                    ; +$0498
        bsr.s diskio_L047e                               ; +$049c; call internal helper
        cmp.l (sp)+,d0                                   ; +$049e
        rts                                              ; +$04a0; return to caller
diskio_L04a2:
        lea 8(a5),a0                                     ; +$04a2
        moveq #40,d1                                     ; +$04a6
diskio_L04a8:
        move.l d2,-(sp)                                  ; +$04a8
        lsr.w #2,d1                                      ; +$04aa
        subq.w #1,d1                                     ; +$04ac
        moveq #0,d0                                      ; +$04ae
diskio_L04b0:
        move.l (a0)+,d2                                  ; +$04b0
        eor.l d2,d0                                      ; +$04b2
        dbf d1,diskio_L04b0                              ; +$04b4
        move.l (sp)+,d2                                  ; +$04b8
        andi.l #1431655765,d0                            ; +$04ba
        rts                                              ; +$04c0; return to caller
diskio_L04c2:
        movea.l -24(a6),a0                               ; +$04c2
        move.l #-1431655766,d0                           ; +$04c6
        move.l d0,d1                                     ; +$04cc
        move.l d0,d2                                     ; +$04ce
        move.l d0,d3                                     ; +$04d0
        move.l d0,d4                                     ; +$04d2
        move.l d0,d5                                     ; +$04d4
        move.l d0,d6                                     ; +$04d6
        move.l d0,d7                                     ; +$04d8
        lea 1024(a0),a1                                  ; +$04da
        cmpi.b #1,-29(a6)                                ; +$04de
        beq.s diskio_L04ea                               ; +$04e4
        lea 12992(a0),a1                                 ; +$04e6
diskio_L04ea:
        movem.l d0-d7,-(a1)                              ; +$04ea; register save/restore
        cmpa.l a1,a0                                     ; +$04ee
        bne.s diskio_L04ea                               ; +$04f0
        rts                                              ; +$04f2; return to caller
diskio_L04f4:
        movem.l d0-d3/a0-a2,-(sp)                        ; +$04f4; register save/restore
        moveq #127,d0                                    ; +$04f8
        lea 512(a0),a2                                   ; +$04fa
        move.l #1431655765,d3                            ; +$04fe
diskio_L0504:
        move.l (a0)+,d1                                  ; +$0504
        move.l (a2)+,d2                                  ; +$0506
        and.l d3,d1                                      ; +$0508
        and.l d3,d2                                      ; +$050a
        add.l d1,d1                                      ; +$050c
        or.l d2,d1                                       ; +$050e
        move.l d1,(a1)+                                  ; +$0510
        dbf d0,diskio_L0504                              ; +$0512
        movem.l (sp)+,d0-d3/a0-a2                        ; +$0516; register save/restore
        rts                                              ; +$051a; return to caller
diskio_L051c:
        movem.l d0-d5/a0-a1,-(sp)                        ; +$051c; register save/restore
        exg a1,a0                                        ; +$0520
        move.l d0,d3                                     ; +$0522
        lsr.l #2,d3                                      ; +$0524
        subq.l #1,d3                                     ; +$0526
        move.l d0,d5                                     ; +$0528
diskio_L052a:
        move.l (a1),d0                                   ; +$052a
        lsr.l #1,d0                                      ; +$052c
        bsr.s diskio_L0572                               ; +$052e; call internal helper
        move.l (a1)+,d0                                  ; +$0530
        lea (-4,a0,d5.l),a0                              ; +$0532
        bsr.s diskio_L0572                               ; +$0536; call internal helper
        suba.l d5,a0                                     ; +$0538
        dbf d3,diskio_L052a                              ; +$053a
        bsr.s diskio_L0554                               ; +$053e; call internal helper
        adda.l d5,a0                                     ; +$0540
        bsr.s diskio_L0554                               ; +$0542; call internal helper
        movem.l (sp)+,d0-d5/a0-a1                        ; +$0544; register save/restore
        rts                                              ; +$0548; return to caller
diskio_L054a:
        move.l d0,-(sp)                                  ; +$054a
        lsr.l #1,d0                                      ; +$054c
        bsr.s diskio_L0572                               ; +$054e; call internal helper
        move.l (sp)+,d0                                  ; +$0550
        bsr.s diskio_L0572                               ; +$0552; call internal helper
diskio_L0554:
        move.b (a0),d0                                   ; +$0554
        btst #0,-1(a0)                                   ; +$0556
        bne.s diskio_L056a                               ; +$055c
        btst #6,d0                                       ; +$055e
        bne.s diskio_L0570                               ; +$0562
        bset #7,d0                                       ; +$0564
        bra.s diskio_L056e                               ; +$0568
diskio_L056a:
        bclr #7,d0                                       ; +$056a
diskio_L056e:
        move.b d0,(a0)                                   ; +$056e
diskio_L0570:
        rts                                              ; +$0570; return to caller
diskio_L0572:
        andi.l #1431655765,d0                            ; +$0572
        move.l d0,d2                                     ; +$0578
        eori.l #1431655765,d2                            ; +$057a
        move.l d2,d1                                     ; +$0580
        add.l d2,d2                                      ; +$0582
        lsr.l #1,d1                                      ; +$0584
        bset #31,d1                                      ; +$0586
        and.l d2,d1                                      ; +$058a
        or.l d1,d0                                       ; +$058c
        btst #0,-1(a0)                                   ; +$058e
        beq.s diskio_L059a                               ; +$0594
        bclr #31,d0                                      ; +$0596
diskio_L059a:
        move.l d0,(a0)+                                  ; +$059a
        rts                                              ; +$059c; return to caller
; Program disk DMA pointer, sync word, length and control bits.
diskio_start_read_dma:
        lea $dff000.l,a1                                 ; +$059e; custom register base
        move.w #16384,36(a1)                             ; +$05a4
        move.w #-32752,150(a1)                           ; +$05aa
        move.w #26112,158(a1)                            ; +$05b0
        move.w #-27392,158(a1)                           ; +$05b6
        move.w #17545,126(a1)                            ; +$05bc
        move.l a0,32(a1)                                 ; +$05c2
        move.w #2,156(a1)                                ; +$05c6
        lsr.w #1,d0                                      ; +$05cc
        ori.w #-32768,d0                                 ; +$05ce
        move.w d0,36(a1)                                 ; +$05d2
        move.w d0,36(a1)                                 ; +$05d6
        rts                                              ; +$05da; return to caller
; Poll disk-block completion with CIA-B timer-based timeout.
diskio_wait_dma:
        lea $dff000.l,a1                                 ; +$05dc; custom register base
        move.l #2500,d0                                  ; +$05e2
        bsr.w diskio_start_timer                         ; +$05e8; call internal helper
diskio_L05ec:
        btst #1,31(a1)                                   ; +$05ec
        bne.s diskio_L05fe                               ; +$05f2
        bsr.w diskio_poll_timer                          ; +$05f4; call internal helper
        bne.s diskio_L05ec                               ; +$05f8
        moveq #-1,d0                                     ; +$05fa
        bra.s diskio_L0600                               ; +$05fc
diskio_L05fe:
        moveq #0,d0                                      ; +$05fe
diskio_L0600:
        move.w #2,$dff09c.l                              ; +$0600; INTREQ
        move.w #16384,$dff024.l                          ; +$0608; DSKLEN
        tst.l d0                                         ; +$0610
        rts                                              ; +$0612; return to caller
diskio_L0614:
        move.w #1024,$dff09e.l                           ; +$0614
        tst.w -30(a6)                                    ; +$061c
        bpl.s diskio_L0640                               ; +$0620
        moveq #-1,d1                                     ; +$0622
diskio_L0624:
        move.b d1,$bfd100.l                              ; +$0624; CIA-B PRB: drive select/side/step/motor
        move.w -36(a6),d0                                ; +$062a
        addq.l #3,d0                                     ; +$062e
        bclr d0,d1                                       ; +$0630
        move.b d1,$bfd100.l                              ; +$0632; CIA-B PRB: drive select/side/step/motor
        bset d0,d1                                       ; +$0638
        move.b d1,$bfd100.l                              ; +$063a; CIA-B PRB: drive select/side/step/motor
diskio_L0640:
        rts                                              ; +$0640; return to caller
diskio_L0642:
        moveq #-1,d1                                     ; +$0642
        move.b d1,$bfd100.l                              ; +$0644; CIA-B PRB: drive select/side/step/motor
        bclr #7,d1                                       ; +$064a
        bsr.s diskio_L0624                               ; +$064e; call internal helper
        move.l #200,d0                                   ; +$0650
        bra.w diskio_L073a                               ; +$0656
diskio_L065a:
        movem.l d2-d3,-(sp)                              ; +$065a; register save/restore
        move.l d2,d3                                     ; +$065e
        bsr.w diskio_L0700                               ; +$0660; call internal helper
        move.w -36(a6),d0                                ; +$0664
        add.w d0,d0                                      ; +$0668
        lea diskio_drive_state(pc),a0                    ; +$066a
        move.w (0,a0,d0.w),d0                            ; +$066e
        bpl.s diskio_L0678                               ; +$0672
        bsr.s diskio_L06a8                               ; +$0674; call internal helper
        bne.s diskio_L06a2                               ; +$0676
diskio_L0678:
        lsr.w #1,d0                                      ; +$0678
        lsr.w #1,d2                                      ; +$067a
        moveq #1,d1                                      ; +$067c
        sub.w d0,d2                                      ; +$067e
        beq.s diskio_L0690                               ; +$0680
        bpl.s diskio_L0688                               ; +$0682
        moveq #-1,d1                                     ; +$0684
        neg.w d2                                         ; +$0686
diskio_L0688:
        moveq #3,d0                                      ; +$0688
        bsr.s diskio_L06dc                               ; +$068a; call internal helper
        subq.w #1,d2                                     ; +$068c
        bne.s diskio_L0688                               ; +$068e
diskio_L0690:
        move.w -36(a6),d0                                ; +$0690
        add.w d0,d0                                      ; +$0694
        lea diskio_drive_state(pc),a0                    ; +$0696
        move.w d3,(0,a0,d0.w)                            ; +$069a
        bsr.s diskio_L0700                               ; +$069e; call internal helper
        moveq #0,d0                                      ; +$06a0
diskio_L06a2:
        movem.l (sp)+,d2-d3                              ; +$06a2; register save/restore
        rts                                              ; +$06a6; return to caller
diskio_L06a8:
        movem.l d2,-(sp)                                 ; +$06a8; register save/restore
        moveq #85,d2                                     ; +$06ac
diskio_L06ae:
        btst #4,$bfe001.l                                ; +$06ae; CIA-A PRA: disk/input status
        beq.s diskio_L06c6                               ; +$06b6
        moveq #3,d0                                      ; +$06b8
        moveq #-1,d1                                     ; +$06ba
        bsr.s diskio_L06dc                               ; +$06bc; call internal helper
        dbf d2,diskio_L06ae                              ; +$06be
        moveq #30,d0                                     ; +$06c2
        bra.s diskio_L06d6                               ; +$06c4
diskio_L06c6:
        move.w -36(a6),d0                                ; +$06c6
        add.w d0,d0                                      ; +$06ca
        lea diskio_drive_state(pc),a0                    ; +$06cc
        clr.w (0,a0,d0.w)                                ; +$06d0
        moveq #0,d0                                      ; +$06d4
diskio_L06d6:
        movem.l (sp)+,d2                                 ; +$06d6; register save/restore
        rts                                              ; +$06da; return to caller
diskio_L06dc:
        move.l d0,-(sp)                                  ; +$06dc
        bsr.s diskio_L070a                               ; +$06de; call internal helper
        tst.b d1                                         ; +$06e0
        bmi.s diskio_L06e8                               ; +$06e2
        bclr #1,d0                                       ; +$06e4
diskio_L06e8:
        bclr #0,d0                                       ; +$06e8
        move.b d0,$bfd100.l                              ; +$06ec; CIA-B PRB: drive select/side/step/motor
        bset #0,d0                                       ; +$06f2
        move.b d0,$bfd100.l                              ; +$06f6; CIA-B PRB: drive select/side/step/motor
        move.l (sp)+,d0                                  ; +$06fc
        bra.s diskio_L073a                               ; +$06fe
diskio_L0700:
        bsr.s diskio_L070a                               ; +$0700; call internal helper
        move.b d0,$bfd100.l                              ; +$0702; CIA-B PRB: drive select/side/step/motor
        rts                                              ; +$0708; return to caller
diskio_L070a:
        movem.w d1-d2,-(sp)                              ; +$070a; register save/restore
        move.w -36(a6),d0                                ; +$070e
        move.b $bfd100.l,d2                              ; +$0712; CIA-B PRB: drive select/side/step/motor
        ori.b #127,d2                                    ; +$0718
        addq.b #3,d0                                     ; +$071c
        bclr d0,d2                                       ; +$071e
        subq.b #3,d0                                     ; +$0720
        add.w d0,d0                                      ; +$0722
        move.w diskio_drive_state(pc,d0.w),d1            ; +$0724
        btst #0,d1                                       ; +$0728
        beq.s diskio_L0732                               ; +$072c
        bclr #2,d2                                       ; +$072e
diskio_L0732:
        move.b d2,d0                                     ; +$0732
        movem.w (sp)+,d1-d2                              ; +$0734; register save/restore
        rts                                              ; +$0738; return to caller
diskio_L073a:
        bsr.s diskio_start_timer                         ; +$073a; call internal helper
diskio_L073c:
        btst #0,$bfde00.l                                ; +$073c; CIA-B CRA: timer A control
        bne.s diskio_L073c                               ; +$0744
        subq.l #1,d0                                     ; +$0746
        bne.s diskio_L073a                               ; +$0748
        rts                                              ; +$074a; return to caller
; Poll CIA-B timer A control while decrementing the software counter.
diskio_poll_timer:
        btst #0,$bfde00.l                                ; +$074c; CIA-B CRA: timer A control
        bne.s diskio_L0772                               ; +$0754
        subq.l #1,d0                                     ; +$0756
        beq.s diskio_L0772                               ; +$0758
; Program CIA-B timer A for the polling delay.
diskio_start_timer:
        move.b #8,$bfde00.l                              ; +$075a; CIA-B CRA: timer A control
        move.b #-118,$bfd400.l                           ; +$0762; CIA-B timer A low
        move.b #2,$bfd500.l                              ; +$076a; CIA-B timer A high
diskio_L0772:
        rts                                              ; +$0772; return to caller
; Four cached drive-state words initialized to $ffff; updated via PC-relative pointers.
diskio_drive_state:
        dc.w    $ffff                  ; +$0774: writable resident state / buffer
        dc.w    $ffff                  ; +$0776: writable resident state / buffer
        dc.w    $ffff                  ; +$0778: writable resident state / buffer
        dc.w    $ffff                  ; +$077a: writable resident state / buffer
