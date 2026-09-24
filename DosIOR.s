********************************************************************************
*-------------------------------------------------------------------------------
* AmigaDOS Read File Function for AmigaDos disks
*
* Copyright (c) 1988-92 Rob Northen Computing, U.K. All Rights Reserved.
*
* File: DOSioR.s
*
* Date: 07.09.92
*-------------------------------------------------------------------------------
* AmigaDOS Read File Function
* on entry,
*	d0.l = function
*		0=load file
*	a0.l = full pathname of file, terminated with 0
*	a1.l = file buffer (even word boundary)
*	a2.l = workspace buffer ($3300 bytes of CHIPmem required)
* on exit,
*	d0.l = result
*		  0 = no error
*		204 = directory not found
*		205 = file not found
*		225 = not a DOS disk
*		405 = bad block checksum
*		see diskio.s for error codes
*	d1.l = length of file in bytes
*	all others registers are preserved
*
* IMPORTANT :
*
* File loading time can be greatly reduced by writing the file onto the
* disk using the source file DOSIO.S. This has the advantage over using
* AmigaDos to copy the file onto the disk by writing out the file's data
* block lists, as well as the actual data blocks, on contiguous sectors.
*-------------------------------------------------------------------------------

        opt o-                  ; Preserve original instruction encodings.

; Annotated 68000 disassembly. Offsets are relative to this file entry.
; Data remains data; branch widths and absolute address widths are explicit.
; Original copyright and calling convention above are retained.
; Labels ending Lxxxx are address-derived control-flow labels; descriptive
; labels/comments document observable operations, not recovered source names.
; This module contains writable state and must not be placed in ROM.
; opt o- intentionally disables assembler shortening/rewriting for exact bytes.

; Public entry; preserves caller registers as documented above.
dosior:
        movem.l d0-a6,-(sp)                              ; +$0000; register save/restore
        move.l d0,d6                                     ; +$0004
        move.l d1,d7                                     ; +$0006
        movea.l a0,a4                                    ; +$0008
        movea.l a1,a5                                    ; +$000a
        movea.l a2,a6                                    ; +$000c
        bsr.w dosior_L0178                               ; +$000e; call internal helper
        tst.l d6                                         ; +$0012
        bne.s dosior_L001c                               ; +$0014
        bsr.s dosior_L0032                               ; +$0016; call internal helper
        move.l d1,4(sp)                                  ; +$0018
dosior_L001c:
        move.l d0,(sp)                                   ; +$001c
        moveq #0,d1                                      ; +$001e
        moveq #0,d2                                      ; +$0020
        move.w #-32768,d3                                ; +$0022
        bsr.w dosior_L0294                               ; +$0026; call internal helper
        movem.l (sp)+,d0-a6                              ; +$002a; register save/restore
        tst.l d0                                         ; +$002e
        rts                                              ; +$0030; return to caller
dosior_L0032:
        bsr.w dosior_L00d4                               ; +$0032; call internal helper
        bne.s dosior_L00a8                               ; +$0036
        move.l 324(a0),d7                                ; +$0038
        move.l d7,-(sp)                                  ; +$003c
        lea 312(a0),a2                                   ; +$003e
        bsr.s dosior_L00aa                               ; +$0042; call internal helper
        bne.s dosior_L00a4                               ; +$0044
dosior_L0046:
        move.l d1,d2                                     ; +$0046
        move.l d1,d4                                     ; +$0048
        movea.l a5,a3                                    ; +$004a
        move.l d7,d6                                     ; +$004c
        lsr.l #8,d6                                      ; +$004e
        lsr.l #1,d6                                      ; +$0050
        bne.s dosior_L0056                               ; +$0052
        movea.l a6,a3                                    ; +$0054
dosior_L0056:
        bsr.s dosior_L00aa                               ; +$0056; call internal helper
        bne.s dosior_L00a4                               ; +$0058
        addq.l #1,d2                                     ; +$005a
        move.l d1,d5                                     ; +$005c
        beq.s dosior_L0068                               ; +$005e
        cmp.l d1,d2                                      ; +$0060
        bne.s dosior_L0068                               ; +$0062
        subq.l #1,d6                                     ; +$0064
        bne.s dosior_L0056                               ; +$0066
dosior_L0068:
        move.l d4,d1                                     ; +$0068
        sub.l d4,d2                                      ; +$006a
        movea.l a3,a0                                    ; +$006c
        bsr.w dosior_L0292                               ; +$006e; call internal helper
        bne.s dosior_L00a4                               ; +$0072
dosior_L0074:
        bsr.w dosior_L0240                               ; +$0074; call internal helper
        bne.s dosior_L00a4                               ; +$0078
        move.l 12(a0),d0                                 ; +$007a
        sub.l d0,d7                                      ; +$007e
        lea 24(a0),a0                                    ; +$0080
        ror.l #2,d0                                      ; +$0084
        bra.s dosior_L008a                               ; +$0086
dosior_L0088:
        move.l (a0)+,(a5)+                               ; +$0088
dosior_L008a:
        dbf d0,dosior_L0088                              ; +$008a
        clr.w d0                                         ; +$008e
        rol.l #2,d0                                      ; +$0090
        bra.s dosior_L0096                               ; +$0092
dosior_L0094:
        move.b (a0)+,(a5)+                               ; +$0094
dosior_L0096:
        dbf d0,dosior_L0094                              ; +$0096
        subq.l #1,d2                                     ; +$009a
        bne.s dosior_L0074                               ; +$009c
        move.l d5,d1                                     ; +$009e
        move.l d7,d0                                     ; +$00a0
        bne.s dosior_L0046                               ; +$00a2
dosior_L00a4:
        move.l (sp)+,d1                                  ; +$00a4
        tst.l d0                                         ; +$00a6
dosior_L00a8:
        rts                                              ; +$00a8; return to caller
dosior_L00aa:
        move.l d2,-(sp)                                  ; +$00aa
        movea.l a6,a0                                    ; +$00ac
        move.l 8(a0),d0                                  ; +$00ae
        bne.s dosior_L00c4                               ; +$00b2
        move.l 504(a0),d1                                ; +$00b4
        beq.s dosior_L00ce                               ; +$00b8
        bsr.w dosior_L023c                               ; +$00ba; call internal helper
        bne.s dosior_L00ce                               ; +$00be
        lea 312(a0),a2                                   ; +$00c0
dosior_L00c4:
        move.l -(a2),d1                                  ; +$00c4
        moveq #1,d0                                      ; +$00c6
        sub.l d0,8(a0)                                   ; +$00c8
        moveq #0,d0                                      ; +$00cc
dosior_L00ce:
        move.l (sp)+,d2                                  ; +$00ce
        tst.l d0                                         ; +$00d0
        rts                                              ; +$00d2; return to caller
dosior_L00d4:
        move.l a4,-(sp)                                  ; +$00d4
        movea.l a6,a0                                    ; +$00d6
        bsr.w dosior_L0214                               ; +$00d8; call internal helper
        bne.w dosior_L0172                               ; +$00dc
dosior_L00e0:
        lea dosior_volume_state(pc),a1                   ; +$00e0
        move.w d1,(a1)                                   ; +$00e4
        move.l a4,(sp)                                   ; +$00e6
        bsr.w dosior_L01c0                               ; +$00e8; call internal helper
        beq.w dosior_L0172                               ; +$00ec
        lsl.w #2,d0                                      ; +$00f0
dosior_L00f2:
        lea dosior_L0268(pc),a1                          ; +$00f2
        move.w d1,(a1)+                                  ; +$00f6
        move.w d0,(a1)+                                  ; +$00f8
        move.l (0,a0,d0.w),d1                            ; +$00fa
        beq.s dosior_L014a                               ; +$00fe
        bsr.w dosior_L023c                               ; +$0100; call internal helper
        bne.s dosior_L0172                               ; +$0104
        move.l #225,d0                                   ; +$0106
        moveq #2,d2                                      ; +$010c
        cmp.l (a0),d2                                    ; +$010e
        bne.s dosior_L0172                               ; +$0110
        moveq #2,d2                                      ; +$0112
        cmp.b #3,d6                                      ; +$0114
        beq.s dosior_L0120                               ; +$0118
        tst.b (a4)                                       ; +$011a
        bne.s dosior_L0120                               ; +$011c
        moveq #-3,d2                                     ; +$011e
dosior_L0120:
        cmp.l 508(a0),d2                                 ; +$0120
        bne.s dosior_L0144                               ; +$0124
        lea 432(a0),a1                                   ; +$0126
        lea dosior_name_buffer(pc),a2                    ; +$012a
        moveq #0,d2                                      ; +$012e
        move.b (a1)+,d2                                  ; +$0130
        subq.b #1,d2                                     ; +$0132
dosior_L0134:
        move.b (a1)+,d0                                  ; +$0134
        bsr.s dosior_L01ac                               ; +$0136; call internal helper
        cmp.b (a2)+,d0                                   ; +$0138
        dbne d2,dosior_L0134                             ; +$013a
        bne.s dosior_L0144                               ; +$013e
        tst.b (a2)                                       ; +$0140
        beq.s dosior_L0162                               ; +$0142
dosior_L0144:
        move.w #496,d0                                   ; +$0144
        bra.s dosior_L00f2                               ; +$0148
dosior_L014a:
        move.l #204,d0                                   ; +$014a
        cmp.b #3,d6                                      ; +$0150
        beq.s dosior_L0172                               ; +$0154
        tst.b (a4)                                       ; +$0156
        bne.s dosior_L0172                               ; +$0158
        move.l #205,d0                                   ; +$015a
        bra.s dosior_L0172                               ; +$0160
dosior_L0162:
        tst.b (a4)                                       ; +$0162
        bne.w dosior_L00e0                               ; +$0164
        lea dosior_L026c(pc),a1                          ; +$0168
        move.w 498(a0),(a1)                              ; +$016c
        moveq #0,d0                                      ; +$0170
dosior_L0172:
        movea.l (sp)+,a4                                 ; +$0172
        tst.l d0                                         ; +$0174
        rts                                              ; +$0176; return to caller
dosior_L0178:
        movea.l a4,a0                                    ; +$0178
        bsr.s dosior_L01aa                               ; +$017a; call internal helper
        cmp.b #68,d0                                     ; +$017c
        bne.s dosior_L01a8                               ; +$0180
        bsr.s dosior_L01aa                               ; +$0182; call internal helper
        cmp.b #70,d0                                     ; +$0184
        bne.s dosior_L01a8                               ; +$0188
        move.b (a0)+,d0                                  ; +$018a
        subi.b #48,d0                                    ; +$018c
        blt.s dosior_L01a8                               ; +$0190
        cmp.b #51,d0                                     ; +$0192
        bgt.s dosior_L01a8                               ; +$0196
        cmpi.b #58,(a0)+                                 ; +$0198
        bne.s dosior_L01a8                               ; +$019c
        lea dosior_L026e(pc),a0                          ; +$019e
        move.b d0,1(a0)                                  ; +$01a2
        addq.w #4,a4                                     ; +$01a6
dosior_L01a8:
        rts                                              ; +$01a8; return to caller
dosior_L01aa:
        move.b (a0)+,d0                                  ; +$01aa
dosior_L01ac:
        cmp.b #97,d0                                     ; +$01ac
        blt.s dosior_L01bc                               ; +$01b0
        cmp.b #122,d0                                    ; +$01b2
        bgt.s dosior_L01bc                               ; +$01b6
        andi.b #-33,d0                                   ; +$01b8
dosior_L01bc:
        tst.b d0                                         ; +$01bc
        rts                                              ; +$01be; return to caller
dosior_L01c0:
        movem.l d1/a0-a1,-(sp)                           ; +$01c0; register save/restore
        moveq #0,d0                                      ; +$01c4
        moveq #-1,d1                                     ; +$01c6
        movea.l a4,a0                                    ; +$01c8
        lea dosior_name_buffer(pc),a1                    ; +$01ca
        clr.b (a1)                                       ; +$01ce
dosior_L01d0:
        addq.l #1,d1                                     ; +$01d0
        tst.b (a4)                                       ; +$01d2
        beq.s dosior_L01de                               ; +$01d4
        cmpi.b #47,(a4)+                                 ; +$01d6
        bne.s dosior_L01d0                               ; +$01da
        subq.w #1,a4                                     ; +$01dc
dosior_L01de:
        tst.l d1                                         ; +$01de
        beq.s dosior_L020c                               ; +$01e0
dosior_L01e2:
        mulu.w #13,d1                                    ; +$01e2
        move.b (a0)+,d0                                  ; +$01e6
        bsr.s dosior_L01ac                               ; +$01e8; call internal helper
        move.b d0,(a1)+                                  ; +$01ea
        add.w d0,d1                                      ; +$01ec
        andi.l #2047,d1                                  ; +$01ee
        cmpa.l a0,a4                                     ; +$01f4
        bne.s dosior_L01e2                               ; +$01f6
        cmpi.b #47,(a4)                                  ; +$01f8
        bne.s dosior_L0200                               ; +$01fc
        addq.w #1,a4                                     ; +$01fe
dosior_L0200:
        clr.b (a1)+                                      ; +$0200
        divu.w #72,d1                                    ; +$0202
        clr.w d1                                         ; +$0206
        swap d1                                          ; +$0208
        addq.w #6,d1                                     ; +$020a
dosior_L020c:
        move.l d1,d0                                     ; +$020c
        movem.l (sp)+,d1/a0-a1                           ; +$020e; register save/restore
        rts                                              ; +$0212; return to caller
dosior_L0214:
        move.w #880,d1                                   ; +$0214
        bsr.s dosior_L023c                               ; +$0218; call internal helper
        bne.s dosior_L023a                               ; +$021a
        move.l #225,d0                                   ; +$021c
        moveq #2,d2                                      ; +$0222
        cmp.l (a0),d2                                    ; +$0224
        bne.s dosior_L023a                               ; +$0226
        moveq #1,d2                                      ; +$0228
        cmp.l 508(a0),d2                                 ; +$022a
        bne.s dosior_L023a                               ; +$022e
        lea dosior_L0266(pc),a1                          ; +$0230
        move.w 318(a0),(a1)                              ; +$0234
        moveq #0,d0                                      ; +$0238
dosior_L023a:
        rts                                              ; +$023a; return to caller
dosior_L023c:
        bsr.s dosior_read_block                          ; +$023c; call internal helper
        bne.s dosior_L024a                               ; +$023e
dosior_L0240:
        bsr.s dosior_block_checksum                      ; +$0240; call internal helper
        beq.s dosior_L024a                               ; +$0242
        move.l #405,d0                                   ; +$0244
dosior_L024a:
        rts                                              ; +$024a; return to caller
; Sum 128 longwords of a 512-byte filesystem block; return negated sum.
dosior_block_checksum:
        movem.l d1/a0,-(sp)                              ; +$024c; register save/restore
        moveq #0,d0                                      ; +$0250
        move.w #127,d1                                   ; +$0252
dosior_L0256:
        add.l (a0)+,d0                                   ; +$0256
        dbf d1,dosior_L0256                              ; +$0258
        neg.l d0                                         ; +$025c
        movem.l (sp)+,d1/a0                              ; +$025e; register save/restore
        rts                                              ; +$0262; return to caller
; Writable filesystem/drive state; PC-relative accesses retain original placement.
dosior_volume_state:
        dc.w    $0000                  ; +$0264: writable resident state / buffer
dosior_L0266:
        dc.w    $0000                  ; +$0266: writable resident state / buffer
dosior_L0268:
        dc.w    $0000                  ; +$0268: writable resident state / buffer
        dc.w    $0000                  ; +$026a: writable resident state / buffer
dosior_L026c:
        dc.w    $0000                  ; +$026c: writable resident state / buffer
dosior_L026e:
        dc.w    $0000                  ; +$026e: writable resident state / buffer
; Writable filename scratch area, not executable instructions.
dosior_name_buffer:
        dc.w    $0000                  ; +$0270: writable resident state / buffer
        dc.w    $0000                  ; +$0272: writable resident state / buffer
        dc.w    $0000                  ; +$0274: writable resident state / buffer
        dc.w    $0000                  ; +$0276: writable resident state / buffer
        dc.w    $0000                  ; +$0278: writable resident state / buffer
        dc.w    $0000                  ; +$027a: writable resident state / buffer
        dc.w    $0000                  ; +$027c: writable resident state / buffer
        dc.w    $0000                  ; +$027e: writable resident state / buffer
        dc.w    $0000                  ; +$0280: writable resident state / buffer
        dc.w    $0000                  ; +$0282: writable resident state / buffer
        dc.w    $0000                  ; +$0284: writable resident state / buffer
        dc.w    $0000                  ; +$0286: writable resident state / buffer
        dc.w    $0000                  ; +$0288: writable resident state / buffer
        dc.w    $0000                  ; +$028a: writable resident state / buffer
        dc.w    $0000                  ; +$028c: writable resident state / buffer
        dc.w    $0000                  ; +$028e: writable resident state / buffer
dosior_read_block:
        moveq #1,d2                                      ; +$0290
dosior_L0292:
        moveq #0,d3                                      ; +$0292
dosior_L0294:
        movea.l a6,a1                                    ; +$0294
        move.w dosior_L026e(pc),d0                       ; +$0296
; Embedded read-only low-level disk driver; distinct from full DiskIO.
diskior:
        movem.l d1-a5,-(sp)                              ; +$029a; register save/restore
        link.w a6,#-36                                   ; +$029e
        move.w d0,d4                                     ; +$02a2
        andi.w #3,d4                                     ; +$02a4
        move.w d4,-36(a6)                                ; +$02a8
        move.w d1,-34(a6)                                ; +$02ac
        move.w d2,-32(a6)                                ; +$02b0
        move.w d3,-30(a6)                                ; +$02b4
        move.l a0,-28(a6)                                ; +$02b8
        move.l a1,-24(a6)                                ; +$02bc
        ror.w #2,d0                                      ; +$02c0
        andi.w #1,d0                                     ; +$02c2
        addq.w #1,d0                                     ; +$02c6
        move.w d0,-20(a6)                                ; +$02c8
        moveq #0,d0                                      ; +$02cc
        move.w d2,d3                                     ; +$02ce
        beq.s dosior_L0342                               ; +$02d0
        moveq #30,d0                                     ; +$02d2
        add.w d1,d3                                      ; +$02d4
        cmp.w #1760,d3                                   ; +$02d6
        bgt.w dosior_L036c                               ; +$02da
        andi.l #65535,d1                                 ; +$02de
        divu.w #11,d1                                    ; +$02e4
        cmpi.w #1,-20(a6)                                ; +$02e8
        beq.s dosior_L02f2                               ; +$02ee
        add.w d1,d1                                      ; +$02f0
dosior_L02f2:
        move.w d1,-18(a6)                                ; +$02f2
        swap d1                                          ; +$02f6
        move.w d1,-16(a6)                                ; +$02f8
        bsr.w dosior_L06cc                               ; +$02fc; call internal helper
dosior_L0300:
        move.w -16(a6),d0                                ; +$0300
        moveq #11,d1                                     ; +$0304
        sub.w d0,d1                                      ; +$0306
        cmp.w -32(a6),d1                                 ; +$0308
        ble.s dosior_L0312                               ; +$030c
        move.w -32(a6),d1                                ; +$030e
dosior_L0312:
        move.w d1,-14(a6)                                ; +$0312
        bsr.s dosior_L0376                               ; +$0316; call internal helper
        bne.s dosior_L0342                               ; +$0318
        move.w -32(a6),d0                                ; +$031a
        sub.w -14(a6),d0                                 ; +$031e
        beq.s dosior_L0342                               ; +$0322
        move.w d0,-32(a6)                                ; +$0324
        move.w -14(a6),d0                                ; +$0328
        lsl.l #8,d0                                      ; +$032c
        add.l d0,d0                                      ; +$032e
        add.l d0,-28(a6)                                 ; +$0330
        clr.w -16(a6)                                    ; +$0334
        move.w -20(a6),d0                                ; +$0338
        add.w d0,-18(a6)                                 ; +$033c
        bra.s dosior_L0300                               ; +$0340
dosior_L0342:
        move.l d0,-(sp)                                  ; +$0342
        bsr.w dosior_L069e                               ; +$0344; call internal helper
        move.l (sp)+,d0                                  ; +$0348
        beq.s dosior_L036c                               ; +$034a
        moveq #0,d1                                      ; +$034c
        move.w -18(a6),d1                                ; +$034e
        cmpi.w #1,-20(a6)                                ; +$0352
        beq.s dosior_L035c                               ; +$0358
        lsr.w #1,d1                                      ; +$035a
dosior_L035c:
        mulu.w #11,d1                                    ; +$035c
        add.w -16(a6),d1                                 ; +$0360
        add.w -6(a6),d1                                  ; +$0364
        move.l d1,40(sp)                                 ; +$0368
dosior_L036c:
        unlk a6                                          ; +$036c
        tst.l d0                                         ; +$036e
        movem.l (sp)+,d1-a5                              ; +$0370; register save/restore
        rts                                              ; +$0374; return to caller
dosior_L0376:
        moveq #2,d4                                      ; +$0376
dosior_L0378:
        clr.w -4(a6)                                     ; +$0378
        clr.w -6(a6)                                     ; +$037c
        clr.w -8(a6)                                     ; +$0380
        move.w -18(a6),d2                                ; +$0384
        bsr.w dosior_L06e4                               ; +$0388; call internal helper
        bne.w dosior_L0432                               ; +$038c
        moveq #29,d0                                     ; +$0390
        btst #2,$bfe001.l                                ; +$0392; CIA-A PRA: disk/input status
        beq.w dosior_L0432                               ; +$039a
        movea.l -24(a6),a5                               ; +$039e
        lea 1024(a5),a5                                  ; +$03a2
        move.l #-1431655766,(a5)                         ; +$03a6
        move.w #17545,4(a5)                              ; +$03ac
        bsr.w dosior_L058a                               ; +$03b2; call internal helper
        bsr.w dosior_L0452                               ; +$03b6; call internal helper
        bne.s dosior_L0432                               ; +$03ba
        move.w -12(a6),d0                                ; +$03bc
        beq.s dosior_L0408                               ; +$03c0
        mulu.w #1088,d0                                  ; +$03c2
        lea 6(a5),a0                                     ; +$03c6
        bsr.w dosior_L0628                               ; +$03ca; call internal helper
        lea $dff01e.l,a4                                 ; +$03ce
        bsr.w dosior_L04aa                               ; +$03d4; call internal helper
        bne.s dosior_L044a                               ; +$03d8
        move.w -6(a6),d0                                 ; +$03da
        sub.w -14(a6),d0                                 ; +$03de
        beq.s dosior_L044e                               ; +$03e2
        movea.l -24(a6),a5                               ; +$03e4
        lea 1024(a5),a5                                  ; +$03e8
        move.w -12(a6),d0                                ; +$03ec
        mulu.w #1088,d0                                  ; +$03f0
        adda.l d0,a5                                     ; +$03f4
        move.l #-1431655766,(a5)                         ; +$03f6
        move.w #17545,4(a5)                              ; +$03fc
        movea.l a5,a0                                    ; +$0402
        bsr.w dosior_L0628                               ; +$0404; call internal helper
dosior_L0408:
        move.w -10(a6),d0                                ; +$0408
        beq.s dosior_L0426                               ; +$040c
        mulu.w #1088,d0                                  ; +$040e
        lea 6(a5),a0                                     ; +$0412
        bsr.w dosior_L0628                               ; +$0416; call internal helper
        lea -2(a6),a4                                    ; +$041a
        clr.w (a4)                                       ; +$041e
        bsr.w dosior_L04aa                               ; +$0420; call internal helper
        bne.s dosior_L044a                               ; +$0424
dosior_L0426:
        move.w -6(a6),d0                                 ; +$0426
        sub.w -14(a6),d0                                 ; +$042a
        beq.s dosior_L044e                               ; +$042e
        moveq #26,d0                                     ; +$0430
dosior_L0432:
        move.l d0,-(sp)                                  ; +$0432
        moveq #2,d2                                      ; +$0434
        bsr.w dosior_L06e4                               ; +$0436; call internal helper
        bsr.w dosior_L0732                               ; +$043a; call internal helper
        move.l (sp)+,d0                                  ; +$043e
        btst #2,$bfe001.l                                ; +$0440; CIA-A PRA: disk/input status
        beq.s dosior_L044e                               ; +$0448
dosior_L044a:
        dbf d4,dosior_L0378                              ; +$044a
dosior_L044e:
        bra.w dosior_L068a                               ; +$044e
dosior_L0452:
        moveq #10,d2                                     ; +$0452
dosior_L0454:
        lea 6(a5),a0                                     ; +$0454
        move.w #64,d0                                    ; +$0458
        bsr.w dosior_L0628                               ; +$045c; call internal helper
        bsr.w dosior_L0666                               ; +$0460; call internal helper
        bne.s dosior_L049c                               ; +$0464
        bsr.w dosior_L05d2                               ; +$0466; call internal helper
        beq.s dosior_L0472                               ; +$046a
        dbf d2,dosior_L0454                              ; +$046c
        bra.s dosior_L049e                               ; +$0470
dosior_L0472:
        bsr.w dosior_L059c                               ; +$0472; call internal helper
        bne.s dosior_L04a2                               ; +$0476
        cmp.w -18(a6),d1                                 ; +$0478
        bne.s dosior_L04a2                               ; +$047c
        cmp.b #11,d2                                     ; +$047e
        bge.s dosior_L04a2                               ; +$0482
        cmp.b #11,d3                                     ; +$0484
        bgt.s dosior_L04a2                               ; +$0488
        subq.b #1,d3                                     ; +$048a
        move.w d3,-12(a6)                                ; +$048c
        move.w #11,-10(a6)                               ; +$0490
        sub.w d3,-10(a6)                                 ; +$0496
        moveq #0,d0                                      ; +$049a
dosior_L049c:
        rts                                              ; +$049c; return to caller
dosior_L049e:
        moveq #24,d0                                     ; +$049e
        rts                                              ; +$04a0; return to caller
dosior_L04a2:
        moveq #27,d0                                     ; +$04a2
        rts                                              ; +$04a4; return to caller
dosior_L04a6:
        moveq #25,d0                                     ; +$04a6
        rts                                              ; +$04a8; return to caller
dosior_L04aa:
        movea.l -24(a6),a5                               ; +$04aa
        lea 1024(a5),a5                                  ; +$04ae
        move.w -8(a6),d0                                 ; +$04b2
        mulu.w #1088,d0                                  ; +$04b6
        adda.l d0,a5                                     ; +$04ba
        move.l #6000,d0                                  ; +$04bc
        bsr.w dosior_L07e4                               ; +$04c2; call internal helper
dosior_L04c6:
        btst #1,1(a4)                                    ; +$04c6
        bne.w dosior_L0562                               ; +$04cc
        bsr.w dosior_L07d6                               ; +$04d0; call internal helper
        beq.w dosior_L0566                               ; +$04d4
        tst.l 1088(a5)                                   ; +$04d8
        beq.s dosior_L04c6                               ; +$04dc
        bsr.w dosior_L05d2                               ; +$04de; call internal helper
        bne.s dosior_L049e                               ; +$04e2
        bsr.w dosior_L059c                               ; +$04e4; call internal helper
        bne.s dosior_L04a2                               ; +$04e8
        cmp.w -18(a6),d1                                 ; +$04ea
        bne.s dosior_L04a2                               ; +$04ee
        move.w d2,d3                                     ; +$04f0
        cmp.w -16(a6),d3                                 ; +$04f2
        blt.s dosior_L0554                               ; +$04f6
        move.w -14(a6),d0                                ; +$04f8
        add.w -16(a6),d0                                 ; +$04fc
        cmp.w d0,d3                                      ; +$0500
        bge.s dosior_L0554                               ; +$0502
        btst #1,1(a4)                                    ; +$0504
        bne.s dosior_L0562                               ; +$050a
        move.w -4(a6),d0                                 ; +$050c
        btst d3,d0                                       ; +$0510
        bne.s dosior_L0554                               ; +$0512
        lea 64(a5),a0                                    ; +$0514
        move.w #1024,d1                                  ; +$0518
        bsr.w dosior_L05e6                               ; +$051c; call internal helper
        move.l d0,-(sp)                                  ; +$0520
        lea 56(a5),a0                                    ; +$0522
        bsr.w dosior_L05bc                               ; +$0526; call internal helper
        cmp.l (sp)+,d0                                   ; +$052a
        bne.w dosior_L04a6                               ; +$052c
        btst #1,1(a4)                                    ; +$0530
        bne.s dosior_L0562                               ; +$0536
        bsr.s dosior_L056a                               ; +$0538; call internal helper
        lea 64(a5),a0                                    ; +$053a
        movea.l -28(a6),a1                               ; +$053e
        adda.l d1,a1                                     ; +$0542
        bsr.w dosior_L0600                               ; +$0544; call internal helper
        bsr.s dosior_L057a                               ; +$0548; call internal helper
        move.w -6(a6),d0                                 ; +$054a
        cmp.w -14(a6),d0                                 ; +$054e
        beq.s dosior_L0562                               ; +$0552
dosior_L0554:
        addq.w #1,-8(a6)                                 ; +$0554
        cmpi.w #11,-8(a6)                                ; +$0558
        bne.w dosior_L04aa                               ; +$055e
dosior_L0562:
        moveq #0,d0                                      ; +$0562
        rts                                              ; +$0564; return to caller
dosior_L0566:
        moveq #-1,d0                                     ; +$0566
        rts                                              ; +$0568; return to caller
dosior_L056a:
        move.l d3,d1                                     ; +$056a
        sub.w -16(a6),d1                                 ; +$056c
        move.l #512,d0                                   ; +$0570
        mulu.w d0,d1                                     ; +$0576
        rts                                              ; +$0578; return to caller
dosior_L057a:
        move.w -4(a6),d0                                 ; +$057a
        bset d3,d0                                       ; +$057e
        move.w d0,-4(a6)                                 ; +$0580
        addq.w #1,-6(a6)                                 ; +$0584
        rts                                              ; +$0588; return to caller
dosior_L058a:
        movea.l a5,a0                                    ; +$058a
        moveq #10,d1                                     ; +$058c
        moveq #0,d0                                      ; +$058e
dosior_L0590:
        lea 1088(a0),a0                                  ; +$0590
        move.l d0,(a0)                                   ; +$0594
        dbf d1,dosior_L0590                              ; +$0596
        rts                                              ; +$059a; return to caller
dosior_L059c:
        lea 8(a5),a0                                     ; +$059c
        bsr.s dosior_L05bc                               ; +$05a0; call internal helper
        move.w d0,d3                                     ; +$05a2
        andi.w #255,d3                                   ; +$05a4
        move.w d0,d2                                     ; +$05a8
        lsr.w #8,d2                                      ; +$05aa
        swap d0                                          ; +$05ac
        move.w d0,d1                                     ; +$05ae
        andi.w #255,d1                                   ; +$05b0
        lsr.w #8,d0                                      ; +$05b4
        cmp.b #-1,d0                                     ; +$05b6
        rts                                              ; +$05ba; return to caller
dosior_L05bc:
        move.l (a0)+,d0                                  ; +$05bc
        move.l (a0)+,d1                                  ; +$05be
        andi.l #1431655765,d0                            ; +$05c0
        andi.l #1431655765,d1                            ; +$05c6
        add.l d0,d0                                      ; +$05cc
        or.l d1,d0                                       ; +$05ce
        rts                                              ; +$05d0; return to caller
dosior_L05d2:
        bsr.s dosior_L05e0                               ; +$05d2; call internal helper
        move.l d0,-(sp)                                  ; +$05d4
        lea 48(a5),a0                                    ; +$05d6
        bsr.s dosior_L05bc                               ; +$05da; call internal helper
        cmp.l (sp)+,d0                                   ; +$05dc
        rts                                              ; +$05de; return to caller
dosior_L05e0:
        lea 8(a5),a0                                     ; +$05e0
        moveq #40,d1                                     ; +$05e4
dosior_L05e6:
        move.l d2,-(sp)                                  ; +$05e6
        lsr.w #2,d1                                      ; +$05e8
        subq.w #1,d1                                     ; +$05ea
        moveq #0,d0                                      ; +$05ec
dosior_L05ee:
        move.l (a0)+,d2                                  ; +$05ee
        eor.l d2,d0                                      ; +$05f0
        dbf d1,dosior_L05ee                              ; +$05f2
        move.l (sp)+,d2                                  ; +$05f6
        andi.l #1431655765,d0                            ; +$05f8
        rts                                              ; +$05fe; return to caller
dosior_L0600:
        movem.l d0-d3/a0-a2,-(sp)                        ; +$0600; register save/restore
        moveq #127,d0                                    ; +$0604
        lea 512(a0),a2                                   ; +$0606
        move.l #1431655765,d3                            ; +$060a
dosior_L0610:
        move.l (a0)+,d1                                  ; +$0610
        move.l (a2)+,d2                                  ; +$0612
        and.l d3,d1                                      ; +$0614
        and.l d3,d2                                      ; +$0616
        add.l d1,d1                                      ; +$0618
        or.l d2,d1                                       ; +$061a
        move.l d1,(a1)+                                  ; +$061c
        dbf d0,dosior_L0610                              ; +$061e
        movem.l (sp)+,d0-d3/a0-a2                        ; +$0622; register save/restore
        rts                                              ; +$0626; return to caller
dosior_L0628:
        lea $dff000.l,a1                                 ; +$0628; custom register base
        move.w #16384,36(a1)                             ; +$062e
        move.w #-32752,150(a1)                           ; +$0634
        move.w #26112,158(a1)                            ; +$063a
        move.w #-27392,158(a1)                           ; +$0640
        move.w #17545,126(a1)                            ; +$0646
        move.l a0,32(a1)                                 ; +$064c
        move.w #2,156(a1)                                ; +$0650
        lsr.w #1,d0                                      ; +$0656
        ori.w #-32768,d0                                 ; +$0658
        move.w d0,36(a1)                                 ; +$065c
        move.w d0,36(a1)                                 ; +$0660
        rts                                              ; +$0664; return to caller
dosior_L0666:
        lea $dff000.l,a1                                 ; +$0666; custom register base
        move.l #2500,d0                                  ; +$066c
        bsr.w dosior_L07e4                               ; +$0672; call internal helper
dosior_L0676:
        btst #1,31(a1)                                   ; +$0676
        bne.s dosior_L0688                               ; +$067c
        bsr.w dosior_L07d6                               ; +$067e; call internal helper
        bne.s dosior_L0676                               ; +$0682
        moveq #-1,d0                                     ; +$0684
        bra.s dosior_L068a                               ; +$0686
dosior_L0688:
        moveq #0,d0                                      ; +$0688
dosior_L068a:
        move.w #2,$dff09c.l                              ; +$068a; INTREQ
        move.w #16384,$dff024.l                          ; +$0692; DSKLEN
        tst.l d0                                         ; +$069a
        rts                                              ; +$069c; return to caller
dosior_L069e:
        move.w #1024,$dff09e.l                           ; +$069e
        tst.w -30(a6)                                    ; +$06a6
        bpl.s dosior_L06ca                               ; +$06aa
        moveq #-1,d1                                     ; +$06ac
dosior_L06ae:
        move.b d1,$bfd100.l                              ; +$06ae; CIA-B PRB: drive select/side/step/motor
        move.w -36(a6),d0                                ; +$06b4
        addq.l #3,d0                                     ; +$06b8
        bclr d0,d1                                       ; +$06ba
        move.b d1,$bfd100.l                              ; +$06bc; CIA-B PRB: drive select/side/step/motor
        bset d0,d1                                       ; +$06c2
        move.b d1,$bfd100.l                              ; +$06c4; CIA-B PRB: drive select/side/step/motor
dosior_L06ca:
        rts                                              ; +$06ca; return to caller
dosior_L06cc:
        moveq #-1,d1                                     ; +$06cc
        move.b d1,$bfd100.l                              ; +$06ce; CIA-B PRB: drive select/side/step/motor
        bclr #7,d1                                       ; +$06d4
        bsr.s dosior_L06ae                               ; +$06d8; call internal helper
        move.l #200,d0                                   ; +$06da
        bra.w dosior_L07c4                               ; +$06e0
dosior_L06e4:
        movem.l d2-d3,-(sp)                              ; +$06e4; register save/restore
        move.l d2,d3                                     ; +$06e8
        bsr.w dosior_L078a                               ; +$06ea; call internal helper
        move.w -36(a6),d0                                ; +$06ee
        add.w d0,d0                                      ; +$06f2
        lea diskior_drive_state(pc),a0                   ; +$06f4
        move.w (0,a0,d0.w),d0                            ; +$06f8
        bpl.s dosior_L0702                               ; +$06fc
        bsr.s dosior_L0732                               ; +$06fe; call internal helper
        bne.s dosior_L072c                               ; +$0700
dosior_L0702:
        lsr.w #1,d0                                      ; +$0702
        lsr.w #1,d2                                      ; +$0704
        moveq #1,d1                                      ; +$0706
        sub.w d0,d2                                      ; +$0708
        beq.s dosior_L071a                               ; +$070a
        bpl.s dosior_L0712                               ; +$070c
        moveq #-1,d1                                     ; +$070e
        neg.w d2                                         ; +$0710
dosior_L0712:
        moveq #3,d0                                      ; +$0712
        bsr.s dosior_L0766                               ; +$0714; call internal helper
        subq.w #1,d2                                     ; +$0716
        bne.s dosior_L0712                               ; +$0718
dosior_L071a:
        move.w -36(a6),d0                                ; +$071a
        add.w d0,d0                                      ; +$071e
        lea diskior_drive_state(pc),a0                   ; +$0720
        move.w d3,(0,a0,d0.w)                            ; +$0724
        bsr.s dosior_L078a                               ; +$0728; call internal helper
        moveq #0,d0                                      ; +$072a
dosior_L072c:
        movem.l (sp)+,d2-d3                              ; +$072c; register save/restore
        rts                                              ; +$0730; return to caller
dosior_L0732:
        movem.l d2,-(sp)                                 ; +$0732; register save/restore
        moveq #85,d2                                     ; +$0736
dosior_L0738:
        btst #4,$bfe001.l                                ; +$0738; CIA-A PRA: disk/input status
        beq.s dosior_L0750                               ; +$0740
        moveq #3,d0                                      ; +$0742
        moveq #-1,d1                                     ; +$0744
        bsr.s dosior_L0766                               ; +$0746; call internal helper
        dbf d2,dosior_L0738                              ; +$0748
        moveq #30,d0                                     ; +$074c
        bra.s dosior_L0760                               ; +$074e
dosior_L0750:
        move.w -36(a6),d0                                ; +$0750
        add.w d0,d0                                      ; +$0754
        lea diskior_drive_state(pc),a0                   ; +$0756
        clr.w (0,a0,d0.w)                                ; +$075a
        moveq #0,d0                                      ; +$075e
dosior_L0760:
        movem.l (sp)+,d2                                 ; +$0760; register save/restore
        rts                                              ; +$0764; return to caller
dosior_L0766:
        move.l d0,-(sp)                                  ; +$0766
        bsr.s dosior_L0794                               ; +$0768; call internal helper
        tst.b d1                                         ; +$076a
        bmi.s dosior_L0772                               ; +$076c
        bclr #1,d0                                       ; +$076e
dosior_L0772:
        bclr #0,d0                                       ; +$0772
        move.b d0,$bfd100.l                              ; +$0776; CIA-B PRB: drive select/side/step/motor
        bset #0,d0                                       ; +$077c
        move.b d0,$bfd100.l                              ; +$0780; CIA-B PRB: drive select/side/step/motor
        move.l (sp)+,d0                                  ; +$0786
        bra.s dosior_L07c4                               ; +$0788
dosior_L078a:
        bsr.s dosior_L0794                               ; +$078a; call internal helper
        move.b d0,$bfd100.l                              ; +$078c; CIA-B PRB: drive select/side/step/motor
        rts                                              ; +$0792; return to caller
dosior_L0794:
        movem.w d1-d2,-(sp)                              ; +$0794; register save/restore
        move.w -36(a6),d0                                ; +$0798
        move.b $bfd100.l,d2                              ; +$079c; CIA-B PRB: drive select/side/step/motor
        ori.b #127,d2                                    ; +$07a2
        addq.b #3,d0                                     ; +$07a6
        bclr d0,d2                                       ; +$07a8
        subq.b #3,d0                                     ; +$07aa
        add.w d0,d0                                      ; +$07ac
        move.w diskior_drive_state(pc,d0.w),d1           ; +$07ae
        btst #0,d1                                       ; +$07b2
        beq.s dosior_L07bc                               ; +$07b6
        bclr #2,d2                                       ; +$07b8
dosior_L07bc:
        move.b d2,d0                                     ; +$07bc
        movem.w (sp)+,d1-d2                              ; +$07be; register save/restore
        rts                                              ; +$07c2; return to caller
dosior_L07c4:
        bsr.s dosior_L07e4                               ; +$07c4; call internal helper
dosior_L07c6:
        btst #0,$bfde00.l                                ; +$07c6; CIA-B CRA: timer A control
        bne.s dosior_L07c6                               ; +$07ce
        subq.l #1,d0                                     ; +$07d0
        bne.s dosior_L07c4                               ; +$07d2
        rts                                              ; +$07d4; return to caller
dosior_L07d6:
        btst #0,$bfde00.l                                ; +$07d6; CIA-B CRA: timer A control
        bne.s dosior_L07fc                               ; +$07de
        subq.l #1,d0                                     ; +$07e0
        beq.s dosior_L07fc                               ; +$07e2
dosior_L07e4:
        move.b #8,$bfde00.l                              ; +$07e4; CIA-B CRA: timer A control
        move.b #-118,$bfd400.l                           ; +$07ec; CIA-B timer A low
        move.b #2,$bfd500.l                              ; +$07f4; CIA-B timer A high
dosior_L07fc:
        rts                                              ; +$07fc; return to caller
; Read-only driver cache: four drive-state words initialized to $ffff.
diskior_drive_state:
        dc.w    $ffff                  ; +$07fe: writable resident state / buffer
        dc.w    $ffff                  ; +$0800: writable resident state / buffer
        dc.w    $ffff                  ; +$0802: writable resident state / buffer
        dc.w    $ffff                  ; +$0804: writable resident state / buffer
