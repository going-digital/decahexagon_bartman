*------------------------------------------------------------------------------
* AmigaDOS File/Disk Functions for AmigaDos disks
*
* Copyright (c) 1988-92 Rob Northen Computing, U.K. All Rights Reserved.
*
* File: DOSio.s
*
* Date: 07.09.92
*------------------------------------------------------------------------------

*------------------------------------------------------------------------------
* AmigaDOS File/Disk Functions
* on entry,
*	d0.l = function
*		0=load file
*		1=save file
*		2=delete file
*		3=read file/directory names from directory
*		4=format disk
*			to format DF0: a0.l = ptr to "DF0:Volume_Name",0
*			to format DF1: a0.l = ptr to "DF1:Volume_Name",0
*			Note: volume name must be <=30 characters
*	d1.l = length of file in bytes (for save function only)
*	a0.l = full pathname of file, terminated with 0
*	a1.l = file buffer (even word boundary)
*		if d0.l=3 a1.l = ptr to file name buffer
*	a2.l = workspace buffer ($4d00 bytes of CHIPmem required)
* on exit,
*	d0.l = result
*		  0 = no error
*		204 = directory not found
*		205 = file not found
*		221 = disk full
*		225 = not a DOS disk
*		405 = bad block checksum
*		see diskio.s for error codes
*	d1.l = length of file in bytes, for load and save file functions
*		OR no. of file entries for directory function)
*	all others registers are preserved
*
* For the Directory function the output buffer is filled as follows:
*
* offset  field length  field description
*
* $00     1             entry type  (2=dir entry, -3=file entry)
* $01     1             entry name length
* $02     30            entry name
* $20     1             entry type
* $21     1             entry name length
* $22     30            entry name
* etc.
*
* Note: the output buffer must be large enough to hold all file names in the
* directory.
*
* IMPORTANT :
*
* File loading time can be greatly reduced by writing the file onto the
* disk using the source file DOSIO.S. This has the advantage over using
* AmigaDos to copy the file onto the disk by writing out the file's data
* block lists, as well as the actual data blocks, on contiguous sectors.
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
dosio:
        movem.l d0-a6,-(sp)                              ; +$0000; register save/restore
        move.l d0,d6                                     ; +$0004
        move.l d1,d7                                     ; +$0006
        movea.l a0,a4                                    ; +$0008
        movea.l a1,a5                                    ; +$000a
        movea.l a2,a6                                    ; +$000c
        bsr.w dosio_parse_drive                                ; +$000e; call internal helper
        tst.l d6                                         ; +$0012
        bne.s dosio_L001e                                ; +$0014
        bsr.s dosio_load_file                                ; +$0016; call internal helper
        move.l d1,4(sp)                                  ; +$0018
        bra.s dosio_L0050                                ; +$001c
dosio_L001e:
        cmp.b #1,d6                                      ; +$001e
        bne.s dosio_L002a                                ; +$0022
        bsr.w dosio_save_file                                ; +$0024; call internal helper
        bra.s dosio_L0050                                ; +$0028
dosio_L002a:
        cmp.b #2,d6                                      ; +$002a
        bne.s dosio_L0036                                ; +$002e
        bsr.w dosio_delete_file                                ; +$0030; call internal helper
        bra.s dosio_L0050                                ; +$0034
dosio_L0036:
        cmp.b #3,d6                                      ; +$0036
        bne.s dosio_L0046                                ; +$003a
        bsr.w dosio_list_directory                                ; +$003c; call internal helper
        move.l d1,4(sp)                                  ; +$0040
        bra.s dosio_L0050                                ; +$0044
dosio_L0046:
        cmp.b #4,d6                                      ; +$0046
        bne.s dosio_L0050                                ; +$004a
        bsr.w dosio_format_disk                                ; +$004c; call internal helper
dosio_L0050:
        move.l d0,(sp)                                   ; +$0050
        moveq #0,d1                                      ; +$0052
        moveq #0,d2                                      ; +$0054
        move.w #-32768,d3                                ; +$0056
        bsr.w dosio_sector_transfer                      ; +$005a; call internal helper
        movem.l (sp)+,d0-a6                              ; +$005e; register save/restore
        tst.l d0                                         ; +$0062
        rts                                              ; +$0064; return to caller
; Function 0: locate file and copy its data blocks into the caller buffer.
dosio_load_file:
        bsr.w dosio_lookup_path                                ; +$0066; call internal helper
        bne.s dosio_L00dc                                ; +$006a
        move.l 324(a0),d7                                ; +$006c
        move.l d7,-(sp)                                  ; +$0070
        lea 312(a0),a2                                   ; +$0072
        bsr.s dosio_L00de                                ; +$0076; call internal helper
        bne.s dosio_L00d8                                ; +$0078
dosio_L007a:
        move.l d1,d2                                     ; +$007a
        move.l d1,d4                                     ; +$007c
        movea.l a5,a3                                    ; +$007e
        move.l d7,d6                                     ; +$0080
        lsr.l #8,d6                                      ; +$0082
        lsr.l #1,d6                                      ; +$0084
        bne.s dosio_L008a                                ; +$0086
        movea.l a6,a3                                    ; +$0088
dosio_L008a:
        bsr.s dosio_L00de                                ; +$008a; call internal helper
        bne.s dosio_L00d8                                ; +$008c
        addq.l #1,d2                                     ; +$008e
        move.l d1,d5                                     ; +$0090
        beq.s dosio_L009c                                ; +$0092
        cmp.l d1,d2                                      ; +$0094
        bne.s dosio_L009c                                ; +$0096
        subq.l #1,d6                                     ; +$0098
        bne.s dosio_L008a                                ; +$009a
dosio_L009c:
        move.l d4,d1                                     ; +$009c
        sub.l d4,d2                                      ; +$009e
        movea.l a3,a0                                    ; +$00a0
        bsr.w dosio_L066e                                ; +$00a2; call internal helper
        bne.s dosio_L00d8                                ; +$00a6
dosio_L00a8:
        bsr.w dosio_L0604                                ; +$00a8; call internal helper
        bne.s dosio_L00d8                                ; +$00ac
        move.l 12(a0),d0                                 ; +$00ae
        sub.l d0,d7                                      ; +$00b2
        lea 24(a0),a0                                    ; +$00b4
        ror.l #2,d0                                      ; +$00b8
        bra.s dosio_L00be                                ; +$00ba
dosio_L00bc:
        move.l (a0)+,(a5)+                               ; +$00bc
dosio_L00be:
        dbf d0,dosio_L00bc                               ; +$00be
        clr.w d0                                         ; +$00c2
        rol.l #2,d0                                      ; +$00c4
        bra.s dosio_L00ca                                ; +$00c6
dosio_L00c8:
        move.b (a0)+,(a5)+                               ; +$00c8
dosio_L00ca:
        dbf d0,dosio_L00c8                               ; +$00ca
        subq.l #1,d2                                     ; +$00ce
        bne.s dosio_L00a8                                ; +$00d0
        move.l d5,d1                                     ; +$00d2
        move.l d7,d0                                     ; +$00d4
        bne.s dosio_L007a                                ; +$00d6
dosio_L00d8:
        move.l (sp)+,d1                                  ; +$00d8
        tst.l d0                                         ; +$00da
dosio_L00dc:
        rts                                              ; +$00dc; return to caller
dosio_L00de:
        move.l d2,-(sp)                                  ; +$00de
        movea.l a6,a0                                    ; +$00e0
        move.l 8(a0),d0                                  ; +$00e2
        bne.s dosio_L00f8                                ; +$00e6
        move.l 504(a0),d1                                ; +$00e8
        beq.s dosio_L0102                                ; +$00ec
        bsr.w dosio_L0600                                ; +$00ee; call internal helper
        bne.s dosio_L0102                                ; +$00f2
        lea 312(a0),a2                                   ; +$00f4
dosio_L00f8:
        move.l -(a2),d1                                  ; +$00f8
        moveq #1,d0                                      ; +$00fa
        sub.l d0,8(a0)                                   ; +$00fc
        moveq #0,d0                                      ; +$0100
dosio_L0102:
        move.l (sp)+,d2                                  ; +$0102
        tst.l d0                                         ; +$0104
        rts                                              ; +$0106; return to caller
; Function 1: save file; updates filesystem allocation and block checksums.
dosio_save_file:
        bsr.w dosio_delete_file                                ; +$0108; call internal helper
        beq.s dosio_L0120                                ; +$010c
        cmp.l #205,d0                                    ; +$010e
        bne.w dosio_L0282                                ; +$0114
        bsr.w dosio_L036e                                ; +$0118; call internal helper
        bne.w dosio_L0282                                ; +$011c
dosio_L0120:
        bsr.w dosio_L03f2                                ; +$0120; call internal helper
        moveq #2,d0                                      ; +$0124
        move.l d0,(a0)                                   ; +$0126
        move.l d7,324(a0)                                ; +$0128
        bsr.w dosio_L0386                                ; +$012c; call internal helper
        move.w dosio_volume_state(pc),502(a0)            ; +$0130
        moveq #-3,d0                                     ; +$0136
        move.l d0,508(a0)                                ; +$0138
        moveq #1,d0                                      ; +$013c
        move.l d0,13564(a6)                              ; +$013e
        moveq #2,d6                                      ; +$0142
        bsr.w dosio_L02fc                                ; +$0144; call internal helper
        beq.w dosio_L027c                                ; +$0148
        lea dosio_L0630(pc),a0                           ; +$014c
        move.w d0,(a0)                                   ; +$0150
        move.l d0,13572(a6)                              ; +$0152
        move.l d7,d1                                     ; +$0156
dosio_L0158:
        bsr.w dosio_L02fc                                ; +$0158; call internal helper
        beq.w dosio_L027c                                ; +$015c
        addq.l #1,d6                                     ; +$0160
        subi.l #35136,d1                                 ; +$0162
        bhi.s dosio_L0158                                ; +$0168
        move.l d6,d5                                     ; +$016a
dosio_L016c:
        move.l 13572(a6),d6                              ; +$016c
        bsr.w dosio_L02fc                                ; +$0170; call internal helper
        bsr.w dosio_L034a                                ; +$0174; call internal helper
        move.l d6,13572(a6)                              ; +$0178
        clr.l 13576(a6)                                  ; +$017c
        clr.l 14072(a6)                                  ; +$0180
        lea 13880(a6),a2                                 ; +$0184
dosio_L0188:
        lea 14080(a6),a1                                 ; +$0188
        move.l d5,d6                                     ; +$018c
        bsr.w dosio_L0310                                ; +$018e; call internal helper
        beq.w dosio_L027c                                ; +$0192
        move.l d6,d5                                     ; +$0196
        moveq #0,d3                                      ; +$0198
        moveq #11,d4                                     ; +$019a
        cmp.l d4,d1                                      ; +$019c
        bge.s dosio_L01a2                                ; +$019e
        move.l d1,d4                                     ; +$01a0
dosio_L01a2:
        move.l d6,-(a2)                                  ; +$01a2
        move.l d6,d0                                     ; +$01a4
        bsr.w dosio_L034a                                ; +$01a6; call internal helper
        addq.l #1,d6                                     ; +$01aa
        movea.l a1,a3                                    ; +$01ac
        moveq #8,d0                                      ; +$01ae
        move.l d0,(a1)+                                  ; +$01b0
        move.l 13572(a6),(a1)+                           ; +$01b2
        move.l 13564(a6),(a1)+                           ; +$01b6
        move.l #488,d0                                   ; +$01ba
        cmp.l d0,d7                                      ; +$01c0
        bge.s dosio_L01c6                                ; +$01c2
        move.l d7,d0                                     ; +$01c4
dosio_L01c6:
        sub.l d0,d7                                      ; +$01c6
        move.l d0,(a1)+                                  ; +$01c8
        move.l d6,(a1)+                                  ; +$01ca
        clr.l (a1)+                                      ; +$01cc
        tst.w d0                                         ; +$01ce
        beq.s dosio_L01d4                                ; +$01d0
        subq.w #1,d0                                     ; +$01d2
dosio_L01d4:
        move.b (a5)+,(a1)+                               ; +$01d4
        dbf d0,dosio_L01d4                               ; +$01d6
        movea.l a3,a0                                    ; +$01da
        bsr.w dosio_block_checksum                       ; +$01dc; call internal helper
        move.l d0,20(a0)                                 ; +$01e0
        addq.l #1,d3                                     ; +$01e4
        moveq #1,d0                                      ; +$01e6
        add.l d0,13564(a6)                               ; +$01e8
        add.l d0,13576(a6)                               ; +$01ec
        tst.l d7                                         ; +$01f0
        beq.s dosio_L020e                                ; +$01f2
        moveq #72,d0                                     ; +$01f4
        cmp.l 13576(a6),d0                               ; +$01f6
        beq.s dosio_L0202                                ; +$01fa
        cmp.l d3,d4                                      ; +$01fc
        bne.s dosio_L01a2                                ; +$01fe
        bra.s dosio_L0228                                ; +$0200
dosio_L0202:
        move.l 13572(a6),d6                              ; +$0202
        bsr.w dosio_L02fc                                ; +$0206; call internal helper
        move.l d6,14072(a6)                              ; +$020a
dosio_L020e:
        lea 13568(a6),a0                                 ; +$020e
        move.l 308(a0),d0                                ; +$0212
        move.l d0,16(a0)                                 ; +$0216
        move.l 4(a0),d1                                  ; +$021a
        bsr.w dosio_write_block                          ; +$021e; call internal helper
        bne.s dosio_L0282                                ; +$0222
        moveq #16,d0                                     ; +$0224
        move.l d0,(a0)                                   ; +$0226
dosio_L0228:
        move.l d7,d0                                     ; +$0228
        beq.s dosio_L0232                                ; +$022a
        move.l d5,d6                                     ; +$022c
        bsr.w dosio_L02fc                                ; +$022e; call internal helper
dosio_L0232:
        movea.l a3,a0                                    ; +$0232
        move.l d0,16(a0)                                 ; +$0234
        clr.l 20(a0)                                     ; +$0238
        bsr.w dosio_block_checksum                       ; +$023c; call internal helper
        move.l d0,20(a0)                                 ; +$0240
        move.l d5,d1                                     ; +$0244
        move.l d3,d2                                     ; +$0246
        lea 14080(a6),a0                                 ; +$0248
        bsr.w dosio_L0660                                ; +$024c; call internal helper
        bne.s dosio_L0282                                ; +$0250
        tst.l d7                                         ; +$0252
        beq.s dosio_L0264                                ; +$0254
        moveq #72,d0                                     ; +$0256
        cmp.l 13576(a6),d0                               ; +$0258
        beq.w dosio_L016c                                ; +$025c
        bra.w dosio_L0188                                ; +$0260
dosio_L0264:
        bsr.s dosio_L0284                                ; +$0264; call internal helper
        bne.s dosio_L0282                                ; +$0266
        lea 13056(a6),a0                                 ; +$0268
        move.w dosio_L062a(pc),d1                        ; +$026c
        clr.l (a0)                                       ; +$0270
        bsr.w dosio_block_checksum                       ; +$0272; call internal helper
        move.l d0,(a0)                                   ; +$0276
        bra.w dosio_L065e                                ; +$0278
dosio_L027c:
        move.l #221,d0                                   ; +$027c
dosio_L0282:
        rts                                              ; +$0282; return to caller
dosio_L0284:
        move.w dosio_L062c(pc),d1                        ; +$0284
        lea 13568(a6),a0                                 ; +$0288
        bsr.w dosio_L0600                                ; +$028c; call internal helper
        bne.s dosio_L02fa                                ; +$0290
        move.w dosio_L0630(pc),d0                        ; +$0292
        move.w dosio_L062e(pc),d3                        ; +$0296
        move.l d0,(0,a0,d3.w)                            ; +$029a
        bra.w dosio_write_block                          ; +$029e
; Function 2: remove file and release its allocated blocks.
dosio_delete_file:
        bsr.w dosio_lookup_path                                ; +$02a2; call internal helper
        bne.s dosio_L02fa                                ; +$02a6
        move.l d1,d4                                     ; +$02a8
        bsr.w dosio_L036e                                ; +$02aa; call internal helper
        bne.s dosio_L02fa                                ; +$02ae
        move.l d4,d0                                     ; +$02b0
dosio_L02b2:
        bsr.s dosio_L02d8                                ; +$02b2; call internal helper
        lea 312(a6),a1                                   ; +$02b4
        move.l 8(a6),d1                                  ; +$02b8
        subq.l #1,d1                                     ; +$02bc
dosio_L02be:
        move.l -(a1),d0                                  ; +$02be
        bsr.s dosio_L02d8                                ; +$02c0; call internal helper
        dbf d1,dosio_L02be                               ; +$02c2
        move.l 504(a6),d1                                ; +$02c6
        beq.s dosio_L0264                                ; +$02ca
        movea.l a6,a0                                    ; +$02cc
        bsr.w dosio_L0600                                ; +$02ce; call internal helper
        bne.s dosio_L02fa                                ; +$02d2
        move.l d1,d0                                     ; +$02d4
        bra.s dosio_L02b2                                ; +$02d6
dosio_L02d8:
        movem.l d0-d1,-(sp)                              ; +$02d8; register save/restore
        subq.w #2,d0                                     ; +$02dc
        divu.w #32,d0                                    ; +$02de
        lsl.w #2,d0                                      ; +$02e2
        addi.w #13060,d0                                 ; +$02e4
        move.l (0,a6,d0.w),d1                            ; +$02e8
        swap d0                                          ; +$02ec
        bset d0,d1                                       ; +$02ee
        swap d0                                          ; +$02f0
        move.l d1,(0,a6,d0.w)                            ; +$02f2
        movem.l (sp)+,d0-d1                              ; +$02f6; register save/restore
dosio_L02fa:
        rts                                              ; +$02fa; return to caller
dosio_L02fc:
        move.l d6,d0                                     ; +$02fc
        subq.w #1,d0                                     ; +$02fe
dosio_L0300:
        addq.w #1,d0                                     ; +$0300
        cmp.w #1760,d0                                   ; +$0302
        beq.s dosio_L030e                                ; +$0306
        bsr.s dosio_L032c                                ; +$0308; call internal helper
        beq.s dosio_L0300                                ; +$030a
        move.l d0,d6                                     ; +$030c
dosio_L030e:
        rts                                              ; +$030e; return to caller
dosio_L0310:
        moveq #0,d1                                      ; +$0310
        bsr.s dosio_L02fc                                ; +$0312; call internal helper
        beq.s dosio_L0326                                ; +$0314
        move.l d6,d0                                     ; +$0316
dosio_L0318:
        addq.w #1,d1                                     ; +$0318
        addq.w #1,d0                                     ; +$031a
        cmp.w #1760,d0                                   ; +$031c
        beq.s dosio_L0326                                ; +$0320
        bsr.s dosio_L032c                                ; +$0322; call internal helper
        bne.s dosio_L0318                                ; +$0324
dosio_L0326:
        move.l d6,d0                                     ; +$0326
        tst.l d1                                         ; +$0328
        rts                                              ; +$032a; return to caller
dosio_L032c:
        movem.l d0-d1,-(sp)                              ; +$032c; register save/restore
        subq.w #2,d0                                     ; +$0330
        divu.w #32,d0                                    ; +$0332
        lsl.w #2,d0                                      ; +$0336
        addi.w #13060,d0                                 ; +$0338
        move.l (0,a6,d0.w),d1                            ; +$033c
        swap d0                                          ; +$0340
        btst d0,d1                                       ; +$0342
        movem.l (sp)+,d0-d1                              ; +$0344; register save/restore
        rts                                              ; +$0348; return to caller
dosio_L034a:
        movem.l d0-d1,-(sp)                              ; +$034a; register save/restore
        subq.w #2,d0                                     ; +$034e
        divu.w #32,d0                                    ; +$0350
        lsl.w #2,d0                                      ; +$0354
        addi.w #13060,d0                                 ; +$0356
        move.l (0,a6,d0.w),d1                            ; +$035a
        swap d0                                          ; +$035e
        bclr d0,d1                                       ; +$0360
        swap d0                                          ; +$0362
        move.l d1,(0,a6,d0.w)                            ; +$0364
        movem.l (sp)+,d0-d1                              ; +$0368; register save/restore
        rts                                              ; +$036c; return to caller
dosio_L036e:
        move.w dosio_L062a(pc),d1                        ; +$036e
        lea 13056(a6),a0                                 ; +$0372
        bsr.w dosio_read_block                           ; +$0376; call internal helper
        bne.s dosio_L0384                                ; +$037a
        bsr.w dosio_block_checksum                       ; +$037c; call internal helper
        bne.w dosio_L0608                                ; +$0380
dosio_L0384:
        rts                                              ; +$0384; return to caller
dosio_L0386:
        lea 433(a0),a1                                   ; +$0386
        moveq #-1,d0                                     ; +$038a
dosio_L038c:
        addq.b #1,d0                                     ; +$038c
        cmp.b #30,d0                                     ; +$038e
        beq.s dosio_L0398                                ; +$0392
        move.b (a4)+,(a1)+                               ; +$0394
        bne.s dosio_L038c                                ; +$0396
dosio_L0398:
        move.b d0,432(a0)                                ; +$0398
        rts                                              ; +$039c; return to caller
; Function 3: enumerate directory into caller-provided entry buffer.
dosio_list_directory:
        bsr.w dosio_lookup_path                                ; +$039e; call internal helper
        bne.s dosio_L03f0                                ; +$03a2
        moveq #0,d4                                      ; +$03a4
        moveq #0,d5                                      ; +$03a6
dosio_L03a8:
        move.l (24,a6,d4.w),d1                           ; +$03a8
        beq.s dosio_L03e4                                ; +$03ac
dosio_L03ae:
        lea 13056(a6),a0                                 ; +$03ae
        bsr.w dosio_L0600                                ; +$03b2; call internal helper
        bne.s dosio_L03f0                                ; +$03b6
        moveq #2,d0                                      ; +$03b8
        cmp.l (a0),d0                                    ; +$03ba
        bne.s dosio_L03e4                                ; +$03bc
        moveq #-3,d0                                     ; +$03be
        cmp.l 508(a0),d0                                 ; +$03c0
        beq.s dosio_L03ce                                ; +$03c4
        moveq #2,d0                                      ; +$03c6
        cmp.l 508(a0),d0                                 ; +$03c8
        bne.s dosio_L03e4                                ; +$03cc
dosio_L03ce:
        move.b d0,(a5)+                                  ; +$03ce
        lea 432(a0),a1                                   ; +$03d0
        moveq #30,d0                                     ; +$03d4
dosio_L03d6:
        move.b (a1)+,(a5)+                               ; +$03d6
        dbf d0,dosio_L03d6                               ; +$03d8
        addq.l #1,d5                                     ; +$03dc
        move.l 496(a0),d1                                ; +$03de
        bne.s dosio_L03ae                                ; +$03e2
dosio_L03e4:
        addq.w #4,d4                                     ; +$03e4
        cmp.w #288,d4                                    ; +$03e6
        bne.s dosio_L03a8                                ; +$03ea
        move.l d5,d1                                     ; +$03ec
        moveq #0,d0                                      ; +$03ee
dosio_L03f0:
        rts                                              ; +$03f0; return to caller
dosio_L03f2:
        lea 13568(a6),a0                                 ; +$03f2
        movea.l a0,a1                                    ; +$03f6
        move.w #1535,d0                                  ; +$03f8
dosio_L03fc:
        clr.l (a1)+                                      ; +$03fc
        dbf d0,dosio_L03fc                               ; +$03fe
        rts                                              ; +$0402; return to caller
; Function 4: initialize disk and filesystem structures.
dosio_format_disk:
        moveq #0,d1                                      ; +$0404
        move.w #1760,d2                                  ; +$0406
        moveq #2,d3                                      ; +$040a
        bsr.w dosio_sector_transfer                      ; +$040c; call internal helper
        bne.w dosio_L0496                                ; +$0410
        bsr.s dosio_L03f2                                ; +$0414; call internal helper
        moveq #2,d0                                      ; +$0416
        move.l d0,(a0)                                   ; +$0418
        moveq #72,d0                                     ; +$041a
        move.l d0,12(a0)                                 ; +$041c
        moveq #-1,d0                                     ; +$0420
        move.l d0,312(a0)                                ; +$0422
        move.l #881,316(a0)                              ; +$0426
        bsr.w dosio_L0386                                ; +$042e; call internal helper
        moveq #1,d0                                      ; +$0432
        move.l d0,508(a0)                                ; +$0434
        adda.w #1024,a6                                  ; +$0438
        moveq #2,d0                                      ; +$043c
dosio_L043e:
        bsr.w dosio_L02d8                                ; +$043e; call internal helper
        addq.w #1,d0                                     ; +$0442
        cmp.w #1760,d0                                   ; +$0444
        bne.s dosio_L043e                                ; +$0448
        move.l #880,d0                                   ; +$044a
        bsr.w dosio_L034a                                ; +$0450; call internal helper
        addq.l #1,d0                                     ; +$0454
        bsr.w dosio_L034a                                ; +$0456; call internal helper
        suba.w #1024,a6                                  ; +$045a
        bsr.w dosio_block_checksum                       ; +$045e; call internal helper
        move.l d0,20(a0)                                 ; +$0462
        adda.w #512,a0                                   ; +$0466
        bsr.w dosio_block_checksum                       ; +$046a; call internal helper
        move.l d0,(a0)                                   ; +$046e
        suba.w #512,a0                                   ; +$0470
        move.w #880,d1                                   ; +$0474
        moveq #2,d2                                      ; +$0478
        bsr.w dosio_L0660                                ; +$047a; call internal helper
        bne.s dosio_L0496                                ; +$047e
        movea.l a0,a1                                    ; +$0480
        move.w #127,d0                                   ; +$0482
dosio_L0486:
        move.l #1146049280,(a1)+                         ; +$0486
        dbf d0,dosio_L0486                               ; +$048c
        moveq #0,d1                                      ; +$0490
        bsr.w dosio_L065e                                ; +$0492; call internal helper
dosio_L0496:
        rts                                              ; +$0496; return to caller
; Traverse filesystem directory/file headers for the requested pathname.
dosio_lookup_path:
        move.l a4,-(sp)                                  ; +$0498
        movea.l a6,a0                                    ; +$049a
        bsr.w dosio_L05d8                                ; +$049c; call internal helper
        bne.w dosio_L0536                                ; +$04a0
dosio_L04a4:
        lea dosio_volume_state(pc),a1                    ; +$04a4
        move.w d1,(a1)                                   ; +$04a8
        move.l a4,(sp)                                   ; +$04aa
        bsr.w dosio_L0584                                ; +$04ac; call internal helper
        beq.w dosio_L0536                                ; +$04b0
        lsl.w #2,d0                                      ; +$04b4
dosio_L04b6:
        lea dosio_L062c(pc),a1                           ; +$04b6
        move.w d1,(a1)+                                  ; +$04ba
        move.w d0,(a1)+                                  ; +$04bc
        move.l (0,a0,d0.w),d1                            ; +$04be
        beq.s dosio_L050e                                ; +$04c2
        bsr.w dosio_L0600                                ; +$04c4; call internal helper
        bne.s dosio_L0536                                ; +$04c8
        move.l #225,d0                                   ; +$04ca
        moveq #2,d2                                      ; +$04d0
        cmp.l (a0),d2                                    ; +$04d2
        bne.s dosio_L0536                                ; +$04d4
        moveq #2,d2                                      ; +$04d6
        cmp.b #3,d6                                      ; +$04d8
        beq.s dosio_L04e4                                ; +$04dc
        tst.b (a4)                                       ; +$04de
        bne.s dosio_L04e4                                ; +$04e0
        moveq #-3,d2                                     ; +$04e2
dosio_L04e4:
        cmp.l 508(a0),d2                                 ; +$04e4
        bne.s dosio_L0508                                ; +$04e8
        lea 432(a0),a1                                   ; +$04ea
        lea dosio_name_buffer(pc),a2                     ; +$04ee
        moveq #0,d2                                      ; +$04f2
        move.b (a1)+,d2                                  ; +$04f4
        subq.b #1,d2                                     ; +$04f6
dosio_L04f8:
        move.b (a1)+,d0                                  ; +$04f8
        bsr.s dosio_uppercase_char                                ; +$04fa; call internal helper
        cmp.b (a2)+,d0                                   ; +$04fc
        dbne d2,dosio_L04f8                              ; +$04fe
        bne.s dosio_L0508                                ; +$0502
        tst.b (a2)                                       ; +$0504
        beq.s dosio_L0526                                ; +$0506
dosio_L0508:
        move.w #496,d0                                   ; +$0508
        bra.s dosio_L04b6                                ; +$050c
dosio_L050e:
        move.l #204,d0                                   ; +$050e
        cmp.b #3,d6                                      ; +$0514
        beq.s dosio_L0536                                ; +$0518
        tst.b (a4)                                       ; +$051a
        bne.s dosio_L0536                                ; +$051c
        move.l #205,d0                                   ; +$051e
        bra.s dosio_L0536                                ; +$0524
dosio_L0526:
        tst.b (a4)                                       ; +$0526
        bne.w dosio_L04a4                                ; +$0528
        lea dosio_L0630(pc),a1                           ; +$052c
        move.w 498(a0),(a1)                              ; +$0530
        moveq #0,d0                                      ; +$0534
dosio_L0536:
        movea.l (sp)+,a4                                 ; +$0536
        tst.l d0                                         ; +$0538
        rts                                              ; +$053a; return to caller
; Parse DF0: through DF3: and establish drive/path state.
dosio_parse_drive:
        movea.l a4,a0                                    ; +$053c
        bsr.s dosio_L056e                                ; +$053e; call internal helper
        cmp.b #68,d0                                     ; +$0540
        bne.s dosio_L056c                                ; +$0544
        bsr.s dosio_L056e                                ; +$0546; call internal helper
        cmp.b #70,d0                                     ; +$0548
        bne.s dosio_L056c                                ; +$054c
        move.b (a0)+,d0                                  ; +$054e
        subi.b #48,d0                                    ; +$0550
        blt.s dosio_L056c                                ; +$0554
        cmp.b #51,d0                                     ; +$0556
        bgt.s dosio_L056c                                ; +$055a
        cmpi.b #58,(a0)+                                 ; +$055c
        bne.s dosio_L056c                                ; +$0560
        lea dosio_L0632(pc),a0                           ; +$0562
        move.b d0,1(a0)                                  ; +$0566
        addq.w #4,a4                                     ; +$056a
dosio_L056c:
        rts                                              ; +$056c; return to caller
dosio_L056e:
        move.b (a0)+,d0                                  ; +$056e
; Read next path character and fold ASCII lowercase to uppercase.
dosio_uppercase_char:
        cmp.b #97,d0                                     ; +$0570
        blt.s dosio_L0580                                ; +$0574
        cmp.b #122,d0                                    ; +$0576
        bgt.s dosio_L0580                                ; +$057a
        andi.b #-33,d0                                   ; +$057c
dosio_L0580:
        tst.b d0                                         ; +$0580
        rts                                              ; +$0582; return to caller
dosio_L0584:
        movem.l d1/a0-a1,-(sp)                           ; +$0584; register save/restore
        moveq #0,d0                                      ; +$0588
        moveq #-1,d1                                     ; +$058a
        movea.l a4,a0                                    ; +$058c
        lea dosio_name_buffer(pc),a1                     ; +$058e
        clr.b (a1)                                       ; +$0592
dosio_L0594:
        addq.l #1,d1                                     ; +$0594
        tst.b (a4)                                       ; +$0596
        beq.s dosio_L05a2                                ; +$0598
        cmpi.b #47,(a4)+                                 ; +$059a
        bne.s dosio_L0594                                ; +$059e
        subq.w #1,a4                                     ; +$05a0
dosio_L05a2:
        tst.l d1                                         ; +$05a2
        beq.s dosio_L05d0                                ; +$05a4
dosio_L05a6:
        mulu.w #13,d1                                    ; +$05a6
        move.b (a0)+,d0                                  ; +$05aa
        bsr.s dosio_uppercase_char                                ; +$05ac; call internal helper
        move.b d0,(a1)+                                  ; +$05ae
        add.w d0,d1                                      ; +$05b0
        andi.l #2047,d1                                  ; +$05b2
        cmpa.l a0,a4                                     ; +$05b8
        bne.s dosio_L05a6                                ; +$05ba
        cmpi.b #47,(a4)                                  ; +$05bc
        bne.s dosio_L05c4                                ; +$05c0
        addq.w #1,a4                                     ; +$05c2
dosio_L05c4:
        clr.b (a1)+                                      ; +$05c4
        divu.w #72,d1                                    ; +$05c6
        clr.w d1                                         ; +$05ca
        swap d1                                          ; +$05cc
        addq.w #6,d1                                     ; +$05ce
dosio_L05d0:
        move.l d1,d0                                     ; +$05d0
        movem.l (sp)+,d1/a0-a1                           ; +$05d2; register save/restore
        rts                                              ; +$05d6; return to caller
dosio_L05d8:
        move.w #880,d1                                   ; +$05d8
        bsr.s dosio_L0600                                ; +$05dc; call internal helper
        bne.s dosio_L05fe                                ; +$05de
        move.l #225,d0                                   ; +$05e0
        moveq #2,d2                                      ; +$05e6
        cmp.l (a0),d2                                    ; +$05e8
        bne.s dosio_L05fe                                ; +$05ea
        moveq #1,d2                                      ; +$05ec
        cmp.l 508(a0),d2                                 ; +$05ee
        bne.s dosio_L05fe                                ; +$05f2
        lea dosio_L062a(pc),a1                           ; +$05f4
        move.w 318(a0),(a1)                              ; +$05f8
        moveq #0,d0                                      ; +$05fc
dosio_L05fe:
        rts                                              ; +$05fe; return to caller
dosio_L0600:
        bsr.s dosio_read_block                           ; +$0600; call internal helper
        bne.s dosio_L060e                                ; +$0602
dosio_L0604:
        bsr.s dosio_block_checksum                       ; +$0604; call internal helper
        beq.s dosio_L060e                                ; +$0606
dosio_L0608:
        move.l #405,d0                                   ; +$0608
dosio_L060e:
        rts                                              ; +$060e; return to caller
; Sum 128 longwords of a 512-byte filesystem block; return negated sum.
dosio_block_checksum:
        movem.l d1/a0,-(sp)                              ; +$0610; register save/restore
        moveq #0,d0                                      ; +$0614
        move.w #127,d1                                   ; +$0616
dosio_L061a:
        add.l (a0)+,d0                                   ; +$061a
        dbf d1,dosio_L061a                               ; +$061c
        neg.l d0                                         ; +$0620
        movem.l (sp)+,d1/a0                              ; +$0622; register save/restore
        rts                                              ; +$0626; return to caller
; Writable filesystem/drive state; PC-relative accesses retain original placement.
dosio_volume_state:
        dc.w    $0000                  ; +$0628: writable resident state / buffer
dosio_L062a:
        dc.w    $0000                  ; +$062a: writable resident state / buffer
dosio_L062c:
        dc.w    $0000                  ; +$062c: writable resident state / buffer
dosio_L062e:
        dc.w    $0000                  ; +$062e: writable resident state / buffer
dosio_L0630:
        dc.w    $0000                  ; +$0630: writable resident state / buffer
dosio_L0632:
        dc.w    $0000                  ; +$0632: writable resident state / buffer
; Writable filename scratch area, not executable instructions.
dosio_name_buffer:
        dc.w    $0000                  ; +$0634: writable resident state / buffer
        dc.w    $0000                  ; +$0636: writable resident state / buffer
        dc.w    $0000                  ; +$0638: writable resident state / buffer
        dc.w    $0000                  ; +$063a: writable resident state / buffer
        dc.w    $0000                  ; +$063c: writable resident state / buffer
        dc.w    $0000                  ; +$063e: writable resident state / buffer
        dc.w    $0000                  ; +$0640: writable resident state / buffer
        dc.w    $0000                  ; +$0642: writable resident state / buffer
        dc.w    $0000                  ; +$0644: writable resident state / buffer
        dc.w    $0000                  ; +$0646: writable resident state / buffer
        dc.w    $0000                  ; +$0648: writable resident state / buffer
        dc.w    $0000                  ; +$064a: writable resident state / buffer
        dc.w    $0000                  ; +$064c: writable resident state / buffer
        dc.w    $0000                  ; +$064e: writable resident state / buffer
        dc.w    $0000                  ; +$0650: writable resident state / buffer
        dc.w    $0000                  ; +$0652: writable resident state / buffer
dosio_write_block:
        clr.l 20(a0)                                     ; +$0654
        bsr.s dosio_block_checksum                       ; +$0658; call internal helper
        move.l d0,20(a0)                                 ; +$065a
dosio_L065e:
        moveq #1,d2                                      ; +$065e
dosio_L0660:
        move.l d3,-(sp)                                  ; +$0660
        moveq #1,d3                                      ; +$0662
        bsr.s dosio_sector_transfer                      ; +$0664; call internal helper
        move.l (sp)+,d3                                  ; +$0666
        tst.l d0                                         ; +$0668
        rts                                              ; +$066a; return to caller
dosio_read_block:
        moveq #1,d2                                      ; +$066c
dosio_L066e:
        moveq #0,d3                                      ; +$066e
dosio_sector_transfer:
        movea.l a6,a1                                    ; +$0670
        move.w dosio_L0632(pc),d0                        ; +$0672

; Intentional fall-through from dosio_sector_transfer into diskio.
        include "DiskIO.s"
