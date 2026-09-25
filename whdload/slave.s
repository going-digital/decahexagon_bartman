        include "whdload.i"
        include "whdmacros.i"
        section slave,code
base:   SLAVE_HEADER
        dc.w 17 ; no version-20 memory-configuration API is used
        dc.w WHDLF_NoKbd ; game polls CIA keyboard outside PORTS IRQ
        dc.l $80000
        dc.l 0
        dc.w start-base
        dc.w data_dir-base
        dc.w 0
        dc.b 0,$59
expmem: dc.l EXP_BYTES
        dc.w title-base,credit-base,info-base
        dc.w 0
        dc.l 0
        dc.w 0,0 ; kick CRC, config (version-17 header)
title:  dc.b 'Hexagon',0
credit: dc.b '2026 Hexagon contributors',0
info:   dc.b 'Native WHDLoad backend - development build',0
data_dir: dc.b 'data',0
whd_error: dc.b 'Invalid or unavailable Hexagon payload',0
        even
resload_base: dc.l 0
whd_save_failed:
        pea save_error(pc)
        pea TDREASON_FAILMSG
        move.l resload_base(pc),a0
        jmp resload_Abort(a0)
save_error: dc.b "Progress could not be saved. Previous save retained.",0
        even
start:
        lea resload_base(pc),a1
        move.l a0,(a1)
        ; BaseMem workspace [0x10000,0x6e000), leaving low vectors untouched.
        move.l #$10000,a4
        move.l a4,a1
        move.l #385024/4-1,d0
clear:  clr.l (a1)+
        subq.l #1,d0
        bpl.s clear
        move.l expmem(pc),d4
        moveq #0,d5
        moveq #0,d6
        ifgt CACHE_SLOTS-1
        move.l d4,d5
        add.l #500000,d5
        endif
        ifgt CACHE_SLOTS-2
        move.l d5,d6
        add.l #500000,d6
        endif
        bra.w resident_entry
resident_entry:
        include "resident.s"
