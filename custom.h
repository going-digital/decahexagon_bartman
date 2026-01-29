#ifndef HARDWARE_CUSTOM2_H
#define HARDWARE_CUSTOM2_H
/*
**	$Filename: hardware/custom.h $
**	$Release: 1.3 $
**
**	
**
**	(C) Copyright 1985,1986,1987,1988 Commodore-Amiga, Inc.
**	    All Rights Reserved
*/

/*
 * do this to get base of custom registers:
 * extern struct Custom custom;
 */


struct Custom2 {
    volatile UWORD   bltddat;
    volatile UWORD   dmaconr;
    union {
      volatile ULONG vpos32;
      struct {
        union {
          volatile UWORD vposr;
          struct {
            volatile UBYTE vposr_h;
            volatile UBYTE vposr_l;
          };
        };
        union {
          volatile UWORD vhposr;
          struct {
            volatile UBYTE vhposr_h;
            volatile UBYTE vhposr_l;
          };
        };
      };
    };
    volatile UWORD   dskdatr;
    volatile UWORD   joy0dat;
    volatile UWORD   joy1dat;
    volatile UWORD   clxdat;
    volatile UWORD   adkconr;
    volatile UWORD   pot0dat;
    volatile UWORD   pot1dat;
    volatile UWORD   potinp;
    volatile UWORD   serdatr;
    volatile UWORD   dskbytr;
    volatile UWORD   intenar;
    volatile UWORD   intreqr;
    volatile APTR    dskpt;
    volatile UWORD   dsklen;
    volatile UWORD   dskdat;
    volatile UWORD   refptr;
    volatile UWORD   vposw;
    volatile UWORD   vhposw;
    volatile UWORD   copcon;
    volatile UWORD   serdat;
    volatile UWORD   serper;
    volatile UWORD   potgo;
    volatile UWORD   joytest;
    volatile UWORD   strequ;
    volatile UWORD   strvbl;
    volatile UWORD   strhor;
    volatile UWORD   strlong;
    volatile UWORD   bltcon0;
    volatile UWORD   bltcon1;
    volatile UWORD   bltafwm;
    volatile UWORD   bltalwm;
    union {
      volatile APTR bltcpt;
      struct {
        volatile UWORD bltcpt_h;
        volatile UWORD bltcpt_l;
      };
    };
    union {
      volatile APTR bltbpt;
      struct {
        volatile UWORD bltbpt_h;
        volatile UWORD bltbpt_l;
      };
    };
    union {
      volatile APTR bltapt;
      struct {
        volatile UWORD bltapt_h;
        volatile UWORD bltapt_l;
      };
    };
    union {
      volatile APTR bltdpt;
      struct {
        volatile UWORD bltdpt_h;
        volatile UWORD bltdpt_l;
      };
    };
    volatile UWORD bltsize;
    #ifdef AGA
    volatile UBYTE pad2d;
    volatile UBYTE bltcon0l;
    volatile UWORD bltsizv;
    volatile UWORD bltsizh; 
    #else
    volatile UWORD   pad2d[3];
    #endif // AGA
    volatile UWORD   bltcmod;
    volatile UWORD   bltbmod;
    volatile UWORD   bltamod;
    volatile UWORD   bltdmod;
    volatile UWORD   pad34[4];
    volatile UWORD   bltcdat;
    volatile UWORD   bltbdat;
    volatile UWORD   bltadat;
    volatile UWORD   pad3b[3];
    volatile UWORD   deniseid;
    volatile UWORD   dsksync;
    volatile ULONG   cop1lc;
    volatile ULONG   cop2lc;
    volatile UWORD   copjmp1;
    volatile UWORD   copjmp2;
    volatile UWORD   copins;
    volatile UWORD   diwstrt;
    volatile UWORD   diwstop;
    volatile UWORD   ddfstrt;
    volatile UWORD   ddfstop;
    volatile UWORD   dmacon;
    volatile UWORD   clxcon;
    volatile UWORD   intena;
    volatile UWORD   intreq;
    volatile UWORD   adkcon;
    struct  AudChannel2 {
      volatile UWORD *ac_ptr; /* ptr to start of waveform data */
      volatile UWORD ac_len;	/* length of waveform in words */
      volatile UWORD ac_per;	/* sample period */
      volatile UWORD ac_vol;	/* volume */
      volatile UWORD ac_dat;	/* sample pair */
      volatile UWORD ac_pad[2];	/* unused */
    } aud[4];
    
    #ifdef AGA
    volatile APTR    bplpt[8];
    #else
    volatile APTR    bplpt[6];
    volatile UWORD   pad7c[4];
    #endif // AGA
    
    volatile UWORD   bplcon0;
    volatile UWORD   bplcon1;
    volatile UWORD   bplcon2;
    
    #ifdef AGA
    volatile UWORD   bplcon3;
    #else
    volatile UWORD   pad83;
    #endif // AGA

    volatile UWORD   bpl1mod;
    volatile UWORD   bpl2mod;
    
    #ifdef AGA
    volatile UWORD   bplcon4;
    volatile UWORD   clxcon2;
    volatile UWORD   bpldat[8];
    #else
    volatile UWORD   pad86[2];
    volatile UWORD   bpldat[6];
    volatile UWORD   pad8e[2];
    #endif //AGA

    volatile APTR    sprpt[8];
    struct  SpriteDef2 {
      volatile UWORD pos;
      volatile UWORD ctl;
      volatile UWORD dataa;
      volatile UWORD datab;
    } spr[8];
    volatile UWORD color[32];
    volatile UWORD htotal;
    volatile UWORD hsstop;
    volatile UWORD hbstrt;
    volatile UWORD hbstop;
    volatile UWORD vtotal;
    volatile UWORD vsstop;
    volatile UWORD vbstrt;
    volatile UWORD vbstop;
    volatile UWORD sprhstrt;
    volatile UWORD sprhstop;
    volatile UWORD bplhstrt;
    volatile UWORD bplhstop;
    volatile UWORD hhposw;
    volatile UWORD hhposr;
    volatile UWORD beamcon0;
    volatile UWORD hsstrt;
    volatile UWORD vsstrt;
    volatile UWORD hcenter;
    volatile UWORD diwhigh;	/* 1e4 */
    volatile UWORD padf3[11];
    volatile UWORD fmode;
};

#endif	/* HARDWARE_CUSTOM2_H */
