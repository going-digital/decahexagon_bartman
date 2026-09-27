#pragma once
#include <stdint.h>
#define ARCADE_ROWS 5
#define ARCADE_NAME 10
typedef struct { uint32_t ticks; char name[ARCADE_NAME+1]; } ArcadeScore;
typedef struct {
    ArcadeScore scores[6][ARCADE_ROWS];
    uint16_t revision;
    uint8_t enabled, pending, profile, row, length;
} Arcade;
/* Strictly greater inserts ahead; equal scores retain their existing order. */
static inline int arcade_rank(const Arcade *a,unsigned profile,uint32_t ticks) {
    if(!ticks || profile>=6)return -1;
    for(unsigned i=0;i<ARCADE_ROWS;++i)if(ticks>a->scores[profile][i].ticks)return i;
    return -1;
}
#ifdef __m68k__
__attribute__((optimize("Os")))
#endif
static inline int arcade_enter(Arcade *a,unsigned profile,uint32_t ticks) {
    int rank=arcade_rank(a,profile,ticks);
    if(rank<0)return 0;
    for(int i=ARCADE_ROWS-1;i>rank;--i)a->scores[profile][i]=a->scores[profile][i-1];
    a->scores[profile][rank]=(ArcadeScore){ticks,""};
    a->profile=profile;a->row=rank;a->length=0;a->pending=1;++a->revision;
    return 1;
}
static inline void arcade_type(Arcade *a,unsigned ch) {
    if(!a->pending)return;
    char *name=a->scores[a->profile][a->row].name;
    if(ch==8) {if(a->length)name[--a->length]=0;}
    else if(a->length<ARCADE_NAME && ((ch>='A' && ch<='Z') ||
            (ch>='0' && ch<='9') || ch==' ' || ch=='-')) {
        name[a->length++]=ch;name[a->length]=0;
    } else return;
    ++a->revision;
}
static inline void arcade_finish(Arcade *a) {
    if(!a->length) {
        char *s=a->scores[a->profile][a->row].name;
        s[0]='A';s[1]='N';s[2]='O';s[3]='N';s[4]=0;
    }
    a->pending=0;++a->revision;
}
/* Amiga physical US key positions. Uppercase names need no Shift state. */
static inline unsigned arcade_key(unsigned code) {
    if(code>=0x10 && code<=0x19)return "QWERTYUIOP"[code-0x10];
    if(code>=0x20 && code<=0x28)return "ASDFGHJKL"[code-0x20];
    if(code>=0x31 && code<=0x37)return "ZXCVBNM"[code-0x31];
    if(code>=1 && code<=10)return "1234567890"[code-1];
    return code==0x40?' ':code==0x41?8:code==0x0b?'-':0;
}
