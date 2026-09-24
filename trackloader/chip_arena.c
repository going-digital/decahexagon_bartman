#include "chip_arena.h"
#include <stddef.h>
#include <stdint.h>
typedef struct { uint32_t span, used; } Block;
static unsigned char *arena;
static uint32_t arena_bytes;
int track_chip_init(void *memory,uint32_t bytes) {
    arena=0;arena_bytes=0;
    if (!memory || ((uintptr_t)memory&7) || bytes<16 ||
        (uintptr_t)memory>UINTPTR_MAX-bytes) return 0;
    bytes&=~7u;arena=memory;arena_bytes=bytes;
    Block *b=(Block *)arena;b->span=bytes;b->used=0;
    return 1;
}
void *track_chip_alloc(uint32_t bytes) {
    if (!bytes || bytes>UINT32_MAX-15) return 0;
    uint32_t need=((bytes+7)&~7u)+sizeof(Block);
    for(uint32_t at=0;at<arena_bytes;) {
        Block *b=(Block *)(arena+at);
        if(!b->used && b->span>=need) {
            if(b->span-need>=16) {
                Block *next=(Block *)(arena+at+need);
                next->span=b->span-need;next->used=0;b->span=need;
            }
            b->used=1;
            unsigned char *result=arena+at+sizeof(Block);
            for(uint32_t i=0;i<b->span-sizeof(Block);i++)result[i]=0;
            return result;
        }
        at+=b->span;
    }
    return 0;
}
void track_chip_free(void *memory) {
    Block *previous=0;
    for(uint32_t at=0;at<arena_bytes;) {
        Block *b=(Block *)(arena+at);
        if(arena+at+sizeof(Block)==memory) {
            if(!b->used)return;
            b->used=0;
            uint32_t next_at=at+b->span;
            if(next_at<arena_bytes) {
                Block *next=(Block *)(arena+next_at);
                if(!next->used)b->span+=next->span;
            }
            if(previous && !previous->used)previous->span+=b->span;
            return;
        }
        previous=b;at+=b->span;
    }
}
uint32_t track_chip_available(void) {
    uint32_t total=0;
    for(uint32_t at=0;at<arena_bytes;) {
        Block *b=(Block *)(arena+at);
        if(!b->used)total+=b->span-sizeof(Block);
        at+=b->span;
    }
    return total;
}
