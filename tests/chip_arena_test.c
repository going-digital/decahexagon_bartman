#include "../trackloader/chip_arena.h"
#include <assert.h>
#include <stdint.h>
#include <string.h>
static union { uint64_t align; unsigned char bytes[4096]; } store;
int main(void) {
 unsigned char *p=store.bytes;
 memset(p,0xa5,sizeof(store.bytes));
 assert(!track_chip_init(p+1,4000));assert(!track_chip_alloc(1));
 assert(track_chip_init(p+8,4080));
 assert(!track_chip_alloc(0));assert(!track_chip_alloc(UINT32_MAX));
 assert(!track_chip_alloc(4080));
 void *a=track_chip_alloc(17),*b=track_chip_alloc(55),*c=track_chip_alloc(32);
 assert(a && b && c && !((uintptr_t)a&7));
 for(unsigned i=0;i<17;i++)assert(!((unsigned char*)a)[i]);
 memset(b,0x5a,55);track_chip_free(b);
 void *d=track_chip_alloc(55);assert(d==b);
 for(unsigned i=0;i<55;i++)assert(!((unsigned char*)d)[i]);
 track_chip_free(a);track_chip_free(c);track_chip_free(d);
 assert(track_chip_available()==4072);
 void *all=track_chip_alloc(4072);assert(all && !track_chip_alloc(1));
 track_chip_free(all);track_chip_free(all);track_chip_free(0);track_chip_free(p);
 assert(track_chip_available()==4072);
 /* Fragmentation, arbitrary-order release and metadata reuse. */
 void *slots[120];
 for(unsigned pass=0;pass<20;pass++) {
  for(unsigned i=0;i<120;i++){slots[i]=track_chip_alloc(1+i%31);assert(slots[i]);}
  for(unsigned i=0;i<120;i+=2)track_chip_free(slots[i]);
  for(unsigned i=1;i<120;i+=2)track_chip_free(slots[i]);
  assert(track_chip_available()==4072);
 }
 for(unsigned i=0;i<8;i++)assert(p[i]==0xa5 && p[4088+i]==0xa5);
 return 0;
}
