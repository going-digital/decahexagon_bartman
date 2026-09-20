#include "../fib_pcm.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void be32(unsigned char *p,unsigned v){p[0]=v>>24;p[1]=v>>16;p[2]=v>>8;p[3]=v;}
int main(void){
 unsigned char blob[74]={0},out[37];
 const unsigned char expected[]={11,12,13,21,22,23,24,11,12,13,31,32};
 memcpy(blob,"FBP1",4);
 unsigned header[]={12,4,3,32,48,64,74};
 for(unsigned i=0;i<7;++i)be32(blob+4+4*i,header[i]);
 unsigned offsets[]={0,4,8,10};
 for(unsigned i=0;i<4;++i)be32(blob+32+4*i,offsets[i]);
 unsigned ids[]={0,1,0,2},lengths[]={3,4,3,2};
 for(unsigned i=0;i<4;++i){blob[49+4*i]=ids[i];blob[51+4*i]=lengths[i];}
 const unsigned char data[]={11,12,13,0,21,22,23,24,31,32};memcpy(blob+64,data,10);
 for(unsigned format=1;format<=2;++format)for(unsigned split=0;split<2;++split){
  blob[3]='0'+format;
  PcmSong s;
  assert(fib_pcm_init_split(&s,blob,split?68:74,split?blob+68:0,split?6:0));
  for(unsigned pos=0;pos<12;++pos){
   assert(fib_song_seek(&s,pos));
   fib_song_read(&s,out,sizeof(out));
   for(unsigned i=0;i<sizeof(out);++i)assert(out[i]==expected[(pos+i)%12]);
  }
  PcmSong before=s;assert(!fib_song_seek(&s,12));assert(!memcmp(&s,&before,sizeof(s)));
  assert(!fib_song_seek(&s,~0u));assert(!memcmp(&s,&before,sizeof(s)));
 }
 puts("PCM seek: split/contiguous FBP1/FBP2, odd offsets, repeated slices, padding and wrap passed");
}
