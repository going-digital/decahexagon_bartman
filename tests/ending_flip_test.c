#include "../pc_ending_flip.h"
#include <assert.h>
#include <stdio.h>
/* Source-derived trajectories; not an executed native flip oracle. */
int main(void) {
 for(unsigned mode=0;mode<=10;mode++) {
  PcEndingFlip flip={0};int sign=(mode==1 || mode==3 || mode==5 || mode==7)?1:-1;
  pc_ending_flip_request(&flip);assert(flip.phase==1);
  assert(pc_ending_flip_tick(&flip,mode)==0);
  int sum=0;
  for(int tick=1;tick<=14;tick++){int d=pc_ending_flip_tick(&flip,mode);assert(d==sign*tick);sum+=d;}
  for(int tick=0;tick<31;tick++){int d=pc_ending_flip_tick(&flip,mode);assert(d==sign*14);sum+=d;}
  for(int tick=9;tick>=0;tick--){int d=pc_ending_flip_tick(&flip,mode);assert(d==sign*tick);sum+=d;}
  assert(!flip.phase && !flip.timer && sum==sign*584);
  assert(pc_ending_flip_tick(&flip,mode)==0);
  pc_ending_flip_request(&flip);assert(pc_ending_flip_tick(&flip,mode)==0);
 }
 puts("Ending flip: 11 mode directions, 56-tick trajectories and repeat requests pass");
}
