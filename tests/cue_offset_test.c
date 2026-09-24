#include "../pc_pulse.h"
#include <assert.h>
#include <stdint.h>
int main(void) {
 const uint32_t leads[]={0,582,612};
 for(unsigned t=0;t<3;t++) {
  uint32_t lead=leads[t];
  assert(pc_pulse_cue_index_offset(0,lead)==0);
  if(lead)assert(pc_pulse_cue_index_offset(lead-1,lead)==0);
  assert(pc_pulse_cue_index_offset(lead,lead)==0);
  assert(pc_pulse_cue_index_offset(lead+199,lead)==0);
  assert(pc_pulse_cue_index_offset(lead+200,lead)==1);
  assert(pc_pulse_cue_index_offset(lead+12000,lead)==60);
 }
 assert(pc_pulse_cue_index_offset(UINT32_MAX,0)==UINT32_MAX/200);
 assert(pc_pulse_cue_index_offset(0,UINT32_MAX)==0);
 for(uint32_t sample=0;sample<2400000;sample+=137)
  assert(pc_pulse_cue_index(sample)==pc_pulse_cue_index_offset(sample,612));
 return 0;
}
