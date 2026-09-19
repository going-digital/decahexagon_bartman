#include "pc_pulse.h"
uint16_t pc_pulse_tick(uint16_t envelope,int cue,unsigned stage) {
    unsigned magnitude=cue<0?-(int32_t)cue:cue;
    unsigned target=magnitude/(stage==2?3:2);
    /* PC attacks to target then subtracts dt; on decay it subtracts dt twice. */
    if(target>envelope) return target?target-1:0;
    return envelope>1?envelope-2:0;
}
unsigned pc_pulse_cue_index(uint32_t sample) {
    return sample<612?0:(sample-612)/200;
}
uint32_t pc_pcm_position(uint32_t block,uint32_t length,unsigned lines,unsigned period) {
    unsigned offset=lines>700?511:(lines*227u)/period;
    if(offset>511) offset=511;
    uint32_t position=block+offset;
    return position>=length?position-length:position;
}
