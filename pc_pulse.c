#include "pc_pulse.h"
#ifdef __m68k__
#include "support/gcc8_c_support.h"
#endif
static uint32_t pulse_divide_three(uint16_t value) {
#ifdef __m68k__
    return muluw(value,43691u)>>17;
#else
    return ((uint32_t)value*43691u)>>17;
#endif
}
uint16_t pc_pulse_tick(uint16_t envelope,int cue,unsigned stage) {
    unsigned magnitude=cue<0?-(int32_t)cue:cue;
    /* Exact reciprocal for every unsigned 16-bit magnitude; retain the
     * full-width API for callers outside the music cue range. */
    unsigned target=stage!=2 ? magnitude>>1 : magnitude<=65535u ?
        pulse_divide_three((uint16_t)magnitude) : magnitude/3;
    /* PC attacks to target then subtracts dt; on decay it subtracts dt twice. */
    if(target>envelope) return target?target-1:0;
    return envelope>1?envelope-2:0;
}
unsigned pc_pulse_cue_index_offset(uint32_t sample,uint32_t lead) {
    if(sample<lead)return 0;
    uint32_t position=sample-lead;
#ifdef __m68k__
    /* DIVU.W has a 32-bit dividend and a 16-bit quotient. All soundtrack
     * positions fit this path; retain full-width semantics for other callers. */
    if(position<65536u*200u)return divuw(position,200);
#endif
    return position/200u;
}
unsigned pc_pulse_cue_index(uint32_t sample) {
    return pc_pulse_cue_index_offset(sample,612);
}
uint32_t pc_pcm_position(uint32_t block,uint32_t length,unsigned lines,unsigned period) {
    unsigned offset=lines>700?511:(lines*227u)/period;
    if(offset>511) offset=511;
    uint32_t position=block+offset;
    return position>=length?position-length:position;
}
