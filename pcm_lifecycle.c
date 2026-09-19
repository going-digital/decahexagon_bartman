#include "pcm_lifecycle.h"
unsigned pcm_lifecycle_tick(PcmLifecycle *s,unsigned playing,unsigned menu) {
    unsigned action=0;
    if(playing && !s->was_playing) {
        action=PCM_START|(s->active?PCM_STOP:0);
        s->active=1;s->fade_in=30;s->fade_out=0;s->volume=0;
    } else if(menu) {
        if(s->active) action=PCM_STOP;
        s->active=s->fade_in=s->fade_out=s->volume=0;
    } else if(!playing && s->was_playing && s->active) {
        s->fade_out=45;s->fade_in=0;
    } else if(s->active && s->fade_out) {
        if(!--s->fade_out) {s->active=0;action=PCM_STOP;}
        s->volume=64*s->fade_out/45;
    } else if(s->active && s->fade_in) {
        --s->fade_in;s->volume=64*(30-s->fade_in)/30;
    }
    s->was_playing=playing;
    return action;
}
