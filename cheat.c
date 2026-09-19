#include "cheat.h"
#if CHEAT_MODE
/* Bounded look-ahead over existing walls only. Treat thick wall bodies as
 * forbidden too, rather than relying on collision to push the player back.
 * Future spawns/morphs are unknown; follow a bounded prediction horizon. */
#define CHEAT_HORIZON 48
static uint8_t cheat_hazards[CHEAT_HORIZON];
static uint8_t cheat_next[CHEAT_HORIZON+1][6];
static uint8_t cheat_sector[360],cheat_cached_sides;
static uint8_t cheat_remaining,cheat_direction;
static uint16_t cheat_speed;
static int16_t cheat_target;
void cheat_reset(void) { cheat_remaining=0; }
static uint8_t toward(int16_t angle,int16_t target,uint8_t direction,uint8_t rate) {
    int16_t delta=direction==PC_INPUT_POSITIVE ? target-angle:angle-target;
    if (delta<0) delta+=360;
    return direction && delta>rate/2 && delta<360-rate/2 ? direction:0;
}


__attribute__((noinline,used))
uint8_t cheat_steer(const PcPlayer *player,const PcWorld *world,
                    uint8_t sides,uint8_t turn_rate) {
    /* Replan at 10 Hz; execute the chosen route at the full 60 Hz turn rate.
     * This bounds CPU cost on a stock 68000. Release/reset cancels the plan. */
    if (cheat_remaining && cheat_cached_sides==sides && cheat_speed==world->speed) {
        --cheat_remaining;
        return toward(player->angle,cheat_target,cheat_direction,turn_rate);
    }
    cheat_remaining=5;cheat_speed=world->speed;cheat_direction=0;
    if (cheat_cached_sides!=sides) {
        uint16_t angle=0,arc=360/sides;
        for (uint8_t slot=0;slot<sides;++slot)
            for (uint16_t offset=0;offset<arc;++offset) cheat_sector[angle++]=slot;
        cheat_cached_sides=sides;
    }
    for (unsigned t=0;t<CHEAT_HORIZON;++t) cheat_hazards[t]=0;
    for (uint16_t i=0;i<world->count;++i) {
        PcWall w=world->walls[i];
        if (!w.active || w.slot>=sides ||
            w.distance>151+(int32_t)world->speed*CHEAT_HORIZON) continue;
        uint8_t bit=(uint8_t)(1u<<w.slot);
        int32_t speed=world->speed;
        int32_t start=w.distance>150 ? (w.distance-150+speed-1)/speed:0;
        int32_t end=w.distance>=145-speed ? (w.distance-(145-speed))/speed+1:0;
        if (w.width>199) {
            int32_t inner=w.distance>0 ? (w.distance+speed-1)/speed-1:0;
            int32_t body_end=inner+(w.width-199+speed-1)/speed;
            if (body_end>end) end=body_end;
        }
        if (end>CHEAT_HORIZON) end=CHEAT_HORIZON;
        for (int32_t t=start;t<end;++t) cheat_hazards[t]|=bit;
    }
    uint8_t any=0;
    for (unsigned t=0;t<CHEAT_HORIZON;++t) any|=cheat_hazards[t];
    if (!(any & (1u<<cheat_sector[player->angle]))) return 0;
    for (uint8_t slot=0;slot<sides;++slot) {
        uint8_t next=CHEAT_HORIZON;
        cheat_next[CHEAT_HORIZON][slot]=next;
        for (int16_t t=CHEAT_HORIZON-1;t>=0;--t) {
            if (cheat_hazards[t] & (1u<<slot)) next=(uint8_t)t;
            cheat_next[t][slot]=next;
        }
    }
    uint8_t best_input=0;
    unsigned best_time=0,best_moves=CHEAT_HORIZON+1;
    int16_t arc=360/sides;
    /* First candidate stays put. Then approach each sector centre from
     * either direction. Prefer longest survival, then least movement. */
    for (unsigned plan=0;plan<=2*sides;++plan) {
        int16_t angle=player->angle;
        unsigned moves=0,t;
        uint8_t first=0;
        int16_t target=plan ? (int16_t)(((plan-1)/2)*arc+arc/2):angle;
        uint8_t direction=(plan&1) ? PC_INPUT_POSITIVE:PC_INPUT_NEGATIVE;
        for (t=0;t<CHEAT_HORIZON;++t) {
            int16_t delta=direction==PC_INPUT_POSITIVE ? target-angle:angle-target;
            if (delta<0) delta+=360;
            uint8_t input=plan && delta>turn_rate/2 && delta<360-turn_rate/2 ? direction:0;
            if (!t) first=input;
            if (input) {angle=pc_turn(angle,input,turn_rate);++moves;}
            uint8_t sector=cheat_sector[angle];
            if (!input) {t=cheat_next[t][sector];break;}
            if (cheat_hazards[t] & (1u<<sector)) break;
        }
        if (t>best_time || (t==best_time && moves<best_moves)) {
            best_time=t;best_moves=moves;best_input=first;
            cheat_target=target;cheat_direction=plan ? direction:0;
        }
    }
    return best_input;
}
#endif
