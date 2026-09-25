#include "../pc_ending.h"
#include <assert.h>
#include <stdio.h>
/* Thresholds independently transcribed from PC gamelogic branch immediates.
 * Source translation tests, not a claim of native full-timeline equivalence. */
int main(void) {
    const unsigned cases[][6]={
      {2,24500,3,20,1,9},{4,35500,5,35,2,10},
      {5,46500,6,35,3,7},{6,57600,7,45,4,6},
      {7,68600,8,45,3,5},{8,96200,9,45,5,1},
      {9,107500,10,45,6,3},{11,118000,101,30,5,0},
      {12,140000,13,20,2,200},{13,151300,14,40,8,30}
    };
    /* Source-derived handoff boundary, evaluated after death expansion.
     * Distinct from the native entry routine fixture, which starts at entry. */
    for(unsigned completion=0;completion<4;completion++) {
        assert(!pc_ending_death_entry(completion,59,200,0));
        assert(!pc_ending_death_entry(completion,60,199,0));
        assert(pc_ending_death_entry(completion,60,200,0)==(completion==3));
        assert(!pc_ending_death_entry(completion,60,200,1));
    }
    PcEnding s;
    pc_ending_init(&s);assert(s.phase==0 && s.palette==30 && s.wave_mode==8);
    pc_ending_tick(&s,0,0);assert(s.phase==1 && s.speed==0);
    for(unsigned n=0;n<30;n++)pc_ending_tick(&s,0,0);
    assert(s.phase==1 && s.speed==5);
    pc_ending_tick(&s,0,0);assert(s.phase==2 && s.speed==5);
    for(unsigned i=0;i<sizeof(cases)/sizeof(*cases);i++) {
        pc_ending_init(&s);s.phase=cases[i][0];
        pc_ending_step(&s,cases[i][1]-1,0,1);assert(s.phase==cases[i][0]);
        unsigned e=pc_ending_step(&s,cases[i][1],0,1);
        assert((e&(PC_END_PALETTE|PC_END_FLIP))==(PC_END_PALETTE|PC_END_FLIP));
        assert(s.phase==cases[i][2] && s.speed==cases[i][3]);
        assert(s.wave_mode==cases[i][4] && s.palette==cases[i][5]);
    }
    pc_ending_init(&s);s.phase=3;
    pc_ending_step(&s,30000,1,0);assert(s.phase==3);
    pc_ending_step(&s,30000,0,0);assert(s.phase==4);
    s.phase=5;s.palette=10;
    assert(!pc_ending_step(&s,37000,0,1) && s.palette==10);
    assert(pc_ending_step(&s,37000,0,0)==PC_END_PALETTE && s.palette==11);
    s.phase=10;pc_ending_step(&s,113999,0,0);assert(s.phase==10);
    pc_ending_step(&s,114000,0,0);assert(s.phase==11);
    s.phase=101;pc_ending_step(&s,129499,0,0);assert(s.phase==101);
    pc_ending_step(&s,129500,0,0);assert(s.phase==12);
    s.phase=14;assert(!pc_ending_step(&s,163499,0,0));
    assert(pc_ending_step(&s,163500,0,0)==PC_END_STOP_MUSIC);
    assert(!pc_ending_step(&s,163501,0,0));
    assert(!pc_ending_step(&s,167999,0,0));
    assert(!pc_ending_step(&s,168000,0,0));
    PcWorld world={0};
    assert(pc_ending_gate(&s,&world,168000)==PC_END_GATE_FINISH);
    assert(!pc_ending_step(&s,168001,0,0));
    pc_ending_init(&s);
    for(unsigned tick=1;tick<=10080;tick++) {
        pc_ending_tick(&s,0,0);
        world.delay_ticks=0;
        unsigned gate=pc_ending_gate(&s,&world,tick/60*1000+tick%60*1000/60);
        assert((gate==PC_END_GATE_FINISH)==(tick==10080));
    }
    puts("Ending timeline: entry, phase boundaries, renderer gates and 168-second exit pass");
}
