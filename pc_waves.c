#include "pc_world.h"

typedef struct { int16_t slot,distance,width; } PcWaveRecord;
typedef struct { uint16_t limit,next; } PcWaveEdge;
typedef struct {
    uint16_t first_wall,wall_count,first_edge,edge_count,random_bound,delay_base;
    int16_t spawn_base;
    uint16_t marker_wait;
} PcWaveNode;
typedef struct { uint16_t id,node; } PcWaveRoot;
#include "pc_wave_data.inc"

int pc_generate_wave(PcWorld *w,uint16_t wave,PcRandom random,void *context) {
    uint16_t root=0;
    w->spawn_base=0; /* Reset by the reference, including unknown IDs. */
    while (root<sizeof(wave_roots)/sizeof(wave_roots[0]) && wave_roots[root].id!=wave) ++root;
    if (root==sizeof(wave_roots)/sizeof(wave_roots[0])) return 0;
    uint16_t index=wave_roots[root].node;
    for (;;) {
        const PcWaveNode *n=&wave_nodes[index];
        for (uint16_t i=0;i<n->wall_count;++i) {
            const PcWaveRecord *r=&wave_records[n->first_wall+i];
            if (!pc_world_add(w,(uint8_t)r->slot,r->distance,r->width)) return 0;
        }
        if (!n->edge_count) {
            w->delay_numerator=(uint32_t)n->delay_base*20;
            w->delay_ticks=w->speed ? (uint16_t)((w->delay_numerator+w->speed-1)/w->speed) : n->delay_base;
            w->spawn_base=n->spawn_base;
            if (n->marker_wait) w->marker_wait=1; /* Not cleared by other waves. */
            return 1;
        }
        uint16_t r=random(context,n->random_bound),i=0;
        if (r>=n->random_bound) return 0; /* Broken RNG contract, no wrapping bias. */
        while (r>=wave_edges[n->first_edge+i].limit) ++i;
        index=wave_edges[n->first_edge+i].next;
    }
}
