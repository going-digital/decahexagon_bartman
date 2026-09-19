// Execute only selected pure routines from the owned x86_64 Mach-O.
// No app startup, Steam, audio, rendering, or game filesystem routines run.
// capture_pc_death.py checks the executable hash before invoking this.
// The original file is never modified; ofRandom is replaced in private memory.
#include <mach-o/loader.h>
#include <sys/mman.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <math.h>

static int draws, seed;
static uintptr_t slide;
static uintptr_t relay_space;
static int scripted,script_len,script_values[128];
static float controlled_random(float maximum) {
    if(scripted){assert(draws<script_len);int v=script_values[draws++];assert(v>=0 && v<(int)maximum);return v+0.25f;}
    // Same 32-bit arithmetic for both the native and decompile probes.
    uint32_t x = (uint32_t)seed + (uint32_t)(++draws) * 0x9e3779b9u;
    x ^= x >> 16; x *= 0x7feb352du; x ^= x >> 15;
    return (float)(x % (uint32_t)maximum) + 0.25f;
}
static void patch(uintptr_t addr, void *fn) {
    unsigned char op[12] = {0x48,0xb8,0,0,0,0,0,0,0,0,0xff,0xe0};
    memcpy(op + 2, &fn, 8); memcpy((void *)addr, op, sizeof(op));
}
static void load(const char *path) {
    FILE *f = fopen(path, "rb"); assert(f);
    struct mach_header_64 h; assert(fread(&h,sizeof(h),1,f)==1);
    assert(h.magic==MH_MAGIC_64 && h.cputype==CPU_TYPE_X86_64);
    unsigned char *cmds=malloc(h.sizeofcmds); assert(fread(cmds,h.sizeofcmds,1,f)==1);
    unsigned char *p=cmds;
    uint64_t low=UINT64_MAX,high=0;
    for(unsigned i=0;i<h.ncmds;i++) {
        struct load_command *lc=(void*)p;
        if(lc->cmd==LC_SEGMENT_64) {
            struct segment_command_64 *s=(void*)p;
            if(strcmp(s->segname,"__PAGEZERO") && s->vmsize) {
                if(s->vmaddr<low)low=s->vmaddr;
                if(s->vmaddr+s->vmsize>high)high=s->vmaddr+s->vmsize;
            }
        }
        p+=lc->cmdsize;
    }
    void *region=mmap(NULL,high-low+4096,PROT_READ|PROT_WRITE|PROT_EXEC,MAP_PRIVATE|MAP_ANON,-1,0);
    if(region==MAP_FAILED){perror("mmap");exit(2);}
    slide=(uintptr_t)region-low;
    relay_space=(uintptr_t)region+(high-low);
    p=cmds;
    for(unsigned i=0;i<h.ncmds;i++) {
        struct load_command *lc=(void*)p;
        if(lc->cmd==LC_SEGMENT_64) {
            struct segment_command_64 *s=(void*)p;
            if(strcmp(s->segname,"__PAGEZERO") && s->vmsize) {
                void *got=(void*)(uintptr_t)(s->vmaddr+slide);
                if(s->filesize) { fseek(f,s->fileoff,SEEK_SET); assert(fread(got,s->filesize,1,f)==1); }
            }
        }
        p+=lc->cmdsize;
    }
    fclose(f); free(cmds); patch(0x100075a40+slide,controlled_random);
}
static uint32_t rd32(unsigned char *p,int o) { uint32_t n;memcpy(&n,p+o,4);return n; }
static float rdf(unsigned char *p,int o) {float n;memcpy(&n,p+o,4);return n;}
static void wr32(unsigned char *p,int o,uint32_t n) {memcpy(p+o,&n,4);}
static void wrf(unsigned char *p,int o,float n) {memcpy(p+o,&n,4);}
static void wrd(unsigned char *p,int o,double n) {memcpy(p+o,&n,8);}
static void no_effect(void) {}
static void install_hooks(void) {
    uintptr_t hooks[]={0x100051000,0x10005fef0,0x10005d370,0x10005dc30,
                      0x10004c8b0,0x10004dab0,0x10004f1a0,0x10005d6b0};
    for(unsigned i=0;i<sizeof(hooks)/sizeof(*hooks);i++)patch(hooks[i]+slide,no_effect);
    // Six-byte import stubs are adjacent: use a five-byte relative jump to a
    // private nearby relay, rather than overwriting the following stub.
    uintptr_t relay=relay_space;
    patch(relay,fmod);patch(relay+16,floor);
    uintptr_t imports[]={0x100175132+slide,0x100175126+slide};
    for(int i=0;i<2;i++) {
        int32_t delta=(int32_t)((relay+i*16)-(imports[i]+5));
        *(unsigned char*)imports[i]=0xe9;memcpy((void*)(imports[i]+1),&delta,4);
    }
}
static void init_state(unsigned char *s) {
    memset(s,0,0x50000); wrd(s,0x2960,1.0);wr32(s,0x1a4,6);
    wr32(s,0x2994,7);wr32(s,0x298c,30);wr32(s,0x2990,30);
    wr32(s,0x40828,-1);wr32(s,0x19c,40);wrf(s,0x2978,22);
    wrf(s,0x296c,100000);wr32(s,0x29a8,100000);
    for(int i=0;i<6;i++)wr32(s,0x5538+i*4,100000);
    wr32(s,0x57bc,-1);wr32(s,0x57c4,-1);
}



static float fixed_delta(void) { return 1.0f; }

int main(int argc,char **argv) {
 assert(argc==2);load(argv[1]);install_hooks();
 uintptr_t hooks[]={0x100009470,0x100009b00,0x10004fda0,0x10004fdd0,0x10005dbf0,0x100060020,0x100060050,0x100060160};
 for(unsigned i=0;i<sizeof(hooks)/sizeof(*hooks);i++)patch(hooks[i]+slide,no_effect);
 patch(0x100061530+slide,fixed_delta);
 void(*logic)(void*)=(void*)(0x100056f50+slide);
 unsigned char *s=malloc(0x50000);
 const int walls[6][4]={{0,1000,200,1},{4,0,77,1},{20,0,0,1},{21,-5,0,1},{22,500,0,0},{3,2200,333,0}};
 for(int initial=0;initial<2;initial++)for(int profile=0;profile<12;profile++)for(int shape=0;shape<6;shape++) {
  init_state(s);wr32(s,0x54c4,profile%3);s[0x5518]=profile/3;wr32(s,0x575c,1);
  wrf(s,0x2934,123);wrf(s,0x194,initial?59:1);wr32(s,0x2990,137);wr32(s,0x298c,137);wr32(s,0x551c,1);
  if(profile>=6) {
   int selected=profile<10?0:1;
   wr32(s,0x54c4,selected);s[0x5518]=profile&1;
   wr32(s,0x57b8,selected);s[0x57b4]=profile&1;
   if(profile<10)wr32(s,0x57c4,1);
   if(profile>=8)wr32(s,0x57bc,2);
   wrf(s,0x2934,profile==8 || profile==9 ? 20000:14000);
  }
  int sides=shape==2 || shape==5 ? 4:shape==3 ? 3:6;
  int state=shape==1 || shape==4 ? 1:shape==2 ? 2:shape==5 ? 3:0;
  double phase=0;
  if(shape==4)for(int i=0;i<5;i++)phase+=0.1;
  if(shape==5){phase=1;for(int i=0;i<4;i++)phase-=0.1;}
  wr32(s,0x1a4,sides);wr32(s,0x210,state);wrd(s,0x208,phase);wr32(s,0x2930,6);
  for(int i=0;i<6;i++) {unsigned char *w=s+0x220+i*20;
   for(int j=0;j<3;j++)wr32(w,j*4,walls[i][j]);w[16]=walls[i][3];
  }
  if(shape&1){s[0x230+20]=0;s[0x230+5*20]=1;}
  for(int tick=1;tick<=120;tick++) {
   logic(s);double phase;memcpy(&phase,s+0x208,8);
   uint32_t hash=2166136261u;
   for(int i=0;i<6;i++){unsigned char *w=s+0x220+i*20;
    for(int j=0;j<3;j++)hash=(hash^rd32(w,j*4))*16777619u;
    hash=(hash^w[16])*16777619u;
   }
   printf("%d %d %d %d %.0f %u %u %u %d %u %u %.0f %u\n",initial,profile,shape,tick,rdf(s,0x194),rd32(s,0x19c),rd32(s,0x1a4),rd32(s,0x210),(int)(360.0/(rd32(s,0x1a4)-phase)),rd32(s,0x551c),hash,rdf(s,0x2934),rd32(s,0x2930));
  }
 }
}
