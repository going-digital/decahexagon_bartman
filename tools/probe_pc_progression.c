// Execute only selected pure routines from the owned x86_64 Mach-O.
// No app startup, Steam, audio, rendering, or game filesystem routines run.
// capture_pc_progression.py checks the executable hash before invoking this.
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
static unsigned char original_generate[12];
static int chosen[16],nchosen;
static void capture_generate(void *state,int wave) {
    assert(nchosen<16);chosen[nchosen++]=wave;
    uintptr_t addr=0x10000d0d0+slide;
    memcpy((void*)addr,original_generate,12);
    ((void(*)(void*,int))addr)(state,wave);
    patch(addr,capture_generate);
}
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

int main(int argc,char **argv) {
 assert(argc==2);load(argv[1]);install_hooks();
 memcpy(original_generate,(void*)(0x10000d0d0+slide),12);
 patch(0x10000d0d0+slide,capture_generate);
 void(*logic)(void*)=(void*)(0x100056f50+slide);
 unsigned char *s=malloc(0x50000);
 for(int stage=0;stage<3;stage++)for(int hyper=0;hyper<2;hyper++)for(int scenario=0;scenario<4;scenario++)for(seed=0;seed<16;seed++) {
  if((stage==2 && scenario) || (scenario==3 && stage!=0))continue;
  init_state(s);wr32(s,0x54c4,stage);s[0x5518]=hyper;
  int start=scenario ? (hyper?7198:10798):0;
  if(scenario==3) { start+=7200;wr32(s,0x57c4,1);wr32(s,0x57b8,stage);s[0x57b4]=hyper; }
  wr32(s,0x2970,scenario?80:hyper?(stage?51:31):0);
  wrf(s,0x2978,scenario?33:hyper?(stage==2?40:33):(stage==2?35:stage?24:22));
  wr32(s,0x5514,!scenario && hyper);wr32(s,0x29a0,hyper?4:0);
  wr32(s,0x2994,stage==2 ? 9:7);wrf(s,0x21c,5);
  wrf(s,0x296c,scenario>=2?100:0);draws=0;
  if(!scenario) {
   int r,base=hyper?4:stage==2?2:0;
   do { r=base+(int)controlled_random(2.0); } while(r==0);
   wr32(s,0x29a0,r);
  }
  for(int n=0;n<8;n++) {
   int tick=start+n+1;nchosen=0;wrf(s,0x2934,tick-1);
   logic(s);
   printf("%d %d %d %d %d",stage,hyper,scenario,seed,tick);
   printf(" %d %d %.0f %.0f %.9g %d %d %d %d %d %d %d",(int)rd32(s,0x54c4),s[0x5518],rdf(s,0x2934),rdf(s,0x2978),rdf(s,0x296c),(int)rd32(s,0x2970),(int)rd32(s,0x5514),(int)rd32(s,0x57bc),(int)rd32(s,0x57c4),(int)rd32(s,0x2994),(int)rd32(s,0x29a0),draws);
   printf(" %d",nchosen);for(int j=0;j<nchosen;j++)printf(" %d",chosen[j]);
   uint32_t hash=2166136261u;int count=rd32(s,0x2930);
   for(int j=0;j<count;j++) { unsigned char *w=s+0x220+j*20;
    for(int k=0;k<3;k++)hash=(hash^rd32(w,k*4))*16777619u;
    hash=(hash^w[16])*16777619u;
   }
   printf(" %d %u %.0f\n",count,hash,rdf(s,0x21c));
  }
 }
}
