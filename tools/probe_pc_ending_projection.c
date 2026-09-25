// Execute only selected pure routines from the owned x86_64 Mach-O.
// No app startup, Steam, audio, rendering, or game filesystem routines run.
// Invoked by capture_pc_palette.py, which verifies the executable SHA-256.
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
static float controlled_random(float maximum) {

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

int main(int argc,char **argv) {
 assert(argc==2);load(argv[1]);install_hooks();
 unsigned char *s=calloc(1,0x10000),*g=calloc(1,0x3000);
 double sine[360],cosine[360];
 for(int i=0;i<360;i++){sine[i]=sin(i*acos(-1.0)/180);cosine[i]=cos(i*acos(-1.0)/180);}
 uintptr_t a=(uintptr_t)sine,b=(uintptr_t)cosine;
 memcpy(s+0xd908,&a,8);memcpy(s+0xd920,&b,8);
 wr32(s,0x5508,1);wr32(s,0x54ac,4);wr32(s,0x5480,-1);wr32(s,0x548c,-1);
 wr32(g,8,384);wr32(g,12,240);wrd(s,0x57b8,600);wrd(s,0x57c0,600);
 const int points[][3]={{100,0,0},{0,100,0},{100,100,0},{-100,-100,0},{0,0,100},{300,200,-100}};
 for(int tilt=0;tilt<=30;tilt+=10)for(int otis=-20;otis<=20;otis+=10)
 for(int depth=0;depth<=600;depth+=300)for(unsigned p=0;p<6;p++){
  wrf(s,0x547c,tilt);wrf(s,0x54b0,otis);wr32(s,0x5730,depth);
  wrd(s,0x2a18,points[p][0]);wrd(s,0x3378,points[p][1]);wrd(s,0x3cd8,points[p][2]);
  ((void(*)(void*,void*))(0x10000ab90+slide))(s,g);
  double x,y,z;memcpy(&x,s+0x2a18,8);memcpy(&y,s+0x3378,8);memcpy(&z,s+0x3cd8,8);
  printf("%d %d %d %d %d %d %.12g %.12g %.12g %d %d\n",tilt,otis,depth,
   points[p][0],points[p][1],points[p][2],x,y,z,(int)rd32(s,0x4ae8),(int)rd32(s,0x4f98));
 }free(g);free(s);return 0;
}
