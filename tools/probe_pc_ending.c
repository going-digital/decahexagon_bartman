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
static uint32_t rd(unsigned char *p,unsigned o){uint32_t v;memcpy(&v,p+o,4);return v;}
static void wr(unsigned char *p,unsigned o,uint32_t v){memcpy(p+o,&v,4);}
static unsigned saves,clears,detaches;static int music,effect,palette;
static void save(void*p){(void)p;saves++;}
static float delta(void){return 1.0f;}
static unsigned now(void){return 123456;}
static void play(void*p,int n){(void)p;music=n;}
static void sfx(void*p,int n){(void)p;effect=n;}
static void pal(void*p,int n,int fade){(void)p;(void)fade;palette=n;}
static void clear(void*p){(void)p;clears++;}
static void detach(void*p){(void)p;detaches++;}
int main(int argc,char**argv){assert(argc==2);load(argv[1]);
 patch(0x100009470+slide,save);
 patch(0x100061530+slide,delta);
 patch(0x1000755c0+slide,now);
 patch(0x10005d6b0+slide,play);
 patch(0x10005d370+slide,sfx);
 patch(0x10004dab0+slide,pal);
 patch(0x100048e80+slide,clear);
 patch(0x10004fde0+slide,detach);
 unsigned char *g=calloc(1,0x6000),*gfx=calloc(1,0x100),*audio=calloc(1,0x100);
 for(unsigned profile=0;profile<6;profile++)for(unsigned completed=0;completed<2;completed++){
  memset(g,0,0x6000);saves=0;
  wr(g,0x54ac,profile%3);g[0x5500]=profile>=3;
  unsigned o=(profile>=3?0x54d0:0x54b8)+4*(profile%3);wr(g,o,completed);
  ((void(*)(void*))(0x100009b50+slide))(g);
  unsigned expected=completed?0:profile==5?3:profile==2?2:1;
  assert(rd(g,o)==1 && rd(g,0x574c)==expected && saves==1);
  printf("completion %u %u %u\n",profile,completed,rd(g,0x574c));
 }
 memset(g,0x55,0x6000);music=effect=palette=-1;
 ((void(*)(void*,void*,void*))(0x100029a90+slide))(g,gfx,audio);
 assert(rd(g,0x54ac)==4 && rd(g,0x18c)==6 && !rd(g,0x17c));
 assert(!rd(g,0x2918) && !rd(g,0x57e0) && !rd(g,0x57f8));
 assert(rd(g,0x57f0)==123456 && rd(g,0x2988)==8);
 assert(music==5 && effect==3 && palette==30 && clears==1 && detaches==1);
 puts("secret_entry stage=4 sides=6 extent=0 timer=0 music=5 effect=3 palette=30 speed_mode=8 clear=1 detach=1");
 free(g);free(gfx);free(audio);return 0;
}
