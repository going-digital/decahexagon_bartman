#define main original_main
#include "probe_pc_progression.c"
#undef main
static float dt(void){return 1.0f;}
int main(int argc,char **argv){
 assert(argc==3);load(argv[1]);install_hooks();patch(0x100061530+slide,dt);
 memcpy(original_generate,(void*)(0x10000d0d0+slide),12);patch(0x10000d0d0+slide,capture_generate);
 unsigned char *s=malloc(0x50000);init_state(s);seed=atoi(argv[2]);draws=0;
 wr32(s,0x2990,720);wr32(s,0x298c,720);wrf(s,0x296c,0);wr32(s,0x2970,0);wrf(s,0x21c,5);wrf(s,0x2980,5);
 for(int tick=1;tick<=3600;++tick){nchosen=0;wrf(s,0x2934,tick-1);((void(*)(void*))(0x100056f50+slide))(s);
 printf("%d %u %u %.0f %u %.0f %d",tick,rd32(s,0x2970),rd32(s,0x1a4),rdf(s,0x54c0),rd32(s,0x210),rdf(s,0x2978),draws);
 for(int i=0;i<nchosen;++i)printf(" %d",chosen[i]);uint32_t hash=2166136261u;unsigned active=0;
 for(unsigned j=0;j<rd32(s,0x2930);++j){unsigned char *w=s+0x220+j*20;if(!w[16])continue;++active;
 for(unsigned k=0;k<3;++k)hash=(hash^rd32(w,4*k))*16777619u;}
 printf(" | %u %u\n",active,hash);
 }
}
