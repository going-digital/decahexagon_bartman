/* Execute the real write path, bypassing ONLY the preceding MFM read routine.
 * No floppy encoding or physical write timing is modeled. */
#define main seek_probe_main
#include "disk_change_probe.c"
#undef main
#include <assert.h>
static uint32_t be32(const unsigned char *p) {
 return ((uint32_t)p[0]<<24)|((uint32_t)p[1]<<16)|((uint32_t)p[2]<<8)|p[3];
}
int main(int argc,char **argv) {
 assert(argc==2);FILE *f=fopen(argv[1],"rb");assert(f);
 unsigned char code[16384];unsigned n=fread(code,1,sizeof code,f);fclose(f);assert(n>20);
 uint32_t read_entry=0x1000+be32(code+n-20),write_entry=0x1000+be32(code+n-16);
 uint32_t pre_dma=0x1000+be32(code+n-12),enable=0x1000+be32(code+n-8);
 uint32_t drive_state=0x1000+be32(code+n-4);
 m68k_init();m68k_set_cpu_type(M68K_CPU_TYPE_68000);
 for(unsigned guarded=0;guarded<2;guarded++)for(unsigned fault=0;fault<7;fault++) {
  memset(mem,0xa5,sizeof mem);memcpy(mem+0x1000,code,n);
  prb=255;dma_starts=timer_starts=steps=changed=dma_complete=dma_active=0;scenario=0;
  mem[enable]=guarded;m68k_write_memory_16(drive_state,158);
  m68k_write_memory_32(0,0x1f0000);m68k_write_memory_32(4,0x1000);m68k_pulse_reset();
  m68k_write_memory_32(0x1f0000,0x800);
  m68k_set_reg(M68K_REG_D0,0);m68k_set_reg(M68K_REG_D1,1738);
  m68k_set_reg(M68K_REG_D2,1);m68k_set_reg(M68K_REG_D3,0x8001);
  m68k_set_reg(M68K_REG_A0,0x40000);m68k_set_reg(M68K_REG_A1,0x60000);
  uint64_t cycles=0;unsigned skipped=0,injected=0;
  while(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 && cycles<100000000) {
   uint32_t pc=m68k_get_reg(NULL,M68K_REG_PC);
   if(pc==read_entry) {
    /* Supply a successful pre-read, retaining the driver's real stack frame. */
    uint32_t sp=m68k_get_reg(NULL,M68K_REG_SP);
    m68k_set_reg(M68K_REG_PC,m68k_read_memory_32(sp));m68k_set_reg(M68K_REG_SP,sp+4);
    m68k_set_reg(M68K_REG_D0,0);
    m68k_set_reg(M68K_REG_SR,(m68k_get_reg(NULL,M68K_REG_SR)&~15u)|4);skipped++;continue;
   }
   if(!injected && ((fault==1 && pc==write_entry) || (fault==2 && pc==pre_dma) ||
                    (fault==3 && dma_starts==2 && pc==write_entry) ||
                    (fault>=4 && dma_starts==2))) {
    changed=fault!=6;dma_complete=fault>=5;injected=1;
   }
   cycles+=m68k_execute(1);
  }
  int result=(int32_t)m68k_get_reg(NULL,M68K_REG_D0);
  unsigned expected_dma=guarded && fault ? (fault>=3?2:0):(fault>=5?2:6);
  assert(m68k_get_reg(NULL,M68K_REG_PC)==0x800 && skipped==1);
  assert(result==(guarded && fault && fault!=6?29:(fault>=5?0:-1)));assert(dma_starts==expected_dma);
  assert(!dma_active);assert(injected==(fault!=0));assert((prb&0x78)==0x78);
  assert(m68k_get_reg(NULL,M68K_REG_SP)==0x1f0004);
  for(unsigned i=0x40000;i<0x50000;i++)assert(mem[i]==0xa5);
  printf("{\"guarded\":%u,\"fault\":%u,\"error\":%d,\"dma_arm_writes\":%u,\"stack_restored\":true,\"drive_deselected\":true}\n",guarded,fault,result,dma_starts);
 }
 return 0;
}
