/* Execute unmodified DosIO/DiskIO with controlled CIA/custom failure inputs.
 * Timer reads expire immediately: cycle counts are NOT real timeout durations. */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "m68k.h"
static unsigned char mem[0x200000];
static unsigned scenario,prb=255,dma_starts,timer_starts;
unsigned int m68k_read_memory_8(unsigned int a) {
 if(a<sizeof(mem))return mem[a];
 if(a==0xbfe001)return scenario==0 ? 0xeb : scenario==2 ? 0xff : 0xef;
 if(a==0xbfd100)return prb;
 if(a==0xbfde00)return 0; /* one-shot timer expired */
 if(a>=0xdff000 && a<0xe00000)return 0; /* DMA never completes */
 if(a>=0xbfd000 && a<=0xbfff01)return 0;
 fprintf(stderr,"unexpected read %x\n",a);exit(2);
}
unsigned int m68k_read_memory_16(unsigned int a){return (m68k_read_memory_8(a)<<8)|m68k_read_memory_8(a+1);}
unsigned int m68k_read_memory_32(unsigned int a){return (m68k_read_memory_16(a)<<16)|m68k_read_memory_16(a+2);}
void m68k_write_memory_8(unsigned int a,unsigned int v){
 if(a<sizeof(mem)){mem[a]=v;return;}
 if(a==0xbfd100)prb=v;
 if(a==0xbfd500)timer_starts++;
 if(a==0xdff024 && (v&0x80))dma_starts++;
 if((a>=0xbfd000 && a<=0xbfff01)||(a>=0xdff000 && a<0xe00000))return;
 fprintf(stderr,"unexpected write %x\n",a);exit(2);
}
void m68k_write_memory_16(unsigned int a,unsigned int v){m68k_write_memory_8(a,v>>8);m68k_write_memory_8(a+1,v);}
void m68k_write_memory_32(unsigned int a,unsigned int v){m68k_write_memory_16(a,v>>16);m68k_write_memory_16(a+2,v);}
int main(int argc,char **argv){
 if(argc!=2)return 2;
 FILE*f=fopen(argv[1],"rb");if(!f)return 2;
 unsigned char code[16384];unsigned n=fread(code,1,sizeof(code),f);fclose(f);
 m68k_init();m68k_set_cpu_type(M68K_CPU_TYPE_68000);
 for(scenario=0;scenario<3;scenario++) {
  memset(mem,0xa5,sizeof(mem));memcpy(mem+0x1000,code,n);
  strcpy((char*)mem+0x30000,"DF0:courtesy");prb=255;dma_starts=timer_starts=0;
  m68k_write_memory_32(0,0x1f0000);m68k_write_memory_32(4,0x1000);m68k_pulse_reset();
  m68k_write_memory_32(0x1f0000,0x800);
  m68k_set_reg(M68K_REG_D0,0);m68k_set_reg(M68K_REG_A0,0x30000);
  m68k_set_reg(M68K_REG_A1,0x40000);m68k_set_reg(M68K_REG_A2,0x60000);
  uint64_t cycles=0;
  while(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 && cycles<100000000)
   cycles+=m68k_execute(1);
  int result=(int32_t)m68k_get_reg(NULL,M68K_REG_D0);
  int expected=scenario==0?29:scenario==1?-1:30;
  if(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 || result!=expected){fprintf(stderr,"scenario %u: result %d pc %x\n",scenario,result,m68k_get_reg(NULL,M68K_REG_PC));return 1;}
  for(unsigned i=0x40000;i<0x50000;i++)if(mem[i]!=0xa5)return 1;
  if((prb&0x78)!=0x78)return 1; /* drive deselected by DosIO cleanup */
  printf("{\"scenario\":%u,\"error\":%d,\"cycles\":%llu,\"timer_starts\":%u,\"dma_starts\":%u,\"destination_unchanged\":true,\"drive_deselected\":true}\n",scenario,result,(unsigned long long)cycles,timer_starts,dma_starts);
 }
 return 0;
}
