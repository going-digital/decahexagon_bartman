/* Actual 68000 execution of EXE1 validation/relocation, no hardware emulation. */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "m68k.h"
static unsigned char mem[0x200000];
unsigned int m68k_read_memory_8(unsigned int a){if(a>=sizeof(mem))exit(2);return mem[a];}
unsigned int m68k_read_memory_16(unsigned int a){return m68k_read_memory_8(a)*256+m68k_read_memory_8(a+1);}
unsigned int m68k_read_memory_32(unsigned int a){return m68k_read_memory_16(a)*65536+m68k_read_memory_16(a+2);}
void m68k_write_memory_8(unsigned int a,unsigned int v){if(a>=sizeof(mem))exit(2);mem[a]=v;}
void m68k_write_memory_16(unsigned int a,unsigned int v){m68k_write_memory_8(a,v>>8);m68k_write_memory_8(a+1,v);}
void m68k_write_memory_32(unsigned int a,unsigned int v){m68k_write_memory_16(a,v>>16);m68k_write_memory_16(a+2,v);}
static unsigned char *load(char *path,unsigned *n){FILE*f=fopen(path,"rb");if(!f)exit(2);fseek(f,0,SEEK_END);*n=ftell(f);rewind(f);unsigned char*p=malloc(*n);if(fread(p,1,*n,f)!=*n)exit(2);fclose(f);return p;}
int main(int argc,char **argv){
 if(argc!=5)return 2;
 unsigned code_n,raw_n,expected_n;
 unsigned char *code=load(argv[1],&code_n),*raw=load(argv[2],&raw_n),*expected=load(argv[3],&expected_n);
 unsigned entry=((unsigned)raw[16]<<24)|((unsigned)raw[17]<<16)|((unsigned)raw[18]<<8)|raw[19];
 unsigned base=strtoul(argv[4],0,0),capacity=raw_n>expected_n?raw_n:expected_n;
 if(code_n>0x10000 || base<0x20000 || base+capacity>=0x100000)return 2;
 uint64_t cycles[2]={0};
 m68k_init();m68k_set_cpu_type(M68K_CPU_TYPE_68000);
 for(unsigned bad=0;bad<2;bad++){
  memset(mem,0xa5,sizeof(mem));memcpy(mem+0x1000,code,code_n);memcpy(mem+base,raw,raw_n);
  if(bad)mem[base+raw_n-1]^=1;
  m68k_write_memory_32(0,0x1f0000);m68k_write_memory_32(4,0x1000);m68k_pulse_reset();
  m68k_write_memory_32(0x1f0000,0x800);
  m68k_write_memory_32(0x1f0004,base);m68k_write_memory_32(0x1f0008,raw_n);
  m68k_write_memory_32(0x1f000c,capacity);m68k_write_memory_32(0x1f0010,base);m68k_write_memory_32(0x1f0014,0x180000);
  while(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 && cycles[bad]<500000000)cycles[bad]+=m68k_execute(1);
  if(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 || !!m68k_get_reg(NULL,M68K_REG_D0)==bad)return 1;
  if(mem[base-1]!=0xa5 || mem[base+capacity]!=0xa5)return 1;
  if(bad){mem[base+raw_n-1]^=1;if(memcmp(mem+base,raw,raw_n) || m68k_read_memory_32(0x180000)!=0xa5a5a5a5)return 1;}
  else if(memcmp(mem+base,expected,expected_n) || m68k_read_memory_32(0x180000)!=base+entry || m68k_read_memory_32(0x180004)!=expected_n)return 1;
 }
 printf("{\"base\":%u,\"matches\":true,\"corrupt_rejected\":true,\"cycles\":%llu}\n",base,(unsigned long long)cycles[0]);return 0;
}
