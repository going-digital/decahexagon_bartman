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
 if(argc!=3)return 2;
 unsigned n;unsigned char *code=load(argv[1],&n);
 unsigned count=strtoul(argv[2],0,0);if(n>0x10000)return 2;
 memcpy(mem+0x1000,code,n);free(code);
 m68k_init();m68k_set_cpu_type(M68K_CPU_TYPE_68000);
 m68k_write_memory_32(0,0x1f0000);m68k_write_memory_32(4,0x1000);m68k_pulse_reset();
 m68k_write_memory_32(0x1f0000,0x800);m68k_write_memory_32(0x1f0004,count);
 uint64_t cycles=0;
 while(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 && cycles<500000000)cycles+=m68k_execute(1);
 if(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 || m68k_get_reg(NULL,M68K_REG_D0)!=count)return 1;
 printf("{\"vertices\":%u,\"cycles\":%llu}\n",count,(unsigned long long)cycles);return 0;
}
