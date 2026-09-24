/* Run the actual inflate core on a 68000; measure then replay overlapping. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "m68k.h"
static unsigned char mem[8*1024*1024];
static unsigned input,packed_len,output=0x100000,output_len,consumed,margin;
static int tracking;
static unsigned stack_low=0x700000;
unsigned int m68k_read_memory_8(unsigned int a){
 if(a>=sizeof(mem)){fprintf(stderr,"read outside memory\n");exit(2);}
 if(tracking && a>=input && a<input+packed_len && a-input+1>consumed) consumed=a-input+1;
 return mem[a];
}
unsigned int m68k_read_memory_16(unsigned int a){return m68k_read_memory_8(a)*256+m68k_read_memory_8(a+1);}
unsigned int m68k_read_memory_32(unsigned int a){return (m68k_read_memory_16(a)<<16)|m68k_read_memory_16(a+2);}
void m68k_write_memory_8(unsigned int a,unsigned int v){
 if(a>=sizeof(mem)){fprintf(stderr,"write outside memory\n");exit(2);}
 if(tracking && a>=output && a<output+output_len && a-output+1>consumed && a-output+1-consumed>margin)margin=a-output+1-consumed;
 if(a>=0x6f0000 && a<stack_low)stack_low=a;
 mem[a]=v;
}
void m68k_write_memory_16(unsigned int a,unsigned int v){m68k_write_memory_8(a,v>>8);m68k_write_memory_8(a+1,v);}
void m68k_write_memory_32(unsigned int a,unsigned int v){m68k_write_memory_16(a,v>>16);m68k_write_memory_16(a+2,v);}
static unsigned char *readfile(char *name,unsigned *size){FILE*f=fopen(name,"rb");if(!f){perror(name);exit(2);}fseek(f,0,SEEK_END);*size=ftell(f);rewind(f);unsigned char*b=malloc(*size);if(fread(b,1,*size,f)!=*size)exit(2);fclose(f);return b;}
int main(int argc,char**argv){
 if(argc!=4 && argc!=6)return 2;
 unsigned code_len;unsigned char*code=readfile(argv[1],&code_len),*packed=readfile(argv[2],&packed_len),*expected=readfile(argv[3],&output_len);
 uint64_t cycles[2]={0};unsigned arena=0;
 m68k_init();m68k_set_cpu_type(M68K_CPU_TYPE_68000);
 for(int pass=0;pass<2;pass++){
  memset(mem,0xa5,sizeof(mem));memcpy(mem+0x1000,code,code_len);
  input=pass?output+((margin+1)&~1u):0x300000;
  arena=output_len>input-output+packed_len?output_len:input-output+packed_len;
  memcpy(mem+input,packed,packed_len);
  tracking=0;m68k_write_memory_32(0,0x700000);m68k_write_memory_32(4,0x1000);m68k_pulse_reset();
  m68k_set_reg(M68K_REG_A4,output);m68k_set_reg(M68K_REG_A5,input);m68k_set_reg(M68K_REG_A6,0x601000);
  m68k_write_memory_32(0x700000,0x800);consumed=0;tracking=!pass;
  while(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 && cycles[pass]<500000000)cycles[pass]+=m68k_execute(1);
  tracking=0;
  if(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 || memcmp(mem+output,expected,output_len)){unsigned first=0;while(first<output_len && mem[output+first]==expected[first])first++;fprintf(stderr,"decode mismatch/pass %d pc=%x cycles=%llu first=%u got=%u expected=%u\n",pass,m68k_get_reg(NULL,M68K_REG_PC),(unsigned long long)cycles[pass],first,mem[output+first],first<output_len?expected[first]:0);return 1;}
  if(mem[output-1]!=0xa5 || mem[output+arena]!=0xa5 || mem[0x601000]!=0xa5 || mem[0x601000-2928-1]!=0xa5){fprintf(stderr,"guard damaged\n");return 1;}
 }
 if(argc==6){
  unsigned fib_code_len,final_len;
  unsigned char *fib_code=readfile(argv[4],&fib_code_len),*final=readfile(argv[5],&final_len);
  unsigned entries=m68k_read_memory_32(output+12),seq=m68k_read_memory_32(output+16);
  unsigned metadata=24+4*(entries+1)+4*seq;
  unsigned samples=m68k_read_memory_32(output+20);
  if(final_len!=metadata+samples || fib_code_len>0x10000)return 2;
  memcpy(mem+0x10000,fib_code,fib_code_len);
  /* Inflate result stays in place; no intermediate copy of the bank. */
  m68k_write_memory_32(0x700000,0x800);
  m68k_write_memory_32(0x700004,output+metadata);
  m68k_write_memory_32(0x700008,output_len-metadata);
  m68k_write_memory_32(0x70000c,samples);
  m68k_set_reg(M68K_REG_SP,0x700000);m68k_set_reg(M68K_REG_PC,0x10000);
  stack_low=0x700000;uint64_t expansion=0;
  while(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 && expansion<500000000)expansion+=m68k_execute(1);
  if(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 || m68k_get_reg(NULL,M68K_REG_D0)!=samples || memcmp(mem+output,final,final_len) || mem[output-1]!=0xa5 || mem[output+final_len]!=0xa5){fprintf(stderr,"combined expansion failed\n");return 1;}
  printf("{\"combined_matches\":true,\"arena_bytes\":%u,\"inflate_cycles\":%llu,\"expansion_cycles\":%llu,\"expansion_stack_bytes\":%u}\n",final_len,(unsigned long long)cycles[1],(unsigned long long)expansion,0x700000-stack_low);
  return 0;
 }
 printf("{\"output_bytes\":%u,\"packed_bytes\":%u,\"source_offset\":%u,\"arena_bytes\":%u,\"extra_tail_bytes\":%u,\"cycles\":%llu,\"overlap_matches\":true}\n",output_len,packed_len,input-output,arena,arena-output_len,(unsigned long long)cycles[1]);
 return 0;
}
