/* Execute DosIO filesystem code on 68000; intercept only DiskIO sector calls. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "m68k.h"
static unsigned char mem[8*1024*1024];
unsigned int m68k_read_memory_8(unsigned int a){
 if(a>=sizeof(mem)){fprintf(stderr,"read outside memory\n");exit(2);}
 return mem[a];
}
unsigned int m68k_read_memory_16(unsigned int a){return m68k_read_memory_8(a)*256+m68k_read_memory_8(a+1);}
unsigned int m68k_read_memory_32(unsigned int a){return (m68k_read_memory_16(a)<<16)|m68k_read_memory_16(a+2);}
void m68k_write_memory_8(unsigned int a,unsigned int v){
 if(a>=sizeof(mem)){fprintf(stderr,"write outside memory\n");exit(2);}
 mem[a]=v;
}
void m68k_write_memory_16(unsigned int a,unsigned int v){m68k_write_memory_8(a,v>>8);m68k_write_memory_8(a+1,v);}
void m68k_write_memory_32(unsigned int a,unsigned int v){m68k_write_memory_16(a,v>>16);m68k_write_memory_16(a+2,v);}
static unsigned char *readfile(char *name,unsigned *size){FILE*f=fopen(name,"rb");if(!f){perror(name);exit(2);}fseek(f,0,SEEK_END);*size=ftell(f);rewind(f);unsigned char*b=malloc(*size);if(fread(b,1,*size,f)!=*size)exit(2);fclose(f);return b;}
static unsigned char disk[901120];
static unsigned reads,writes,formats;
static void call(unsigned function,const char *path,const unsigned char *data,unsigned size){
 memset(mem+0x100000,0,128);strcpy((char*)mem+0x100000,path);
 if(data)memcpy(mem+0x200000,data,size);
 m68k_set_reg(M68K_REG_PC,0x1000);m68k_set_reg(M68K_REG_SP,0x700000);
 m68k_set_reg(M68K_REG_D0,function);m68k_set_reg(M68K_REG_D1,size);
 m68k_set_reg(M68K_REG_A0,0x100000);m68k_set_reg(M68K_REG_A1,0x200000);m68k_set_reg(M68K_REG_A2,0x50000);
 m68k_write_memory_32(0x700000,0x800);
 unsigned long long cycles=0;
 while(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 && cycles<1000000000){
  if(m68k_get_reg(NULL,M68K_REG_PC)==0x1676){
   unsigned sector=m68k_get_reg(NULL,M68K_REG_D1)&65535,n=m68k_get_reg(NULL,M68K_REG_D2)&65535;
   unsigned op=m68k_get_reg(NULL,M68K_REG_D3)&255,buf=m68k_get_reg(NULL,M68K_REG_A0);
   if(sector+n>1760 || buf+(unsigned long long)n*512>sizeof(mem)){fprintf(stderr,"invalid disk request\n");exit(1);}
   if(n){
    if(op==0){memcpy(mem+buf,disk+sector*512,n*512);reads++;}
    else if(op==1){memcpy(disk+sector*512,mem+buf,n*512);writes++;}
    else if(op==2){memset(disk+sector*512,0,n*512);formats++;}
    else exit(1);
   }
   m68k_set_reg(M68K_REG_D0,0);
   unsigned sp=m68k_get_reg(NULL,M68K_REG_SP);m68k_set_reg(M68K_REG_PC,m68k_read_memory_32(sp));m68k_set_reg(M68K_REG_SP,sp+4);
  }else cycles+=m68k_execute(1);
 }
 if(m68k_get_reg(NULL,M68K_REG_PC)!=0x800 || m68k_get_reg(NULL,M68K_REG_D0)!=0){fprintf(stderr,"dosio failed %s function %u error %u\n",path,function,m68k_get_reg(NULL,M68K_REG_D0));exit(1);}
}
int main(int argc,char**argv){
 if(argc<5 || (argc-3)%2)return 2;
 unsigned code_len;unsigned char*code=readfile(argv[1],&code_len);memcpy(mem+0x1000,code,code_len);
 m68k_init();m68k_set_cpu_type(M68K_CPU_TYPE_68000);m68k_write_memory_32(0,0x700000);m68k_write_memory_32(4,0x1000);m68k_pulse_reset();
 call(4,"DF0:Hexagon",NULL,0);
 for(int i=3;i<argc;i+=2){
  unsigned size;unsigned char*data=readfile(argv[i+1],&size);
  call(1,argv[i],data,size);memset(mem+0x200000,0xa5,size+16);
  call(0,argv[i],NULL,0);
  if(m68k_get_reg(NULL,M68K_REG_D1)!=size || memcmp(mem+0x200000,data,size)){fprintf(stderr,"file readback mismatch\n");return 1;}
  for(unsigned j=0;j<16;j++)if(mem[0x200000+size+j]!=0xa5)return 1;
  printf("%s: %u bytes saved and loaded identically\n",argv[i],size);free(data);
 }
 FILE*f=fopen(argv[2],"wb");if(!f)return 2;fwrite(disk,1,sizeof(disk),f);fclose(f);
 printf("sector calls: %u reads, %u writes, %u formats\n",reads,writes,formats);return 0;
}
