#include "executable.h"
/* Four-bit CRC table keeps the resident validator small and avoids eight
 * conditional bit steps per package byte on a 68000. */
static const uint32_t crc_nibble[16]={
    0x00000000u,0x1db71064u,0x3b6e20c8u,0x26d930acu,
    0x76dc4190u,0x6b6b51f4u,0x4db26158u,0x5005713cu,
    0xedb88320u,0xf00f9344u,0xd6d6a3e8u,0xcb61b38cu,
    0x9b64c2b0u,0x86d3d2d4u,0xa00ae278u,0xbdbdf21cu
};
static uint32_t read32(const unsigned char *p) {
    return (uint32_t)p[0]<<24|(uint32_t)p[1]<<16|(uint32_t)p[2]<<8|p[3];
}
static void write32(unsigned char *p,uint32_t n) {
    p[0]=n>>24;p[1]=n>>16;p[2]=n>>8;p[3]=n;
}
int track_executable_prepare(unsigned char *p,uint32_t bytes,
    uint32_t capacity,uint32_t base,TrackExecutable *info) {
    if(!p || !info || bytes<32 || bytes>capacity || (base&1))return 0;
    if(read32(p)!=0x45584531 || read32(p+4)!=32 || read32(p+28))return 0;
    uint32_t image=read32(p+8),memory=read32(p+12),entry=read32(p+16),count=read32(p+20);
    if(!image || image>memory || memory>capacity || memory>UINT32_MAX-base ||
       entry>=image || (entry&1) || count>(bytes-32)/4)return 0;
    uint32_t start=32+4*count;
    if(image!=bytes-start)return 0;
    uint32_t crc=~0u;
    for(uint32_t i=0;i<bytes;i++) {
        crc^=(i>=24 && i<28)?0:p[i];
        crc=(crc>>4)^crc_nibble[crc&15];
        crc=(crc>>4)^crc_nibble[crc&15];
    }
    if((crc^~0u)!=read32(p+24))return 0;
    uint32_t previous=0;
    for(uint32_t i=0;i<count;i++) {
        uint32_t offset=read32(p+32+4*i);
        if((offset&1) || image<4 || offset>image-4 ||
           (i && offset<previous+4) || read32(p+start+offset)>memory)return 0;
        previous=offset;
    }
    for(uint32_t i=0;i<count;i++) {
        uint32_t offset=read32(p+32+4*i);
        write32(p+start+offset,read32(p+start+offset)+base);
    }
    for(uint32_t i=0;i<image;i++)p[i]=p[start+i];
    for(uint32_t i=image;i<memory;i++)p[i]=0;
    info->entry=base+entry;info->memory_bytes=memory;
    return 1;
}
