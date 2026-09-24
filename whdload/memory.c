/* Freestanding PIC helpers used by compiler-generated structure copies. */
void *memcpy(void *dst,const void *src,unsigned long size) {
    unsigned char *d=dst;const unsigned char *s=src;
    while(size--)*d++=*s++;
    return dst;
}
void *memset(void *dst,int value,unsigned long size) {
    unsigned char *d=dst;
    while(size--)*d++=(unsigned char)value;
    return dst;
}
