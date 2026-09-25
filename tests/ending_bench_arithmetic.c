/* Benchmark-only portable helpers. Not an optimized production runtime.
 * Shift/add multiplication and restoring division make the baseline runnable
 * with the freestanding SDK, which does not supply libgcc. */
typedef unsigned long long U64;
long long __muldi3(long long a,long long b) {
    U64 x=(U64)a,y=(U64)b,r=0;
    while(y){if(y&1)r+=x;x<<=1;y>>=1;}
    return (long long)r;
}
long long __divdi3(long long a,long long b) {
    U64 x=a<0 ? -(U64)a:(U64)a,y=b<0 ? -(U64)b:(U64)b;
    U64 q=0,r=0;
    for(unsigned i=0;i<64;i++) {
        unsigned carry=(unsigned)(r>>63);
        r=(r<<1)|(x>>63);x<<=1;q<<=1;
        if(carry || r>=y){r-=y;q|=1;}
    }
    return (a<0)!=(b<0) ? (long long)(-q):(long long)q;
}
