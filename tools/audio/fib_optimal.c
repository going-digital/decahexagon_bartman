/* Offline exhaustive-state encoder. Playback still uses the existing FIB1
 * decoder. Minimize squared sample error over each predictor-reset block. */
#include <stdint.h>
#include <limits.h>
static const int delta[16]={-34,-21,-13,-8,-5,-3,-2,-1,0,1,2,3,5,8,13,21};
int fib_optimal(const int8_t *samples, int n, uint8_t *codes) {
    if (n<1 || n>512) return -1;
    int32_t cost[256], next[256];
    uint8_t back[512][256];
    for(int s=0;s<256;s++) cost[s]=1000000000;
    cost[(int)samples[0]+128]=0;
    for(int t=1;t<n;t++) {
        for(int s=0;s<256;s++) {
            int32_t best=1000000000; int code=0;
            for(int c=0;c<16;c++) {
                int p=(s-delta[c])&255;
                if(cost[p]<best) {best=cost[p];code=c;}
            }
            int error=s-128-(int)samples[t];
            next[s]=best+error*error;back[t][s]=(uint8_t)code;
        }
        for(int s=0;s<256;s++) cost[s]=next[s];
    }
    int s=0;
    for(int p=1;p<256;p++) if(cost[p]<cost[s]) s=p;
    int result=cost[s];
    for(int t=n-1;t>0;t--) {int c=back[t][s];codes[t-1]=(uint8_t)c;s=(s-delta[c])&255;}
    return result;
}
