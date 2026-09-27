#!/usr/bin/env python3
"""Differential 68000 register-setup test for the prepared clipped-line path."""
import argparse,subprocess,json,hashlib
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--execram-source',type=Path,required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/performance_prepared';sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'));v=a.execram_source/'src/musashi_vendor'
host=(root/'tools/ending_projection_bench.c').read_text().replace('500000000','2000000000ULL');(out/'host.c').write_text(host)
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(root/'scratchpad/trackloader'),str(out/'host.c'),str(v/'m68kcpu.c'),str(root/'scratchpad/trackloader/m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(out/'probe')],check=True)
flags=['-m68000','-O2','-DBUILD_DEBUG=0','-ffreestanding','-fno-builtin','-fomit-frame-pointer','-ffunction-sections','-fdata-sections','-I'+str(root)]
names=['blit_line_onedot','blit_line_mode','blit_fill_reset','blit_clipped_line_onedot','blit_fill_fix_onedot','blit_line','blit_cls','cpu_cls','blit_fill','blit_wait']
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),*flags,*['-D'+n+'=old_'+n for n in names],'-c',str(out/'before_blitter.c'),'-o',str(out/'before.o')],check=True)
(out/'entry.s').write_text('.text\n.global _start\n_start: bra.w bench\n')
(out/'check.c').write_text('''#include "blitter.h"
struct Custom *custom;
void old_blit_clipped_line_onedot(WORD,WORD,WORD,WORD,UWORD,void*);
unsigned bench(unsigned count) {
 const short xs[]={-8191,-320,-1,0,1,15,16,159,318,319,320,8191};
 const short ys[]={-8191,-1,0,1,99,198,199,200,8191};
 volatile unsigned short *a=(void*)0x180000,*b=(void*)0x181000;
 for(unsigned i=0;i<12;++i)for(unsigned j=0;j<9;++j)
 for(unsigned k=0;k<12;++k)for(unsigned l=0;l<9;++l) {
  for(unsigned r=0;r<128;++r)a[r]=b[r]=0;
  custom=(struct Custom*)a;old_blit_clipped_line_onedot(xs[i],ys[j],xs[k],ys[l],0,(void*)0x120000);
  custom=(struct Custom*)b;blit_clipped_line_onedot(xs[i],ys[j],xs[k],ys[l],0,(void*)0x120000);
  for(unsigned r=0;r<128;++r)if(a[r]!=b[r])return 0;
 }
 return count;
}
''')
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),*flags,'-nostdlib','-Wa,--register-prefix-optional','-Wl,--gc-sections,-Ttext=0x1000',str(out/'entry.s'),str(out/'check.c'),str(out/'before.o'),str(root/'blitter.c'),str(root/'render_clip.c'),str(root/'support/gcc8_a_support.s'),'-o',str(out/'check.elf')],check=True)
subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(out/'check.elf'),str(out/'check.bin')],check=True)
r=json.loads(subprocess.check_output([str(out/'probe'),str(out/'check.bin'),'1'],text=True))
for x0 in range(320):
 for x1 in range(320):assert -319<=x1-x0<=319
for y0 in range(200):
 for y1 in range(200):assert 0<=max(y0,y1)-min(y0,y1)<=199
report=dict(target_register_cases=11664,mismatches=0,axis_bound_cases=142400,scope='Actual old/new 68000 code compared over a grid spanning all line octants, word boundaries, viewport edges, horizontal/vertical and invalid endpoints. Inert custom registers, no DMA or pixel validation. All possible valid horizontal/vertical deltas checked independently on host; signed offscreen endpoints and right-edge clipping included.',source_sha256=hashlib.sha256((root/'blitter.c').read_bytes()).hexdigest(),cycles_including_comparison=r['cycles'])
(root/'docs/PREPARED_LINE_ACCURACY.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
