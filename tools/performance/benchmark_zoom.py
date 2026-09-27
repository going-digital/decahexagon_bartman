#!/usr/bin/env python3
"""Run extracted radius scaling on a 68000, checking every radius bit pattern."""
from pathlib import Path
import subprocess,json
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/performance_zoom'
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
probe=root/'scratchpad/performance_cues/probe'
assert probe.exists(), 'Build the Musashi probe with benchmark_cues.py first'
(out/'entry.s').write_text('.text\n.global _start\n_start: bra.w bench\n')
header='#include "support/gcc8_c_support.h"\ntypedef short WORD;typedef unsigned short UWORD;static UWORD zoom;\n'
def extract(p):
 s=p.read_text();a=s.index('static WORD zscale(');b=s.index('\n}',a)+2;return s[a:b]
def run(code,n):
 (out/'bench.c').write_text(header+code)
 subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-O2','-ffreestanding','-fno-builtin','-fomit-frame-pointer','-nostdlib','-I'+str(root),'-Wa,--register-prefix-optional','-Wl,-Ttext=0x1000',str(out/'entry.s'),str(out/'bench.c'),str(root/'support/gcc8_a_support.s'),'-o',str(out/'bench.elf')],check=True)
 subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(out/'bench.elf'),str(out/'bench.bin')],check=True)
 return json.loads(subprocess.check_output([str(probe),str(out/'bench.bin'),str(n)],text=True))['cycles']
before=extract(out/'before_render.c');after=extract(root/'render.c')
# Keep the routines distinct for the differential test; benchmark separately
# with inlining allowed, as in the production render translation unit.
code=before.replace('static WORD zscale','static __attribute__((noinline)) WORD oldscale')+after.replace('static WORD zscale','static __attribute__((noinline)) WORD newscale')
code+='''unsigned bench(unsigned count){
 const unsigned short levels[]={0,64,127,128,129,256,320,512,65535};
 for(unsigned z=0;z<9;++z){zoom=levels[z];for(unsigned r=0;r<65536;++r)
 if(oldscale((WORD)r)!=newscale((WORD)r))return 0xffffffffu;}
 return count;}
'''
accuracy=run(code,1);trials=[]
for name,fn in [('before',before),('after',after)]:
 for z in (128,127,320):
  # Separate translation-unit style runtime zoom: volatile source prevents folding.
  code=fn+f'volatile UWORD input_zoom={z};volatile WORD result;\n'
  code+='unsigned bench(unsigned count){zoom=input_zoom;for(unsigned n=0;n<count;++n)for(unsigned r=0;r<1024;++r)result=zscale((WORD)r);return count;}'
  overhead=run(code,0)
  cycles=json.loads(subprocess.check_output([str(probe),str(out/'bench.bin'),'10'],text=True))['cycles']-overhead
  trials.append(dict(variant=name,zoom=z,calls=10240,cycles=cycles,cycles_per_call=cycles/10240))
r=dict(scope='Extracted production zscale functions, actual Musashi 68000 -O2 execution, no DMA/IRQs. Every radius bit pattern at nine zooms including fallback and extrema. Microbenchmark includes identical loop/store overhead; full rendering profiles are separate. Compiler may hoist invariant zoom checks, so timings are not isolated call latency.',accuracy_cases=9*65536,accuracy_cycles=accuracy,trials=trials)
(root/'docs/ZOOM_BENCHMARK.json').write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(r,indent=2))
