#!/usr/bin/env python3
"""Exhaustive host accuracy and instruction-cycle checks of the PAL/NTSC clock."""
import json, subprocess, hashlib
from pathlib import Path
import argparse
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--execram-source',type=Path,required=True)
a=p.parse_args()
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/performance_clock';out.mkdir(exist_ok=True)
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
(out/'accuracy.c').write_text('''#include "pc_core.h"
#include <stdio.h>
int main(void) {
 unsigned cases=0,errors=0;
 for(unsigned hz=50;hz<=60;hz+=10)
  for(unsigned r=0;r<60;++r)
   for(unsigned f=0;f<65536;++f) {
    PcClock c={r};unsigned e=f*60+r;
    unsigned q=pc_clock_advance(&c,f,hz);
    ++cases;if(q!=e/hz || c.remainder!=e%hz)++errors;
   }
 printf("{\\"cases\\":%u,\\"mismatches\\":%u}\\n",cases,errors);
 return errors!=0;
}
''')
subprocess.run(['cc','-O2','-I'+str(root),str(out/'accuracy.c'),str(root/'pc_core.c'),'-o',str(out/'accuracy')],check=True)
accuracy=json.loads(subprocess.check_output([str(out/'accuracy')],text=True))
v=a.execram_source/'src/musashi_vendor'
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(root/'scratchpad/trackloader'),str(root/'tools/ending_projection_bench.c'),str(v/'m68kcpu.c'),str(root/'scratchpad/trackloader/m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(out/'probe')],check=True)
(out/'entry.s').write_text('.text\n.global _start\n_start: bra.w bench\n')
rows=[]
for variant in ('before','after'):
 source=subprocess.check_output(['git','show','cbf1e4b:pc_core.c'],cwd=root) if variant=='before' else (root/'pc_core.c').read_bytes()
 (out/'core.c').write_bytes(source)
 for hz in (50,60):
  # Include ordinary one-frame updates and large missed-frame counts; table
  # reference calculations are offline, so no reference divisions are timed.
  for label,frames in [('single_frame',[1]),('boundary',[0,2,5,255,32767,65535])]:
   cases=[(f,r,(f*60+r)//hz,(f*60+r)%hz) for f in frames for r in range(60)]
   table=','.join('{'+','.join(map(str,c))+'}' for c in cases)
   (out/'bench.c').write_text('''#include "pc_core.h"
static const unsigned cases[][4]={'''+table+'''};
unsigned bench(unsigned count) {
 for(unsigned n=0;n<count;++n)
  for(unsigned i=0;i<sizeof(cases)/sizeof(cases[0]);++i) {
   PcClock c={cases[i][1]};
   unsigned q=pc_clock_advance(&c,cases[i][0],'''+str(hz)+''');
   if(q!=cases[i][2] || c.remainder!=cases[i][3])return 0xffffffffu;
  }
 return count;
}
''')
   subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-O2','-ffreestanding','-fno-builtin','-fomit-frame-pointer','-nostdlib','-I'+str(root),'-Wa,--register-prefix-optional','-Wl,-Ttext=0x1000',str(out/'entry.s'),str(out/'bench.c'),str(out/'core.c'),str(root/'support/gcc8_a_support.s'),'-o',str(out/'bench.elf')],check=True)
   subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(out/'bench.elf'),str(out/'bench.bin')],check=True)
   def run(n):return json.loads(subprocess.check_output([str(out/'probe'),str(out/'bench.bin'),str(n)],text=True))['cycles']
   cycles=run(10)-run(0)
   rows.append(dict(variant=variant,hz=hz,inputs=label,calls=len(cases)*10,cycles_per_call=cycles/(len(cases)*10)))
report=dict(accuracy=accuracy,current_pc_core_sha256=hashlib.sha256((root/'pc_core.c').read_bytes()).hexdigest(),scope='Musashi 68000 -O2, loop/call/table-check overhead included, no DMA/IRQs; original clock from cbf1e4b versus current source',trials=rows)
(root/'docs/CLOCK_BENCHMARK.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
