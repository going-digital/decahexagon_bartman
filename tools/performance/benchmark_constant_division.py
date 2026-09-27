#!/usr/bin/env python3
"""Compare old/new production arithmetic in Musashi 68000 instruction cycles."""
import argparse
import json
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--execram-source', type=Path, required=True)
p.add_argument('--baseline', default='HEAD')
a = p.parse_args()
out = ROOT / 'scratchpad/constant_division_benchmark'
out.mkdir(exist_ok=True)
v = a.execram_source / 'src/musashi_vendor'
sdk = next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
host = (ROOT/'tools/ending_projection_bench.c').read_text()
(out/'host.c').write_text(host)
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(ROOT/'scratchpad/trackloader'),str(out/'host.c'),str(v/'m68kcpu.c'),str(ROOT/'scratchpad/trackloader/m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(out/'probe')], check=True)
(out/'entry.s').write_text('.text\n.global _start\n_start: bra.w bench\n')
rows = []
for variant in ('before', 'after'):
    for name in ('pc_core.c','pc_pulse.c'):
        data = subprocess.check_output(['git','show',a.baseline+':'+name],cwd=ROOT) if variant=='before' else (ROOT/name).read_bytes()
        (out/name).write_bytes(data)
    for kind in ('angle','pulse_two','pulse_three'):
        expr = 'pc_render_angle(i)' if kind=='angle' else 'pc_pulse_tick(0,(int)i-120,'+('2' if kind=='pulse_three' else '0')+')'
        n = 360 if kind=='angle' else 241
        (out/'bench.c').write_text(f'''#include "pc_core.h"
#include "pc_pulse.h"
unsigned bench(unsigned count) {{
 for(unsigned k=0;k<count;++k)
  for(unsigned i=0;i<{n};++i) {{
   unsigned actual={expr};
   /* Expected values are tabled offline so reference division isn't timed. */
   extern const unsigned short expected[];
   if(actual!=expected[i])return 0xffffffffu;
  }}
 return count;
}}
''')
        vals = [i*65536//360 for i in range(n)] if kind=='angle' else [max(0,abs(i-120)//(3 if kind=='pulse_three' else 2)-1) for i in range(n)]
        (out/'expected.c').write_text('const unsigned short expected[]={'+','.join(map(str,vals))+'};\n')
        subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-O2','-ffreestanding','-fno-builtin','-fomit-frame-pointer','-nostdlib','-I'+str(ROOT),'-Wa,--register-prefix-optional','-Wl,-Ttext=0x1000',str(out/'entry.s'),str(out/'bench.c'),str(out/'expected.c'),str(out/'pc_core.c'),str(out/'pc_pulse.c'),str(ROOT/'support/gcc8_a_support.s'),'-o',str(out/'bench.elf')],check=True)
        subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(out/'bench.elf'),str(out/'bench.bin')],check=True)
        def run(count):
            return json.loads(subprocess.check_output([str(out/'probe'),str(out/'bench.bin'),str(count)],text=True))['cycles']
        cycles=run(10)-run(0)
        rows.append(dict(variant=variant,kind=kind,calls=n*10,cycles=cycles,cycles_per_call=cycles/(n*10)))
report=dict(scope='Musashi 68000 instruction cycles, no DMA/interrupts. Production functions at -O2; loop, call and result-check overhead included; identical inputs and expected outputs.',baseline=a.baseline,trials=rows)
(ROOT/'docs/CONSTANT_DIVISION_BENCHMARK.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
