#!/usr/bin/env python3
"""Compare signed division and reciprocal wall-span preparation on 68000."""
import argparse,json,struct,subprocess,hashlib
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__);p.add_argument('--execram-source',type=Path,required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/performance_spanmath';out.mkdir(exist_ok=True)
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'));binutils=sdk.parent/'m68k-amiga-elf/bin'
elf=root/'scratchpad/trackloader/native_game/game.elf'
fixtures=json.loads((root/'docs/WALL_PIPELINE_BENCHMARK.json').read_text())['fixtures']
# Persist the exact workload: snapshot reconstruction is not a captured edge stream.
(out/'fixtures.json').write_text(json.dumps(fixtures,indent=2))
host=(root/'tools/ending_projection_bench.c').read_text().replace(' printf("{', ' FILE *dump=fopen("scratchpad/performance_spanmath/output.bin","wb");fwrite(mem+0x100000,1,65536,dump);fclose(dump);\n printf("{')
(out/'host.c').write_text(host);v=a.execram_source/'src/musashi_vendor'
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(root/'scratchpad/trackloader'),str(out/'host.c'),str(v/'m68kcpu.c'),str(root/'scratchpad/trackloader/m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(out/'probe')],check=True)
(out/'entry.s').write_text('.text\n.global _start\n_start: bra.w bench\n')
projection=(root/'pc_projection.c').read_text()
rows=[];reference={}
for kind in ('spans',):
 for variant in ('baseline','candidate'):
  (out/'projection.c').write_text((out/'before_projection.c').read_text() if variant=='baseline' else projection)
  for f in fixtures:
   walls=','.join('{'+','.join(map(str,w))+'}' for w in f['walls']);lines=','.join('{'+','.join(map(str,l))+'}' for l in f['lines'])
   header='#include "pc_projection.h"\n#include "render_clip.h"\n#include "blitter.h"\nstruct Custom *custom=(struct Custom*)0x180000;\n'
   header+='static PcWorld world={.walls={'+walls+'},.count='+str(len(f['walls']))+'};\nstatic const short lines[][4]={'+lines+'};\n'
   if kind=='spans':body=f'''PcSpan *s=(PcSpan*)0x100002;*(volatile unsigned short*)0x100000=pc_project_spans(&world,{f['sides']},s);'''
   (out/'bench.c').write_text(header+'unsigned bench(unsigned count){for(unsigned n=0;n<count;++n){'+body+'}return count;}\n')
   cmd=[str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-O2','-DBUILD_DEBUG=0','-DBENCH_VERIFY=1','-ffreestanding','-fno-builtin','-fomit-frame-pointer','-ffunction-sections','-fdata-sections','-nostdlib','-I'+str(root),'-Wa,--register-prefix-optional','-Wl,--gc-sections,-Ttext=0x1000',str(out/'entry.s'),str(out/'bench.c'),str(out/'projection.c'),str(root/'support/gcc8_a_support.s'),'-o',str(out/'bench.elf')]
   subprocess.run(cmd,check=True)
   subprocess.run([str(binutils/'objcopy'),'-O','binary',str(out/'bench.elf'),str(out/'bench.bin')],check=True)
   def run(n):return json.loads(subprocess.check_output([str(out/'probe'),str(out/'bench.bin'),str(n)],cwd=root,text=True))['cycles']
   zero=run(0);cycles=run(10)-zero;data=(out/'output.bin').read_bytes()
   # Span slots after the merged count are scratch space, not API output.
   if kind=='spans':data=data[:2+6*int.from_bytes(data[:2],'big')]
   key=(kind,f['level'])
   if variant=='baseline':reference[key]=data
   else:assert data==reference[key],key
   row=dict(kind=kind,variant=variant,level=f['level'],wall_records=len(f['walls']),line_requests=len(f['lines']),cycles_per_batch=cycles/10,output_sha256=hashlib.sha256(data).hexdigest());rows.append(row);print(row,flush=True)
report=dict(fixtures=fixtures,source_sha256={n:hashlib.sha256((root/n).read_bytes()).hexdigest() for n in ('pc_projection.c','render_clip.c','blitter.c')},scope='Musashi 68000 -O2 instruction cycles, 10 repetitions less empty loop setup; captured world records with reconstructed wall edges (not recorded frame edge stream). No DMA, raster contention or interrupts. Reciprocal divide-by-five candidate only. Output equality limited to these workloads.',trials=rows)
(root/'docs/SPAN_MATH_BENCHMARK.json').write_text(json.dumps(report,indent=2)+'\n')
