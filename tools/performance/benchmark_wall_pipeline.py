#!/usr/bin/env python3
"""Compare scratch-only renderer candidates with production 68000 routines."""
import argparse,json,struct,subprocess,hashlib
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__);p.add_argument('--execram-source',type=Path,required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/wall_pipeline_bench';out.mkdir(exist_ok=True)
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'));binutils=sdk.parent/'m68k-amiga-elf/bin'
elf=root/'scratchpad/trackloader/native_game/game.elf'
sy={x.split()[2]:int(x.split()[0],16) for x in subprocess.check_output([str(binutils/'nm'),'-n',str(elf)],text=True).splitlines() if len(x.split())==3}
fixtures=[]
for level in ('hexagon','hexagonest'):
 folder=root/'scratchpad/performance_noassist';d=json.loads((folder/(level+'.json')).read_text());assert hashlib.sha256(elf.read_bytes()).hexdigest()==d['elf_sha256']
 ram=(folder/(level+'_profile/chip-ram.bin')).read_bytes();base=d['runtime_base'];wa=base+sy['game_world'];count=struct.unpack_from('>H',ram,wa+5000)[0]
 walls=[struct.unpack_from('>iiBB',ram,wa+10*i) for i in range(count)]
 sides=ram[base+sy['morph']];scene=struct.unpack_from('>5H',ram,base+sy['scene']);zoom=max(64,scene[3]);pulse=scene[4]
 sins=struct.unpack_from('>6h',ram,base+sy['frame_sin']);coss=struct.unpack_from('>6h',ram,base+sy['frame_cos'])
 spans=[]
 for dist,width,slot,active in walls:
  if active and slot<sides and width>=5:spans.append((slot,40+int(dist/5),40+int(dist/5)+int(width/5)))
 merged=[]
 for slot,inner,outer in sorted(spans):
  if merged and merged[-1][0]==slot and merged[-1][2]>=inner:merged[-1]=(slot,merged[-1][1],max(outer,merged[-1][2]))
  else:merged.append((slot,inner,outer))
 def pt(slot,r):
  r=(((r+pulse)&65535)*zoom>>8)&65535;r=r if r<32768 else r-65536
  return (160+((coss[slot]*r)>>14),100+((sins[slot]*r)>>14))
 lines=[]
 for slot,i,o in merged:
  n=(slot+1)%sides;p0,p1,p2,p3=pt(slot,i),pt(n,i),pt(n,o),pt(slot,o)
  if all(v[1]<0 for v in (p0,p1,p2,p3)) or all(v[1]>199 for v in (p0,p1,p2,p3)) or all(v[0]<0 for v in (p0,p1,p2,p3)):continue
  lines.extend([p0+p1,p3+p2])
  if ((slot-1)%sides,i,o) not in merged:lines.append(p3+p0)
  if (n,i,o) not in merged:lines.append(p2+p1)
 fixtures.append(dict(level=level,walls=walls,sides=sides,lines=lines))
# Persist the exact workload: snapshot reconstruction is not a captured edge stream.
(out/'fixtures.json').write_text(json.dumps(fixtures,indent=2))
host=(root/'tools/ending_projection_bench.c').read_text().replace(' printf("{', ' FILE *dump=fopen("scratchpad/wall_pipeline_bench/output.bin","wb");fwrite(mem+0x100000,1,65536,dump);fclose(dump);\n printf("{')
(out/'host.c').write_text(host);v=a.execram_source/'src/musashi_vendor'
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(root/'scratchpad/trackloader'),str(out/'host.c'),str(v/'m68kcpu.c'),str(root/'scratchpad/trackloader/m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(out/'probe')],check=True)
(out/'entry.s').write_text('.text\n.global _start\n_start: bra.w bench\n')
projection=(root/'pc_projection.c').read_text()
start=projection.index('        uint16_t j=count;');end=projection.index('    uint16_t merged=0;',start)
shell=projection[:start]+'''        spans[count++]=p;
    }
    for(uint16_t gap=count/2;gap;gap/=2)
        for(uint16_t i=gap;i<count;++i) {
            PcSpan p=spans[i];uint16_t j=i;
            while(j>=gap && (spans[j-gap].slot>p.slot ||
                  (spans[j-gap].slot==p.slot && spans[j-gap].inner>p.inner))) {
                spans[j]=spans[j-gap];j-=gap;
            }
            spans[j]=p;
        }
'''+projection[end:]
sector=projection.replace('    for (uint16_t i=0;i<world->count;++i) {', '    for(uint8_t sector=0;sector<sides;++sector) {\n    uint16_t first=count;\n    for (uint16_t i=0;i<world->count;++i) {',1).replace('if (!w->active || w->slot>=sides || w->width<5)', 'if (!w->active || w->slot!=sector || w->width<5)',1)
sector=sector.replace('while (j && (spans[j-1].slot>p.slot ||\n               (spans[j-1].slot==p.slot && spans[j-1].inner>p.inner)))', 'while (j>first && spans[j-1].inner>p.inner)',1).replace('    uint16_t merged=0;', '    }\n    uint16_t merged=0;',1)
clip=(root/'render_clip.c').read_text()
fast=clip.replace('    if (y0>y1)', '''    if(y0==y1 || (x0<0 && x1<0))return;
    if((uint16_t)x0<=XMAX && (uint16_t)x1<=XMAX &&
       (uint16_t)y0<=YMAX && (uint16_t)y1<=YMAX) {
        if(x0>x1 || (x0==x1 && y0>y1)) {
            t=x0;x0=x1;x1=t;t=y0;y0=y1;y1=t;
        }
        out->x0=x0;out->y0=y0;out->x1=x1;out->y1=y1;out->line=1;return;
    }
    if (y0>y1)''',1)
blit=(root/'blitter.c').read_text();lean=blit.replace('    if (ed > XMAX || ed < -XMAX || sd > (UWORD)YMAX) return;','    /* Endpoint range checks already imply these delta bounds. */',1)
rows=[];reference={}
for kind in ('spans','clip','line_setup'):
 for variant in (('baseline','candidate','sector_insertion') if kind=='spans' else ('baseline','candidate')):
  (out/'projection.c').write_text(sector if variant=='sector_insertion' else shell if kind=='spans' and variant=='candidate' else projection)
  (out/'clip.c').write_text(fast if kind=='clip' and variant=='candidate' else clip)
  (out/'blitter.c').write_text(lean if kind=='line_setup' and variant=='candidate' else blit)
  for f in fixtures:
   walls=','.join('{'+','.join(map(str,w))+'}' for w in f['walls']);lines=','.join('{'+','.join(map(str,l))+'}' for l in f['lines'])
   header='#include "pc_projection.h"\n#include "render_clip.h"\n#include "blitter.h"\nstruct Custom *custom=(struct Custom*)0x180000;\n'
   header+='static PcWorld world={.walls={'+walls+'},.count='+str(len(f['walls']))+'};\nstatic const short lines[][4]={'+lines+'};\n'
   if kind=='spans':body=f'''PcSpan *s=(PcSpan*)0x100002;*(volatile unsigned short*)0x100000=pc_project_spans(&world,{f['sides']},s);'''
   elif kind=='clip':body='''for(unsigned i=0;i<sizeof(lines)/sizeof(lines[0]);++i) {RenderClip c={0};render_clip_line(lines[i][0],lines[i][1],lines[i][2],lines[i][3],&c);((RenderClip*)0x100000)[i]=c;}'''
   else:body='''blit_fill_reset();blit_line_mode();for(unsigned i=0;i<sizeof(lines)/sizeof(lines[0]);++i) {blit_clipped_line_onedot(lines[i][0],lines[i][1],lines[i][2],lines[i][3],0,(void*)0x120000);if(BENCH_VERIFY)for(unsigned j=0;j<128;++j)((volatile unsigned short*)0x100000)[i*128+j]=((volatile unsigned short*)custom)[j];}'''
   (out/'bench.c').write_text(header+'unsigned bench(unsigned count){for(unsigned n=0;n<count;++n){'+body+'}return count;}\n')
   cmd=[str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-O2','-DBUILD_DEBUG=0','-DBENCH_VERIFY=1','-ffreestanding','-fno-builtin','-fomit-frame-pointer','-ffunction-sections','-fdata-sections','-nostdlib','-I'+str(root),'-Wa,--register-prefix-optional','-Wl,--gc-sections,-Ttext=0x1000',str(out/'entry.s'),str(out/'bench.c'),str(out/'projection.c'),str(out/'clip.c'),str(out/'blitter.c'),str(root/'support/gcc8_a_support.s'),'-o',str(out/'bench.elf')]
   subprocess.run(cmd,check=True)
   subprocess.run([str(binutils/'objcopy'),'-O','binary',str(out/'bench.elf'),str(out/'bench.bin')],check=True)
   def run(n):return json.loads(subprocess.check_output([str(out/'probe'),str(out/'bench.bin'),str(n)],cwd=root,text=True))['cycles']
   zero=run(0);cycles=run(10)-zero;data=(out/'output.bin').read_bytes()
   # Span slots after the merged count are scratch space, not API output.
   if kind=='spans':data=data[:2+6*int.from_bytes(data[:2],'big')]
   key=(kind,f['level'])
   if variant=='baseline':reference[key]=data
   else:assert data==reference[key],key
   if kind=='line_setup':
    cmd[cmd.index('-DBENCH_VERIFY=1')]='-DBENCH_VERIFY=0'
    subprocess.run(cmd,check=True)
    subprocess.run([str(binutils/'objcopy'),'-O','binary',str(out/'bench.elf'),str(out/'bench.bin')],check=True)
    zero=run(0);cycles=run(10)-zero
   row=dict(kind=kind,variant=variant,level=f['level'],wall_records=len(f['walls']),line_requests=len(f['lines']),cycles_per_batch=cycles/10,output_sha256=hashlib.sha256(data).hexdigest());rows.append(row);print(row,flush=True)
report=dict(fixtures=fixtures,source_sha256={n:hashlib.sha256((root/n).read_bytes()).hexdigest() for n in ('pc_projection.c','render_clip.c','blitter.c')},scope='Musashi 68000 -O2 instruction cycles, 10 repetitions less empty loop setup; captured world records with reconstructed wall edges (not recorded frame edge stream). Line setup includes clipping, idle simulated registers; register snapshots verified in separate runs and excluded from timing: no DMA, raster contention, busy waits or pixel validation. Scratch-only Shell sort and per-sector insertion, trivial clip accept/reject, redundant delta-guard removal candidates. Output equality limited to these workloads.',trials=rows)
(root/'docs/WALL_PIPELINE_BENCHMARK.json').write_text(json.dumps(report,indent=2)+'\n')
