#!/usr/bin/env python3
"""Compare old/new production reverse readers in Musashi 68000 instruction cycles."""
import argparse, subprocess, json, zlib, struct, hashlib
from pathlib import Path
root=Path(__file__).resolve().parents[2]
p=argparse.ArgumentParser();p.add_argument('--execram-source',type=Path,required=True);a=p.parse_args()
out=root/'scratchpad/reverse_benchmark';out.mkdir(exist_ok=True)
v=a.execram_source/'src/musashi_vendor'
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
blob=zlib.decompress((root/'scratchpad/trackloader/focus.pcm.deflate').read_bytes(),-15)
(out/'focus.pcm').write_bytes(blob)
total,banks,count=struct.unpack_from('>III',blob,8);meta=24+4*(banks+1)+4*count
h=(root/'tools/ending_projection_bench.c').read_text().replace('argc!=3','argc!=5').replace('500000000','5000000000ULL')
h=h.replace('m68k_init();', '''unsigned size;unsigned char *pcm=load(argv[3],&size);
 memcpy(mem+0x40000,pcm,size);free(pcm);
 m68k_write_memory_32(0x900,strtoul(argv[4],0,0));
 m68k_init();''')
h=h.replace('printf("{', '''FILE *f=fopen("scratchpad/reverse_benchmark/output.bin","wb");
 fwrite(mem+0x100000,512,count,f);fclose(f);
 printf("{''')
(out/'host.c').write_text(h)
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(root/'scratchpad/trackloader'),str(out/'host.c'),str(v/'m68kcpu.c'),str(root/'scratchpad/trackloader/m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(out/'probe')],check=True)
(out/'entry.s').write_text('.text\n.global _start\n_start: bra.w bench\n')
rows=[]
for variant in ('old','new'):
 for name in ('fib_pcm.c','fib_pcm.h'):
  data=subprocess.check_output(['git','show','b8923ed:'+name]) if variant=='old' else (root/name).read_bytes()
  (out/name).write_bytes(data)
 decl='unsigned reverse=start;' if variant=='old' else 'PcmReverse reverse={start,0};'
 (out/'bench.c').write_text(f'''#include "fib_pcm.h"
unsigned bench(unsigned blocks) {{
 unsigned start=*(volatile unsigned*)0x900;
 PcmSong song={{0}};unsigned char *blob=(unsigned char*)0x40000;
 song.offsets=blob+24;song.sequence=blob+{24+4*(banks+1)};
 song.bank=blob+{meta};song.pcm_split={len(blob)-meta};song.seq_count={count};
 {decl}
 for(unsigned i=0;i<blocks;++i)
  fib_song_read_reverse(&song,&reverse,(unsigned char*)0x100000+i*512,512);
 return blocks;
}}
''')
 subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-O2','-ffreestanding','-fno-builtin','-nostdlib','-I'+str(root),'-Wa,--register-prefix-optional','-Wl,-Ttext=0x1000',str(out/'entry.s'),str(out/'bench.c'),str(out/'fib_pcm.c'),str(root/'support/gcc8_a_support.s'),'-o',str(out/'bench.elf')],check=True)
 subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(out/'bench.elf'),str(out/'bench.bin')],check=True)
 for label,start in [('start',total),('middle',total//2),('near_end',64*512)]:
  def run(n):
   return json.loads(subprocess.check_output([str(out/'probe'),str(out/'bench.bin'),str(n),str(out/'focus.pcm'),str(start)],cwd=root,text=True,timeout=60))['cycles']
  overhead=run(0);first=run(1)-overhead;cycles=run(64)-overhead
  output=(out/'output.bin').read_bytes()
  offsets=struct.unpack_from('>'+str(banks+1)+'I',blob,24)
  pcm=b''.join(blob[meta+offsets[i]:meta+offsets[i]+n] for i,n in struct.iter_unpack('>HH',blob[24+4*(banks+1):meta]))
  assert output==pcm[start-64*512:start][::-1]
  row=dict(variant=variant,position=label,blocks=64,cycles=cycles,cycles_per_block=cycles/64,first_block_cycles=first,steady_cycles_per_block=(cycles-first)/63,output_sha256=hashlib.sha256(output).hexdigest())
  rows.append(row);print(row,flush=True)
report=dict(scope='Musashi 68000 instruction cycles; no DMA contention or interrupts; 512-sample blocks, 12000 Hz; -m68000 -O2, no LTO; function/loop overhead included, setup subtracted',source_sha256=hashlib.sha256(blob).hexdigest(),sequence_entries=count,trials=rows)
(root/'docs/REVERSE_AUDIO_BENCHMARK.json').write_text(json.dumps(report,indent=2)+'\n')
