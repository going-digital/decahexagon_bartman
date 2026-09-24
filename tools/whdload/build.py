#!/usr/bin/env python3
"""Build standalone WHDLoad slaves from verified native payloads; no ADF edits."""
import argparse,hashlib,json,shutil,struct,subprocess,sys,zlib,zipfile
from pathlib import Path
root=Path(__file__).resolve().parents[2]
sys.path.insert(0,str(root/'tools/trackloader'))
from package_native_game import package
from icon import launcher_icon
p=argparse.ArgumentParser();p.add_argument('--sdk',type=Path,default=root/'scratchpad/whdload/sdk/WHDLoad');p.add_argument('--no-archive',action='store_true',help='Build the directory only, for the combined distribution');a=p.parse_args()
assert (a.sdk/'Include/whdload.i').is_file(),'Set --sdk to the extracted official WHDLoad developer SDK'
work=root/'scratchpad/whdload/game';dest=root/'out/whdload/Hexagon';data=dest/'data'
data.mkdir(parents=True,exist_ok=True)
native=root/'scratchpad/trackloader/native_game';packed=root/'scratchpad/trackloader'
assert (native/'executable_pic.bin').exists(),'Build make trackloader-adf first'
subprocess.run([sys.executable,str(root/'tools/trackloader/check_native_game.py'),'--whdload'],check=True)
raw=package(work/'game.elf');assert len(raw)<=196608 and struct.unpack_from('>I',raw,12)[0]<=196608
(data/'game').write_bytes(raw)
# Uncompressed EXE1 eliminates an unnecessary second decompression path on HD.
# Audio keeps the exact proven Zultra/Fibonacci assets and overlap offsets.
proofs=json.loads((root/'docs/TRACKLOADER_INFLATE_RESULTS.json').read_text())['trials']
for name in ('courtesy','otis','focus'):
 proof=next(r for r in proofs if r['name']==f'{name}.fibonacci.deflate')
 assert proof['sha256']==hashlib.sha256((packed/f'{name}.fibonacci.deflate').read_bytes()).hexdigest() and proof['overlap_matches'] and proof['arena_bytes']<=500000
 shutil.copyfile(packed/f'{name}.fibonacci.deflate',data/name)
for name in ('cues1.deflate','cues2.deflate','cues3.deflate'):
 shutil.copyfile(native/name,work/name)
descriptors=[]
for index,name in enumerate(('courtesy','otis','focus'),1):
 proof=next(r for r in proofs if r['name']==f'{name}.fibonacci.deflate')
 tune=(data/name).read_bytes();cues=(root/f'assets/music{index}.cues').read_bytes()
 assert 0<len(cues)<=16384 and zlib.decompress((work/f'cues{index}.deflate').read_bytes(),-15)==cues
 values=[proof['source_offset'],len(tune),sum(tune),len(zlib.decompress(tune,-15)),0,len(cues)]
 filename='DF0:'+name
 descriptors.append(' dc.l '+','.join(map(str,values))+f',cues{index}-descriptors,{sum(cues)}\n'+f" dc.b '{filename}',0\n dcb.b {15-len(filename)},0\n")
(work/'tunes.i').write_text(''.join(descriptors))
# Stable schema ID: keep existing saves compatible across display-name changes.
(work/'save_identity.bin').write_bytes(bytes.fromhex('f15af4fd78061709977a4b41096a47fa'))
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
entry=work/'pic.s';entry.write_text('.text\n.global _start\n_start: bra.w track_executable_prepare\n bra.w resident_tune_prepare\n rts\n nop\n bra.w whd_save_commit\n')
objects=[]
for src in ['trackloader/executable.c','trackloader/tests/resident_tune.c','trackloader/tune.c','trackloader/fib_expand.c','fib_decode.c','trackloader/save.c','whdload/save_files.c','whdload/memory.c']:
 obj=work/(src.replace('/','_')+'.pic.o');objects.append(str(obj))
 subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-mpcrel','-Os','-ffreestanding','-fno-builtin','-c',str(root/src),'-o',str(obj)],check=True)
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-mpcrel','-Wa,--register-prefix-optional','-nostdlib','-Wl,-Ttext=0',str(entry),*objects,'-o',str(work/'pic.elf')],check=True)
binutils=sdk.parent/'m68k-amiga-elf/bin'
subprocess.run([str(binutils/'objcopy'),'-O','binary',str(work/'pic.elf'),str(work/'executable_pic.bin')],check=True)
symbols=subprocess.check_output([str(binutils/'nm'),str(work/'game.elf')],text=True)
values=dict(GAME_SOURCE_OFFSET=0,GAME_PACKED_BYTES=len(raw),GAME_PACKED_SUM=sum(raw),GAME_PACKAGE_BYTES=len(raw))
for name,symbol in [('GAME_STOP','fib_stream_stop'),('GAME_UNBIND','fib_stream_unbind'),('GAME_BIND','fib_stream_bind'),('GAME_SFX_STOP','sfx_shutdown'),('GAME_SFX_INIT','sfx_init'),('GAME_CUE_LEAD','fib_stream_set_cue_lead'),('GAME_SAVE_SELECT','track_save_select')]:
 values[name]=next(int(line.split()[0],16) for line in symbols.splitlines() if line.split()[-1]==symbol)
resident=(root/'trackloader/tests/native_stage.s').read_text().replace('scratchpad/trackloader/native_game/',str(work)+'/')
(work/'resident.s').write_text(resident)
for slots,exp in [(1,524288),(2,1048576),(3,1572864)]:
 subprocess.run(['vasmm68k_mot','-m68000','-Fhunkexe','-quiet','-I'+str(a.sdk/'Include'),'-I'+str(sdk.parent/'m68k-amiga-elf/sys-include'),'-I'+str(work),'-I'+str(root),'-DWHDLOAD=1',f'-DCACHE_SLOTS={slots}',f'-DEXP_BYTES={exp}',*[f'-D{k}={v}' for k,v in values.items()],'-o',str(dest/f'Hexagon-{slots}.slave'),str(root/'whdload/slave.s')],check=True)
shutil.copyfile(root/'whdload/README.md',dest/'README.md')
for slots in (1,2,3):
 (dest/f'Hexagon-{slots}.info').write_bytes(launcher_icon(slots))
 (dest/f'Run-{slots}').write_text(f'WHDLoad SLAVE=Hexagon-{slots}.slave PRELOAD NOWRITECACHE\n')
names=['README.md']+[f'Hexagon-{i}.{ext}' for i in (1,2,3) for ext in ('slave','info')]+[f'Run-{i}' for i in (1,2,3)]+['data/'+name for name in ('game','courtesy','otis','focus')]
files={name:dict(bytes=(dest/name).stat().st_size,sha256=hashlib.sha256((dest/name).read_bytes()).hexdigest()) for name in sorted(names)}
report=dict(status='Built; runtime evidence is recorded separately in docs/WHDLOAD_RESULTS.json',chip_bytes=524288,game_memory_bytes=struct.unpack_from('>I',raw,12)[0],files=files)
(dest/'manifest.json').write_text(json.dumps(report,indent=2)+'\n')
if not a.no_archive:
 with zipfile.ZipFile(dest.parent/'Hexagon.zip','w',compression=zipfile.ZIP_DEFLATED) as archive:
  for name in sorted([*files,'manifest.json']):
   entry=zipfile.ZipInfo(str(Path(dest.name)/name),(2026,1,1,0,0,0))
   entry.compress_type=zipfile.ZIP_DEFLATED
   entry.external_attr=0o100644<<16
   archive.writestr(entry,(dest/name).read_bytes())

print(dest)
