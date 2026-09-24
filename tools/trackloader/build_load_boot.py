#!/usr/bin/env python3
"""Build a selected-tune disk->inflate->validated PCM cold-boot diagnostic."""
import argparse,hashlib,json,struct,subprocess,zlib,runpy,re
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader'
parser=argparse.ArgumentParser();parser.add_argument('--paula',action='store_true');parser.add_argument('--irq',action='store_true');parser.add_argument('--track',choices=['courtesy','focus','otis'],default='focus');parser.add_argument('--switch',action='store_true',help='Load all three banks in one boot; requires --irq');parser.add_argument('--disk',type=Path,default=out/'native_game/native_menu.adf',help='Fresh filesystem with the current encoded tunes (build_native_boot.py)');args=parser.parse_args()
if args.switch and not args.irq: parser.error('--switch requires --irq')
tracks=[args.track]+[t for t in ['courtesy','focus','otis'] if t!=args.track] if args.switch else [args.track]
descriptors=[]
for track in tracks:
 packed=(out/f'{track}.fibonacci.deflate').read_bytes()
 inflated=zlib.decompress(packed,-15)
 final=zlib.decompress((out/f'{track}.pcm.deflate').read_bytes(),-15)
 record=next(r for r in json.loads((root/'docs/TRACKLOADER_INFLATE_RESULTS.json').read_text())['trials'] if r['name']==f'{track}.fibonacci.deflate')
 assert record['sha256']==hashlib.sha256(packed).hexdigest() and record['overlap_matches']
 assert max(len(final),record['source_offset']+len(packed))<=500000
 manifest=dict(track=track,packed_bytes=len(packed),inflated_bytes=len(inflated),final_bytes=len(final),source_offset=record['source_offset'],packed_sha256=record['sha256'])
 (out/f'{track}.load_manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
 values=[record['source_offset'],len(packed),sum(packed),len(inflated),len(final),sum(final)]
 name=f'DF0:{track}'
 descriptors.append(' dc.l '+','.join(map(str,values))+f"\n dc.b '{name}',0\n dcb.b {15-len(name)},0\n")
(out/'tune_descriptors.i').write_text(''.join(descriptors)+' dc.l -1\n')
defines=[]
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
entry=out/'fib_pic.s';entry.write_text('.text\n.global _start\n_start:\n bra.w trackloader_tune_prepare\n bra.w trackloader_paula_probe\n')
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-mpcrel','-Wa,--register-prefix-optional','-O2','-nostdlib','-fomit-frame-pointer','-fno-builtin','-Wl,-Ttext=0',str(entry),str(root/'trackloader/fib_expand.c'),str(root/'fib_decode.c'),str(root/'trackloader/tune.c'),str(root/'fib_pcm.c'),*([str(root/f) for f in ['trackloader/tests/irq_probe.c','tests/fib_stream.c','paula_irq.c','pcm_lifecycle.c','pc_pulse.c','video.c','support/gcc8_a_support.s']] if args.irq else [str(root/'trackloader/tests/paula_probe.c')]),'-DPCM_EXTERNAL_ONLY=1','-DBUILD_DEBUG=0','-DSOUND_EFFECTS=0','-DAUDIO_DIAGNOSTICS=0','-o',str(out/'fib_pic.elf')],check=True)
subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(out/'fib_pic.elf'),str(out/'fib_pic.bin')],check=True)
for name in ['load_boot','load_stage']:
 subprocess.run(['vasmm68k_mot','-m68000','-Fbin','-quiet','-I'+str(root),]+defines+(['-DPAULA_PROBE=1'] if args.paula or args.irq else [])+['-o',str(out/(name+'.bin')),str(root/'trackloader/tests'/(name+'.s'))],check=True)
data=bytearray(args.disk.read_bytes());stage=(out/'load_stage.bin').read_bytes();assert len(stage)<=22528
sections=subprocess.check_output([str(sdk.parent/'m68k-amiga-elf/bin/objdump'),'-h',str(out/'fib_pic.elf')],text=True)
bss=re.search(r'\s\d+ \.bss\s+([0-9a-f]+)\s+([0-9a-f]+)',sections)
if bss:
 image_base=len(stage)-(out/'fib_pic.bin').stat().st_size
 assert image_base+int(bss[1],16)+int(bss[2],16)<=24000, 'resident BSS overlaps disk workspace'
boot=(out/'load_boot.bin').read_bytes();assert len(boot)<=1024
block=bytearray(1024);block[:len(boot)]=boot
s=0
for w in struct.unpack('>256I',block):s+=w;s=(s&0xffffffff)+(s>>32)
struct.pack_into('>I',block,4,(~s)&0xffffffff);data[:1024]=block;data[1661*512:1661*512+len(stage)]=stage
(out/'load_trial.adf').write_bytes(data);print('stage',len(stage))
