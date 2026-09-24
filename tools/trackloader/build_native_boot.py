#!/usr/bin/env python3
"""Disposable PAL A500 ADF with resident three-track loading and cues."""
import hashlib,json,struct,subprocess,zlib,runpy,sys
from pathlib import Path
from native_layout import reservations as native_reservations
from check_adf_layout import inspect
from package_native_game import compress
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader';native=out/'native_game'
runpy.run_path(str(root/'tools/trackloader/make_guarded_diskio.py'))
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
# Resident workspaces must remain disjoint even as code/data budgets evolve.
regions=[('stage',0,22528),('diskio',24000,37056),('track_cache',38000,43632),
 ('save_anchors',45000,46548),('save_scratch',46800,48848),
 ('inflate_scratch',52000-2928,52000),('metadata',54000,55244),('audio_dma',56000,58048)]
for left,right in zip(regions,regions[1:]):assert left[2]<=right[1],(left,right)
raw=(native/'game.exe1').read_bytes();packed=(native/'game.deflate').read_bytes()
assert zlib.decompress(packed,-15)==raw
proof=json.loads((root/'docs/TRACKLOADER_EXECUTABLE_TARGET_RESULTS.json').read_text())['inflate']
assert proof['packed_sha256']==hashlib.sha256(packed).hexdigest() and proof['overlap_matches']
assert proof['arena_bytes']<=196608 and struct.unpack_from('>I',raw,12)[0]<=196608
entry=native/'executable_pic.s';entry.write_text('.text\n.global _start\n_start: bra.w track_executable_prepare\n bra.w resident_tune_prepare\n bra.w track_ofs_load\n')
# Keep tune expansion/decoding at O2; shrink validation and boot-only glue.
pic_objects=[]
for source in ['trackloader/ofs.c','trackloader/executable.c','trackloader/tests/resident_tune.c','trackloader/tune.c','trackloader/fib_expand.c','fib_decode.c']:
 obj=native/('pic_'+source.replace('/','_')+'.o')
 optimization='-Os' if source in ['trackloader/ofs.c','trackloader/executable.c','trackloader/tests/resident_tune.c'] else '-O2'
 subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-mpcrel',optimization,'-ffreestanding','-fno-builtin','-c',str(root/source),'-o',str(obj)],check=True)
 pic_objects.append(str(obj))
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-mpcrel','-Wa,--register-prefix-optional','-nostdlib','-Wl,-Ttext=0',str(entry),*pic_objects,'-o',str(native/'executable_pic.elf')],check=True)
subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(native/'executable_pic.elf'),str(native/'executable_pic.bin')],check=True)
boot_source=(root/'trackloader/tests/load_boot.s').read_text().replace('move.l #65536,d0','move.l #360448,d0')
(native/'native_boot.s').write_text(boot_source)
defines=['-DNATIVE_CACHE=1']+[f'-D{k}={v}' for k,v in dict(GAME_SOURCE_OFFSET=proof['source_offset'],GAME_PACKED_BYTES=len(packed),GAME_PACKED_SUM=sum(packed),GAME_PACKAGE_BYTES=len(raw)).items()]
descriptors=[]
for index,(track,lead) in enumerate([('courtesy',0),('otis',0),('focus',0)],1):
 tune=(out/f'{track}.fibonacci.deflate').read_bytes()
 proof_tune=next(r for r in json.loads((root/'docs/TRACKLOADER_INFLATE_RESULTS.json').read_text())['trials'] if r['name']==f'{track}.fibonacci.deflate')
 assert proof_tune['sha256']==hashlib.sha256(tune).hexdigest() and proof_tune['overlap_matches']
 assert proof_tune['arena_bytes']<=500000
 cues=(root/f'assets/music{index}.cues').read_bytes();assert 0<len(cues)<=16384
 (native/f'cues{index}.deflate').write_bytes(compress(cues,out/'libzultra.dylib'))
 values=[proof_tune['source_offset'],len(tune),sum(tune),len(zlib.decompress(tune,-15)),lead,len(cues)]
 name='DF0:'+track
 descriptors.append(' dc.l '+','.join(map(str,values))+f',cues{index}-descriptors,{sum(cues)}\n'+f" dc.b '{name}',0\n dcb.b {15-len(name)},0\n")
(native/'tunes.i').write_text(''.join(descriptors))
package_report=json.loads((root/'docs/TRACKLOADER_EXECUTABLE_PACKAGE_RESULTS.json').read_text())
assert package_report['elf_sha256']==hashlib.sha256((native/'game.elf').read_bytes()).hexdigest()
assert package_report['packed_sha256']==hashlib.sha256(packed).hexdigest()
symbols=subprocess.check_output([str(sdk.parent/'m68k-amiga-elf/bin/nm'),str(native/'game.elf')],text=True)
for name,symbol in [('GAME_STOP','fib_stream_stop'),('GAME_UNBIND','fib_stream_unbind'),('GAME_BIND','fib_stream_bind'),('GAME_SFX_STOP','sfx_shutdown'),('GAME_SFX_INIT','sfx_init'),('GAME_CUE_LEAD','fib_stream_set_cue_lead'),('GAME_SAVE_SELECT','track_save_select')]:
 address=next(int(line.split()[0],16) for line in symbols.splitlines() if line.split()[-1]==symbol)
 defines.append(f'-D{name}={address}')
assert len((root/'assets/music1.cues').read_bytes())==11441
save_identity=hashlib.sha256(b'HXS1 payload identity\0'+hashlib.sha256(packed).digest()+b''.join(hashlib.sha256((out/f'{t}.fibonacci.deflate').read_bytes()).digest() for t in ['courtesy','otis','focus'])).digest()[:16]
(native/'save_identity.bin').write_bytes(save_identity)
(native/'save_anchors.deflate').write_bytes(compress(bytes(1548),out/'libzultra.dylib'))
for name,source in [('boot',native/'native_boot.s'),('stage',root/'trackloader/tests/native_stage.s')]:
 subprocess.run(['vasmm68k_mot','-m68000','-Fbin','-quiet','-I'+str(root),*defines,'-o',str(native/(name+'.bin')),str(source)],check=True)
payload_report=json.loads((root/'docs/TRACKLOADER_PAYLOAD_MEASUREMENTS.json').read_text())
assert payload_report['gain_stage']=='before_fibonacci'
for row in payload_report['tunes']:
 if row['codec']=='fibonacci':
  assert row['packed_sha256']==hashlib.sha256((out/(row['track']+'.fibonacci.deflate')).read_bytes()).hexdigest()
expected={t:(out/f'{t}.fibonacci.deflate').read_bytes() for t in ['courtesy','focus','otis']};expected['game']=packed
args=[str(out/'dosio_probe'),str(out/'dosio.bin'),str(native/'native_menu.adf')]
for t in ['courtesy','focus','otis']:args+=['DF0:'+t,str(out/f'{t}.fibonacci.deflate')]
args+=['DF0:game',str(native/'game.deflate')]
for name,byte in [('save_a',51),('save_b',34)]:
 expected[name]=bytes([byte])*512
 fixture=native/(name+'.fixture');fixture.write_bytes(expected[name]);args+=['DF0:'+name,str(fixture)]
result=subprocess.run(args,capture_output=True,text=True,check=True)
data=bytearray((native/'native_menu.adf').read_bytes());inspect(data,expected)
# Fixed on-disk ABI: stage tracks 151-154 and independent save tracks 158/159.
# Validate every sector is free below; do not depend on a historical trial image.
reservations=native_reservations()
bitmap=struct.unpack_from('>I',data,880*512+316)[0];words=list(struct.unpack_from('>128I',data,bitmap*512))
for sectors in reservations.values():
 for sector in sectors:
  assert words[1+(sector-2)//32]&(1<<((sector-2)%32)), 'reserved sector already occupied'
  words[1+(sector-2)//32]&=~(1<<((sector-2)%32))
words[0]=0;words[0]=(-sum(words))&0xffffffff;struct.pack_into('>128I',data,bitmap*512,*words)
layout=inspect(data,expected,reservations)
assert reservations['bootstrap']==list(range(1661,1705))
boot=(native/'boot.bin').read_bytes();assert len(boot)<=1024
block=bytearray(1024);block[:len(boot)]=boot
checksum=0
for value in struct.unpack('>256I',block):checksum+=value;checksum=(checksum&0xffffffff)+(checksum>>32)
struct.pack_into('>I',block,4,(~checksum)&0xffffffff);data[:1024]=block
anchor_sectors=[0,880,bitmap]
anchors=struct.pack('>3I',*anchor_sectors)+b''.join(data[n*512:(n+1)*512] for n in anchor_sectors)
(native/'save_anchors.deflate').write_bytes(compress(anchors,out/'libzultra.dylib'))
subprocess.run(['vasmm68k_mot','-m68000','-Fbin','-quiet','-I'+str(root),*defines,'-o',str(native/'stage.bin'),str(root/'trackloader/tests/native_stage.s')],check=True)
stage=(native/'stage.bin').read_bytes();assert len(stage)<=22528
assert zlib.decompress((native/'save_anchors.deflate').read_bytes(),-15)==anchors
data[1661*512:1661*512+len(stage)]=stage
(native/'native_menu.adf').write_bytes(data)
(native/'native_menu.toml').write_text(f'[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = "{native/"native_menu.adf"}"\nwrite_protected = true\n')
report=dict(status='Native three-track diagnostic built; not yet boot verified',stage_bytes=len(stage),chip_block_bytes=360448,image_offset=65536,image_capacity=196608,heap_offset=278528,heap_bytes=81920,cue_offset=262144,cue_capacity=16384,free_disk_sectors=layout['free_sectors'],adf_sha256=hashlib.sha256(data).hexdigest(),limitations=['A500 PAL/NTSC auto-detection; full NTSC lifecycle validation pending','All profiles mapped; Hyper unlock rules unchanged','Native saving requires writable disposable media; hardware validation pending'])
(root/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n');print(report)

subprocess.run([sys.executable,str(root/"tools/trackloader/make_save_manifest.py")],check=True)
