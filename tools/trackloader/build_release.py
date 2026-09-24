#!/usr/bin/env python3
"""Rebuild and publish a factory ADF from the prepared soundtrack workspace."""
import argparse
from asset_identity import verify_assets
import hashlib
import json
import shutil
import subprocess
import sys
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--execram-source',type=Path,required=True)
a=p.parse_args()
root=Path(__file__).resolve().parents[2];work=root/'scratchpad/trackloader'
required=[work/'libzultra.dylib',root/'docs/TRACKLOADER_PAYLOAD_MEASUREMENTS.json',
          root/'docs/TRACKLOADER_INFLATE_RESULTS.json',
          a.execram_source/'src/musashi_vendor/m68kmake.c',a.execram_source/'stubs/inflate/inflate_core.s']
required += [work/(name+'.fibonacci.deflate') for name in ('courtesy','otis','focus')]
required += [root/f'assets/music{i}.cues' for i in (1,2,3)]
required += [root/'assets/sounds'/(name+'.ogg') for name in ('begin','excellent','gameover','start','die','rankup','line','triangle','square','pentagon','hexagon','menuselect','menuchoose','awesome','wonderful','superhexagon')]
missing=[str(path) for path in required if not path.is_file()]
missing += [name+' (PATH)' for name in ('cc','vasmm68k_mot','ffmpeg') if not shutil.which(name)]
if not list((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin/m68k-amiga-elf-gcc')):
    missing.append('Bartman Amiga GCC SDK')
if missing:p.error('Missing prerequisites:\n'+'\n'.join(missing))
try:verify_assets('disk_assets')
except (ValueError,OSError) as error:p.error(str(error))
def run(name,*args):
    print('Running',name,flush=True)
    subprocess.run([sys.executable,str(root/'tools/trackloader'/name),*map(str,args)],cwd=root,check=True)
print('Preparing sound effects from source OGG files',flush=True)
subprocess.run([sys.executable,str(root/'tools/audio/prepare_sfx.py')],cwd=root,check=True)
run('check_native_game.py')
run('package_native_game.py')
run('check_executable.py')
run('build_inflate_probe.py','--execram-source',a.execram_source.resolve())
run('check_executable_target.py','--execram-source',a.execram_source.resolve())
run('build_dosio_probe.py','--execram-source',a.execram_source.resolve())
run('build_native_boot.py')
image=(work/'native_game/native_menu.adf').read_bytes();digest=hashlib.sha256(image).hexdigest()
assert len(image)==901120
for sector in (1738,1749):assert image[sector*512:(sector+1)*512]==bytes(512)
boot=json.loads((root/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').read_text())
assert boot['adf_sha256']==digest
package=json.loads((root/'docs/TRACKLOADER_EXECUTABLE_PACKAGE_RESULTS.json').read_text())
manifest=dict(status='Factory image built and statically checked; emulator trials are separate',
    sha256=digest,bytes=len(image),stage_bytes=boot['stage_bytes'],
    game_memory_bytes=package['memory_bytes'],factory_save_slots_blank=True,
    soundtrack_lock_sha256=hashlib.sha256((root/'soundtrack/trackloader_assets.lock.json').read_bytes()).hexdigest(),
    save_identity=(work/'native_game/save_identity.bin').read_bytes().hex(),
    prepared_assets={path.name:hashlib.sha256(path.read_bytes()).hexdigest() for path in required if path.is_file()},
    limitations=['Requires prepared soundtrack assets and their existing validation reports',
                 'Not a clean-checkout asset-generation pipeline','A500 PAL/NTSC detection; full NTSC lifecycle validation pending'])
output=root/'out/trackloader';output.mkdir(parents=True,exist_ok=True)
dest=output/(digest+'.adf')
# Never truncate an existing image: it might contain somebody's saves.
try:
    with dest.open('xb') as f:f.write(image)
except FileExistsError:
    if dest.read_bytes()!=image:raise SystemExit('Refusing to overwrite changed image: '+str(dest))
(output/(digest+'.json')).write_text(json.dumps(manifest,indent=2)+'\n')
print('Factory ADF:',dest)
