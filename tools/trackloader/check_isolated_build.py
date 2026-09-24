#!/usr/bin/env python3
"""Rebuild cues, audio and ADF in a fresh source snapshot and compare bytes."""
import argparse
import hashlib
import json
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT=Path(__file__).resolve().parents[2]
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--binary',type=Path,required=True)
p.add_argument('--music-dir',type=Path,required=True)
p.add_argument('--execram-source',type=Path,required=True)
p.add_argument('--expected-adf',type=Path,default=ROOT/'scratchpad/trackloader/native_game/native_menu.adf')
a=p.parse_args()
for name in ('binary','music_dir','execram_source','expected_adf'):
    setattr(a,name,getattr(a,name).resolve())
if not a.expected_adf.is_file():p.error('Build the reference ADF first, or supply --expected-adf')
expected=a.expected_adf.read_bytes()
if len(expected)!=901120:p.error('Expected a standard DD ADF')
# Run non-mutating source checks before allocating the isolated workspace.
for script,args in [('tools/audio/extract_pc_cues.py',['--binary',str(a.binary),'--check-only']),
                    ('tools/audio/build_pc_banks.py',['--music-dir',str(a.music_dir),'--check-only'])]:
    subprocess.run([sys.executable,str(ROOT/script),*args],cwd=ROOT,check=True)
parent=ROOT/'scratchpad/trackloader';parent.mkdir(parents=True,exist_ok=True)
work=Path(tempfile.mkdtemp(prefix='isolated_build_',dir=parent))
ignore=shutil.ignore_patterns('__pycache__','*.pyc','*.o','*.dylib','music*.cues')
for directory in ('tools/audio','tools/trackloader','support','tests','trackloader','assets','gfx','include'):
    shutil.copytree(ROOT/directory,work/directory,ignore=ignore)
for path in ROOT.iterdir():
    if path.is_file() and (path.suffix in ('.c','.h','.s','.asm','.i','.inc','.ld') or path.name=='Makefile'):
        shutil.copy2(path,work/path.name)
(work/'soundtrack').mkdir();(work/'docs').mkdir()
for name in ('courtesy.analysis.json','otis.analysis.json','focus.analysis.json',
             'otis_pc_comparison.json','pc_track_verification.json','trackloader_assets.lock.json'):
    shutil.copy2(ROOT/'soundtrack'/name,work/'soundtrack'/name)
assert not (work/'out').exists() and not (work/'scratchpad').exists()
assert not list((work/'assets').glob('music*.cues'))
files=sorted(path for path in work.rglob('*') if path.is_file())
snapshot=hashlib.sha256()
for path in files:
    snapshot.update(str(path.relative_to(work)).encode()+b'\0'+hashlib.sha256(path.read_bytes()).digest())
steps=[('tools/audio/extract_pc_cues.py',['--binary',str(a.binary)]),
       ('tools/audio/build_pc_banks.py',['--music-dir',str(a.music_dir)]),
       ('tools/trackloader/prepare_payloads.py',['--execram-source',str(a.execram_source)]),
       ('tools/trackloader/build_release.py',['--execram-source',str(a.execram_source)])]
print('Isolated workspace:',work,flush=True)
with (work/'build.log').open('w') as log:
    for script,args in steps:
        print('Running',script,flush=True)
        result=subprocess.run([sys.executable,str(work/script),*args],cwd=work,stdout=log,stderr=subprocess.STDOUT)
        if result.returncode:raise SystemExit('Build failed; inspect '+str(work/'build.log'))
actual=(work/'scratchpad/trackloader/native_game/native_menu.adf').read_bytes()
if actual!=expected:raise SystemExit('ADF differs from reference; inspect '+str(work/'build.log'))
assert a.expected_adf.read_bytes()==expected,'Reference disk changed during verification'
report=dict(status='Automated isolated cue/audio/ADF rebuild matches reference byte-for-byte',
            adf_sha256=hashlib.sha256(actual).hexdigest(),workspace=str(work.relative_to(ROOT)),
            source_snapshot_sha256=snapshot.hexdigest(),source_files=len(files),
            cached_cues_supplied=False,cached_audio_supplied=False,cached_sfx_supplied=False,
            cached_trackloader_outputs_supplied=False,reference_disk_unchanged=True,
            limitations=['Uses installed ffmpeg, Amiga SDK, vasm and local execram checkout',
                         'Working-tree source snapshot, not a committed clean checkout',
                         'Reproduces existing bytes; does not replace emulator or hardware tests'])
(ROOT/'docs/TRACKLOADER_ISOLATED_BUILD_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(report['status'],report['adf_sha256'])
