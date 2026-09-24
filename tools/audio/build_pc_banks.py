#!/usr/bin/env python3
"""Rebuild the locked PC banks directly from the three installed recordings."""
import argparse
import importlib.util
import json
import shutil
import subprocess
import sys
from pathlib import Path

ROOT=Path(__file__).resolve().parents[2]
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--music-dir',type=Path,required=True)
p.add_argument('--check-only',action='store_true')
a=p.parse_args()
missing=[name for name in ('numpy','scipy') if importlib.util.find_spec(name) is None]
missing += [name+' (PATH)' for name in ('ffmpeg','cc') if not shutil.which(name)]
if missing:p.error('Missing audio dependencies: '+', '.join(missing))
def run(script,*args):
    subprocess.run([sys.executable,str(ROOT/'tools/audio'/script),*map(str,args)],cwd=ROOT,check=True)
run('prepare_pc_tracks.py','--music-dir',a.music_dir,'--check-only')
if a.check_only:raise SystemExit(0)
run('prepare_pc_tracks.py','--music-dir',a.music_dir)
for track in ('courtesy','otis','focus'):
    extra=['--reserve-bytes','2048'] if track=='courtesy' else []
    run('compress_beat_bank.py','--optimal','--track',track,'--sample-rate','12000','--budget-kib','250',*extra)
run('audition_codecs.py','--budget-kib','250','--codec','fibonacci','--gain-stage','before','--max-package-kib','250')
sys.path.insert(0,str(ROOT/'tools/trackloader'))
from asset_identity import verify_assets
verify_assets('banks') # Never silently accept a different encode or rewrite the lock.
import numpy,scipy
report=dict(status='All three PC banks regenerated and match the accepted byte hashes',
            python=sys.version.split()[0],numpy=numpy.__version__,scipy=scipy.__version__,
            ffmpeg=subprocess.check_output(['ffmpeg','-version'],text=True).splitlines()[0],
            music_dir=str(a.music_dir.resolve()),
            limitations=['Requires the matching licensed PC recordings and repository analysis metadata',
                         'Audio banks only; payload packaging and ADF build are separate commands'])
(ROOT/'docs/TRACKLOADER_SOURCE_AUDIO_BUILD_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(report['status'])
