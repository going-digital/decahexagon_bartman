#!/usr/bin/env python3
"""Select the installed PC Otis recording, preserving its exact decoded origin."""
import hashlib,json,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2]
p=json.loads((ROOT/'scratchpad/pc_verification/evidence/provenance.json').read_text())
binary=Path(p['binary']);assert hashlib.sha256(binary.read_bytes()).hexdigest()==p['sha256']
source=binary.parent.parent/'Resources/data/music/music2.dat'
comparison=json.loads((ROOT/'soundtrack/otis_pc_comparison.json').read_text())
sha=hashlib.sha256(source.read_bytes()).hexdigest();assert sha==comparison['pc']['sha256']
work=ROOT/'scratchpad/audio/otis_pc';work.mkdir(parents=True,exist_ok=True)
subprocess.run(['ffmpeg','-v','error','-y','-i',str(source),'-c:a','pcm_f32le',str(work/'decoded.wav')],check=True)
grid=json.loads((ROOT/'soundtrack/otis.analysis.json').read_text())['constant_grid_hypothesis'].copy()
period=30/grid['bpm'];grid['phase_seconds']=(grid['phase_seconds']-comparison['estimated_trim']['start_seconds'])%period
report=dict(source=str(source),sha256=sha,pc_track=2,decoded='scratchpad/audio/otis_pc/decoded.wav',time_origin='First ffmpeg decoded PCM sample; no manual trim, shift or speed change',cue_asset='assets/music2.cues',cue_offset_seconds=0,compression_grid=grid,grid_note='Approximate beat phase transferred for dictionary slicing only; does not shift playback or PC cues.',previous_source='assets/Chipzel - Otis [1413220136].mp3')
(ROOT/'soundtrack/otis.pc_source.json').write_text(json.dumps(report,indent=2)+'\n');print(report)
