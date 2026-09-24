#!/usr/bin/env python3
"""Select installed PC Courtesy, Otis and Focus, preserving decoded sample zero."""
import argparse
import hashlib
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--music-dir', type=Path, help='Directory containing PC music1.dat, music2.dat and music3.dat')
parser.add_argument('--check-only', action='store_true')
args = parser.parse_args()
if args.music_dir is None:
    provenance = json.loads((ROOT/'scratchpad/pc_verification/evidence/provenance.json').read_text())
    binary = Path(provenance['binary'])
    assert hashlib.sha256(binary.read_bytes()).hexdigest() == provenance['sha256']
    args.music_dir = binary.parent.parent/'Resources/data/music'
args.music_dir = args.music_dir.resolve()
verification = json.loads((ROOT/'soundtrack/pc_track_verification.json').read_text())
comparison = json.loads((ROOT/'soundtrack/otis_pc_comparison.json').read_text())
tracks = [('courtesy', 1), ('otis', 2), ('focus', 3)]
# Reject every missing/changed source before decoding or updating metadata.
for track, number in tracks:
    source = args.music_dir/f'music{number}.dat'
    if not source.is_file(): parser.error('Missing PC source: '+str(source))
    if hashlib.sha256(source.read_bytes()).hexdigest() != verification['pc_audio_sha256'][str(number)]:
        parser.error('Unexpected PC source hash: '+str(source))
if args.check_only:
    print('All three PC source hashes match')
    raise SystemExit(0)
for track, number in tracks:
    source = args.music_dir/f'music{number}.dat'
    sha = hashlib.sha256(source.read_bytes()).hexdigest()
    assert sha == verification['pc_audio_sha256'][str(number)]
    work = ROOT/f'scratchpad/audio/{track}_pc'
    work.mkdir(parents=True, exist_ok=True)
    subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', str(source),
                    '-c:a', 'pcm_f32le', str(work/'decoded.wav')], check=True)
    grid = json.loads((ROOT/f'soundtrack/{track}.analysis.json').read_text())['constant_grid_hypothesis'].copy()
    shift = ({'courtesy': -0.051, 'focus': -0.0485}[track] if track != 'otis'
             else -comparison['estimated_trim']['start_seconds'])
    grid['phase_seconds'] = (grid['phase_seconds'] + shift) % (30/grid['bpm'])
    report = dict(source=str(source), sha256=sha, pc_track=number,
                  decoded=str((work/'decoded.wav').relative_to(ROOT)),
                  time_origin='First ffmpeg decoded PCM sample; no manual trim, shift or speed change',
                  cue_asset=f'assets/music{number}.cues', cue_offset_seconds=0,
                  compression_grid=grid,
                  grid_note='Approximate beat phase transferred for dictionary slicing only; does not shift playback or PC cues.')
    (ROOT/f'soundtrack/{track}.pc_source.json').write_text(json.dumps(report, indent=2)+'\n')
    print(track, sha)
