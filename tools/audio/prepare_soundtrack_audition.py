#!/usr/bin/env python3
"""Render game-gain WAVs and a local listening page from compact PCM trials."""
import json
from pathlib import Path
import numpy as np
from scipy.io import wavfile
from boost_pcm import boost_pcm

ROOT = Path(__file__).resolve().parents[2]
work = ROOT / 'scratchpad/audio/audition'
work.mkdir(parents=True, exist_ok=True)
rows = []
sections = []
for track in ['courtesy', 'focus', 'otis']:
    report_path = ROOT / 'soundtrack' / ('pcm_reuse_trials.json' if track == 'courtesy' else f'{track}.pcm_reuse_trials.json')
    report = json.loads(report_path.read_text())
    trial = next(t for t in report['trials'] if t['dictionary_slices'] == 96) if track == 'courtesy' else report['trials'][0]
    preview = ROOT / trial['preview']
    sources = {'compressed': preview, 'reference': ROOT / 'scratchpad/audio' / track / 'rate_test/12000/reference.wav'}
    files = {}
    for label, path in sources.items():
        rate, audio = wavfile.read(path)
        assert rate == 12000 and audio.dtype == np.int16 and audio.ndim == 1
        assert np.all(audio % 256 == 0)
        samples = np.frombuffer(boost_pcm((audio // 256).astype(np.int8).tobytes()), dtype=np.int8)
        dest = work / f'{track}_{label}.wav'
        wavfile.write(dest, rate, samples.astype(np.int16) * 256)
        files[label] = dest.name
    assert wavfile.read(sources['reference'])[1].shape == audio.shape
    rows.append({'track': track, 'dictionary_slices': trial['dictionary_slices'], 'asset_bytes': trial['asset_bytes'], 'seconds': len(audio)/rate, 'files': files, 'asset_sha256': trial['sha256']})
    sections.append(f'<section><h2>{track.title()}</h2><p>{trial["dictionary_slices"]} slices · {trial["asset_bytes"]:,} bytes · {len(audio)/rate:.1f} seconds</p>' + ''.join(f'<p>{label.title()}<br><audio controls preload="metadata" src="{name}"></audio></p>' for label,name in files.items()) + '</section>')
(work / 'index.html').write_text('''<!doctype html><meta charset="utf-8"><title>Soundtrack auditions</title><style>body{font:18px system-ui;max-width:850px;margin:40px auto;background:#171923;color:#eee}audio{width:100%}section{padding:16px;margin:20px 0;background:#252936}button{padding:10px}</style><h1>Soundtrack auditions</h1><p>12 kHz mono. Both versions use the game’s volume boost. Reference is the original recording downsampled to the same format; compressed is reconstructed from the reusable sample bank. Pause before switching; the next player starts at the same time.</p>''' + ''.join(sections) + '''<script>let previous;document.querySelectorAll('audio').forEach(a=>a.addEventListener('play',()=>{if(previous&&previous!==a){if(previous.closest('section')===a.closest('section'))a.currentTime=previous.currentTime;previous.pause()}previous=a}));</script>''')
(ROOT / 'soundtrack/other_tracks_audition.json').write_text(json.dumps({'status': 'Prepared for listening; quality not yet accepted; no target playback claimed', 'sample_rate': 12000, 'runtime_decompression': False, 'gain': 'Same boost_pcm soft-knee curve as game, applied equally to reference and reconstruction', 'tracks': rows}, indent=2)+'\n')
print(work / 'index.html')
