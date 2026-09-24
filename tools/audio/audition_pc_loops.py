#!/usr/bin/env python3
"""Audition unmodified song wraps reconstructed from production TUN1 payloads."""
import hashlib
import json
import struct
import sys
import wave
import zlib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT/'tools/trackloader'))
from asset_identity import verify_assets
from fib_reference import fib_decode

verify_assets('banks')
verify_assets('disk_assets')
out = ROOT/'scratchpad/audio/pc_loop_audition'
out.mkdir(parents=True, exist_ok=True)
rows, sections = [], []

def read_pcm(path):
    with wave.open(str(path)) as f:
        assert (f.getnchannels(), f.getsampwidth(), f.getframerate()) == (1, 2, 12000)
        data = f.readframes(f.getnframes())
    values = [v[0] for v in struct.iter_unpack('<h', data)]
    assert all(v % 256 == 0 for v in values)
    return bytes((v//256) & 255 for v in values)

def signed(value):
    return value if value < 128 else value-256

def excerpt(track, name, pcm):
    # Four seconds before and after the exact wrap, with no fade or crossfade.
    clip = pcm[-48000:] + pcm[:48000]
    filename = f'{track}_{name}_wrap.wav'
    with wave.open(str(out/filename), 'wb') as f:
        f.setparams((1, 2, 12000, 0, 'NONE', 'not compressed'))
        f.writeframes(b''.join(struct.pack('<h', signed(v)*256) for v in clip))
    jump = abs(signed(pcm[0])-signed(pcm[-1]))
    local = clip[48000-120:48000+120]
    nearby = max(abs(signed(b)-signed(a)) for a, b in zip(local, local[1:]))
    return dict(file=filename, boundary_jump_8bit=jump, nearby_max_step_8bit=nearby)

for track in ('courtesy', 'otis', 'focus'):
    path = ROOT/f'scratchpad/trackloader/{track}.fibonacci.deflate'
    packed = path.read_bytes()
    raw = zlib.decompress(packed, -15)
    magic, rate, samples, entries, count, pcm_bytes = struct.unpack_from('>4s5I', raw)
    assert magic == b'TUN1' and rate == 12000
    offsets = struct.unpack_from('>'+str(entries+1)+'I', raw, 24)
    start = 24+4*(entries+1)
    sequence = list(struct.iter_unpack('>HH', raw[start:start+4*count]))
    bank = fib_decode(raw[start+4*count:])
    assert len(bank) == pcm_bytes == offsets[-1]
    assert offsets[0] == 0 and all(a < b for a, b in zip(offsets, offsets[1:]))
    assert all(i < entries and 0 < n <= offsets[i+1]-offsets[i] for i, n in sequence)
    pcm = b''.join(bank[offsets[i]:offsets[i]+n] for i, n in sequence)
    assert len(pcm) == samples
    auditions = ROOT/'scratchpad/audio/codec_audition_budget_250_preboost_pc_tracks'
    assert pcm == read_pcm(auditions/f'{track}_expanded_fibonacci.wav'), 'Disk/audition mismatch'
    reference = read_pcm(auditions/f'{track}_reference.wav')
    assert len(reference) == samples
    original = excerpt(track, 'reference', reference)
    encoded = excerpt(track, 'disk', pcm)
    rows.append(dict(track=track, samples=samples, payload_sha256=hashlib.sha256(packed).hexdigest(),
                     reference=original, disk=encoded, disk_matches_full_audition=True))
    players = ''.join(f'<p>{label}</p><audio controls preload="none" src="{item["file"]}"></audio>'
                      for label, item in [('PC reference (12 kHz, boosted)', original),
                                          ('Production Fibonacci payload', encoded)])
    sections.append(f'<section><h2>{track.title()}</h2>{players}</section>')
report = dict(status='Production payload reconstructions match full auditions sample-for-sample',
              boundary_seconds=4, tracks=rows,
              limitations=['Host PCM only; no Paula filtering or hardware capture',
                           'Boundary step measurements do not establish audible quality'])
(ROOT/'docs/TRACKLOADER_LOOP_AUDITION_RESULTS.json').write_text(json.dumps(report, indent=2)+'\n')
(out/'index.html').write_text('''<!doctype html><meta charset="utf-8"><title>PC soundtrack loop auditions</title>
<style>body{font:17px system-ui;background:#171923;color:#eee;max-width:850px;margin:40px auto}section{padding:20px;background:#252936;margin:20px 0}audio{width:100%}</style>
<h1>Song loop boundaries</h1><p>Each clip contains the final four seconds followed immediately by the first four seconds. The song wraps at 0:04. No fades, trimming or crossfades have been added.</p>
<p>Compare the boosted 12 kHz PC reference against the decoded production disk payload. These are host previews, without Paula filtering. Switching players within a track preserves position.</p>'''+''.join(sections)+'''
<script>let old;document.querySelectorAll('audio').forEach(a=>a.addEventListener('play',()=>{if(old&&old!==a){if(old.closest('section')===a.closest('section'))a.currentTime=old.currentTime;old.pause()}old=a}));</script>''')
print(json.dumps(report, indent=2))
print(out/'index.html')
