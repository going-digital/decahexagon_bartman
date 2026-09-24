#!/usr/bin/env python3
"""Require current-image evidence for all-PC soundtrack loading and endurance."""
import hashlib
import json
import struct
import zlib
from pathlib import Path
from asset_identity import verify_assets

ROOT = Path(__file__).resolve().parents[2]

def report(name):
    return json.loads((ROOT/'docs'/name).read_text())

verify_assets('banks')
verify_assets('disk_assets')
image = ROOT/'scratchpad/trackloader/native_game/native_menu.adf'
digest = hashlib.sha256(image.read_bytes()).hexdigest()
assert report('TRACKLOADER_NATIVE_BOOT_RESULTS.json')['adf_sha256'] == digest
for standard in ('PAL', 'NTSC'):
    suffix = '_NTSC' if standard == 'NTSC' else ''
    switching = report('TRACKLOADER_OFS_SWITCHING_RESULTS'+suffix+'_OFS_SWITCHING.json')
    assert switching['adf_sha256'] == digest, 'Stale switching evidence: '+standard
    case = switching['scenarios']['ofs_switching']
    expected = ['DF0:'+track+'NATIVE-TUNE-READY' for track in ('courtesy', 'otis', 'focus', 'courtesy')]
    assert case['events'] == expected
    assert (ROOT/case['log']).read_text().splitlines() == ['NATIVE-SAVE-DEFAULT', 'NATIVE-GAME-ENTRY']+expected

# Read the descriptors used to build the current image, not historical offsets.
descriptors = [line for line in (ROOT/'scratchpad/trackloader/native_game/tunes.i').read_text().splitlines() if line.startswith(' dc.l')]
assert len(descriptors) == 3
rows = []
for number, (track, descriptor) in enumerate(zip(('courtesy', 'otis', 'focus'), descriptors), 1):
    source = json.loads((ROOT/f'soundtrack/{track}.pc_source.json').read_text())
    assert source['pc_track'] == number and source['cue_offset_seconds'] == 0
    assert hashlib.sha256(Path(source['source']).read_bytes()).hexdigest() == source['sha256']
    assert int(descriptor.split('dc.l ', 1)[1].split(',')[4]) == 0
    raw = zlib.decompress((ROOT/f'scratchpad/trackloader/{track}.fibonacci.deflate').read_bytes(), -15)
    magic, rate, samples = struct.unpack_from('>4sII', raw)
    assert magic == b'TUN1' and rate == 12000
    suffix = '_'+track.upper() if track != 'courtesy' else ''
    loop = report('TRACKLOADER_NTSC_LOOP_RESULTS'+suffix+'.json')
    assert loop['source_sha256'] == digest, 'Stale endurance evidence: '+track
    assert loop['song_samples'] == samples
    assert loop['video_timing'] == [60, 262, 200, 298, 447]
    trials = loop['trials']
    assert len(trials) >= 2
    assert all(t['track'] == track and t['underruns'] == 0 and t['running'] == 1 for t in trials)
    assert all(b['blocks'] > a['blocks'] for a, b in zip(trials, trials[1:]))
    consumed = (trials[-1]['blocks']-trials[0]['blocks'])*512
    assert consumed > samples
    rows.append(dict(track=track, song_samples=samples, consumed_samples=consumed, underruns=0))
result = dict(status='Current all-PC image: PAL/NTSC switching and all three NTSC endurance checks pass',
              adf_sha256=digest, tracks=rows,
              limitations=['Endurance bypasses collisions in emulator RAM',
                           'Silent tests do not assess audible quality or cue alignment',
                           'No physical hardware validation'])
(ROOT/'docs/TRACKLOADER_PC_AUDIO_RELEASE_RESULTS.json').write_text(json.dumps(result, indent=2)+'\n')
print(json.dumps(result, indent=2))
