#!/usr/bin/env python3
"""Extract the verified PC cue tables without audio-analysis scratch files."""
import argparse
import hashlib
import json
import struct
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--binary', type=Path, required=True, help='Verified PC game executable')
p.add_argument('--output-dir', type=Path, default=ROOT/'assets')
p.add_argument('--check-only', action='store_true')
a = p.parse_args()
verification = json.loads((ROOT/'soundtrack/pc_track_verification.json').read_text())
lock = json.loads((ROOT/'soundtrack/trackloader_assets.lock.json').read_text())
try:
    data = a.binary.read_bytes()
except OSError as error:
    p.error(str(error))
digest = hashlib.sha256(data).hexdigest()
if digest != verification['source_sha256']:
    p.error('Unsupported PC executable hash; no cue files written')
outputs, rows = [], []
# Instruction offsets from the verified PC disassembly. Derive each literal
# through its signed RIP-relative displacement rather than a cached text dump.
for track, instruction in [(1, 0x5d3fb), (2, 0x5d42b), (3, 0x5d443)]:
    if data[instruction:instruction+3] != b'\x48\x8d\x35':
        p.error('Cue instruction mismatch')
    offset = instruction+7+struct.unpack_from('<i', data, instruction+3)[0]
    if not 0 <= offset < len(data):
        p.error('Cue literal outside executable')
    try:
        end = data.index(b'\0', offset)
        values = [int(v) for v in data[offset:end].split(b',') if v.strip()]
        if track == 2:
            values = [int(v*0.6) for v in values] # PC double multiply, then truncation
        cue = bytes(values)
    except (ValueError, OverflowError) as error:
        p.error('Invalid cue literal: '+str(error))
    expected = next(r for r in lock['disk_assets'] if r['path'] == f'assets/music{track}.cues')
    sha = hashlib.sha256(cue).hexdigest()
    if len(cue) != expected['bytes'] or sha != expected['sha256']:
        p.error('Cue table differs from accepted assets; no cue files written')
    outputs.append((f'music{track}.cues', cue))
    rows.append(dict(track=track, entries=len(cue), sha256=sha, literal_offset=offset))
# Validate every table before creating the output directory or writing anything.
if not a.check_only:
    a.output_dir.mkdir(parents=True, exist_ok=True)
    for name, cue in outputs:
        path = a.output_dir/name
        if not path.exists() or path.read_bytes() != cue:
            path.write_bytes(cue)
print(json.dumps(dict(status='All three PC cue tables match accepted hashes',
                      executable_sha256=digest, check_only=a.check_only,
                      output_dir=str(a.output_dir.resolve()), tracks=rows), indent=2))
