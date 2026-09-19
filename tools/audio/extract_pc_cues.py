#!/usr/bin/env python3
"""Extract the verified desktop track-1 cue literal, without analysing audio at runtime."""
import hashlib,json
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2]
p=json.loads((ROOT/'scratchpad/pc_verification/evidence/provenance.json').read_text())
b=Path(p['binary']).read_bytes()
assert hashlib.sha256(b).hexdigest()==p['sha256']
# __TEXT has file offset 0; RIP-relative LEA at 0x10005d3fb.
a=0x176f92
values=[int(v) for v in b[a:b.index(b'\0',a)].split(b',') if v.strip()]
assert len(values)==11441 and min(values)==0 and max(values)==80
out=ROOT/'assets/music1.cues';out.write_bytes(bytes(values))
(ROOT/'soundtrack/pc_cues.json').write_text(json.dumps(dict(
    source_sha256=p['sha256'],literal_address='0x100176f92',track_id=1,
    entries=len(values),entries_per_second=60,bytes=len(values),
    sha256=hashlib.sha256(out.read_bytes()).hexdigest(),
    soundtrack='Courtesy',source_to_pc_offset_samples_12000=-612,
    alignment='MP3 at 10/30/90/170 seconds matches PC music1.dat at 9.949/29.949/89.949/169.949 seconds; 2 kHz waveform cross-correlation 0.9084..0.9639',
    after_table='zero (PC musicinit zeroes unused cue entries)'),indent=2)+'\n')
print(len(values),'cue bytes')
