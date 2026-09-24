#!/usr/bin/env python3
"""Extended ADF fixtures with one save track having no valid MFM sector sync.

Models the aftermath of track damage, not timed power removal during a write.
Format follows Copperline's UAE-1ADF decoder: 12-byte header/descriptors;
normal tracks type 0, raw MFM type 1. Original boot/filesystem tracks untouched.
Run make_startup_save_fixtures.py first to seed valid current-identity slots.
"""
import hashlib,json,struct
from pathlib import Path
root=Path(__file__).resolve().parents[2];p=root/'scratchpad/trackloader/native_game'
source=(p/'save_latest.adf').read_bytes();rows=[]
assert len(source)==901120
for bad_track,expected in [(158,'075.01'),(159,'065.01')]:
 descriptors=bytearray();payload=bytearray()
 for track in range(160):
  data=bytes([0xaa])*12668 if track==bad_track else source[track*5632:(track+1)*5632]
  kind=int(track==bad_track)
  descriptors+=struct.pack('>HHII',0,kind,len(data),len(data)*8 if kind else 0)
  payload+=data
 disk=b'UAE-1ADF'+struct.pack('>HH',0,160)+descriptors+payload
 name=f'save_unreadable_{bad_track}';path=p/(name+'.adf');path.write_bytes(disk)
 (p/(name+'.toml')).write_text(f'[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = "{path}"\nwrite_protected = true\n')
 rows.append(dict(name=name,track=bad_track,expected_display=expected,sha256=hashlib.sha256(disk).hexdigest()))
(p/'unreadable_save_fixtures.json').write_text(json.dumps(rows,indent=2)+'\n');print(rows)
