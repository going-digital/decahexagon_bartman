#!/usr/bin/env python3
"""Copy the native ADF with corrupt Courtesy data or an oversized file header.

Exercises the resident packed-payload check, without changing the release image
or introducing a disk-block checksum failure earlier in DosIO.
"""
import argparse
import hashlib
import json
import struct
from pathlib import Path
from check_adf_layout import inspect

parser=argparse.ArgumentParser()
parser.add_argument('--oversize',action='store_true',help='Advertise an oversized Courtesy file instead of corrupting its payload')
args=parser.parse_args()
root = Path(__file__).resolve().parents[2]
work = root / 'scratchpad/trackloader'
native = work / 'native_game'
expected = {t: (work / f'{t}.fibonacci.deflate').read_bytes()
            for t in ('courtesy', 'focus', 'otis')}
expected.update(game=(native / 'game.deflate').read_bytes(),
                save_a=bytes([51])*512, save_b=bytes([34])*512)
reservations = json.loads((root / 'docs/TRACKLOADER_RESERVED_LAYOUT.json').read_text())['reservations']
data = bytearray((native / 'native_menu.adf').read_bytes())
layout = inspect(data, expected, reservations)
sector = layout['files']['courtesy']['header' if args.oversize else 'data_sectors']
if args.oversize:
    struct.pack_into('>I',data,sector*512+324,0x100000)
else:
    sector=sector[0]
    data[sector*512+24] ^= 1
words = list(struct.unpack_from('>128I', data, sector*512))
words[5] = 0
words[5] = (-sum(words)) & 0xffffffff
struct.pack_into('>128I', data, sector*512, *words)
if not args.oversize:
    payload = bytearray(expected['courtesy']); payload[0] ^= 1
    expected['courtesy'] = payload
    inspect(data, expected, reservations)
stem='ofs_oversize' if args.oversize else 'load_failure'
path = native / (stem+'.adf')
path.write_bytes(data)
(native / (stem+'.toml')).write_text(
    f'[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = "{path}"\nwrite_protected = true\n')
print(json.dumps(dict(path=str(path), corrupted_sector=sector,
                     sha256=hashlib.sha256(data).hexdigest())))
