#!/usr/bin/env python3
"""Prepare disk payloads and target inflate proofs from accepted AUD1 banks."""
import argparse
import json
import struct
import shutil
from fib_reference import fib_decode
from asset_identity import verify_assets
import subprocess
import sys
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--execram-source',type=Path,required=True)
p.add_argument('--check-only',action='store_true',help='Validate all source banks without writing outputs')
a=p.parse_args()
root=Path(__file__).resolve().parents[2]
source=root/'scratchpad/audio/codec_audition_budget_250_preboost_pc_tracks'
needed=[source/(track+'_expanded_fibonacci.aud') for track in ('courtesy','focus','otis')]
needed += [a.execram_source/'src/backends/zultra_vendor/libzultra.c',
           a.execram_source/'src/musashi_vendor/m68kmake.c',
           a.execram_source/'stubs/inflate/inflate_core.s']
missing=[str(path) for path in needed if not path.is_file()]
missing += [tool+' (PATH)' for tool in ('cc','vasmm68k_mot') if not shutil.which(tool)]
if missing:p.error('Missing source inputs:\n'+'\n'.join(missing))
try:verify_assets('banks')
except (ValueError,OSError) as error:p.error(str(error))
# Validate every input before measure_payloads writes the library or any payload.
for path in needed[:3]:
    try:
        data=path.read_bytes()
        magic,size=struct.unpack_from('>4sI',data)
        if magic!=b'AUD1' or size>len(data)-8:raise ValueError('Invalid AUD1 header')
        meta=json.loads(data[8:8+size])
        if meta['gain_stage']!='before' or meta['rate']!=12000:
            raise ValueError('Expected preboosted 12000 Hz bank')
        pcm=fib_decode(data[8+size:]);offsets=meta['offsets']
        if not offsets or offsets[0]!=0 or offsets[-1]!=len(pcm):
            raise ValueError('PCM offsets do not cover bank')
        if any(b<=a for a,b in zip(offsets,offsets[1:])):
            raise ValueError('Non-increasing PCM offsets')
        total=0
        for index,count in meta['sequence']:
            if not 0<=index<len(offsets)-1 or not 0<count<=65535:
                raise ValueError('Invalid sequence entry')
            if count>offsets[index+1]-offsets[index]:raise ValueError('Sequence exceeds slice')
            total+=count
        if total!=meta['samples']:raise ValueError('Sequence sample count mismatch')
    except (ValueError,KeyError,TypeError,IndexError,struct.error) as error:
        p.error(str(path)+': '+str(error))
print('All three accepted AUD1 source banks validate',flush=True)
if a.check_only:sys.exit(0)
for script,args in [('measure_payloads.py',['--execram-source',str(a.execram_source.resolve())]),
                    ('build_inflate_probe.py',['--execram-source',str(a.execram_source.resolve())]),
                    ('run_inflate_probe.py',[])]:
    print('Running',script,flush=True)
    subprocess.run([sys.executable,str(root/'tools/trackloader'/script),*args],cwd=root,check=True)
verify_assets('disk_assets')
print('Prepared Zultra payloads, library and target inflate proofs from accepted AUD1 banks')
