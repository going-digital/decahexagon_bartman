#!/usr/bin/env python3
"""Audit production cue coverage and the C clock/index functions at song wrap."""
import ctypes as C
import json
import struct
import subprocess
import zlib
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader'
subprocess.run(['cc','-O2','-shared','-fPIC',str(root/'pc_pulse.c'),'-o',str(out/'cue_wrap.dylib')],check=True)
lib=C.CDLL(str(out/'cue_wrap.dylib'))
lib.pc_pcm_position.argtypes=[C.c_uint32,C.c_uint32,C.c_uint,C.c_uint];lib.pc_pcm_position.restype=C.c_uint32
lib.pc_pulse_cue_index_offset.argtypes=[C.c_uint32,C.c_uint32];lib.pc_pulse_cue_index_offset.restype=C.c_uint
rows=[]
descriptors=[line for line in (out/'native_game/tunes.i').read_text().splitlines() if line.startswith(' dc.l')]
for (track,index),descriptor in zip([('courtesy',1),('otis',2),('focus',3)],descriptors):
    fields=descriptor.split('dc.l ',1)[1].split(',');lead=int(fields[4]);cue_count=int(fields[5])
    raw=zlib.decompress((out/(track+'.fibonacci.deflate')).read_bytes(),-15)
    samples=struct.unpack_from('>I',raw,8)[0]
    cues=(root/f'assets/music{index}.cues').read_bytes();assert len(cues)==cue_count
    cases=0
    # A 512-sample DMA buffer may straddle the physical song end. Compare the
    # player clock with the sample sequence that would actually be emitted.
    for period in (296,298):
        for start in range(samples-511,samples):
            sequence=list(range(start,samples))+list(range(512-(samples-start)))
            for lines in (0,1,100,300,500,699,700,701,1000):
                elapsed=min((lines*227)//period,511)
                position=lib.pc_pcm_position(start,samples,lines,period)
                assert position==sequence[elapsed]
                cue=lib.pc_pulse_cue_index_offset(position,lead)
                assert cue==max(0,position-lead)//200
                cases+=1
    assert lib.pc_pcm_position(samples-1,samples,2,298)==0
    assert lib.pc_pulse_cue_index_offset(0,lead)==0
    tail=samples-(lead+len(cues)*200)
    rows.append(dict(track=track,samples=samples,lead_samples=lead,cue_bytes=len(cues),
                     no_cue_tail_samples=max(tail,0),no_cue_tail_seconds=max(tail,0)/12000,
                     unused_cue_extent_samples=max(-tail,0),unused_cue_extent_seconds=max(-tail,0)/12000,
                     wrap_cases=cases))
report=dict(status='Production cue coverage audited; host C wrap arithmetic passes',tracks=rows,
            limitations=['No waveform-to-PC cue alignment measured','Coverage differences are not automatically defects',
                         'Unsigned cue lookup returns zero beyond table; index resets with song position'])
(root/'docs/TRACKLOADER_CUE_WRAP_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
