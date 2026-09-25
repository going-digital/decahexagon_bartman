#!/usr/bin/env python3
"""Compare the production C producer to every sample of the accepted audition."""
import ctypes as C
import struct
import subprocess
import tempfile
from pathlib import Path
import numpy as np
from scipy.io import wavfile
from locate_ending_focus import ROOT, decode

with tempfile.TemporaryDirectory() as tmp:
    lib=Path(tmp)/'stretch.dylib'
    subprocess.run(['cc','-shared','-fPIC','-O2','-Wall','-Wextra','-Werror',str(ROOT/'fib_pcm.c'),'-o',str(lib)],check=True)
    dll=C.CDLL(str(lib))
    class Song(C.Structure):
        _fields_=[(n,C.c_void_p) for n in ('sequence','offsets','bank','edges','raw')]+[('pcm_split',C.c_uint),('seq_count',C.c_ushort),('seq_index',C.c_ushort),('output_left',C.c_ushort)]
    class Stretch(C.Structure):
        _fields_=[('source',Song),('offsets',C.c_void_p)]+[(n,C.c_uint) for n in ('count','total','position')]+[('previous',C.c_byte*256),('grain',C.c_byte*512),('output',C.c_byte*256)]
    pcm=np.clip(np.rint(decode(ROOT/'scratchpad/audio/codec_audition_budget_250_preboost_pc_tracks/focus_expanded_fibonacci.wav')*128),-128,127).astype(np.int8).tobytes()
    # Deliberately awkward slice length exercises reads across dictionary edges.
    chunks=[pcm[i:i+997] for i in range(0,len(pcm),997)]
    offsets=[0];bank=b''
    for chunk in chunks:
        bank+=chunk+b'\0'*(len(chunk)%2);offsets.append(len(bank))
    off=32;seq=off+4*len(offsets);data=seq+4*len(chunks)
    blob=struct.pack('>4s7I',b'FBP1',len(pcm),len(chunks),len(chunks),off,seq,data,data+len(bank))+b''.join(struct.pack('>I',v) for v in offsets)+b''.join(struct.pack('>HH',i,len(v)) for i,v in enumerate(chunks))+bank
    buf=C.create_string_buffer(blob);song=Song()
    assert dll.fib_song_init(C.byref(song),buf,len(blob))==1
    page=ROOT/'scratchpad/audio/ending_focus_overlap'
    starts=struct.unpack('>3645I',(page/'grain_512_search_256.offsets').read_bytes())
    schedule=(C.c_uint*len(starts))(*starts);state=Stretch()
    assert dll.fib_stretch_init(C.byref(state),C.byref(song),len(pcm),schedule,len(starts),932885)==1
    output=bytearray()
    for n in [1,255,513]+[512]*1823:
        block=(C.c_ubyte*n)();dll.fib_stretch_read(C.byref(state),block,n);output.extend(block)
    _,expected=wavfile.read(page/'grain_512_search_256.wav')
    reference=(expected//256).astype(np.int8).tobytes()
    assert bytes(output[:len(reference)])==reference
    assert not any(output[len(reference):])
    remaining=C.c_uint(len(pcm));backwards=bytearray()
    while remaining.value:
        block=(C.c_ubyte*512)()
        dll.fib_song_read_reverse(C.byref(song),C.byref(remaining),block,512)
        backwards.extend(block)
    assert bytes(backwards[:len(pcm)])==pcm[::-1]
    assert not any(backwards[len(pcm):])
    dll.fib_song_read_reverse(C.byref(song),C.byref(remaining),block,512)
    assert not any(block) and not remaining.value
    assert buf.raw[:len(blob)]==blob
    print('PASS: full Focus reversed exactly across dictionary boundaries, cached bank unchanged, tail silence')
    # Hardware path must copy unscaled samples to two independent voices.
    assert dll.fib_stretch_init(C.byref(state),C.byref(song),len(pcm),schedule,len(starts),932885)==1
    for k, offset in enumerate(starts):
        a=(C.c_ubyte*256)();b=(C.c_ubyte*256)()
        dll.fib_stretch_channels(C.byref(state),a,b)
        n=min(256,932885-k*256)
        expected_a=pcm[offset:offset+n] if not k else pcm[starts[k-1]+256:starts[k-1]+256+n]
        expected_b=bytes(n) if not k else pcm[offset:offset+n]
        assert bytes(a)==expected_a+bytes(256-n)
        assert bytes(b)==expected_b+bytes(256-n)
    dll.fib_stretch_channels(C.byref(state),a,b)
    assert not any(a) and not any(b)
    print('PASS: all 3645 hardware hops contain exact unscaled source samples; final padding/silence')
    assert dll.fib_stretch_init(C.byref(state),C.byref(song),512,schedule,len(starts),932885)==0
    print('PASS: 932885 samples byte-identical; irregular blocks, slice boundaries, tail silence and invalid source bounds')
