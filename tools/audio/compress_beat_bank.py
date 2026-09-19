#!/usr/bin/env python3
"""Larger spectral slice bank with independent Fibonacci blocks; host playback."""
import json
import numpy as np
from scipy.io import wavfile
from scipy.spatial.distance import cdist
import reuse_beats as reuse
from codec_experiment import fib_encode,fib_decode


def main():
    root=reuse.ROOT;work=root/'scratchpad/audio/courtesy'
    out=work/'compressed_beats';out.mkdir(exist_ok=True)
    sr,pcm=wavfile.read(work/'beat_reuse/reference_16k_8bit.wav');q=(pcm//256).astype(np.int8)
    meta=json.loads((root/'soundtrack/courtesy.analysis.json').read_text())['constant_grid_hypothesis']
    edges=np.rint(np.arange(meta['phase_seconds'],len(q)/sr,30/meta['bpm'])*sr).astype(int)
    slices=[q[a:b] for a,b in zip(edges[:-1],edges[1:])]
    descriptors=reuse.features(slices);dist=cdist(descriptors,descriptors,'sqeuclidean')
    # Prefix and tail remain verbatim; offsets for independent compressed samples
    # need four bytes each. Three 512-byte decoded buffers are budgeted explicitly.
    prefix=q[:edges[0]];suffix=q[edges[-1]:]
    fixed=len(prefix)+len(suffix)+32+4*(len(slices)+2)+1536
    slots=max(len(fib_encode(np.zeros(n,dtype=np.int8))) for n in set(map(len,slices)))
    report={'status':'host only; decoder runtime/code and disk-loading overhead unmeasured',
            'pcm_buffers_bytes':1536,'trials':[]}
    for budget in [64,192]:
        k=int((budget*1024-fixed-4)//(slots+4))
        chosen,ids=reuse.choose(dist,k)
        bank=bytearray();offsets=[];decoded=[]
        for j in chosen:
            offsets.append(len(bank));data=fib_encode(slices[j]);bank.extend(data)
            decoded.append(fib_decode(data))
        offsets.append(len(bank))
        result=q.copy();uncompressed=q.copy()
        for i,(a,b) in enumerate(zip(edges[:-1],edges[1:])):
            result[a:b]=reuse.matched_length(decoded[ids[i]],b-a)
            uncompressed[a:b]=reuse.matched_length(slices[chosen[ids[i]]],b-a)
        wavfile.write(out/f'{budget}k_fibonacci.wav',sr,result.astype(np.int16)*256)
        wavfile.write(out/f'{budget}k_before_codec.wav',sr,uncompressed.astype(np.int16)*256)
        (out/f'{budget}k.bank').write_bytes(bank)
        (out/f'{budget}k.edges.s8').write_bytes(prefix.tobytes()+suffix.tobytes())
        (out/f'{budget}k.json').write_text(json.dumps({'offsets':offsets,'source_indices':chosen,
            'dictionary_ids':ids.tolist(),'boundaries':edges.tolist(),'prefix_length':len(prefix),
            'suffix_length':len(suffix)})+'\n')
        accounted=fixed+len(bank)+4*len(offsets)
        assert accounted<=budget*1024
        f=reuse.features([result[a:b] for a,b in zip(edges[:-1],edges[1:])])
        report['trials'].append({'budget_kib':budget,'dictionary_slices':k,'accounted_bytes':accounted,
                                'feature_mse':float(np.mean((f-descriptors)**2)),
                                'preview':str((out/f'{budget}k_fibonacci.wav').relative_to(root))})
    (root/'soundtrack/courtesy.compressed_beats.json').write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(report,indent=2))

if __name__=='__main__':main()
