#!/usr/bin/env python3
"""Larger spectral slice bank with independent Fibonacci blocks; host playback."""
import json
import numpy as np
from scipy.io import wavfile
from scipy.spatial.distance import cdist
import reuse_beats as reuse
from audio_sources import work_directory,compression_grid,pc_source
from codec_experiment import fib_encode,fib_decode


def main(optimal=False, sample_rate=None, track="courtesy", budgets=None, reserve_bytes=0):
    root=reuse.ROOT;work=work_directory(track)
    out=work/('compressed_beats_optimal' if optimal else 'compressed_beats')
    if sample_rate: out=work/'rate_test'/str(sample_rate)
    out.mkdir(parents=True,exist_ok=True)
    encoder=fib_encode
    if optimal:
        from compare_fib_encoders import load_encoder,encode
        fn=load_encoder()
        encoder=lambda q: encode(q,fn)
    if sample_rate:
        from scipy import signal
        original_rate,stereo=wavfile.read(work/'decoded.wav')
        sr=sample_rate
        x=signal.resample_poly(stereo.mean(axis=1),sr,original_rate)
        # Courtesy uses unity gain in the existing reference; keep that gain
        # fixed across rates, and reject clipping rather than renormalizing.
        gain = min(1., .98 / max(float(np.max(np.abs(x))), 1e-9)) if track != 'courtesy' else 1.
        assert np.max(np.abs(x * gain))<1
        x = x * gain
        q=np.clip(np.rint(x*127),-128,127).astype(np.int8)
        wavfile.write(out/'reference.wav',sr,q.astype(np.int16)*256)
    else:
        sr,pcm=wavfile.read(work/'beat_reuse/reference_16k_8bit.wav');q=(pcm//256).astype(np.int8)
    meta=compression_grid(track)
    edges=np.rint(np.arange(meta['phase_seconds'],len(q)/sr,30/meta['bpm'])*sr).astype(int)
    slices=[q[a:b] for a,b in zip(edges[:-1],edges[1:])]
    descriptors=reuse.features(slices,sr);dist=cdist(descriptors,descriptors,'sqeuclidean')
    # Prefix and tail remain verbatim; offsets for independent compressed samples
    # need four bytes each. Three 512-byte decoded buffers are budgeted explicitly.
    prefix=q[:edges[0]];suffix=q[edges[-1]:]
    fixed=len(prefix)+len(suffix)+32+4*(len(slices)+2)+1536
    slots=max(len(fib_encode(np.zeros(n,dtype=np.int8))) for n in set(map(len,slices)))
    report={'status':'host only; decoder runtime/code and disk-loading overhead unmeasured',
            'source_gain': gain if sample_rate else None, 'track': track,
            'sample_rate':sr,'reserve_bytes':reserve_bytes,'pcm_buffers_bytes':1536,'encoder':'block-optimal' if optimal else 'greedy','trials':[]}
    for budget in ([64,192] if budgets is None else budgets):
        k=int((budget*1024-fixed-4-reserve_bytes)//(slots+4))
        chosen,ids=reuse.choose(dist,k)
        bank=bytearray();offsets=[];decoded=[]
        for j in chosen:
            offsets.append(len(bank));data=encoder(slices[j]);bank.extend(data)
            decoded.append(fib_decode(data))
        offsets.append(len(bank))
        result=q.copy();uncompressed=q.copy()
        for i,(a,b) in enumerate(zip(edges[:-1],edges[1:])):
            result[a:b]=reuse.matched_length(decoded[ids[i]],b-a)
            uncompressed[a:b]=reuse.matched_length(slices[chosen[ids[i]]],b-a)
        wavfile.write(out/f'{budget:g}k_fibonacci.wav',sr,result.astype(np.int16)*256)
        wavfile.write(out/f'{budget:g}k_before_codec.wav',sr,uncompressed.astype(np.int16)*256)
        (out/f'{budget:g}k.bank').write_bytes(bank)
        (out/f'{budget:g}k.edges.s8').write_bytes(prefix.tobytes()+suffix.tobytes())
        (out/f'{budget:g}k.json').write_text(json.dumps({'offsets':offsets,'source_indices':chosen,
            'dictionary_ids':ids.tolist(),'boundaries':edges.tolist(),'prefix_length':len(prefix),
            'suffix_length':len(suffix)})+'\n')
        accounted=fixed+len(bank)+4*len(offsets)
        assert accounted<=budget*1024
        f=reuse.features([result[a:b] for a,b in zip(edges[:-1],edges[1:])],sr)
        report['trials'].append({'budget_kib':budget,'dictionary_slices':k,'accounted_bytes':accounted,
                                'feature_mse':float(np.mean((f-descriptors)**2)),
                                'preview':str((out/f'{budget:g}k_fibonacci.wav').relative_to(root))})
    target=(root/'soundtrack'/f'{track}.rate_{sample_rate}.json' if sample_rate else
            root/('soundtrack/courtesy.compressed_beats_optimal.json' if optimal else 'soundtrack/courtesy.compressed_beats.json'))
    if budgets is not None:
        target = target.with_name(target.stem + '_budget_' + '_'.join(f'{b:g}' for b in budgets) + '.json')
    if pc_source(track): target=target.with_name(target.stem+'_pc.json')
    target.write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(report,indent=2),flush=True)
    return report

if __name__=='__main__':
    import argparse
    parser=argparse.ArgumentParser()
    parser.add_argument('--optimal',action='store_true')
    parser.add_argument('--track', choices=['courtesy', 'focus', 'otis'], default='courtesy')
    parser.add_argument('--sample-rate', type=int)
    parser.add_argument('--budget-kib', type=float, action='append')
    parser.add_argument('--reserve-bytes', type=int, default=0, help='Reserve additional bytes for final AUD1 metadata and padding')
    args = parser.parse_args()
    if args.reserve_bytes < 0: parser.error('--reserve-bytes must be nonnegative')
    if args.track != 'courtesy' and not args.sample_rate:
        parser.error('Other tracks require --sample-rate')
    main(args.optimal, args.sample_rate, args.track, args.budget_kib, args.reserve_bytes)
