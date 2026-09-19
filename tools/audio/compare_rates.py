#!/usr/bin/env python3
"""Equal-budget rate comparison with common 16 kHz spectral evaluation."""
import json
import numpy as np
from scipy import signal
from scipy.io import wavfile
import reuse_beats as reuse
from compress_beat_bank import main as build

root=reuse.ROOT
work=root/'scratchpad/audio/courtesy'
sr,original=wavfile.read(work/'beat_reuse/reference_16k_8bit.wav')
reference=original.astype(float)/256
meta=json.loads((root/'soundtrack/courtesy.analysis.json').read_text())['constant_grid_hypothesis']
edges=np.rint(np.arange(meta['phase_seconds'],len(reference)/sr,30/meta['bpm'])*sr).astype(int)
ref_features=reuse.features([reference[a:b] for a,b in zip(edges[:-1],edges[1:])])
reports=[]
for rate in [12000,14000,16000]:
    result=build(optimal=True,sample_rate=rate)
    for budget in [None,64,192]:
        source=work/'rate_test'/str(rate)/('reference.wav' if budget is None else f'{budget}k_fibonacci.wav')
        s,y=wavfile.read(source)
        common=signal.resample_poly(y.astype(float)/256,sr,s)[:len(reference)]
        assert len(common)==len(reference)
        wavfile.write(source.with_name(source.stem+'_at16k.wav'),sr,(common/128).astype(np.float32))
        f=reuse.features([common[a:b] for a,b in zip(edges[:-1],edges[1:])])
        error=float(np.mean((f-ref_features)**2))
        if budget is None:result['reference_common_feature_mse']=error
        else:
            trial=next(t for t in result['trials'] if t['budget_kib']==budget)
            trial['common_16k_feature_mse']=error
    if rate==16000:
        _,check=wavfile.read(work/'rate_test/16000/reference.wav')
        assert np.array_equal(check,original),'16 kHz reference regression'
        _,old=wavfile.read(work/'compressed_beats_optimal/192k_fibonacci.wav')
        _,new=wavfile.read(work/'rate_test/16000/192k_fibonacci.wav')
        assert np.array_equal(old,new),'16 kHz bank regression'
    reports.append(result)
(root/'soundtrack/courtesy.rate_comparison.json').write_text(json.dumps({
    'status':'host only; exact nominal rates, no Paula period quantization or CPU timing',
    'comparison':'common original 16 kHz 8-bit mono reference, unity source gain',
    'rates':reports},indent=2)+'\n')
print('Common-rate scoring and 16 kHz regression checks passed')
