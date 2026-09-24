#!/usr/bin/env python3
"""Compare disk-only Fibonacci/IMA coding of original-source sample dictionaries."""
import argparse
import json
import struct
import subprocess
from pathlib import Path
import numpy as np
from scipy.io import wavfile
from codec_experiment import ROOT, fib_decode, metric
from compare_fib_encoders import load_encoder, encode
from boost_pcm import boost_pcm
from audio_sources import work_directory,pc_otis,pc_source

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--budget-kib', type=float, default=192)
parser.add_argument('--codec', choices=['both', 'fibonacci', 'ima_adpcm'], default='both')
parser.add_argument('--previous-budget-kib', type=float, default=192)
parser.add_argument('--max-package-kib', type=float)
parser.add_argument('--gain-stage', choices=['before', 'after'], default='before')
args = parser.parse_args()
codecs = ['fibonacci', 'ima_adpcm'] if args.codec == 'both' else [args.codec]
expanded = args.budget_kib != 192
tag = f'_budget_{args.budget_kib:g}' if expanded else ''
qualities = ['expanded'] if expanded else ['compact', 'fuller']
tag += '_preboost' if args.gain_stage == 'before' else ''
tag += '_pc_tracks' if pc_source('courtesy') else ('_pc_otis' if pc_otis() else '')
out = ROOT/('scratchpad/audio/codec_audition' + tag)
out.mkdir(parents=True, exist_ok=True)
encoder = load_encoder()
reports=[]
sections=[]
for track in ['courtesy','focus','otis']:
    work=work_directory(track)
    meta=json.loads((work/f'rate_test/12000/{args.budget_kib:g}k.json').read_text())
    rate, reference=wavfile.read(work/'rate_test/12000/reference.wav')
    q=(reference//256).astype(np.int8)
    if args.gain_stage == "before":
        q=np.frombuffer(boost_pcm(q.tobytes()),dtype=np.int8)
    boundaries=meta['boundaries']; lengths=np.diff(boundaries).tolist()
    originals=[q[a:b] for a,b in zip(boundaries[:-1],boundaries[1:])]
    trial=None
    if not expanded or not pc_source(track):
        compact=json.loads((ROOT/'soundtrack'/('pcm_reuse_trials.json' if track=='courtesy' else f'{track}.pcm_reuse_trials.json')).read_text())
        trial=next(t for t in compact['trials'] if t['dictionary_slices']==96) if track=='courtesy' else compact['trials'][0]
    players=[]
    def preview(name, samples):
        dest=out/f'{track}_{name}.wav'
        boosted=samples if args.gain_stage == "before" else np.frombuffer(boost_pcm(samples.tobytes()),dtype=np.int8)
        wavfile.write(dest,rate,boosted.astype(np.int16)*256)
        players.append(f'<p>{name}<br><audio controls preload="none" src="{dest.name}"></audio></p>')
        return str(dest.relative_to(ROOT))
    preview('reference',q)
    if not pc_source(track):
        _,old=wavfile.read(ROOT/trial['preview'])
        old=(old//256).astype(np.int8)
        if args.gain_stage == 'before': old=np.frombuffer(boost_pcm(old.tobytes()),dtype=np.int8)
        preview('previous_candidate',old)
    if expanded and not pc_source(track):
        for codec in codecs:
            previous_tag = f'_budget_{args.previous_budget_kib:g}' if args.previous_budget_kib != 192 else ''
            previous_quality = 'expanded' if previous_tag else 'fuller'
            sr, old = wavfile.read(ROOT/f'scratchpad/audio/codec_audition{previous_tag}/{track}_{previous_quality}_{codec}.wav')
            assert sr == rate
            dest = out/f'{track}_previous_fuller_{codec}.wav'
            wavfile.write(dest, sr, old)
            players.append(f'<p>Previous {args.previous_budget_kib:g} KiB dictionary: {codec}<br><audio controls preload="none" src="{dest.name}"></audio></p>')
    for quality in qualities:
        selected=trial['source_slice_ids'] if quality=='compact' else list(range(len(meta['source_indices'])))
        assignment=trial['assignment'] if quality=='compact' else selected
        width=max(lengths)
        # Start from original PCM, avoiding a second lossy codec generation.
        slices=[q[:boundaries[0]]] + [np.pad(originals[meta['source_indices'][i]],(0,width-len(originals[meta['source_indices'][i]])),mode='edge') for i in selected] + [q[boundaries[-1]:]]
        seq=[(0,len(slices[0]))]+[(assignment[i]+1,n) for i,n in zip(meta['dictionary_ids'],lengths)]+[(len(slices)-1,len(slices[-1]))]
        offsets=np.r_[0,np.cumsum([len(s) for s in slices])].tolist()
        bank=np.concatenate(slices)
        layout={'gain_stage':args.gain_stage,'rate':rate,'samples':len(q),'offsets':offsets,'sequence':seq}
        header=json.dumps(layout,separators=(',',':')).encode()
        bankwav=out/f'{track}_{quality}_bank.wav'
        wavfile.write(bankwav,rate,bank.astype(np.int16)*256)
        reconstructed=np.concatenate([slices[i][:n] for i,n in seq])
        preview(quality+'_before_codec',reconstructed)
        for codec in codecs:
            if codec=='fibonacci':
                payload=encode(bank,encoder); decoded=fib_decode(payload)
            else:
                packed=out/f'{track}_{quality}_ima.wav'
                decodedfile=out/f'{track}_{quality}_ima_decoded.wav'
                subprocess.run(['ffmpeg','-v','error','-y','-i',str(bankwav),'-c:a','adpcm_ima_wav',str(packed)],check=True)
                subprocess.run(['ffmpeg','-v','error','-y','-i',str(packed),'-c:a','pcm_s16le',str(decodedfile)],check=True)
                sr,d=wavfile.read(decodedfile);assert sr==rate and len(d)>=len(bank)
                decoded=np.clip(np.rint(d[:len(bank)].astype(float)/256),-128,127).astype(np.int8)
                payload=packed.read_bytes()
            # Self-describing experimental disk package, not a runtime format.
            package=struct.pack('>4sI',b'AUD1',len(header))+header+payload
            if args.max_package_kib is not None:
                assert len(package) <= args.max_package_kib*1024, 'Encoded package exceeds budget'
            dest=out/f'{track}_{quality}_{codec}.aud';dest.write_bytes(package)
            assert dest.read_bytes()[8+len(header):]==payload
            result=np.concatenate([decoded[offsets[i]:offsets[i]+n] for i,n in seq])
            assert len(result)==len(q)
            row={'track':track,'quality':quality,'codec':codec,'dictionary_slices':len(selected),'disk_bytes':len(package),'decoded_bank_bytes':len(bank),'preview':preview(quality+'_'+codec,result),'codec_only':metric(reconstructed,result),'overall':metric(q,result)}
            reports.append(row);print(json.dumps(row),flush=True)
    sections.append('<section><h2>'+track.title()+'</h2><p>Dictionary budget: '+f'{args.budget_kib:g} KiB'+'</p>'+''.join(players)+'</section>')
report={'sources':{track:pc_source(track) for track in ['courtesy','otis','focus']},'status':'Host audition only; experimental disk packages; no Amiga decoder or disk integration', 'sample_rate':12000,'gain':'Game boost applied '+args.gain_stage+' Fibonacci/ADPCM encoding; decoded preboost previews are not boosted again', 'trials':reports,'totals':{quality:{codec:sum(r['disk_bytes'] for r in reports if r['quality']==quality and r['codec']==codec) for codec in codecs} for quality in qualities}}
(ROOT/('soundtrack/disk_codec_audition'+tag+'.json')).write_text(json.dumps(report,indent=2)+'\n')
(out/'index.html').write_text('''<!doctype html><meta charset="utf-8"><title>Disk codec auditions</title><style>body{font:17px system-ui;background:#171923;color:#eee;max-width:850px;margin:40px auto}section{padding:20px;background:#252936;margin:20px 0}audio{width:100%}</style><h1>Fibonacci delta / IMA ADPCM</h1><p>PC Courtesy, Otis and Focus reference and encoded previews use music1.dat, music2.dat and music3.dat at their original time origins; the older MP3 auditions remain on their separate page.</p><p>All previews: 12 kHz mono with game volume boost. Compact keeps the previous dictionary assignments but restores original source samples before encoding. Fuller uses the larger first-stage dictionary. Expanded increases that dictionary budget; previous fuller players preserve the earlier audition for comparison. Before-codec controls expose loss from slice reuse alone. Switching within a track preserves position.</p>'''+''.join(sections)+'''<script>let old;document.querySelectorAll('audio').forEach(a=>a.addEventListener('play',()=>{if(old&&old!==a){if(old.closest('section')===a.closest('section'))a.currentTime=old.currentTime;old.pause()}old=a}));</script>''')
print(json.dumps(report['totals'],indent=2))
