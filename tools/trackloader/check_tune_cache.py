#!/usr/bin/env python3
"""Verify boot preload, decoded soundtrack hits and least-recently-used eviction."""
import argparse,hashlib,json,subprocess,time,struct
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--rom',type=Path,required=True);p.add_argument('--video',choices=['PAL','NTSC'],default='PAL');p.add_argument('--fast',default='0');p.add_argument('--slots',type=int,required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];base=root/'scratchpad/trackloader/native_game'
out=base/('tune_cache_'+str(time.time_ns()));out.mkdir()
original=(base/'native_menu.adf').read_bytes()
(out/'original.adf').write_bytes(original)
(out/'config.toml').write_text('[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = '+json.dumps(str(out/'original.adf'))+'\nwrite_protected = true\n')
info=out/'control.json';events=[]
def call(method,params=None):
    result=json.loads(subprocess.check_output(['copperline-ctl','--info',str(info),method,json.dumps(params or {})],text=True,timeout=120))
    if 'error' in result:raise RuntimeError(result)
    if method!='mem.read':events.append(dict(method=method,params=params,result=result))
    (out/'events.json').write_text(json.dumps(events,indent=2)+'\n')
    return result

from package_native_game import elf_image
image,_,_,relocs,_=elf_image(base/'game.elf')
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/m68k-amiga-elf/bin/nm'))
listing=subprocess.check_output([str(sdk),str(base/'game.elf')],text=True)
symbols={line.split()[-1]:int(line.split()[0],16) for line in listing.splitlines() if len(line.split())==3}
sig_offset=next(i for i in range(0,len(image)-64,64) if not any(i-3<=r<i+64 for r in relocs))
signature=bytes(image[sig_offset:sig_offset+64])
def read(addr,n):
    return bytes.fromhex(call('mem.read',dict(addr=addr,len=n,encoding='hex'))['result']['data'])
rows=[]
with (out/'serial.log').open('w') as log:
    proc=subprocess.Popen(['copperline','--config',str(out/'config.toml'),'--model','A500OCS','--chip','512K','--slow','512K','--fast',a.fast,'--video',a.video,'--noaudio','--serial','stdout','--control',':0','--control-info',str(info),str(a.rom)],stdout=log,stderr=log)
    try:
        for _ in range(100):
            if info.exists():break
            if proc.poll() is not None:raise RuntimeError('Emulator exited')
            time.sleep(.1)
        call('run_until',dict(seconds=240))
        memory=b''.join(read(addr,32768) for addr in range(0,524288,32768))
        location=memory.find(signature);assert location>=0 and memory.find(signature,location+1)<0
        gamebase=location-sig_offset
        timing=struct.unpack('>5H',read(gamebase+symbols['video_timing'],10))
        def snapshot():
            raw=read(gamebase+symbols['tune_cache'],328)
            count,active=struct.unpack_from('>Ii',raw,312)
            slots=[]
            for i in range(count):
                arena=struct.unpack_from('>I',raw,i*104)[0]
                valid,track,rank=raw[i*104+100:i*104+103]
                slots.append(dict(arena=arena,valid=valid,track=track,rank=rank))
            return dict(count=count,active=active,slots=slots)
        initial=snapshot();assert initial['count']==a.slots,initial
        assert {s['track'] for s in initial['slots'] if s['valid']}==set(range(a.slots)),initial
        assert initial['slots'][initial['active']]['track']==0,initial
        rows.append(dict(phase='startup',**initial))
        selected=0;when=245
        # Touch Courtesy again between new tracks: with two slots, it must
        # survive the Focus miss while the older Otis bank is replaced.
        for track in [1,0,2,0,1]:
            before=snapshot()
            hit=any(s['valid'] and s['track']==track for s in before['slots'])
            if len(rows)>1:call('input.key',dict(rawkey=0x45,action='tap',hold_ms=100,at_seconds=when))
            delta=track-selected
            for j in range(abs(delta)):
                call('input.key',dict(rawkey=0x4e if delta>0 else 0x4f,action='tap',hold_ms=100,at_seconds=when+1+j))
            start=when+4
            call('input.key',dict(rawkey=0x40,action='tap',hold_ms=100,at_seconds=start))
            call('run_until',dict(seconds=start+2))
            early=snapshot()
            # A hit retains the valid bank even while cues are being rebound.
            # A miss invalidates its victim before the disk transfer.
            assert any(s['valid'] and s['track']==track for s in early['slots'])==hit,(track,hit,early)
            call('run_until',dict(seconds=start+65))
            after=snapshot();assert after['active']>=0,after
            assert after['slots'][after['active']]['track']==track,after
            if a.slots==2 and track==2:
                assert {s['track'] for s in after['slots'] if s['valid']}=={0,2},after
            rows.append(dict(track=track,hit=hit,early=early,after=after))
            print(a.fast,track,'hit' if hit else 'miss',flush=True)
            selected=track;when=start+70
        call('capture.screenshot',dict(path=str(out/'final.png')))
        call('shutdown');proc.wait(timeout=10)
    finally:
        if proc.poll() is None:proc.terminate();proc.wait(timeout=10)
assert (out/'original.adf').read_bytes()==original
report=dict(status='Startup preload and MRU retention verified in game RAM',fast=a.fast,slots=a.slots,video=a.video,source_sha256=hashlib.sha256(original).hexdigest(),trials=rows,evidence=str(out))
(root/f'docs/TRACKLOADER_TUNE_CACHE_{a.slots}_{a.video}_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(report['status'])
