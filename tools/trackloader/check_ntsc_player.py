#!/usr/bin/env python3
"""Read actual NTSC player counters through the control protocol."""
import argparse,hashlib,json,subprocess,time,struct,zlib
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--rom',type=Path,required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];base=root/'scratchpad/trackloader/native_game'
out=base/('ccp_ntsc_player_'+str(time.time_ns()));out.mkdir()
original=(base/'native_menu.adf').read_bytes()
(out/'original.adf').write_bytes(original);(out/'foreign.adf').write_bytes(bytes(901120))
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
    proc=subprocess.Popen(['copperline','--config',str(out/'config.toml'),'--model','A500OCS','--chip','512K','--slow','512K','--video','NTSC','--noaudio','--serial','stdout','--control',':0','--control-info',str(info),str(a.rom)],stdout=log,stderr=log)
    try:
        for _ in range(100):
            if info.exists():break
            if proc.poll() is not None:raise RuntimeError('Emulator exited')
            time.sleep(.1)
        keys=[(60,0x40),(125,0x45),(126,0x4e),(127,0x40),(190,0x45),(191,0x4e),(192,0x40),(255,0x40),(270,0x45),(271,0x4f),(272,0x4f),(273,0x40)]
        for at,key in keys:call('input.key',dict(rawkey=key,action='tap',hold_ms=100,at_seconds=at))
        call('run_until',dict(seconds=55))
        memory=b''.join(read(addr,32768) for addr in range(0,524288,32768))
        location=memory.find(signature);assert location>=0 and memory.find(signature,location+1)<0
        gamebase=location-sig_offset
        timing=struct.unpack('>5H',read(gamebase+symbols['video_timing'],10))
        assert timing==(60,262,200,298,447),timing
        for seconds,track in [(124,'courtesy'),(189,'otis'),(269,'focus'),(340,'courtesy')]:
            call('run_until',dict(seconds=seconds))
            counters={name:struct.unpack('>H',read(gamebase+symbols[name],2))[0] for name in ['underruns','blocks','running']}
            rows.append(dict(seconds=seconds,track=track,**counters))
            print(rows[-1],flush=True)
        call('shutdown');proc.wait(timeout=10)
    finally:
        if proc.poll() is None:proc.terminate();proc.wait(timeout=10)
assert (out/'original.adf').read_bytes()==original
ready=[line for line in (out/'serial.log').read_text().splitlines() if 'NATIVE-TUNE-' in line]
assert ready==['DF0:'+track+'NATIVE-TUNE-READY' for track in ['courtesy','otis','focus','courtesy']],ready
report=dict(status='NTSC player counters sampled; no waveform quality claim',source_sha256=hashlib.sha256(original).hexdigest(),game_base=gamebase,video_timing=timing,trials=rows,evidence=str(out),limitations=['Short normal runs, not full-song loops','No waveform/cue alignment measurement','No physical hardware validation'])
(root/'docs/TRACKLOADER_NTSC_PLAYER_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
assert all(row['underruns']==0 and row['blocks']>0 for row in rows),rows
print('NTSC periods verified; every sampled track has completed DMA blocks and zero underruns')
