#!/usr/bin/env python3
"""Verify loading-bar animation while the gameplay frame clock is paused."""
import argparse,hashlib,json,subprocess,time,struct,zlib
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--rom',type=Path,required=True);p.add_argument('--video',choices=['PAL','NTSC'],required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];base=root/'scratchpad/trackloader/native_game'
out=base/('loading_activity_'+str(time.time_ns()));out.mkdir()
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
    proc=subprocess.Popen(['copperline','--config',str(out/'config.toml'),'--model','A500OCS','--chip','512K','--slow','512K','--video',a.video,'--noaudio','--serial','stdout','--control',':0','--control-info',str(info),str(a.rom)],stdout=log,stderr=log)
    try:
        for _ in range(100):
            if info.exists():break
            if proc.poll() is not None:raise RuntimeError('Emulator exited')
            time.sleep(.1)
        call('input.key',dict(rawkey=64,action='tap',hold_ms=100,at_seconds=60))
        call('run_until',dict(seconds=55))
        memory=b''.join(read(addr,32768) for addr in range(0,524288,32768))
        location=memory.find(signature);assert location>=0 and memory.find(signature,location+1)<0
        gamebase=location-sig_offset
        timing=struct.unpack('>5H',read(gamebase+symbols['video_timing'],10))
        assert timing==((50,312,256,296,443) if a.video=='PAL' else (60,262,200,298,447)),timing
        for seconds in range(65,111,5):
            call('run_until',dict(seconds=seconds))
            active=struct.unpack('>I',read(0x6c,4))[0]==gamebase+symbols['loading_interrupt']
            frames=struct.unpack('>H',read(gamebase+symbols['busy_frame'],2))[0]
            gameframes=struct.unpack('>H',read(gamebase+symbols['frameCounter'],2))[0]
            rows.append(dict(seconds=seconds,active=active,busy_frames=frames,game_frames=gameframes))
            if active:call('capture.screenshot',dict(path=str(out/f'{seconds}.png')))
        call('run_until',dict(seconds=140))
        assert struct.unpack('>I',read(0x6c,4))[0]==gamebase+symbols['interruptHandler']
        call('shutdown');proc.wait(timeout=10)
    finally:
        if proc.poll() is None:proc.terminate();proc.wait(timeout=10)
assert (out/'original.adf').read_bytes()==original
ready=[line for line in (out/'serial.log').read_text().splitlines() if 'NATIVE-TUNE-' in line]
assert ready==['DF0:courtesyNATIVE-TUNE-READY'],ready
pairs=[(x,y) for x,y in zip(rows,rows[1:]) if x['active'] and y['active'] and x['game_frames']==y['game_frames']]
assert len(pairs)>=3,rows
assert all(y['busy_frames']>x['busy_frames'] for x,y in pairs),rows
report=dict(status='Loading activity advances while gameplay clock stays paused; normal IRQ restored',video=a.video,source_sha256=hashlib.sha256(original).hexdigest(),trials=rows,evidence=str(out))
(root/f'docs/TRACKLOADER_LOADING_ACTIVITY_{a.video}_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
