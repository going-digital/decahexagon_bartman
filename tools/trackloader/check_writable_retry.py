#!/usr/bin/env python3
"""Full-emulator writable reinsertion trial using the installed CCP interface."""
import argparse,hashlib,json,subprocess,time,struct,zlib
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--rom',type=Path,required=True);p.add_argument('--video',choices=['PAL','NTSC'],default='PAL');a=p.parse_args()
root=Path(__file__).resolve().parents[2];base=root/'scratchpad/trackloader/native_game'
out=base/('ccp_writable_retry_'+str(time.time_ns()));out.mkdir()
original=(base/'native_menu.adf').read_bytes()
(out/'original.adf').write_bytes(original);(out/'foreign.adf').write_bytes(bytes(901120))
(out/'config.toml').write_text('[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = '+json.dumps(str(out/'original.adf'))+'\nwrite_protected = false\n')
info=out/'control.json';events=[]
def call(method,params=None):
    result=json.loads(subprocess.check_output(['copperline-ctl','--info',str(info),method,json.dumps(params or {})],text=True,timeout=120))
    if 'error' in result:raise RuntimeError(result)
    if method!='mem.read':
        events.append(dict(method=method,params=params,result=result));(out/'events.json').write_text(json.dumps(events,indent=2)+'\n')
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
observations=[]
with (out/'serial.log').open('w') as log:
    proc=subprocess.Popen(['copperline','--config',str(out/'config.toml'),'--model','A500OCS','--chip','512K','--slow','512K','--video',a.video,'--noaudio','--serial','stdout','--control',':0','--control-info',str(info),str(a.rom)],stdout=log,stderr=log)
    try:
        for _ in range(100):
            if info.exists():break
            if proc.poll() is not None:raise RuntimeError('Emulator exited; inspect serial.log')
            time.sleep(.1)
        for at in (60,150):call('input.key',dict(rawkey=64,action='tap',hold_ms=100,at_seconds=at))
        call('run_until',dict(seconds=55))
        memory=b''.join(read(addr,32768) for addr in range(0,524288,32768))
        location=memory.find(signature);assert location>=0 and memory.find(signature,location+1)<0
        gamebase=location-sig_offset
        for seconds,label in [(115,'saving'),(140,'failed'),(145,None),(153,'resumed'),(225,'retried')]:
            call('run_until',dict(seconds=seconds))
            observations.append(dict(seconds=seconds,dirty=read(gamebase+symbols['save_dirty'],1)[0],best_ticks=struct.unpack('>6I',read(gamebase+symbols['records'],24))))
            if label:call('capture.screenshot',dict(path=str(out/(label+'.png'))))
            if seconds in (115,145):
                call('media.floppy.insert',dict(drive=0,path=str(out/('foreign.adf' if seconds==115 else 'original.adf')),write_protected=False))
                call('media.floppy.query')
            print('Reached',seconds,label,flush=True)
        call('shutdown');proc.wait(timeout=10)
    finally:
        if proc.poll() is None:proc.terminate();proc.wait(timeout=10)
assert observations[1]['dirty']==1,observations
assert observations[-1]['dirty']==0,observations
assert all(row['best_ticks']==observations[1]['best_ticks'] for row in observations[1:-1]),observations
assert all(new>=old for old,new in zip(observations[1]['best_ticks'],observations[-1]['best_ticks'])),observations
assert (base/'native_menu.adf').read_bytes()==original
saved=(out/'original.adf').read_bytes();foreign=(out/'foreign.adf').read_bytes()
assert foreign==bytes(901120)
changed=[i for i in range(1760) if original[i*512:(i+1)*512]!=saved[i*512:(i+1)*512]]
assert changed and set(changed)<=set([1738,1749]),changed
slots=[]
for sector in changed:
    b=saved[sector*512:(sector+1)*512]
    assert b[:4]==b'HXS1' and b[16:32]==(base/'save_identity.bin').read_bytes()
    assert struct.unpack_from('>I',b,508)[0]==zlib.crc32(b[:508])
    slots.append(dict(sector=sector,generation=struct.unpack_from('>I',b,12)[0],best_ticks=list(struct.unpack_from('>6I',b,32))))
assert any(tuple(slot['best_ticks'])==observations[1]['best_ticks'] for slot in slots),slots
assert any(tuple(slot['best_ticks'])==observations[-1]['best_ticks'] for slot in slots),slots
report=dict(video=a.video,observations=observations,status='Writable reinsertion produced valid save data; screenshots require visual review',source_sha256=hashlib.sha256(original).hexdigest(),changed_sectors=changed,slots=slots,foreign_unchanged=True,evidence=str(out),limitations=['Swap phase within SAVE transaction not instrumented','No physical drive test'])
(root/('docs/TRACKLOADER_WRITABLE_RETRY_RESULTS'+('_NTSC' if a.video=='NTSC' else '')+'.json')).write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
