#!/usr/bin/env python3
"""Verify DF1 boot, soundtrack load and saving with DF0 empty (use a ROM supporting external boot)."""
import argparse,hashlib,json,subprocess,time,struct,zlib
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--write-protected',action='store_true');p.add_argument('--drive',type=int,choices=range(4),default=1);p.add_argument('--rom',type=Path,required=True);p.add_argument('--video',choices=['PAL','NTSC'],required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];base=root/'scratchpad/trackloader/native_game'
out=base/('external_boot_'+str(time.time_ns()));out.mkdir()
original=(base/'native_menu.adf').read_bytes()
(out/'original.adf').write_bytes(original)
(out/'config.toml').write_text('[emulation]\npacing_budget = "cycles"\n[floppy.df'+str(a.drive)+']\npath = '+json.dumps(str(out/'original.adf'))+'\nwrite_protected = '+str(a.write_protected).lower()+'\n')
info=out/'control.json';events=[]
def call(method,params=None):
    result=json.loads(subprocess.check_output(['copperline-ctl','--info',str(info),method,json.dumps(params or {})],text=True,timeout=120))
    if 'error' in result:raise RuntimeError(result)
    if method!='mem.read':events.append(dict(method=method,params=params,result=result))
    (out/'events.json').write_text(json.dumps(events,indent=2)+'\n')
    return result

import sys
sys.path.insert(0,str(root/'tools/trackloader'))
from package_native_game import elf_image
image,_,_,relocs,_=elf_image(base/'game.elf')
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/m68k-amiga-elf/bin/nm'))
listing=subprocess.check_output([str(sdk),str(base/'game.elf')],text=True)
symbols={line.split()[-1]:int(line.split()[0],16) for line in listing.splitlines() if len(line.split())==3}
sig_offset=next(i for i in range(0,len(image)-64,64) if not any(i-3<=r<i+64 for r in relocs))
signature=bytes(image[sig_offset:sig_offset+64])
def read(addr,n):
    return bytes.fromhex(call('mem.read',dict(addr=addr,len=n,encoding='hex'))['result']['data'])

with (out/'serial.log').open('w') as log:
    proc=subprocess.Popen(['copperline','--config',str(out/'config.toml'),'--model','A500OCS','--chip','512K','--slow','512K','--video',a.video,'--noaudio','--serial','stdout','--control',':0','--control-info',str(info),str(a.rom)],stdout=log,stderr=log)
    try:
        for _ in range(100):
            if info.exists():break
            if proc.poll() is not None:raise RuntimeError('Emulator exited: '+(out/'serial.log').read_text())
            time.sleep(.1)
        call('run_until',dict(seconds=240))
        memory=b''.join(read(addr,32768) for addr in range(0,524288,32768))
        location=memory.find(signature);assert location>=0
        gamebase=location-sig_offset
        assert read(gamebase+symbols['drive_select'],1)==bytes([255^(8<<a.drive)]),'Wrong selected boot drive'
        call('input.key',dict(rawkey=64,action='tap',hold_ms=100,at_seconds=241))
        call('input.key',dict(rawkey=0x45,action='tap',hold_ms=100,at_seconds=249))
        captured=[]
        for step in range(100):
            seconds=249+step*.1
            call('run_until',dict(seconds=seconds))
            active=struct.unpack('>I',read(0x6c,4))[0]==gamebase+symbols['loading_interrupt']
            if active:
                path=out/f'save-{step:03}.png'
                call('capture.screenshot',dict(path=str(path)))
                captured.append(str(path))
        assert bool(captured)!=a.write_protected,'Unexpected saving display state'
        call('run_until',dict(seconds=270))
        assert struct.unpack('>I',read(0x6c,4))[0]==gamebase+symbols['interruptHandler']
        assert read(gamebase+symbols['save_failed'],1)==bytes([2 if a.write_protected else 0])
        call('shutdown');proc.wait(timeout=10)
        b=(out/'original.adf').read_bytes()
        changed=[i for i in range(1760) if b[i*512:(i+1)*512]!=original[i*512:(i+1)*512]]
        assert changed==([] if a.write_protected else [1738]),changed
        slot=b[1738*512:1739*512]
        if not a.write_protected:assert slot[:4]==b'HXS1' and struct.unpack_from('>I',slot,508)[0]==zlib.crc32(slot[:508])
        (out/'result.json').write_text(json.dumps(dict(video=a.video,drive=a.drive,write_protected=a.write_protected,changed_sectors=changed,captures=captured,source_sha256=hashlib.sha256(original).hexdigest()),indent=2))
        print('DF'+str(a.drive)+' boot, tune preload, gameplay, menu save and display restoration passed; changed sector:',changed,flush=True)
        print(out,flush=True)
    finally:
        if proc.poll() is None:proc.terminate();proc.wait(timeout=10)
