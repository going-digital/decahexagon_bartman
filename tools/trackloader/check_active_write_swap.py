#!/usr/bin/env python3
"""Swap writable media at a selected delay after the second write-DMA arm operation."""
import argparse,hashlib,json,subprocess,time,socket
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--rom',type=Path,required=True);p.add_argument('--delay-ms',type=int,default=0);p.add_argument('--eject-gap-ms',type=int,default=0);a=p.parse_args()
assert 0<=a.delay_ms<=200
assert 0<=a.eject_gap_ms<=1000
root=Path(__file__).resolve().parents[2];base=root/'scratchpad/trackloader/native_game'
out=base/('ccp_active_write_'+str(time.time_ns()));out.mkdir()
original=(base/'native_menu.adf').read_bytes()
(out/'original.adf').write_bytes(original);(out/'foreign.adf').write_bytes(bytes(901120))
(out/'config.toml').write_text('[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = '+json.dumps(str(out/'original.adf'))+'\nwrite_protected = false\n')
info=out/'control.json';events=[]
connection=None;stream=None;request_id=0
def rpc(method,params):
    global request_id
    request_id+=1
    stream.write((json.dumps(dict(jsonrpc='2.0',id=request_id,method=method,params=params))+'\n').encode());stream.flush()
    while True:
        line=stream.readline()
        if not line:raise RuntimeError('Control connection closed')
        result=json.loads(line)
        if result.get('id')==request_id:break
    if 'error' in result:raise RuntimeError(result)
    return result
def call(method,params=None):
    global connection,stream
    if connection is None:
        auth=json.loads(info.read_text());host,port=auth['listen'].rsplit(':',1)
        connection=socket.create_connection((host,int(port)),timeout=120)
        stream=connection.makefile('rwb');rpc('hello',dict(token=auth['token']))
    result=rpc(method,params or {})
    events.append(dict(method=method,params=params,result=result));(out/'events.json').write_text(json.dumps(events,indent=2)+'\n')
    return result
with (out/'serial.log').open('w') as log:
    proc=subprocess.Popen(['copperline','--config',str(out/'config.toml'),'--model','A500OCS','--chip','512K','--slow','512K','--video','PAL','--noaudio','--serial','stdout','--control',':0','--control-info',str(info),str(a.rom)],stdout=log,stderr=log)
    try:
        for _ in range(100):
            if info.exists():break
            if proc.poll() is not None:raise RuntimeError('Emulator exited; inspect serial.log')
            time.sleep(.1)
        for at in (60,):call('input.key',dict(rawkey=64,action='tap',hold_ms=100,at_seconds=at))
        call('run_until',dict(seconds=110))
        call('break.add',dict(kind='reg_watch',reg='DSKLEN'))
        arms=[]
        for _ in range(1000):
            stop=call('run_until',dict(seconds=140))['result']
            assert stop['reason']=='reg_watch',stop
            if 'DSKLEN = D961' in stop['detail']:
                arms.append(stop)
                if len(arms)==2:break
        assert len(arms)==2,arms
        swap_stop=arms[-1]
        if a.delay_ms:
            swap_stop=call('run_until',dict(seconds=arms[-1]['seconds']+a.delay_ms/1000))['result']
            assert swap_stop['reason']=='target',('DMA stopped before requested delay',swap_stop)
        call('capture.screenshot',dict(path=str(out/'write_armed.png')))
        gap_stop=None
        if a.eject_gap_ms:
            call('media.floppy.eject',dict(drive=0))
            # Retain the watchpoint to observe the guard disabling DMA.
            gap_events=[]
            gap_end=swap_stop['seconds']+a.eject_gap_ms/1000
            for _ in range(100):
                gap_stop=call('run_until',dict(seconds=gap_end))['result']
                gap_events.append(gap_stop)
                if gap_stop['reason']=='target':break
            assert gap_stop['reason']=='target',gap_stop
            assert any('DSKLEN = 4000' in e.get('detail','') for e in gap_events),gap_events
            call('media.floppy.query')
        else:gap_events=[]
        call('media.floppy.insert',dict(drive=0,path=str(out/'foreign.adf'),write_protected=False))
        call('break.clear')
        call('run_until',dict(seconds=swap_stop['seconds']+15))
        call('capture.screenshot',dict(path=str(out/'after_swap.png')))
        print('Swapped at',swap_stop['seconds'],'after delay ms',a.delay_ms,flush=True)
        call('shutdown');proc.wait(timeout=10)
    finally:
        if proc.poll() is None:proc.terminate();proc.wait(timeout=10)
saved=(out/'original.adf').read_bytes();foreign=(out/'foreign.adf').read_bytes()
changed=[i for i in range(1760) if original[i*512:(i+1)*512]!=saved[i*512:(i+1)*512]]
foreign_changed=[i for i in range(1760) if any(foreign[i*512:(i+1)*512])]
report=dict(status='Active-write swap completed; screenshot requires visual review',source_sha256=hashlib.sha256(original).hexdigest(),write_arm_events=arms,delay_ms=a.delay_ms,eject_gap_ms=a.eject_gap_ms,gap_events=gap_events,swap_stop=swap_stop,changed_sectors=changed,foreign_changed_sectors=foreign_changed,evidence=str(out),limitations=['No DSKLEN reprogramming before swap; partial ADF persistence is emulator-dependent','ADF emulator model, not physical-drive validation'])
(root/('docs/TRACKLOADER_ACTIVE_WRITE_SWAP_RESULTS'+('_'+str(a.delay_ms)+'MS' if a.delay_ms else '')+('_GAP'+str(a.eject_gap_ms)+'MS' if a.eject_gap_ms else '')+'.json')).write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
assert not foreign_changed,'Replacement disk changed: inspect evidence'
assert set(changed)<=set([1738,1749]),changed
