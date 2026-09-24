#!/usr/bin/env python3
"""Compile external-only IRQ player and audit its remaining link dependencies."""
import json,subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader';out.mkdir(parents=True,exist_ok=True)
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-O2','-ffreestanding','-fomit-frame-pointer','-DBUILD_DEBUG=0','-DPCM_EXTERNAL_ONLY=1','-DSOUND_EFFECTS=0','-DAUDIO_DIAGNOSTICS=0','-c',str(root/'tests/fib_stream.c'),'-o',str(out/'external_player.o')],check=True)
text=subprocess.check_output([str(sdk.parent/'m68k-amiga-elf/bin/nm'),'-u',str(out/'external_player.o')],text=True)
symbols=[line.split()[-1] for line in text.splitlines() if line.strip()]
expected={'custom','fib_song_read','fib_song_seek','frameCounter','paula_irq_set','pc_pcm_position','pc_pulse_cue_index_offset','pcm_lifecycle_tick','video_timing'}
assert set(symbols)==expected,symbols
(root/'docs/TRACKLOADER_EXTERNAL_PLAYER_RESULTS.json').write_text(json.dumps({'status':'68000 object compiled; dependencies audited; not a target playback test','undefined_symbols':symbols},indent=2)+'\n')
print('External-only player has exactly the expected hardware, sequencer, timing and lifecycle dependencies')
