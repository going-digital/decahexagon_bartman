#!/usr/bin/env python3
"""Summarize current-image evidence, rejecting reports from older disk builds."""
import hashlib
import json
from pathlib import Path
root=Path(__file__).resolve().parents[2]
docs=root/'docs'
image=root/'scratchpad/trackloader/native_game/native_menu.adf'
digest=hashlib.sha256(image.read_bytes()).hexdigest()
cases=[('Track switching and full-song DMA endurance','TRACKLOADER_PC_AUDIO_RELEASE_RESULTS.json','adf_sha256'),
       ('Isolated source-to-ADF rebuild','TRACKLOADER_ISOLATED_BUILD_RESULTS.json','adf_sha256')]
for video in ('PAL','NTSC'):
 suffix='_NTSC' if video=='NTSC' else ''
 cases.append((video+' writable save retry',f'TRACKLOADER_WRITABLE_RETRY_RESULTS{suffix}.json','source_sha256'))
 cases.extend([
  (video+' save and cold reboot',f'TRACKLOADER_SAVE_REBOOT_{video}_RESULTS.json','source_sha256'),
  (video+' save selection, corruption, identity and generation wrap',f'TRACKLOADER_SAVE_RECOVERY_{video}_RESULTS.json','source_sha256')])
 for scenario,label in [('OFS_REINSERT','wrong disk and reinsertion'),('OFS_MIDREAD','swap during tune read')]:
  suffix='_NTSC' if video=='NTSC' else ''
  cases.append((video+' '+label,f'TRACKLOADER_OFS_SWITCHING_RESULTS{suffix}_{scenario}.json','adf_sha256'))
rows=[]
for label,name,key in cases:
 report=json.loads((docs/name).read_text())
 assert report[key]==digest, 'Stale evidence: '+name
 rows.append(f'| {label} | [{report["status"]}]({name}) |')
text=f'''# Current trackloader validation

Factory image: `out/trackloader/{digest}.adf`

All reports below identify this exact image. Regenerate this summary with
`python3 tools/trackloader/summarize_release_checks.py`; it rejects stale reports.
This summary checks report identity; run the referenced test tools to reproduce
the underlying validation.

| Check | Evidence |
| --- | --- |
'''+ '\n'.join(rows)+'''

Audio uses all three PC recordings, boosted before Fibonacci encoding, at a
250 KiB budget per tune. Host loop excerpts are available at
`../scratchpad/audio/pc_loop_audition/index.html`.

## Remaining limits

- No physical Amiga or 68060 validation.
- The instantaneous writable-disk-swap emulator issue remains unresolved; see
  the investigation in `TRACKLOADER_FEASIBILITY.md`.
- Full-song endurance bypasses collisions in emulator RAM and uses silent
  playback. Listening quality, waveform/cue alignment and gameplay FPS are
  separate validation work.
- Save recovery uses seeded fixtures. Achievement bits persist, but achievement
  definitions and gameplay triggers remain unfinished.
- An isolated working-tree source snapshot reproduces the ADF without cached
  audio, sound effects or trackloader outputs. PC cue extraction is available
  separately for the verified executable. Dependency installation is not
  automated; this is not a committed clean-checkout test.

Chronological implementation details: [feasibility record](TRACKLOADER_FEASIBILITY.md).
Build and test commands: [build guide](TRACKLOADER_BUILD.md).
'''
(docs/'TRACKLOADER_CURRENT_STATUS.md').write_text(text)
print('Current-image evidence:',len(cases),'reports; SHA-256',digest)
