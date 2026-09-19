# PC sound and speech triggers

The original 16 Ogg clips are in `assets/sounds/`. `prepare_sfx.py` converts them
**offline** to 8 kHz mono signed 8-bit PCM, with even addresses/lengths. The bank
uses 107,778 Chip bytes after removing digital-zero tails and the unused
menuselect payload. All 16 original files are retained; 15 clips are playable. `sfx_assets.json` records source hashes, IDs and offsets.
No codec, software mixer or file access runs during gameplay.

Music reserves AUD0. Effects/speech use AUD1–AUD3, allowing the PC's concurrent
speech/effect requests (begin + start, excellent + start, rankup + rank word).
They share one level-4 dispatcher with music: stopping AUD0 never restores the
OS vector over still-playing effects. The dispatcher is restored only on exit.
Each effect queues silent reload data, then disables its DMA/interrupt on
completion. Retriggering the same clip reuses its voice; if all three voices are
occupied, the oldest is replaced. This hardware polyphony limit is a deliberate
Amiga constraint: four coincident or overlapping PC effects cannot all be kept.
Paula's fixed stereo panning is retained. PAL period 443 / NTSC period 447 gives
approximately 8 kHz, not mathematically exact 8,000 Hz.

## Evidence and hooks

IDs are verified against `musicclass::loadmusic` and the original filenames.
Primary local evidence is under `scratchpad/pc_verification/evidence/`; these
are PC executable exports, not guesses from the web prototype.

| ID / clip | PC trigger | Amiga hook |
| --- | --- | --- |
| 0 begin | Confirm/retry a run | `start_playing`, before the first live tick |
| 1 excellent | Reach the previous nonzero best from below | `pc_sfx_live`, once per run |
| 2 gameover | Death timer reaches exactly 10; no new completion sequence | `pc_sfx_death` |
| 3 start | Together with begin/excellent; stage handoff | Same event tick |
| 4 die | Ordinary collision death, excluding completion state 1 | Collision branch |
| 5 rankup | Rank threshold, or backing out of a run | Rank tick / back edge |
| 6 line | Score strictly greater than 600 | Displayed elapsed score |
| 7 triangle | Strictly greater than 1,200 | Displayed elapsed score |
| 8 square | Strictly greater than 1,800 | Displayed elapsed score |
| 9 pentagon | Strictly greater than 2,700 | Displayed elapsed score |
| 10 hexagon | Strictly greater than 3,600 | Displayed elapsed score |
| 11 menuselect | Confirm settings/leaderboard menu actions | Payload omitted; those PC screens are not implemented |
| 12 menuchoose | Start movement to another selection wedge | Motion starts, not wedge arrival |
| 13 awesome | Strictly greater than 7,200; continuation handoff | Rank tick / `patterns_transitioned` |
| 14 wonderful | New completion result: extent crosses 200, except secret ending | Death extent crossing |
| 15 superhexagon | One startup announcement after a 45-unit countdown expires | Tick 46, once per launch |

Rank words are independent of wall-induced polygon changes. Best-score speech
fires on reaching the old best, not on every subsequent improvement. A zero
initial best does not cause an announcement on the first run. All timing uses
the game's 60 Hz simulation ticks, including PAL builds.

Source anchors:

- `superhex::gameinput` at `0x100052980`: selection motion sets `S+0x5508`
  and calls playef(12); restart paths call playef(0), playef(3); back calls 5.
- `superhex::gamelogic` at `0x100056f50`: best crossing calls 1 + 3; rank
  progression calls 5 + the rank word; death calls 4; timer 10 calls 2;
  completed-result expansion calls 14 unless it enters the secret ending.
- `gameclass::gameclass` at `0x100002660`: G+0x5520..0x5534 contain thresholds
  600, 1200, 1800, 2700, 3600, 7200.
- `gameclass::winlevel` at `0x100009b50`: distinguishes completion states 1/2/3.
- `gameclass::changetostage` at `0x100029ea0`: calls 13 + 3 at continuation.
- Startup disassembly at `0x1000609be`: announcement flag set; countdown 45.0.
  Gamelogic decrements while positive, then plays clip 15 on the following tick.
- `musicclass::playef(int)` at `0x10005d370`: forwards to the clip's sound player
  unless effects are muted.

The port still lacks the PC's scripted ending scenes and settings/leaderboard
screens. Their audio is not invented on unrelated actions. Completion voice
conditions mirror the PC flags, but complete ending-scene timing cannot be
claimed until those scenes exist. The current renderer's death path remains
separate from that work.

## Builds and checks

`SOUND_EFFECTS=1` is the default. `SOUND_EFFECTS=0` excludes the sample bank and
hardware effects backend. PCM music remains an independent build option.

```sh
make test-sfx test-pcm-lifecycle
# Combined music/effects trial, intended for 512 KiB Chip + 512 KiB expansion:
tools/build.sh -B MUSIC_FIB_STREAM=1 \
  PCM_ASSET=scratchpad/audio/courtesy/pcm_reuse/96 \
  EXTRA_CFLAGS=-DBUILD_DEBUG=0 OUT=out/pcm_sfx out/pcm_sfx.adf
```

Host checks cover every rank boundary, zero/nonzero best, coincident record/rank
notifications, completion profiles, startup countdown and death/result timing.
Menu/lifecycle/death regressions also pass against existing PC fixtures.

The optional music trial overlay adds a third row: B = clips begun, R = retired
(including preempted clips), V = active AUD1–3 bitmask. This is diagnostics for
hardware testing, not evidence of perceptual quality or an audio capture.

### PAL target trials (2026-09-19)

- Combined music/effects, 512 KiB Chip + 512 KiB Slow: assisted Hexagonest run
  reached **45.50 seconds**, with **0 music underruns**, 1,065 music blocks,
  11 effect requests and 10 retired voices; one rank voice was still active.
  Evidence: `scratchpad/fsuae/pal/fs-uae-crop-2609192026-03.png`.
  The assisted binary SHA-256 was
  `f510127f7664003b06584e6323aa3334069ee0ab77606f4a780e868a3ffd18cf`.
- Non-assisted combined trials covered startup, menu selection, start, death,
  retry and return to menu. Start clips retired before the later death clips.
  The shared interrupt vector survived music stop/restart. Returning to AmigaDOS
  was captured in `scratchpad/fsuae/pal/fs-uae-crop-2609192027-03.png`.
- Effects-only default build booted and ran on **512 KiB Chip with no expansion**:
  `scratchpad/fsuae/pal512/fs-uae-crop-2609192028-02.png`.
- An earlier one-shot implementation could repeat a sample when its first
  interrupt was delayed. The retained implementation explicitly preloads silent
  reload data after the initial DMA fetch; subsequent target trials used this fix.

These are control-flow, DMA-lifecycle and underrun checks. They do not replace
listening tests. NTSC playback and the unimplemented PC ending scenes have not
been verified on target in this change. Release builds pass the existing audit
that excludes steering-assist code.

The playback overlay is now optional: `-DAUDIO_DIAGNOSTICS=1` enables it.
It defaults to BUILD_DEBUG, so release builds omit it. Historical trial sizes
and screenshots above describe the earlier untrimmed bank.
