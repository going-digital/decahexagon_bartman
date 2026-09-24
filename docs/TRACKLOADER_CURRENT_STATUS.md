# Current trackloader validation

Factory image: `out/trackloader/332e5d41cefeab58003b0a6fb4ac787b22587958b7dbb77daa0a14c848bc8abe.adf`

Only 3 of 19 reports below identify this exact image. Older
reports are explicitly marked as needing a rerun. The summary tool rejects
stale reports by default; `--allow-stale` produces this partial-validation view.
This summary checks report identity; run the referenced test tools to reproduce
the underlying validation.

| Check | Evidence |
| --- | --- |
| Track switching and full-song DMA endurance | Needs rerun: [older image report](TRACKLOADER_PC_AUDIO_RELEASE_RESULTS.json) |
| Isolated source-to-ADF rebuild | Needs rerun: [older image report](TRACKLOADER_ISOLATED_BUILD_RESULTS.json) |
| PAL writable save retry | Needs rerun: [older image report](TRACKLOADER_WRITABLE_RETRY_RESULTS.json) |
| PAL loading activity | Needs rerun: [older image report](TRACKLOADER_LOADING_ACTIVITY_PAL_RESULTS.json) |
| PAL track switching | Needs rerun: [older image report](TRACKLOADER_OFS_SWITCHING_RESULTS_OFS_SWITCHING.json) |
| PAL save and cold reboot | Needs rerun: [older image report](TRACKLOADER_SAVE_REBOOT_PAL_RESULTS.json) |
| PAL save selection, corruption, identity and generation wrap | Needs rerun: [older image report](TRACKLOADER_SAVE_RECOVERY_PAL_RESULTS.json) |
| PAL wrong disk and reinsertion | Needs rerun: [older image report](TRACKLOADER_OFS_SWITCHING_RESULTS_OFS_REINSERT.json) |
| PAL swap during tune read | Needs rerun: [older image report](TRACKLOADER_OFS_SWITCHING_RESULTS_OFS_MIDREAD.json) |
| NTSC writable save retry | Needs rerun: [older image report](TRACKLOADER_WRITABLE_RETRY_RESULTS_NTSC.json) |
| NTSC loading activity | Needs rerun: [older image report](TRACKLOADER_LOADING_ACTIVITY_NTSC_RESULTS.json) |
| NTSC track switching | Needs rerun: [older image report](TRACKLOADER_OFS_SWITCHING_RESULTS_NTSC_OFS_SWITCHING.json) |
| NTSC save and cold reboot | Needs rerun: [older image report](TRACKLOADER_SAVE_REBOOT_NTSC_RESULTS.json) |
| NTSC save selection, corruption, identity and generation wrap | Needs rerun: [older image report](TRACKLOADER_SAVE_RECOVERY_NTSC_RESULTS.json) |
| NTSC wrong disk and reinsertion | Needs rerun: [older image report](TRACKLOADER_OFS_SWITCHING_RESULTS_NTSC_OFS_REINSERT.json) |
| NTSC swap during tune read | Needs rerun: [older image report](TRACKLOADER_OFS_SWITCHING_RESULTS_NTSC_OFS_MIDREAD.json) |
| PAL 1-slot soundtrack cache | [Startup preload and MRU retention verified in game RAM](TRACKLOADER_TUNE_CACHE_1_PAL_RESULTS.json) |
| PAL 2-slot soundtrack cache | [Startup preload and MRU retention verified in game RAM](TRACKLOADER_TUNE_CACHE_2_PAL_RESULTS.json) |
| PAL 3-slot soundtrack cache | [Startup preload and MRU retention verified in game RAM](TRACKLOADER_TUNE_CACHE_3_PAL_RESULTS.json) |

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
