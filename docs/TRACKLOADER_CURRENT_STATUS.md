# Current trackloader validation

Factory image: `out/trackloader/823eafb45c162c2cf0d4f23b20852238bf41286cd7cccc189b22e39ec42fedd3.adf`

All reports below identify this exact image. Regenerate this summary with
`python3 tools/trackloader/summarize_release_checks.py`; it rejects stale reports.
This summary checks report identity; run the referenced test tools to reproduce
the underlying validation.

| Check | Evidence |
| --- | --- |
| Track switching and full-song DMA endurance | [Current all-PC image: PAL/NTSC switching and all three NTSC endurance checks pass](TRACKLOADER_PC_AUDIO_RELEASE_RESULTS.json) |
| Isolated source-to-ADF rebuild | [Automated isolated cue/audio/ADF rebuild matches reference byte-for-byte](TRACKLOADER_ISOLATED_BUILD_RESULTS.json) |
| PAL writable save retry | [Writable reinsertion produced valid save data; screenshots require visual review](TRACKLOADER_WRITABLE_RETRY_RESULTS.json) |
| PAL save and cold reboot | [PAL current-image save and cold reboot pass](TRACKLOADER_SAVE_REBOOT_PAL_RESULTS.json) |
| PAL save selection, corruption, identity and generation wrap | [PAL native cold-boot recovery verified in game RAM](TRACKLOADER_SAVE_RECOVERY_PAL_RESULTS.json) |
| PAL wrong disk and reinsertion | [Expected native switching and media-swap serial sequences verified](TRACKLOADER_OFS_SWITCHING_RESULTS_OFS_REINSERT.json) |
| PAL swap during tune read | [Expected native switching and media-swap serial sequences verified](TRACKLOADER_OFS_SWITCHING_RESULTS_OFS_MIDREAD.json) |
| NTSC writable save retry | [Writable reinsertion produced valid save data; screenshots require visual review](TRACKLOADER_WRITABLE_RETRY_RESULTS_NTSC.json) |
| NTSC save and cold reboot | [NTSC current-image save and cold reboot pass](TRACKLOADER_SAVE_REBOOT_NTSC_RESULTS.json) |
| NTSC save selection, corruption, identity and generation wrap | [NTSC native cold-boot recovery verified in game RAM](TRACKLOADER_SAVE_RECOVERY_NTSC_RESULTS.json) |
| NTSC wrong disk and reinsertion | [Expected native switching and media-swap serial sequences verified](TRACKLOADER_OFS_SWITCHING_RESULTS_NTSC_OFS_REINSERT.json) |
| NTSC swap during tune read | [Expected native switching and media-swap serial sequences verified](TRACKLOADER_OFS_SWITCHING_RESULTS_NTSC_OFS_MIDREAD.json) |

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
