# Game backlog

Recorded 2026-09-20 for pickup next week. These are open tasks, not commitments
to complete them all next week. Historical implementation details remain in
[PC accuracy progress](PC_ACCURACY_PROGRESS.md).

## Requested features

- [ ] **Game credits.** Add an accessible credits screen covering the original
  game, music, Amiga port and relevant contributors/third-party components.
  Review names and attribution text before release.
- [ ] **Save high scores and unlocks.** Persist per-mode records and unlocked
  modes across application launches; these currently last only for the session.
  Define a versioned save format and safe disk access outside hardware takeover.
  Handle missing/corrupt saves and read-only media without preventing play.
- [ ] **Steam-style achievements.** Define achievement conditions, track awards,
  display notifications and provide a view of earned/locked achievements.
  Persist awards with the save data and avoid awarding the same achievement
  repeatedly. This is an in-game feature; Steam service integration is not yet
  specified.
- [ ] **Arcade mode.** Define the rules, progression, scoring and retry behavior
  before implementation. Keep its records distinct where its rules differ from
  the existing modes.
- [ ] **Otis and Focus soundtracks.** Convert and integrate both tracks, verify
  their PC level mappings, loop boundaries, start-offset probabilities and pulse
  cues, and implement track changes. Courtesy currently plays for every profile.
  Measure memory/loading costs on the supported A500 configuration and validate
  PAL/NTSC playback, retries and handoffs. See
  [current soundtrack work](soundtrack/README.md) and
  [implemented Courtesy start behavior](soundtrack/MUSIC_STARTS.md).

## Remaining behavior and presentation gaps

- [ ] **Scripted ending.** Complete the ending sequence and its stage-2 camera
  freeze, audio and transition behavior against the PC reference.
- [ ] **Camera and death presentation parity.** Audit remaining perspective,
  field motion/shake, death flash and audio/cue differences against the current
  implementation. Preserve completed projection, pulse and fade work; measure
  the rendering cost of any additions. See
  [visual synchronization](soundtrack/VISUAL_SYNC.md).
- [ ] **Tutorial and unlock presentation.** Implement the first-run tutorial and
  PC unlock announcements/automatic selection transitions. Unlock eligibility
  already exists; persistence is tracked above.
- [ ] **Options and leaderboard presentation.** Review the missing PC screens
  and decide which belong in the Amiga version. Online leaderboard support is
  not yet specified.
- [ ] **Verify the reported pre-speech click is resolved.** DMA startup was
  hardened, but the original audible symptom was not reproduced in captured
  tests. Recheck immediately before spoken effects on the affected setup;
  investigate further if it persists. See
  [audio investigation](soundtrack/SOUND_EFFECTS.md#pre-speech-click-investigation-2026-09-20).

## Retry behavior reference

Ordinary PC retries clear walls, wave progression and elapsed time; they do not
resume at a later checkpoint. Random soundtrack offsets do not advance gameplay.
Hyper modes start at difficulty progression equivalent to 60 seconds into the
corresponding normal mode while displaying a zero timer. The port already
implements that distinction; no checkpoint feature is required for PC parity.

Local reference evidence: `gameclass::restart()` at `0x100029c40`,
`gameclass::restarthyper()` at `0x100029e00`, and retry branches in
`gameinput` at `0x100052980`, in the inspected PC executable identified in
[soundtrack start provenance](soundtrack/MUSIC_STARTS.md).
