# Plan: PC-accurate Super Hexagon mechanics on the Amiga A500

Status: live normal-Hexagon integration in progress, 2026-09-19. See [progress and verification](PC_ACCURACY_PROGRESS.md). The phases below remain the acceptance plan; an initial implementation batch is not completion of the full specification.

## Target and definition of accuracy

Use the fingerprinted desktop executable in [the PC verification report](scratchpad/pc_verification/README.md) as the behavioral reference. The [web audit](scratchpad/web_verification/README.md) supplies useful names and comparisons, not substitute mechanics. Older scratchpad reports and source comments that conflict with these audits are superseded.

Deliver all three normal levels and their hyper variants, including their own selectors, controls, wall geometry, polygon transitions, progression, death/restart and presentation effects. Establish a fully verified normal Hexagon vertical slice first; it is a milestone, not a reduction of final scope.

Separate three acceptance targets:

1. **Simulation fidelity:** identical gameplay decisions and state transitions for the same initial state, logical input and supplied random draws at the reference update cadence.
2. **Presentation fidelity:** preserve visible geometry, camera behavior, color relationships and cue-driven pulse within explicitly measured OCS rendering limits. Screen resolution and palette quantization cannot be pixel-identical to the desktop.
3. **Platform behavior:** responsive keyboard/joystick input, stable PAL/NTSC timing, safe resource use and bootable releases on the supported A500 configurations.

Reference cadence should initially be desktop 60 Hz with `dt=1`. This is an explicit compatibility target, not a claim that the desktop uses a fixed-step scheduler. Its variable-delta, rounding and long-frame clamp behavior is a separate compatibility issue to document and test. Do not import the web port's timing semantics as evidence for the PC.

Keep the 1 MB A500 configuration and the existing aspiration to support 512 KB chip-only machines. Re-measure before promising either complete asset set: `notes_memory.md` describes an older executable and is not a current budget measurement. No memory expansion requirement should emerge silently from implementation.

## Baseline problems addressed by this plan

| Code at plan creation | Required change |
| --- | --- |
| `game.c` uses screen-radius walls and speed-derived spawn distance/thickness | Keep the PC's world distances and widths; convert only in rendering |
| `patterns.c` emits timed mask steps with approximate patterns | Emit exact per-wave records immediately at their authored distances |
| Generic early/mid/late pools and fairness-biased anchors | Preserve stage-specific branch ordering, thresholds and RNG consumption |
| Default slot flick, fixed six sides, radial-overlap death | Continuous turning, polygon morph state and separate frontal-hit/side-block behavior |
| One update per completed render loop | Decouple elapsed simulation time from render completion |
| Input cancels opposing directions | Preserve both held directions and apply the reference priority in the simulation |
| `MAX_WALLS=32`, silent failure when full | Preserve enough records for overlapping PC waves and command markers; no silent drops |
| Whole-scene zoom and BPM-triggered pulse | Reference camera transform plus cue-driven radial displacement |
| Single playfield colour and approximate transitions | Add the color layers/effects needed for the verified presentation |

Retain the platform takeover, input hardware handling, trig support, clipping/blitter primitives, buffering, sprite HUD and audio backend where they remain suitable. Their interfaces will change; their output still needs regression checks.

## Architecture

The simulation must compile both for a host executable and for the 68000 without hardware registers, dynamic allocation during play, renderer calls or audio-player dependencies.

Suggested responsibilities—not a requirement to create every file immediately:

- `game.c/.h`: run state, ordered tick, score, death and progression.
- `patterns.c/.h`: exact wave emission by source wave ID.
- `levels.c/.h`: normal/hyper setup and per-stage scheduling decisions.
- `effects.c/.h`: polygon morph, planar rotation, camera and pulse state.
- `game_types.h`: explicitly sized portable types and numeric conversion helpers, with compile-time size checks supported by both compilers.
- `render.c`: read-only render snapshot and PC-world-to-OCS projection.
- `main.c`, `input.c`, `audio.c`, `hud.c`: platform clock, input capture, event consumption and presentation.
- Host-only probes and fixture generators under `tests/` and/or `tools/`, excluded explicitly from the Amiga build.

The current Makefile collects C sources from immediate subdirectories. Replace that implicit discovery with explicit target/host source lists before adding host tests; otherwise a test main or host dependency can enter the Amiga link accidentally.

Use explicit fields for stage, hyper flag, wave counter, countdown, actual wall speed, effective score, displayed score, polygon count/fraction/state, marker wait, player candidate/previous angle, hit state and effect phases. Do not reuse a single enum for stage, rank, menu and run status.

Preserve the logical order of wall records. Command markers may have an explicit kind rather than magic render slots, but must retain source ordering, motion and dispatch timing relative to ordinary records. Do not put them in an independently sorted queue without proving equivalence.

## Phase 0 — Turn the audit into an executable specification

**Work**

- Move the required compact specifications and derived fixtures into a versioned location; the current audit is under ignored `scratchpad`. Retain executable hash and source-address provenance. The proprietary executable remains an external local reference, not a checked-in dependency.
- Define the exact tick order from input through visual/score updates, collision, motion, marker consumption and scheduling. Record tie cases explicitly.
- Extend original-machine-code probes for unresolved reset/continuation/ending paths, multi-wall ordering, morph boundaries, non-six-sided generator states, and relevant numeric conversions. Do not fill those gaps by guessing from the web version.
- Add a reference-run format: initial state, logical inputs, RNG draw tape, tick number, emitted events and selected state snapshots. First mismatch should identify a field and tick.
- Re-measure current release memory, stack allowance and raster time, including audio, HUD and keyboard servicing. Preserve a reproducible baseline before replacing mechanics.

**Exit:** every behavior required by the first vertical slice has a source-backed rule or a named unresolved item; build and baseline measurements are reproducible. Later stages cannot claim full fidelity while their required reference paths remain unresolved.

## Phase 1 — Portable core, clock, input and numeric policy

**Work**

- Add host/Amiga build separation, logical input bits and injectable bounded RNG. Preserve both held directions so the core can reproduce priority; map joystick and keyboard signs against rendered motion.
- Separate the PC integer-degree player angle from the renderer's 16-bit full-turn angle. Converting 7 degrees once to an approximate increment and accumulating it would introduce avoidable drift. Preserve integer division in collision slot selection.
- Store wall distances/widths in PC units. Audit maxima over all fixtures before choosing 16-bit storage; use explicitly wide intermediates for sums and transforms. Use wide score/time counters and modularly safe platform-clock differences.
- Choose representation per field: integers where the reference is integer, fixed-point/table-driven state where it reproduces observable float behavior. Validate fractional delays, morph accumulation and threshold crossings against the oracle. Blindly replacing `0.1f` with an exact tenth changes the observed morph completion tick.
- Run nominal 60 Hz simulation independently of display. PAL executes six logic ticks over five 50 Hz display periods; NTSC nominally one per period. Consume button edges once, retain held state for subsequent ticks, and do not multiply gameplay constants by PAL frame rate.
- Base elapsed time on a reliable hardware clock or calibrated display cadence; distinguish nominal standards from measured refresh. Render the latest completed state first; interpolation is optional only after discrete correctness.
- Define missed-frame policy explicitly: preserve elapsed simulation ticks while skipping presentation work as needed, instrument backlog, and treat sustained inability to keep up as a performance failure. Never silently lower game speed. This policy targets normal 60 Hz PC behavior, not its pathological long-frame clamp.

**Exit:** identical logical inputs/draws produce identical host and target state traces; PAL and NTSC advance the same reference state for the same simulation-tick sequence. No per-tick software floating-point requirement is introduced without a measured justification.

## Phase 2 — Exact wall lifecycle, controls and collision

**Work**

- Replace flick gameplay with reference continuous movement: normal stage rates 7/7/9 degrees per reference tick. Keep menu flick behavior separate when implementing level selection.
- Replace radial-overlap death with the ordered reference collision loop: slot calculation, distance <151, lethal boundary `145-speed`, and previous-angle restoration for passed fronts with width >199.
- Preserve wall movement truncation, center clamp, width shrink at ordinary travel speed, active-record reuse and loop ordering. Remove four-times hub shrink and speed-derived thickness.
- Implement marker kinds 20/21/22 and their exact consumption effects. Keep these invisible to the renderer.
- Begin with the source array capacity (500 records, subject to confirming allocation boundaries) to avoid changing behavior while measuring high-water usage. A compact representation is only several kilobytes; scan cost, not just storage, must be measured. Any later capacity reduction needs an actual bound, not only a sampled maximum.
- Remove silent spawn failure and expose allocation diagnostics. No renderer optimization may delete simulation walls or merge their collision contributions.

**Exit:** original-code fixtures match at collision boundaries, two-direction input, angle wrap, wall exhaustion/reuse, multiple matching walls, center-crossing and marker timing. The existing gameplay assumptions no longer participate in the accurate mode.

## Phase 3 — Exact wave generator

**Work**

- Replace `PStep` time-offset approximations with compact authored records and small explicit branch emitters. Preserve wave IDs, independent anchors, literal slots, widths, tier distances, command records, return delay, marker wait and RNG call ordering.
- Implement all 45 known IDs, including 300/400/500/600 families and ending waves. Keep implemented-but-unselected IDs distinct from unimplemented requests; do not invent a fallback for source request 405 if it really emits nothing.
- Encode repeated geometry with shared immutable templates where output remains identical. Do not store all 3,053 expanded random outcomes in the shipping executable.
- Remove fairness-biased anchors, generic side-count fallback, power-of-two pool substitutions, invented calm gaps and consolidated “inspired by” patterns from the reference path.
- Validate the new C output against all 9,159 saved generator cases, including record order, widths, distances, delay and RNG count. Add reachable side-count states not covered by the original six-sided fixture set.

**RNG policy:** fixtures supply draws directly. Shipping bounded selection must preserve the reference branch domains/probabilities; the current mask-and-wrap slot generator is biased. Exact libc random streams are a separate objective: they require the actual algorithm/state and all consumers, including audiovisual draws. Do not claim identical PC seeds from matching wave output under supplied draws.

**Exit:** exact generator fixture match, with a measured code/data footprint and no runtime dependency on the decompiler, JSON or desktop executable.

## Phase 4 — Stage 0 vertical slice, then all stage selectors

**Work**

- Implement normal Hexagon initialization, opener 90, selector precedence, speed updates, countdowns and shape-counter behavior. Include polygon morphs and marker waits before calling this slice playable/accurate.
- Match effective versus actual polygon count during shrinking, fraction transitions and their source rounding. Preserve integer side count for collision independently from visual angular interpolation.
- Add Hexagoner/Hexagonest and hyper setup, openers 91–95, per-stage rate/speed/pool differences, score gates, no-emission branches, forced-wave exceptions and rotation/effect triggers.
- Use the existing 184,320 selector state samples as regression inputs, expanding coverage at exact counter/score thresholds and around initialization/transitions. Synthetic selector states are not proof of natural reachability; add linked traces across scheduling events.
- Exercise complete normal and hyper run sequences with deterministic inputs and draws, including no-input collision runs and scripted state-transition probes.

**Exit:** all six modes dispatch the correct IDs and events, advance/reset the correct counters, preserve delay/speed values, and produce matching polygon evolution. No universal “wave12 moderate pool” or “every fourth wave always101” shortcut remains.

## Phase 5 — A500 renderer for reference geometry

Start renderer feasibility tests during phases 2–4; finish visual integration once the core state is stable.

**Work**

- Convert PC radial geometry at the render boundary: hub + cue displacement + the source's integer distance/5, corresponding width conversion, and reference player placement. Keep viewport scaling independent of spawn distances, speed and collision.
- Implement morph geometry using the source's effective angular width, planar rotation/bursts, then camera X/Y tilt and perspective in fixed-point. Establish projection constants from reference exports/captures before tuning by eye.
- Cache per-frame transformed sector boundaries and shared geometry. Cull offscreen render work while retaining every simulation record.
- Replace or reorganize `wall_at`'s repeated full scans before dense content makes rendering quadratic. Build render-only adjacency/interval data without reordering authoritative wall processing.
- Verify filled-polygon overlap behavior. A single XOR outline/fill pass can create holes where polygons overlap; test real overlapping PC records and use a correct union/layer strategy where needed. Exact matching-edge cancellation alone is not a proof of correct coverage.
- Add sufficient colour layers for alternating sectors, wall/hub/player relationships and effects. Benchmark two- and three-bitplane implementations before fixing the design. At 320×200 with three buffers, these cost48,000 and72,000 bytes of bitplane storage respectively, excluding sprites/copper/scratch.
- Retain the sprite HUD where useful. Adapt it to per-level score, ranks and menu/ending states. Reduce drawing cost before reducing the update cadence or wall content.

**Exit:** captured reference states show matching visible gaps, morph boundaries and player position; no overlap holes or clipping corruption; camera/pulse remain presentation-only. Worst-case rendering fits the measured hardware frame budget with audio and input active, or an explicitly documented presentation-rate compromise is evaluated without changing simulation speed.

## Phase 6 — Progression, selection, death and persistence

**Work**

- Implement all six selectable modes, unlocks, separate per-mode records and strict PC rank thresholds. Do not use web inclusive comparisons or its persistent-rank retry behavior.
- Complete reference start/restart, death freeze/fade, stage continuations, hyper transitions and scripted ending. Remove arbitrary current READY/DEAD durations where they disagree.
- Match score advancement on the death tick, counter resets, effective-score offsets and transition motion overrides.
- Feed sound/HUD commands through a bounded event interface with verified ordering and consumption. A PAL frame containing two logic ticks must not lose an event or replay a one-shot twice.
- Choose an Amiga persistence path compatible with the release medium. The current directly bootable ADF is not automatically a writable score filesystem; keep session records working and specify disk-save behavior separately. Avoid runtime OS calls while the system is taken over.

**Exit:** scripted progression traces cover every normal/hyper transition and ending branch included in the target, with correct unlock/record state across retries and supported persistence boundaries.

## Phase 7 — Cue-driven pulse, audio and palette integration

**Work**

- Replace BPM-as-pulse with reference per-song cue data, envelope logic, offsets, freeze/menu overrides and loop handling. Advance cue lookup from the playback position of the corresponding track, not blindly from survival time.
- Expose a stable audio-position/track-start interface from the CIA-driven backend. Verify restart, random offsets, loops, pause behavior and event latency across PAL/NTSC; compensate measured offsets explicitly.
- Reproduce planar bursts and camera effects separately from the radial cue envelope. Implement palette ramps, interpolated changes and collision flashes in OCS colour precision.
- Establish the music/announcer/sample asset strategy and memory budget early, before promising original audio fidelity. Existing tracker music is not the PC soundtrack: applying PC cue samples to it would be mathematically reference-like but audibly unsynchronized. Exact reference music timing and replacement-music timing are different deliverables.
- Measure resident versus disk-compressed sample size; disk compression does not reduce decoded Chip RAM use. Consider shared sample banks, score data outside Chip RAM and track loading at safe boundaries.

**Exit:** pulse follows the intended playback timeline, event ordering matches, and the release states accurately whether soundtrack/presentation is reference-faithful or adapted. There is no hidden BPM surrogate in the accurate path.

## Phase 8 — Target validation and release gate

- Run host fixture comparisons, target trace comparisons, and PAL/NTSC release builds. Retest numeric helpers under the actual `-Ofast`/LTO configuration and inspect expensive arithmetic/compiler-support calls in hot paths.
- Test stock 68000/OCS emulator configurations with accurate chipset timing, then real hardware when available. Record CPU/memory configuration, peak active walls, raster budget, missed presentations, simulation backlog and audio stability.
- Stress thick/overlapping patterns, six-to-three-to-six morphs, marker waits, transitions, speed 40, large ending geometry and input during missed display frames.
- Report Chip RAM, other RAM, stack headroom and audio/transient allocation peaks for both target memory configurations. Audit the packed loader's peak requirements, not just the final executable size.
- Compare captures/replays at identical simulation ticks. Distinguish intentional resolution/palette/audio adaptation from a simulation discrepancy.

**Done means:** all required reference fixtures and full-mode transition traces pass; no silent wall/event loss or game-speed drift; shipped configuration meets measured A500 budgets; every remaining departure from the verified desktop behavior is listed explicitly.

## Recommended implementation batches

1. Reference fixtures, explicit build lists, portable state/clock/input interfaces and baseline measurements.
2. Exact walls, controls, collision and markers, with a temporary renderer adapter.
3. Exact wave generator and full differential generator tests.
4. Normal Hexagon selector/morph vertical slice; then the other five modes and selector traces.
5. Projection/overlap/colour renderer and full progression/menu/death state machine.
6. Audio assets, cue/palette synchronization, memory/performance tuning and target release verification.

Rendering feasibility, memory measurement and unresolved reference probes run early enough to change implementation choices before they become expensive. Each batch should be reviewable and leave the target build runnable; an intermediate renderer adapter must be labelled as such rather than presented as completed fidelity.
