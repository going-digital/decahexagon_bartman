# Display pipeline benchmark — 23 September 2026

The wider baseline view is retained. Pipelining the free third frame recovers
most of the time previously spent waiting for display acknowledgement.

| Workload | Previous Copperline FPS | New Copperline FPS | New FS-UAE FPS |
| --- | ---: | ---: | ---: |
| Hexagon PAL | 26.20 | 40.50 | 40.51 |
| Hexagon NTSC | 27.98 | 41.88 | 42.01 |
| Hexagonest PAL | 22.73 | 33.73 | 33.74 |

An isolated old-view control with the same blocking handoff measured 26.37 FPS
in PAL Hexagon, versus 26.20 with corrected visibility. In this workload the
visibility change costs less than 1%; the blocking handoff dominated the loss.

## Implementation and ownership

Three 1-KiB copper lists pair with the existing three bitplanes and player
sprite banks: displayed, pending, and free for rendering. Only the free set
is modified. After rendering, the CPU waits for the previous pending frame's
acknowledgement before clearing the retired display and queuing the new frame.
The clear can overlap list construction and presentation.

COP1 always points to a fixed dispatcher that waits for line 16 (ignoring
blitter busy) then jumps through COP2. The VBlank handler updates COP2 before
line 8. It briefly masks higher interrupts around the beam check and pointer
write so an audio IRQ cannot push the write across the dispatcher deadline.
Late interrupts leave the old list selected and acknowledgement pending.
No CPU COPJMP or blitter-DMA pause is used during steady-state presentation.

Cost versus the blocking handoff: 1,024 extra Chip bytes for the third list
and four extra bytes for the dispatcher. No extra bitplane/player bank.

## Measurement and limits

Stock A500 OCS, 512 KiB Chip + 512 KiB Slow RAM, no Fast RAM/JIT, cycle-based
emulation. Release compilation with music and SFX active. Copperline's host
audio output is muted; guest Paula work remains enabled.

Isolated benchmark copies fix RNG to 0x2545, automatically start, supply no
steering, and suppress death after executing collision. Each covers 60 seconds
of simulated play. FPS counts completed render-loop iterations divided by
incoming VBlank intervals, not host presentation rate. Measurement includes
instrumentation and approximately one frame of boundary uncertainty. These
are seeded average workloads, not physical-hardware or worst-case guarantees.

The previous work-time counter ended after queueing; the new one can include
waiting for the previous frame at the end of rendering. Compare FPS directly;
do not treat those work-time counters as identical isolated renderer timings.

Raw counts, executable hashes, controls and both emulator results are in
[display_pipeline_benchmark_results.json](display_pipeline_benchmark_results.json).
Complete local sources and executables are under
`scratchpad/frame_benchmark/visibility_pipeline/`; controls are in
`visibility_current/` and `handoff_oldview/`. The FS-UAE runner is
`scratchpad/frame_benchmark/run_visibility_pipeline_fsuae.py`.

Host checks exercise actual publication/IRQ code with interrupts throughout
list construction and third-buffer reuse, including delayed PAL/NTSC IRQs.
Both emulators completed all three workloads and returned normally.
