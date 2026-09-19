#pragma once

// Music playback. Two mutually exclusive back ends selected in config.h:
//   MUSIC_LSP - LightSpeed Player (CIA timed)
//   MUSIC     - ThePlayer 6.1a (VBL timed, driven from interruptHandler)

#include <exec/types.h>
#include "config.h"

// Doynax LZ depacker (returns end of output data; input must be 16-bit aligned).
void* doynaxdepack(const void* input, void* output);

#ifdef MUSIC_LSP
int p61Init(APTR vbr);
int p61Music(void);
void p61End(void);
UWORD audio_get_bpm(void); // current music BPM, live (tracks a mid-song tempo change)
#endif

#ifdef MUSIC
extern const void* module;
int p61Init(const void* module);
void p61Music(void);
void p61End(void);
#endif
