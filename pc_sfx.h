#pragma once
#include "pc_core.h"
enum { SFX_BEGIN,SFX_EXCELLENT,SFX_GAMEOVER,SFX_START,SFX_DIE,SFX_RANKUP,
 SFX_LINE,SFX_TRIANGLE,SFX_SQUARE,SFX_PENTAGON,SFX_HEXAGON,SFX_MENUSELECT,
 SFX_MENUCHOOSE,SFX_AWESOME,SFX_WONDERFUL,SFX_SUPERHEXAGON,SFX_COUNT };
#define SFX_BIT(id) ((uint16_t)(1u<<(id)))
typedef struct { uint32_t best; uint8_t rank,record,completed,completion,profile,startup; } PcSfx;
void pc_sfx_init(PcSfx *s);
uint16_t pc_sfx_startup(PcSfx *s);
uint16_t pc_sfx_begin(PcSfx *s,uint32_t best,uint8_t completed,uint8_t profile);
uint16_t pc_sfx_live(PcSfx *s,uint32_t elapsed);
uint16_t pc_sfx_death(const PcSfx *s,unsigned timer,unsigned old_extent,unsigned extent);
