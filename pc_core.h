#pragma once
/* Freestanding Amiga SDK has no libc stdint.h; GCC supplies exact types. */
#ifdef __m68k__
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
#else
#include <stdint.h>
#endif
typedef char pc_u16_size_check[sizeof(uint16_t) == 2 ? 1 : -1];
typedef char pc_u32_size_check[sizeof(uint32_t) == 4 ? 1 : -1];

/* PC reference units, independent of screen resolution and PAL/NTSC. */
#define PC_TICK_RATE 60
#define PC_WALL_CAPACITY 500
#define PC_INPUT_POSITIVE 1u /* PC left input43 */
#define PC_INPUT_NEGATIVE 2u /* PC right input44 */

typedef struct {
    int32_t distance;
    int32_t width;
    uint8_t slot; /* Includes command slots 20/21/22. */
    uint8_t active;
} PcWall;

typedef struct {
    int16_t angle;
    int16_t previous_angle;
    uint8_t hit;
    uint8_t blocked;
} PcPlayer;

typedef struct { uint16_t remainder; } PcClock;

/* Nominal reference cadence; does not emulate the desktop long-frame clamp. */
uint32_t pc_clock_advance(PcClock *clock, uint16_t display_frames, uint16_t display_hz);
int16_t pc_turn(int16_t angle, uint8_t held, uint8_t degrees_per_tick);
uint16_t pc_render_angle(int16_t degrees);
/* Ordered collision pass BEFORE wall motion. Integer speed, dt=1 baseline. */
void pc_collide(PcPlayer *player, const PcWall *walls, uint16_t count,
                uint8_t sides, int16_t speed);
/* Ordinary wall motion only; marker dispatch and transition overrides belong to caller. */
void pc_move_wall(PcWall *wall, int16_t speed);
