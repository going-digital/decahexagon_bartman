#include <stdint.h>

typedef struct _tCopMoveCmd {
    unsigned bfUnused:7;
    unsigned bfDestAddr:9;
    unsigned bfValue:16;
} tCopMoveCmd;

typedef struct _tCopWaitCmd {
    unsigned bfWaitY:8;
    unsigned bfWaitX:7;
    unsigned bfIsWait:1;
    unsigned bfBlitterIgnore:1;
    unsigned bfVE:7;
    unsigned bfHE:7;
    unsigned bfIsSkip:1;
} tCopWaitCmd;

typedef union _tCopCmd {
    tCopMoveCmd sMove;
    tCopWaitCmd sWait;
    uint32_t ulCode;
} tCopCmd;

typedef struct _tCopBfr {
    uint16_t uwAllocSize;
    uint16_t uwCmdCount;
    tCopCmd *pList;
} tCopBfr;

typedef struct _tCopBlock {
    struct _tCopBlock *pNext;
    tUwCoordYX uWaitPos;
    uint16_t uwMaxCmds;
    uint16_t uwCurrCount;
    uint8_t ubDisabled;
    uint8_t ubUpdated;
    uint8_t ubResized;
    tCopCmd *pCmds;
} tCopBlock;

typedef struct _tCopList {
    uint16_t uwBlockCount;
    uint8_t ubStatus;
    uint8_t ubMode;
    tCopBfr *pFrontBfr;
    tCopBfr *pBackBfr;
    tCopBlock *pFirstBlock;
} tCopList;

typedef struct _tCopManager {
    tCopList *pCopList;
    tCopList *pBlankList;
} tCopManager;
