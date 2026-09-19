#pragma once
typedef void (*PaulaCallback)(unsigned channel);
void paula_irq_init(void);
void paula_irq_set(unsigned channel,PaulaCallback callback);
void paula_irq_shutdown(void);
