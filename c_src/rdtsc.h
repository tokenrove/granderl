#pragma once

#include <stdint.h>

#ifdef __amd64
inline void rdtsc(uint64_t *t, uint64_t *u)
{
    asm volatile ("rdtsc" : "=a" (*t), "=d" (*u));
}
#else
#error "Read your platform's perf counter here"
#endif
