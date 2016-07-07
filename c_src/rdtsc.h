#pragma once

#include <stdint.h>

#ifdef __amd64
inline void rdtsc(uint64_t *t, uint64_t *u)
{
    __asm__ __volatile__ ("rdtsc" : "=a" (*t), "=d" (*u));
}
#else
#error "Read your platform's perf counter here"
#endif
