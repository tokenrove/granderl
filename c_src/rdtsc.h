#pragma once

#include <stdint.h>

#ifdef __amd64
inline void rdtsc(uint64_t *t, uint64_t *u)
{
    asm volatile ("rdtsc" : "=a" (*t), "=d" (*u));
}
#elif defined(__aarch64__) && defined(__APPLE__)
#include <mach/mach_time.h>

inline void rdtsc(uint64_t *t, uint64_t *u __attribute__((unused)))
{
    *t = mach_absolute_time();
}
#else
#error "Read your platform's perf counter here"
#endif
