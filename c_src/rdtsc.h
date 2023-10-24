#pragma once

#include <stdint.h>

#ifdef __amd64
inline void rdtsc(uint64_t *t, uint64_t *u)
{
    asm volatile ("rdtsc" : "=a" (*t), "=d" (*u));
}
#elif defined(__aarch64__)
// Userspace equivalent of rdtsc readable in user-space:
// https://developer.arm.com/documentation/ddi0595/2021-12/AArch64-Registers/CNTVCT-EL0--Counter-timer-Virtual-Count-register
inline void rdtsc(uint64_t *t, uint64_t *u __attribute__((unused)))
{
    asm volatile("mrs %0, cntvct_el0" : "=r" (*t));
}
#else
#error "Read your platform's perf counter here"
#endif
