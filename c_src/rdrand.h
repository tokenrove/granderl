#pragma once

#include <stdint.h>

#ifdef HAVE_RDRAND
inline uint64_t rdrand64(void) {
    uint64_t t;
    /* XXX should have a fallback if we try a hundred times and get
     * nothing. */
    asm volatile("0:   rdrand %0; jnc 0b" : "=r" (t));
    return t;
}
#else
#error "A platform with rdrand support is required"
#endif
