/*
 * 32-bits of RDRAND (Intel hardware RNG)
 *
 * Note that this just fetches 64-bits and truncates; we could have a
 * variant that buffers half, but then it would need synchronization.
 */

#include "granderl.h"
#include "rdrand.h"

ERL_NIF_TERM uniform_1(ErlNifEnv *env,
                       int argc __attribute__ ((unused)),
                       const ERL_NIF_TERM argv[])
{
    uint32_t n;
    if (unlikely(!enif_get_uint(env, argv[0], &n) || 0 == n))
        return enif_make_badarg(env);

    uint32_t t = rdrand64();
    uint32_t r = ((uint64_t)(t*n)>>32);
    return enif_make_uint(env, 1 + r);
}


uint32_t uniform32(void) { return rdrand64(); }
