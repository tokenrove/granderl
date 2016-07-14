/*
 * xorshift with state stored in TLS
 * Derived from Marsaglia's paper.
 */

#include "granderl.h"
#ifdef HAVE_RDRAND
#include "rdrand.h"
#else
#include "rdtsc.h"
#endif

struct random_state { uint32_t x, y, z, w; };


static void xorshift_seed(struct random_state *state)
{
    *state = (struct random_state) {
        .x = 123456789,
        .y = 362436069,
        .z = 521288629,
        .w = 88675123
    };

    uint64_t t, u;
#ifdef HAVE_RDRAND
    t = rdrand64();
    u = rdrand64();
#else
    rdtsc(&t, &u);
#endif
    state->x ^= t >> 32;
    state->y ^= t;
    state->z ^= u >> 32;
    state->w ^= u;
}


uint32_t uniform32(void)
{
    static __thread bool seeded = false;
    static __thread struct random_state state;
    if (unlikely(!seeded)) {
        xorshift_seed(&state);
        seeded = true;
    }

    uint32_t t = state.x ^ (state.x << 11);
    state = (struct random_state){
        .x = state.y, .y = state.z, .z = state.w,
        .w = state.w ^ (state.w >> 19) ^ (t ^ (t >> 8))};
    return state.w;
}


ERL_NIF_TERM uniform_1(ErlNifEnv *env,
                       int argc __attribute__ ((unused)),
                       const ERL_NIF_TERM argv[])
{
    uint32_t n;
    if (unlikely(!enif_get_uint(env, argv[0], &n) || 0 == n))
        return enif_make_badarg(env);

    uint32_t r = 1 + (((uint64_t)uniform32() * n) >> 32);
    return enif_make_uint(env, r);
}
