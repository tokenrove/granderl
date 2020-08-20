/*
 * middle-square weyl sequence with state stored in TLS
 *
 * See "Middle Square Weyl Sequence RNG" by Widynski,
 * <https://arxiv.org/pdf/1704.00358.pdf>
 */

#include "granderl.h"
#ifdef HAVE_RDRAND
#include "rdrand.h"
#else
#include "rdtsc.h"
#endif

struct random_state {
  uint64_t x, w, s;
};

static void msws_seed(struct random_state *state)
{
    uint64_t t, u;
#ifdef HAVE_RDRAND
    t = rdrand64();
    u = rdrand64();
#else
    rdtsc(&t, &u);
#endif
    state->x = t;
    state->w = 0;
    state->s = (u<<1) | 1;
}


uint32_t uniform32(void)
{
    static __thread bool seeded = false;
    static __thread struct random_state state;
    if (unlikely(!seeded)) {
        msws_seed(&state);
        seeded = true;
    }

    state.x *= state.x;
    state.w += state.s;
    state.x += state.w;
    state.x = (state.x>>32) | (state.x<<32);
    return state.x;
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
