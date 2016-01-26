#include <erl_nif.h>

#include <stdbool.h>
#include <stdint.h>


/*
 * 32-bits of RDRAND (Intel hardware RNG)
 */
static ERL_NIF_TERM rdrand(ErlNifEnv *env,
                           int argc __attribute__ ((unused)),
                           const ERL_NIF_TERM argv[])
{
    uint32_t n;
    if (!enif_get_uint(env, argv[0], &n))
        return enif_make_badarg(env);

    uint32_t t;
    /* XXX should have a fallback if we try a hundred times and get
     * nothing. */
    while (!__builtin_ia32_rdrand32_step(&t));
    return enif_make_uint(env, t%n);
}


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
#ifdef __amd64
    __asm__ __volatile__ ("rdtsc" : "=a" (t), "=d" (u));
#else
#error "Read your platform's perf counter here"
#endif
    state->x ^= t >> 32;
    state->y ^= t;
    state->z ^= u >> 32;
    state->w ^= u;
}


/*
 * xorshift with state stored in TLS
 */
static ERL_NIF_TERM xorshift_tls(ErlNifEnv *env,
                                 int argc __attribute__ ((unused)),
                                 const ERL_NIF_TERM argv[])
{
    static __thread bool seeded = false;
    static __thread struct random_state state;
    if (__builtin_expect(!seeded, 0)) {
        xorshift_seed(&state);
        seeded = true;
    }

    uint32_t n;
    if (!enif_get_uint(env, argv[0], &n))
        return enif_make_badarg(env);

    uint32_t t = state.x ^ (state.x << 11);
    state = (struct random_state){
        .x = state.y, .y = state.z, .z = state.w,
        .w = state.w ^ (state.w >> 19) ^ (t ^ (t >> 8))};
    return enif_make_uint(env, state.w % n);
}


/*
 * xorshift with global state; dangerous!
 *
 * I've included this just to see how the TLS cost compares.
 */
static struct random_state yolo_state;
static ERL_NIF_TERM xorshift_yolo(ErlNifEnv *env,
                                 int argc __attribute__ ((unused)),
                                 const ERL_NIF_TERM argv[])
{
    uint32_t n;
    if (!enif_get_uint(env, argv[0], &n))
        return enif_make_badarg(env);

    uint32_t t = yolo_state.x ^ (yolo_state.x << 11);
    yolo_state = (struct random_state){
        .x = yolo_state.y, .y = yolo_state.z, .z = yolo_state.w,
        .w = yolo_state.w ^ (yolo_state.w >> 19) ^ (t ^ (t >> 8))};
    return enif_make_uint(env, yolo_state.w % n);
}


/*
 * Curtis's trick: timestamp counter modulo n.  Not random at all, but
 * might be volatile enough to be acceptable.
 */
static ERL_NIF_TERM rdtsc_mod(ErlNifEnv *env,
                              int argc __attribute__ ((unused)),
                              const ERL_NIF_TERM argv[])
{
    uint32_t n;
    if (!enif_get_uint(env, argv[0], &n))
        return enif_make_badarg(env);

    uint64_t u, v;
#ifdef __amd64
    __asm__ __volatile__ ("rdtsc" : "=a" (u), "=d" (v));
#else
#error "Read your platform's perf counter here"
#endif
    /* properly, (u | v<<32) */
    return enif_make_uint(env, u%n);
}


/*
 * The Curtis PRNG combined with xorshift to try to be less
 * predictable.  Probably not good.
 */
static ERL_NIF_TERM xorshift_rdtsc(ErlNifEnv *env,
                                   int argc __attribute__ ((unused)),
                                   const ERL_NIF_TERM argv[])
{
    uint32_t n;
    if (!enif_get_uint(env, argv[0], &n))
        return enif_make_badarg(env);

    struct random_state state = {
        .x = 123456789,
        .y = 362436069,
        .z = 521288629,
        .w = 88675123
    };

    uint64_t u, v;
#ifdef __amd64
    __asm__ __volatile__ ("rdtsc" : "=a" (u), "=d" (v));
#else
#error "Read your platform's perf counter here"
#endif
    state.x ^= u >> 32;
    state.y ^= u;
    state.z ^= v >> 32;
    state.w ^= v;

    uint32_t t = state.x ^ (state.x << 11);
    state = (struct random_state){
        .x = state.y, .y = state.z, .z = state.w,
        .w = state.w ^ (state.w >> 19) ^ (t ^ (t >> 8))};
    return enif_make_uint(env, state.w % n);
}


static int on_load(ErlNifEnv *env __attribute__((unused)),
                   void **priv __attribute__((unused)),
                   ERL_NIF_TERM info __attribute__((unused)))
{
    xorshift_seed(&yolo_state);
    return 0;
}

static int upgrade(ErlNifEnv* env __attribute__((unused)),
                   void ** priv __attribute__((unused)),
                   void **old_priv __attribute__((unused)),
                   ERL_NIF_TERM load_info __attribute__((unused)))
{
    return 0;
}


static ErlNifFunc nif_functions[] = {
    {"rdrand", 1, rdrand, 0},
    {"xorshift_tls", 1, xorshift_tls, 0},
    {"xorshift_yolo", 1, xorshift_yolo, 0},
    {"xorshift_rdtsc", 1, xorshift_rdtsc, 0},
    {"rdtsc_mod", 1, rdtsc_mod, 0}
};

ERL_NIF_INIT(granderl, nif_functions, &on_load, NULL, &upgrade, NULL)
