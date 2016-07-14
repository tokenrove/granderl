/*
 * PCG32 with state stored in TLS
 *
 * Derived from
 *   https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c,
 * Any mistakes are due to the me, Julian Squires <julian@cipht.net>,
 * not the original author.  The original has the following copyright
 * header:
 *
 * Copyright 2014 Melissa O'Neill <oneill@pcg-random.org>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For additional information about the PCG random number generation scheme,
 * including its license and other licensing options, visit
 *
 *       http://www.pcg-random.org
 *
 */

#include "granderl.h"
#ifdef HAVE_RDRAND
#include "rdrand.h"
#else
#include "rdtsc.h"
#endif

uint32_t uniform32(void)
{
    static __thread bool seeded = false;
    static __thread uint64_t state;
    if (unlikely(!seeded)) {
#ifdef HAVE_RDRAND
        state = rdrand64();
#else
        uint64_t u;
        rdtsc(&state, &u);
#endif
        seeded = true;
    }

    uint64_t oldstate = state;
    state = oldstate * 6364136223846793005ULL;
    uint32_t xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
    uint32_t rot = oldstate >> 59u;
    uint32_t v = (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
    return v;
}

ERL_NIF_TERM uniform_1(ErlNifEnv *env,
                       int argc __attribute__ ((unused)),
                       const ERL_NIF_TERM argv[])
{
    uint32_t n;
    if (unlikely(!enif_get_uint(env, argv[0], &n) || 0 == n))
        return enif_make_badarg(env);

    /* XXX this introduces bias and we could do better.  But this is
     * explicitly a quick and dirty generator. */
    uint32_t r = 1 + (((uint64_t)uniform32() * n) >> 32);
    return enif_make_uint(env, r);
}
