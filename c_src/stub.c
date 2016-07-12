#include "granderl.h"


int enif_get_uint(ErlNifEnv *env __attribute__((unused)), ERL_NIF_TERM t, unsigned *ip)
{
    *ip = t;
    return 1;
}


ERL_NIF_TERM enif_make_uint(ErlNifEnv *env __attribute__((unused)), unsigned i)
{
    return i;
}


ERL_NIF_TERM enif_make_badarg(ErlNifEnv *env __attribute__((unused)))
{
    abort();
}


void benchmarkable(void)
{
    uniform_1(NULL, 1, (const ERL_NIF_TERM[]){42});
}


#ifdef STANDALONE_FOR_DIEHARDER

#include <unistd.h>

int main(void)
{
    /* XXX NOT FOR BENCHMARKING */
    while (1) {
        uint32_t t[1024];
        for (int i = 0; i < 1024; ++i) t[i] = uniform32();
        write(1, t, 4096);
    }
    __builtin_unreachable();
}

#endif


#ifdef STANDALONE_FOR_PERF

#include <unistd.h>

int main(void)
{
    uint32_t modulus = uniform32();
    for (size_t i = 0; i < 10000000UL; ++i)
        uniform_1(NULL, 1, (const ERL_NIF_TERM[]){modulus});
    return 0;
}

#endif
