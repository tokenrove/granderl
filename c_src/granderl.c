#include "granderl.h"

static int on_load(ErlNifEnv *env __attribute__((unused)),
                   void **priv __attribute__((unused)),
                   ERL_NIF_TERM info __attribute__((unused)))
{
    return 0;
}

static int upgrade(ErlNifEnv* env __attribute__((unused)),
                   void ** priv __attribute__((unused)),
                   void **old_priv __attribute__((unused)),
                   ERL_NIF_TERM load_info __attribute__((unused)))
{
    return 0;
}


static ErlNifFunc nif_functions[] = { {"uniform", 1, uniform_1, 0} };

ERL_NIF_INIT(granderl, nif_functions, &on_load, NULL, &upgrade, NULL)
