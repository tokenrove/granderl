#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <erl_nif.h>

extern uint32_t uniform32(void);
extern ERL_NIF_TERM uniform_1(ErlNifEnv *, int, const ERL_NIF_TERM *);

#define unlikely(x) __builtin_expect(!!(x), 0)
