CURDIR		:= $(shell pwd)
BASEDIR		:= $(abspath $(dir $(CURDIR)))
PROJECT		?= granderl
PROJECT		:= $(strip $(PROJECT))

ERL_CXXFLAGS ?= $(shell erl -noshell -noinput -eval "io:format(\"-I~ts/erts-~ts/include -I~ts\", [code:root_dir(), erlang:system_info(version), code:lib_dir(erl_interface, include)]), halt(0).")
ERL_LDFLAGS  ?= $(shell erl -noshell -noinput -eval "io:format(\"-L~ts\", [code:lib_dir(erl_interface, lib)]), halt(0).")

DEBUG     ?= 0
NIF_DEBUG ?= $(DEBUG)

# System type and C compiler/flags.

ifeq ($(NIF_DEBUG),0)
	OPTIMIZE = -O3 -DNDEBUG
else
	OPTIMIZE = -O0 -g
endif

CC = clang

GEN_CFLAGS = -finline-functions -mavx2 -DHAVE_RDRAND -fPIC \
						 -std=gnu99 -Wall -Wextra -Wno-missing-field-initializers

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CFLAGS ?= -arch x86_64 -pie
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
endif

CFLAGS     += $(SIMDJSON_FLAGS) $(OPTIMIZE) -fPIC $(ERL_CXXFLAGS) $(GEN_CFLAGS)
LDFLAGS    += $(ERL_LDFLAGS) -lei -shared

SRC_DIR     = $(CURDIR)
SO_OUTPUT  ?= $(BASEDIR)/priv/$(PROJECT).so

SOURCES    := granderl.c pcg32.c
OBJECTS     = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C   = $(cpp_verbose) $(CC) $(CFLAGS) -c

# Targets

all: deps $(SO_OUTPUT)

deps: $(wildcard *.h) $(SOURCES)

clean:
	rm -f $(SO_OUTPUT) *.o

$(SO_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(CC) $(OBJECTS) $(LDFLAGS) -o $(SO_OUTPUT)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<
