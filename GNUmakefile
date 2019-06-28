
ERTS_INCLUDE_DIR = $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR = $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR = $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

CC = clang

CFLAGS += -std=c99
CFLAGS += -Wall -Wextra -Werror -Wsign-conversion
CFLAGS += -Wno-unused-parameter -Wno-unused-function
CFLAGS += -fPIC
CFLAGS += $(shell icu-config --cflags)

LDFLAGS += -shared
LDFLAGS += $(shell icu-config --ldflags-searchpath)

LDLIBS += $(shell icu-config --ldflags-libsonly)

NIF_LIB = priv/icu_nif.so
NIF_SRC = $(wildcard c_src/*.c)
NIF_OBJ = $(subst .c,.o,$(NIF_SRC))

PLATFORM = $(shell uname -s)

ifeq ($(PLATFORM), Linux)
	CFLAGS += -D_POSIX_C_SOURCE=200809L
	CFLAGS += -I/usr/lib/erlang/usr/include

	LDFLAGS += -L/usr/lib/erlang/usr/lib
endif

ifeq ($(PLATFORM), Darwin)
	CFLAGS += -I/usr/local/include

	LDFLAGS += -arch x86_64 -flat_namespace -undefined suppress
endif

ifeq ($(PLATFORM), FreeBSD)
	CFLAGS += -I/usr/local/include
	CFLAGS += -I/usr/local/lib/erlang/usr/include

	LDFLAGS += -L/usr/local/lib
	LDFLAGS += -L/usr/local/lib/erlang/usr/lib
endif

all: build

dialyzer:
	rebar3 dialyzer

build: $(NIF_LIB)

$(NIF_LIB): $(NIF_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^ $(LDLIBS)

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

clean:
	$(RM) -r _build
	$(RM) $(NIF_OBJ)

.PHONY: all dialyzer build clean
