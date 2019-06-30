
CC = clang

CFLAGS += -std=c99
CFLAGS += -D_POSIX_C_SOURCE=200809L
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
	CFLAGS += -I/usr/lib/erlang/usr/include

	LDFLAGS += -L/usr/lib/erlang/usr/lib
endif

ifeq ($(PLATFORM), Darwin)
	CFLAGS += -I/opt/local/lib/erlang/usr/include

	LDFLAGS += -arch x86_64 -flat_namespace -undefined suppress
	LDFLAGS += -L/opt/local/lib/erlang/usr/lib
endif

ifeq ($(PLATFORM), FreeBSD)
	CFLAGS += -I/usr/local/lib/erlang/usr/include

	LDFLAGS += -L/usr/local/lib/erlang/usr/lib
endif

all: build doc

dialyzer:
	rebar3 dialyzer

build: nif
	rebar3 compile

test: nif
	rebar3 eunit

nif: $(NIF_LIB)

$(NIF_LIB): $(NIF_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^ $(LDLIBS)

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

doc:
	rebar3 edoc

clean:
	$(RM) -r _build
	$(RM) $(NIF_OBJ)

.PHONY: all dialyzer build test nif doc clean
