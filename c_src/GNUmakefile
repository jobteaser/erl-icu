
CC = clang

CFLAGS += -std=c99
CFLAGS += -Wall -Wextra -Werror -Wsign-conversion
CFLAGS += -Wno-unused-parameter -Wno-unused-function
CFLAGS += -fPIC
CFLAGS += $(shell icu-config --cflags)

LDFLAGS += -shared
LDFLAGS += $(shell icu-config --ldflags-searchpath)

LDLIBS += $(shell icu-config --ldflags-libsonly)

NIF_LIB = ../priv/icu_nif.so
NIF_SRC = $(wildcard *.c)
NIF_OBJ = $(subst .c,.o,$(NIF_SRC))

PLATFORM = $(shell uname -s)

ifeq ($(PLATFORM), Linux)
	CFLAGS += -D_POSIX_C_SOURCE=200809L
	CFLAGS += -I/usr/lib/erlang/usr/include

	LDFLAGS += -L/usr/lib/erlang/usr/lib
endif

ifeq ($(PLATFORM), Darwin)
	CFLAGS += -I/usr/local/include
	CFLAGS += -I/opt/local/lib/erlang/usr/include

	LDFLAGS += -arch x86_64 -flat_namespace -undefined suppress
	LDFLAGS += -L/opt/local/lib/erlang/usr/lib
endif

ifeq ($(PLATFORM), FreeBSD)
	CFLAGS += -I/usr/local/include
	CFLAGS += -I/usr/local/lib/erlang/usr/include

	LDFLAGS += -L/usr/local/lib
	LDFLAGS += -L/usr/local/lib/erlang/usr/lib
endif

all: build

build: $(NIF_LIB)

$(NIF_LIB): $(NIF_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^ $(LDLIBS)

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

clean:
	$(RM) $(NIF_OBJ)

.PHONY: all build clean
