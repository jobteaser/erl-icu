
all: build

dialyzer:
	rebar3 dialyzer

build: nif
	rebar3 compile

test: nif
	rebar3 eunit

nif:
	$(MAKE) -C c_src build

clean:
	$(MAKE) -C c_src clean
	$(RM) -r _build

.PHONY: all dialyzer build test nif clean
