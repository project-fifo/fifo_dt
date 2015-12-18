REBAR = rebar3
VSN = $(shell cat src/*.app.src | grep vsn | sed 's/^[^"]*"//' | sed 's/".*$//')

.PHONY: all test eqc

all: compile

include fifo.mk

clean:
	$(REBAR) clean

eqc:
	$(REBAR) as eqc eqc

eunit:
	$(REBAR) eunit
