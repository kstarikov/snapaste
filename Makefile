PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

.PHONY: all edoc test clean dialyzer run

all:
	@$(REBAR) prepare-deps

edoc: all
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
	rm -rf ebin dialyzer.output .eunit test/*.beam

dialyzer:
	./dialyzer.sh

run:
	erl -pa ebin deps -s snapaste
