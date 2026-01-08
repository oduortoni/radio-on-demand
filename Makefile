.PHONY: all compile clean run

all: compile

compile:
	rebar3 compile

run: compile
	rebar3 shell

clean:
	rebar3 clean