all: compile run

compile:
	@./rebar comp

run:
	@erl -pa deps/*/ebin -pa apps/*/ebin -s oscilloscope_net -s oscilloscope_cache
