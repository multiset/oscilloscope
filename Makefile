all: compile run

compile:
	@./rebar comp

run:
	@erl -pa deps/*/ebin -pa apps/*/ebin -s oscilloscope_cache -s oscilloscope_net -s inets -s crypto -s mochiweb -s webmachine -s oscilloscope_query
