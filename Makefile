all: clean deps compile run

deps:
	@./rebar get-deps

compile:
	@./rebar compile

run:
	@erl -pa deps/*/ebin -pa apps/*/ebin -s oscilloscope_cache -s oscilloscope_net -s oscilloscope_sql -s inets -s crypto -s mochiweb -s webmachine -s oscilloscope_query -s erp -s commutator

clean:
	@./rebar clean
