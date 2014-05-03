SHELL=/bin/bash

all: clean deps compile run

deps:
	@./rebar get-deps

compile:
	@./rebar compile

run:
	@erl -smp enable -pa deps/*/ebin -pa apps/*/ebin -s folsom -s lager -s oscilloscope_net -s oscilloscope_metadata -s inets -s crypto -s mochiweb -s webmachine -s oscilloscope_cache -s ibrowse -s oscilloscope_http -s oscilloscope_net

noshell:
	@erl -name oscilloscope@`hostname -f` -noshell -setcookie monster -smp enable -pa deps/*/ebin -pa apps/*/ebin -s folsom -s lager -s oscilloscope_net -s oscilloscope_metadata -s inets -s crypto -s mochiweb -s webmachine -s oscilloscope_cache -s ibrowse -s oscilloscope_http -s oscilloscope_net

clean:
	@./rebar clean

test: eunit dialyze

eunit:
	@./rebar skip_deps=true eunit

dialyze:
	@dialyzer --src apps/*/src/ -pa apps/oscilloscope -pa apps/oscilloscope_http

release: deps compile
	@./rebar generate
	@cd rel; tar -cvzf oscilloscope.tar.gz oscilloscope

fuck-it-all:
	@echo -n "Killing all the postgres... "
	@killall postgres
	@echo "Done."
	@echo -n "Fuck it all... "
	@rm -rf ./sql_data
	@echo "Done."
	@$(MAKE) cluster

cluster:
	@echo -n "Recreating directory... "
	@mkdir ./sql_data
	@echo "Done."
	@echo -n "Recreating cluster... "
	@initdb ./sql_data -E utf8
	@echo "Done."
	@echo ""
	@echo "You probably want to run the following in different terminals:"
	@echo ""
	@echo "postgres -D ./sql_data"
	@echo "make sql"
	@echo ""

sql: 
	@echo "Creating oscilloscope user... "
	@createuser -d -h 127.0.0.1 oscilloscope
	@echo "Done."
	@echo "Creating oscilloscope database... "
	@createdb -h 127.0.0.1 -U oscilloscope oscilloscope
	@echo "Done."
	@echo "Popluating database with apps/oscilloscope_metadata/priv/schema.sql"
	@psql -U oscilloscope -h 127.0.0.1 oscilloscope < apps/oscilloscope_metadata/priv/schema.sql
	@echo "Done. You're set!"
