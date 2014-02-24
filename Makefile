SHELL=/bin/bash

all: clean deps compile run

deps:
	@./rebar get-deps

compile:
	@./rebar compile

run:
	@erl -pa deps/*/ebin -pa apps/*/ebin -s folsom -s lager -s oscilloscope_net -s oscilloscope_sql -s inets -s crypto -s mochiweb -s webmachine -s erp -s commutator -s oscilloscope_cache -s oscilloscope_http -s oscilloscope_net -s bcrypt

clean:
	@./rebar clean

test: eunit dialyze

eunit:
	@./rebar skip_deps=true eunit

dialyze:
	@dialyzer --src apps/*/src/ -pa apps/oscilloscope -pa apps/oscilloscope_http

release: deps compile
	@./rebar generate
	@erl -pa deps/*/ebin -pa apps/*/ebin -s oscilloscope_sql -s oscilloscope_cache -s oscilloscope_net -s inets -s crypto -s mochiweb -s webmachine -s oscilloscope_http -s erp

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
	@echo "Popluating database with apps/oscilloscope_sql/priv/schema.sql"
	@psql -U oscilloscope -h 127.0.0.1 oscilloscope < apps/oscilloscope_sql/priv/schema.sql
	@echo "Done. You're set!"
