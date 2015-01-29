SHELL=/bin/bash

all: clean deps compile rel console

deps:
	@./rebar get-deps

compile:
	@./rebar compile

rel: relclean deps compile
	@./rebar generate

knit: clean relclean deps compile
	@./knit
	@./artifact.sh

relrun:
	@rel/osc/bin/osc

relclean:
	rm -rf rel/osc

clean:
	@./rebar clean

console:
	@rel/osc/erts-*/bin/erl -name remsh -remsh osc@127.0.0.1 -hidden -setcookie osc

test: eunit dialyze

eunit:
	@./rebar skip_deps=true eunit

dialyze:
	@dialyzer --plt dialyzer.plt --src apps/*/src/ -pa apps/osc/ebin -pa apps/osc_meta/ebin | grep -F -v -f ./dialyzer.ignore-warnings

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
	@echo "Creating osc user... "
	@createuser -d -h 127.0.0.1 osc
	@echo "Done."
	@echo "Creating osc database... "
	@createdb -h 127.0.0.1 -U osc osc
	@echo "Done."
	@echo "Popluating database with apps/osc_meta/priv/schema.sql"
	@psql -U osc -h 127.0.0.1 osc < apps/osc_meta/priv/schema.sql
	@echo "Done. You're set!"
