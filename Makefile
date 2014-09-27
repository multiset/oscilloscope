SHELL=/bin/bash

all: clean deps compile rel console

deps:
	@./rebar get-deps

compile:
	@./rebar compile

rel: deps compile
	./rebar generate

devrel: dev1 dev2 dev3

dev1 dev2 dev3:
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@.config)

devclean:
	rm -rf dev

relclean:
	rm -rf rel/osc

clean:
	@./rebar clean

console:
	@rel/osc/bin/osc console

test: eunit dialyze

eunit:
	@./rebar skip_deps=true eunit

dialyze:
	@dialyzer --src apps/*/src/ -pa apps/osc_persistence/ebin -pa apps/osc_sql/ebin -pa apps/osc_metadata/ebin -pa apps/osc/ebin -pa deps/lager/ebin -pa deps/riak_core/ebin

release: deps compile
	@./rebar generate
	@cd rel; tar -cvzf osc.tar.gz osc

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
	@echo "Popluating database with apps/osc_sql/priv/schema.sql"
	@psql -U osc -h 127.0.0.1 osc < apps/osc_sql/priv/schema.sql
	@echo "Done. You're set!"
