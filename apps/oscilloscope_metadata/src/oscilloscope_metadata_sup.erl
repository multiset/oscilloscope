-module(oscilloscope_metadata_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD_SPEC(Mod), [{Mod, {Mod, start_link, []},
                           permanent, brutal_kill, worker, [Mod]}]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, ?CHILD_SPEC(oscilloscope_metadata_manager)}}.
