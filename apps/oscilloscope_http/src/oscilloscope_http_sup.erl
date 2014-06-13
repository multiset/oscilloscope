-module(oscilloscope_http_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Apps} = application:get_env(oscilloscope_http, dispatch_apps),
    CompleteDispatch = lists:flatmap(fun(App) ->
        {ok, Dispatch} = file:consult(code:priv_dir(App) ++ "/dispatch.conf"),
        Dispatch
    end, Apps),
    WebConfig = [
        {ip, "0.0.0.0"},
        {port, 22222},
        {dispatch, CompleteDispatch}
    ],
    Web = {
        webmachine_mochiweb,
        {webmachine_mochiweb, start, [WebConfig]},
        permanent, 5000, worker, [mochiweb_socket_server]
    },
    {ok, {{one_for_one, 5, 10}, [Web]}}.

