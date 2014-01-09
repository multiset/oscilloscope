-module(oscilloscope_http_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, App} = application:get_application(),
    {ok, Dispatch} = file:consult(code:priv_dir(App) ++ "/dispatch.conf"),
    WebConfig = [
        {ip, "0.0.0.0"},
        {port, 22222},
        {dispatch, Dispatch}
    ],
    Web = {
        webmachine_mochiweb,
        {webmachine_mochiweb, start, [WebConfig]},
        permanent, 5000, worker, [mochiweb_socket_server]
    },
    {ok, {{one_for_one, 5, 10}, [Web]}}.

