-module(oscilloscope_cache_group_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).


start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init({User, Name, Host}=Group) ->
    ok = pg2:create(Group),
    {AF, Resolutions} = case oscilloscope_sql_metrics:get(User, Name, Host) of
        {ok, Val} ->
            io:format("Got a value: ~p~n", [Val]),
            Val;
        _ ->
            %% TODO: get defaults from somewhere
            AF0 = avg,
            Resolutions0 = [{10, 1000, []}, {60, 1000, []}, {3600, 1000, []}],
            ok = oscilloscope_sql_metrics:create(
                User, Name, Host, AF0, Resolutions0
            ),
            {AF0, Resolutions0}
    end,
    Specs = lists:map(
        fun(R) ->
            {
                R,
                {oscilloscope_cache, start_link, [{Group, R, AF}]},
                permanent, 5000, worker, [oscilloscope_cache]
            }
        end, Resolutions
    ),
    {ok, {{one_for_all, 10, 10}, Specs}}.
