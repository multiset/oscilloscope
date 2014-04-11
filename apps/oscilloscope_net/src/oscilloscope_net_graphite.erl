-module(oscilloscope_net_graphite).

-export([start/0]).

start() ->
    ranch:start_listener(
        graphite,
        10,
        ranch_tcp,
        [{port, 2003}],
        oscilloscope_net_tcp,
        [{parser, fun parse/2}]
    ).

parse(Bin, PrevBuffer) ->
    parse_int(<<PrevBuffer/binary, Bin/binary>>, []).

parse_int(Bin, Acc) ->
    case re:split(Bin, "[\r\n]+", [{parts, 2}]) of
        [Buffer] ->
            {Acc, Buffer};
        [Line, Rest] ->
            [Metric, ValueBin, TimestampBin] = re:split(Line, "\s"),
            Acc1 = try
                Value = binary_to_number(ValueBin),
                % Timestamp can't be a float!
                Timestamp = list_to_integer(binary_to_list(TimestampBin)),
                % Host is nulled out for graphite data
                [{Metric, <<"">>, Timestamp, Value}|Acc]
            catch error:badarg -> Acc
            end,
            parse_int(Rest, Acc1)
    end.

binary_to_number(B) ->
    list_to_number(binary_to_list(B)).

list_to_number(L) ->
    try list_to_float(L)
    catch error:badarg -> list_to_integer(L)
    end.
