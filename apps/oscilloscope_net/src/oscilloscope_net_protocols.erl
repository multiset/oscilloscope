-module(oscilloscope_net_protocols).

-export([graphite/2]).

graphite(Bin, PrevBuffer) ->
    graphite_int(<<PrevBuffer/binary, Bin/binary>>, []).

graphite_int(Bin, Acc) ->
    case re:split(Bin, "[\r\n]+", [{parts, 2}]) of
        [Buffer] ->
            {Acc, Buffer};
        [Line, Rest] ->
            [Metric, ValueBin, TimestampBin] = re:split(Line, "\s"),
            Acc1 = try
                Value = binary_to_number(ValueBin),
                % Timestamp can't be a float!
                Timestamp = list_to_integer(binary_to_list(TimestampBin)),
                [{Metric, Value, Timestamp}|Acc]
            catch error:badarg -> Acc
            end,
            graphite_int(Rest, Acc1)
    end.

binary_to_number(B) ->
    list_to_number(binary_to_list(B)).

list_to_number(L) ->
    try list_to_float(L)
    catch error:badarg -> list_to_integer(L)
    end.
