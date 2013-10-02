-module(oscilloscope_net_protocols).

-export([graphite/1]).

graphite(Bin) ->
    BinPoints = re:split(Bin, "[\r\n]+", [trim]),
    [graphite_int(BP) || BP <- BinPoints].

graphite_int(Bin) ->
    [Metric, ValueBin, TimestampBin] = re:split(Bin, "\s"),
    Value = binary_to_number(ValueBin),
    % Timestamp can't be a float!
    Timestamp = list_to_integer(binary_to_list(TimestampBin)),
    {Metric, Value, Timestamp}.

binary_to_number(B) ->
    list_to_number(binary_to_list(B)).

list_to_number(L) ->
    try list_to_float(L)
    catch error:badarg -> list_to_integer(L)
    end.
