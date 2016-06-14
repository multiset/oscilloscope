-module(osc_event).

-export([
    register/1,
    notify/1
]).

register(Module) ->
    ok = gen_event:add_sup_handler(osc_event, Module, []).

notify(Msg) ->
    ok = gen_event:notify(osc_event, Msg).
