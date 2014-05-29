-module(oscilloscope_auth_metrics).

-export([get_authed_metrics/2]).

-include("oscilloscope_auth.hrl").

-spec get_authed_metrics(read | write | admin, tuple() | integer()) -> [].
get_authed_metrics(Type, #user{}=User) ->
    OwnerID = oscilloscope_auth_user:get_owner_id(User),
    get_authed_metrics(Type, OwnerID);

get_authed_metrics(Type, #org{}=Org) ->
    OwnerID = oscilloscope_auth_org:get_owner_id(Org),
    get_authed_metrics(Type, OwnerID);

get_authed_metrics(read, OwnerID) ->
    oscilloscope_metadata:named(get_readable_metrics, [OwnerID]);

get_authed_metrics(write, OwnerID) ->
    oscilloscope_metadata:named(get_writable_metrics, [OwnerID]);

get_authed_metrics(admin, OwnerID) ->
    oscilloscope_metadata:named(get_adminable_metrics, [OwnerID]).
