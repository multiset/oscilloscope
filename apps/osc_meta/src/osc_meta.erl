-module(osc_meta).

-export_type([meta/0]).

-include_lib("osc/include/osc_types.hrl").

-type meta() :: [{atom(), any()}].
