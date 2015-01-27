-module(osc_http_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    mstat:new_histogram([osc_http, requests, total, latency]),
    mstat:new_counter([osc_http, requests, total, count]),
    mstat:new_histogram([osc_http, requests, status_code, 200, latency]),
    mstat:new_counter([osc_http, requests, status_code, 200, count]),
    mstat:new_histogram([osc_http, requests, status_code, 201, latency]),
    mstat:new_counter([osc_http, requests, status_code, 201, count]),
    mstat:new_histogram([osc_http, requests, status_code, 204, latency]),
    mstat:new_counter([osc_http, requests, status_code, 204, count]),
    mstat:new_histogram([osc_http, requests, status_code, 301, latency]),
    mstat:new_counter([osc_http, requests, status_code, 301, count]),
    mstat:new_histogram([osc_http, requests, status_code, 302, latency]),
    mstat:new_counter([osc_http, requests, status_code, 302, count]),
    mstat:new_histogram([osc_http, requests, status_code, 303, latency]),
    mstat:new_counter([osc_http, requests, status_code, 303, count]),
    mstat:new_histogram([osc_http, requests, status_code, 400, latency]),
    mstat:new_counter([osc_http, requests, status_code, 400, count]),
    mstat:new_histogram([osc_http, requests, status_code, 401, latency]),
    mstat:new_counter([osc_http, requests, status_code, 401, count]),
    mstat:new_histogram([osc_http, requests, status_code, 403, latency]),
    mstat:new_counter([osc_http, requests, status_code, 403, count]),
    mstat:new_histogram([osc_http, requests, status_code, 404, latency]),
    mstat:new_counter([osc_http, requests, status_code, 404, count]),
    mstat:new_histogram([osc_http, requests, status_code, 405, latency]),
    mstat:new_counter([osc_http, requests, status_code, 405, count]),
    mstat:new_histogram([osc_http, requests, status_code, 500, latency]),
    mstat:new_counter([osc_http, requests, status_code, 500, count]),
    mstat:new_histogram([osc_http, requests, status_code, unknown, latency]),
    mstat:new_counter([osc_http, requests, status_code, unknown, count]),
    mstat:new_histogram([osc_http, requests, method, get, latency]),
    mstat:new_counter([osc_http, requests, method, get, count]),
    mstat:new_histogram([osc_http, requests, method, patch, latency]),
    mstat:new_counter([osc_http, requests, method, patch, count]),
    mstat:new_histogram([osc_http, requests, method, post, latency]),
    mstat:new_counter([osc_http, requests, method, post, count]),
    mstat:new_histogram([osc_http, requests, method, put, latency]),
    mstat:new_counter([osc_http, requests, method, put, count]),
    mstat:new_histogram([osc_http, requests, method, delete, latency]),
    mstat:new_counter([osc_http, requests, method, delete, count]),
    mstat:new_histogram([osc_http, requests, method, options, latency]),
    mstat:new_counter([osc_http, requests, method, options, count]),
    mstat:new_histogram([osc_http, requests, method, unknown, latency]),
    mstat:new_counter([osc_http, requests, method, unknown, count]),
    {ok, {{one_for_one, 5, 10}, []}}.

