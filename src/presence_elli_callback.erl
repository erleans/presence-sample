-module(presence_elli_callback).

-behaviour(elli_handler).

-export([handle/2,
         handle_event/3]).

-include_lib("elli/include/elli.hrl").


handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('POST', [<<"heartbeat">>], Req) ->
    Body = elli_request:body(Req),
    presence_grain:heartbeat(Body),
    {ok, [], <<>>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
