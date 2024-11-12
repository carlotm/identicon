-module(edenticon_web).
-export([handle/2, handle_event/3]).
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"generate">>, In], Req) ->
    Size = get_size(Req),
    Format = get_format(Req),
    Resp = edenticon_core:as(Format, In, Size),
    {ok, [], Resp};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.

get_size(Req) ->
    Size = elli_request:get_arg(<<"size">>, Req, <<"250">>),
    try binary_to_integer(Size) of
        V -> V
    catch
        error:_ -> 250
    end.

get_format(Req) ->
    Format = elli_request:get_arg(<<"format">>, Req, <<"png">>),
    try binary_to_existing_atom(Format) of
        V -> V
    catch
        error:_ -> png
    end.
