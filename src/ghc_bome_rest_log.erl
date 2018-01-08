-module(ghc_bome_rest_log).

-export([
    text/2, text/3,

    request/2, request/3,
    response/3, response/4
]).

-define(Format,
    "[~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~3..0bZ] ~s~n"
).

text(File, Text) -> log(File, Text).
text(File, Format, Args) -> log(File, format(Format, Args)).

request(File, Req) -> request(File, Req, "").
request(File, Req, Body) ->
    log(File, format("~s --> '~s' '~s' '~s' '~s'", [
        peer(Req),
        maps:get(method, Req),
        maps:get(path, Req),
        maps:get(qs, Req),
        Body
    ])).

response(File, Req, Code) -> response(File, Req, Code, "").
response(File, Req, Code, Body) ->
    log(File, format("~s <-- '~b' '~s'", [peer(Req), Code, Body])).

log(File, Text) ->
    Now = {_MegaSecs, _Secs, Mcs} = erlang:timestamp(),
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(Now),
    Args = [Y, Mo, D, H, Mi, S, Mcs div 1000, Text],
    Log = format(?Format, Args),
    io:format(Log),
    file:write(File, Log).

peer(Req) ->
    case maps:find(peer, Req) of
        {ok, {{A, B, C, D}, Port}} ->
            format("~b.~b.~b.~b:~b", [A, B, C, D, Port]);
        _Other -> "-"
    end.

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
