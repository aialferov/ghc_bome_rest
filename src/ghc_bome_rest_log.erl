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
    {{A, B, C, D}, Port} = maps:get(peer, Req),
    log(File, format("~b.~b.~b.~b:~b --> '~s' '~s' '~s' '~s'", [
        A, B, C, D, Port,
        maps:get(method, Req),
        maps:get(path, Req),
        maps:get(qs, Req),
        Body
    ])).

response(File, Req, Code) -> response(File, Req, Code, "").
response(File, Req, Code, Body) ->
    {{A, B, C, D}, Port} = maps:get(peer, Req),
    log(File, format("~b.~b.~b.~b:~b <-- '~b' '~s'",
                     [A, B, C, D, Port, Code, Body])).

log(File, Text) ->
    Now = {_MegaSecs, _Secs, Mcs} = erlang:timestamp(),
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(Now),
    Args = [Y, Mo, D, H, Mi, S, Mcs div 1000, Text],
    file:write(File, format(?Format, Args)).

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
