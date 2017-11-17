-module(ghc_bome_rest_handler_bad_request).

-export([
    init/2,
    reply/1, reply/2
]).

-include("ghc_bome_rest.hrl").
-include("ghc_bome_rest_handler.hrl").

init(Req, State) -> {ok, reply(Req), State}.

reply(Req) ->
    cowboy_req:reply(
        ?CodeBadRequest, ?ContentTypeText,
        lists:flatten(io_lib:format(?GhcBomeApiUsage, [])), Req
    ).

reply(Reason, Req) ->
    cowboy_req:reply(
        ?CodeBadRequest, ?ContentTypeJson,
        jsx:encode(#{reason => Reason}), Req
    ).
