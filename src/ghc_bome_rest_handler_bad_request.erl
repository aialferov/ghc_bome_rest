-module(ghc_bome_rest_handler_bad_request).

-export([
    init/2,
    reply/2, reply/3
]).

-include("ghc_bome_rest.hrl").
-include("ghc_bome_rest_handler.hrl").

-define(Log, ghc_bome_rest_log).

init(Req, State) -> {ok, reply(Req, State), State}.

reply(Req, State) ->
    Body = lists:flatten(io_lib:format(?GhcBomeApiUsage, [])),
    ?Log:response(State#state.log_file, Req, ?CodeBadRequest, "<Usage>"),
    cowboy_req:reply(?CodeBadRequest, ?ContentTypeText, Body, Req).

reply(Reason, Req, State) ->
    Body = jsx:encode(#{reason => Reason}),
    ?Log:response(State#state.log_file, Req, ?CodeBadRequest, Body),
    cowboy_req:reply(?CodeBadRequest, ?ContentTypeJson, Body, Req).
