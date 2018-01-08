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
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ?Log:request(State#state.log_file, Req1, Body),

    Response = lists:flatten(io_lib:format(?GhcBomeApiUsage, [])),
    ?Log:response(State#state.log_file, Req1, ?CodeBadRequest, "<Usage>"),
    cowboy_req:reply(?CodeBadRequest, ?ContentTypeText, Response, Req1).

reply(Reason, Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ?Log:request(State#state.log_file, Req1, Body),

    Response = jsx:encode(#{reason => Reason}),
    ?Log:response(State#state.log_file, Req1, ?CodeBadRequest, Body),
    cowboy_req:reply(?CodeBadRequest, ?ContentTypeJson, Response, Req1).
