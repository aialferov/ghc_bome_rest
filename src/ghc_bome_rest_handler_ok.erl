-module(ghc_bome_rest_handler_ok).
-export([init/2]).

-include("ghc_bome_rest_handler.hrl").

-define(HandlerBadRequest, ghc_bome_rest_handler_bad_request).

init(Req0 = #{method := Method, has_body := true}, State) when
    Method == <<"PUT">>; Method == <<"PATCH">>; Method == <<"DELETE">>
->
    UserId = cowboy_req:binding(id, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    DbModule = State#state.db_module,

    Req = case decode_body(Body) of
        {ok, Metrics} -> action(Method, UserId, Metrics, Req1, DbModule);
        {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req1)
    end,
    {ok, Req, State};

init(Req0 = #{method := Method, has_body := false}, State) when
    Method == <<"GET">>
->
    UserId = cowboy_req:binding(id, Req0),
    Query = cowboy_req:parse_qs(Req0),
    DbModule = State#state.db_module,

    Req = case query_to_options(Query) of
        {ok, Options} -> action(Method, UserId, Options, Req0, DbModule);
        {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req0)
    end,
    {ok, Req, State};

init(Req, State) -> {ok, ?HandlerBadRequest:reply(Req), State}.

action(<<"PUT">>, UserId, Metrics, Req, DbModule) ->
    case DbModule:put(UserId, Metrics) of
        {ok, created} -> reply_created(Req);
        {ok, modified} -> reply_modified(Req)
    end;

action(<<"PATCH">>, UserId, Metrics, Req, DbModule) ->
    case DbModule:patch(UserId, Metrics) of
        ok -> reply_modified(Req);
        {error, not_found} -> reply_not_found(UserId, Req)
    end;

action(<<"GET">>, UserId, Options, Req, DbModule) ->
    case DbModule:get(UserId, Options) of
        {ok, Metrics} -> reply_data(Metrics, Req);
        {error, not_found} -> reply_not_found(UserId, Req)
    end;

action(<<"DELETE">>, UserId, MetricNames, Req, DbModule) ->
    case DbModule:delete(UserId, MetricNames) of
        ok -> reply_modified(Req);
        {error, not_found} -> reply_not_found(UserId, Req)
    end.

reply_created(Req) -> cowboy_req:reply(?CodeCreated, Req).
reply_modified(Req) -> cowboy_req:reply(?CodeNoContent, Req).

reply_data(Data, Req) ->
    cowboy_req:reply(?CodeOk, ?ContentTypeJson, jsx:encode(Data), Req).

reply_not_found(Resource, Req) ->
    cowboy_req:reply(
        ?CodeNotFound, ?ContentTypeJson,
        jsx:encode(#{Resource => not_found}), Req
    ).

decode_body(Body) ->
    try jsx:decode(Body, [return_maps]) of
        DecodedBody -> {ok, DecodedBody}
    catch
        _:_ -> {error, malformed_json}
    end.

query_to_options(Query) ->
    lists:foldl(fun query_to_options/2, {ok, []}, Query).

query_to_options({Name = <<"filter">>, Arg}, {ok, Options}) ->
    Option = {binary_to_atom(Name, utf8), binary:split(Arg, <<",">>, [global])},
    {ok, [Option|Options]};

query_to_options({Name, _Arg}, {ok, _Options}) ->
    {error, #{unknown_option => Name}};

query_to_options({_Name, _Arg}, {error, Reason}) -> {error, Reason}.
