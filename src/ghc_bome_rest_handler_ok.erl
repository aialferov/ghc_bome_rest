-module(ghc_bome_rest_handler_ok).
-export([init/2]).

-include("ghc_bome_rest_handler.hrl").

-define(HandlerBadRequest, ghc_bome_rest_handler_bad_request).
-define(Log, ghc_bome_rest_log).

init(Req0 = #{method := Method, has_body := true}, State) when
    Method == <<"PUT">>; Method == <<"PATCH">>; Method == <<"DELETE">>
->
    UserId = cowboy_req:binding(id, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    ?Log:request(State#state.log_file, Req1, Body),

    Req = case decode_body(Body) of
        {ok, Metrics} -> action(Method, UserId, Metrics, Req1, State);
        {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req1, State)
    end,
    {ok, Req, State};

init(Req0 = #{method := Method, has_body := false}, State) when
    Method == <<"GET">>
->
    UserId = cowboy_req:binding(id, Req0),
    Query = cowboy_req:parse_qs(Req0),

    ?Log:request(State#state.log_file, Req0),

    Req = case query_to_options(Query) of
        {ok, Options} -> action(Method, UserId, Options, Req0, State);
        {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req0, State)
    end,
    {ok, Req, State};

init(Req0, State) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    ?Log:request(State#state.log_file, Req0, Body),
    {ok, ?HandlerBadRequest:reply(Req, State), State}.

action(<<"PUT">>, UserId, Metrics, Req, State) ->
    DbModule = State#state.db_module,
    case DbModule:put(UserId, Metrics) of
        {ok, created} -> reply_created(Req, State);
        {ok, modified} -> reply_modified(Req, State)
    end;

action(<<"PATCH">>, UserId, Metrics, Req, State) ->
    DbModule = State#state.db_module,
    case DbModule:patch(UserId, Metrics) of
        ok -> reply_modified(Req, State);
        {error, not_found} -> reply_not_found(UserId, Req, State)
    end;

action(<<"GET">>, UserId, Options, Req, State) ->
    DbModule = State#state.db_module,
    case DbModule:get(UserId, Options) of
        {ok, Metrics} -> reply_data(Metrics, Req, State);
        {error, not_found} -> reply_not_found(UserId, Req, State)
    end;

action(<<"DELETE">>, UserId, MetricNames, Req, State) ->
    DbModule = State#state.db_module,
    case DbModule:delete(UserId, MetricNames) of
        ok -> reply_modified(Req, State);
        {error, not_found} -> reply_not_found(UserId, Req, State)
    end.

reply_created(Req, State) ->
    ?Log:response(State#state.log_file, Req, ?CodeCreated),
    cowboy_req:reply(?CodeCreated, Req).

reply_modified(Req, State) ->
    ?Log:response(State#state.log_file, Req, ?CodeNoContent),
    cowboy_req:reply(?CodeNoContent, Req).

reply_data(Data, Req, State) ->
    Body = jsx:encode(Data),
    ?Log:response(State#state.log_file, Req, ?CodeOk, Body),
    cowboy_req:reply(?CodeOk, ?ContentTypeJson, Body, Req).

reply_not_found(Resource, Req, State) ->
    Body = jsx:encode(#{Resource => not_found}),
    ?Log:response(State#state.log_file, Req, ?CodeNotFound, Body),
    cowboy_req:reply(?CodeNotFound, ?ContentTypeJson, Body, Req).

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
