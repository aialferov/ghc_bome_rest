-module(ghc_bome_rest_handler_ok).
-export([init/2]).

-include("ghc_bome_rest_handler.hrl").

-define(HandlerBadRequest, ghc_bome_rest_handler_bad_request).

init(Req0 = #{method := Method, has_body := true}, State) when
    Method == <<"PUT">>; Method == <<"PATCH">>; Method == <<"DELETE">>
->
    Id = cowboy_req:binding(id, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    Req = case decode_body(Body) of
        {ok, Data} -> action(Method, Id, Data, Req1, State#state.db_module);
        {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req1)
    end,
    {ok, Req, State};

init(Req0 = #{method := Method, has_body := false}, State) when
    Method == <<"GET">>
->
    Id = cowboy_req:binding(id, Req0),
    Query = cowboy_req:parse_qs(Req0),

    Req = case decode_query(Query) of
        {ok, Options} -> action(Method, Id, Options, Req0, State#state.db_module);
        {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req0)
    end,
    {ok, Req, State};

init(Req, State) -> {ok, ?HandlerBadRequest:reply(Req), State}.

action(<<"PUT">>, Id, Data, Req, DbModule) ->
    case DbModule:put(Id, Data) of
        {ok, created} -> reply_created(Req);
        {ok, modified} -> reply_modified(Req)
    end;

action(<<"PATCH">>, Id, Data, Req, DbModule) ->
    case DbModule:patch(Id, Data) of
        ok -> reply_modified(Req);
        {error, not_found} -> reply_not_found(Id, Req)
    end;

action(<<"GET">>, Id, Options, Req, DbModule) ->
    case DbModule:get(Id, Options) of
        {ok, Data} -> reply_data(Data, Req);
        {error, not_found} -> reply_not_found(Id, Req)
    end;

action(<<"DELETE">>, Id, Data, Req, DbModule) ->
    case DbModule:delete(Id, Data) of
        ok -> reply_modified(Req);
        {error, not_found} -> reply_not_found(Id, Req)
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

decode_query(Query) ->
    lists:foldl(fun decode_query/2, {ok, []}, Query).

decode_query({Name = <<"filter">>, Arg}, {ok, Query}) ->
    {ok, [{binary_to_atom(Name, utf8),
           binary:split(Arg, <<",">>, [global])}|Query]};

decode_query({Name, _Arg}, {ok, _Query}) ->
    {error, #{unknown_option => Name}};

decode_query({_Name, _Arg}, {error, Reason}) -> {error, Reason}.
