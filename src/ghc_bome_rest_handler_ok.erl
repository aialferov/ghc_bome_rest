-module(ghc_bome_rest_handler_ok).
-export([init/2]).

-include("ghc_bome_rest.hrl").

init(Req0 = #{method := Method, has_body := true}, State) when
    Method == <<"PUT">>; Method == <<"PATCH">>; Method == <<"DELETE">>
->
    Id = cowboy_req:binding(id, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    Req = case decode_body(Body) of
        {ok, Data} -> action(Method, Id, Data, Req1, State#state.db_module);
        {error, Reason} -> reply_bad_request(Reason, Req1)
    end,
    {ok, Req, State};

init(Req0 = #{method := <<"GET">>, has_body := false}, State) ->
    Id = cowboy_req:binding(id, Req0),
    Options = cowboy_req:parse_qs(Req0),

    Req = action(<<"GET">>, Id, Options, Req0, State#state.db_module),
    {ok, Req, State};

init(Req, State) -> {ok, reply_bad_request(Req), State}.

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

decode_body(Body) ->
    try jsx:decode(Body) of
        DecodedBody -> {ok, DecodedBody}
    catch
        _:_ -> {error, invalid_json}
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

reply_bad_request(Req) ->
    cowboy_req:reply(?CodeBadRequest, ?ContentTypeText, ?Usage, Req).

reply_bad_request(Reason, Req) ->
    cowboy_req:reply(
        ?CodeBadRequest, ?ContentTypeJson,
        jsx:encode(#{reason => Reason}), Req
    ).
