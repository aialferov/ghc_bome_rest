-module(ghc_bome_rest_handler_ok).
-export([init/2]).

-include("ghc_bome_rest.hrl").

init(Req0 = #{method := <<"PUT">>, has_body := true}, DbModule) ->
    User = cowboy_req:binding(user, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    Req = case jsx:decode(Body) of
        [{Type, Value}] ->
            ok = DbModule:put(User, {Type, Value}),
            cowboy_req:reply(?CodeCreated, ?ContentTypeJson, Req1);
        _Other ->
            cowboy_req:reply(?CodeBadRequest, ?ContentTypeText, ?Usage, Req1)
    end,
    {ok, Req, DbModule};

init(Req0 = #{method := <<"GET">>, has_body := false}, DbModule) ->
    User = cowboy_req:binding(user, Req0),
    Type = cowboy_req:binding(type, Req0),
    {ok, Data} = DbModule:get(User, Type),

    Req = cowboy_req:reply(?CodeOk, ?ContentTypeJson, jsx:encode(Data), Req0),
    {ok, Req, DbModule};

init(Req0 = #{method := <<"DELETE">>, has_body := false}, DbModule) ->
    User = cowboy_req:binding(user, Req0),
    Type = cowboy_req:binding(type, Req0),
    ok = DbModule:delete(User, Type),

    Req = cowboy_req:reply(?CodeNoContent, Req0),
    {ok, Req, DbModule};

init(Req0, State) ->
    Req = cowboy_req:reply(?CodeBadRequest, ?ContentTypeText, ?Usage, Req0),
    {ok, Req, State}.
