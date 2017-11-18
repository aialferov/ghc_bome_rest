-module(ghc_bome_rest_db).
-compile({no_auto_import, [get/1]}).

-export([
    put/2,
    patch/2,
    get/1, get/2,
    delete/1, delete/2
]).

put(Id = <<"user">>, Data) when is_map(Data) ->
    io:format("PUT ~p => ~p~n", [Id, Data]),
    {ok, modified};

put(Id, Data) when is_map(Data) ->
    io:format("PUT ~p => ~p~n", [Id, Data]),
    {ok, created}.

patch(Id = <<"user">>, Data) when is_map(Data) ->
    io:format("PATCH ~p => ~p~n", [Id, Data]);

patch(Id, Data) when is_map(Data) ->
    io:format("PATCH ~p => ~p~n", [Id, Data]),
    {error, not_found}.

get(Id) -> get(Id, []).

get(Id = <<"user">>, Options = []) ->
    io:format("GET ~p => ~p~n", [Id, Options]),
    {ok, #{
        <<"type1">> => <<"value1">>,
        <<"typeN">> => <<"valueN">>
    }};

get(Id = <<"user">>, Options) ->
    io:format("GET ~p => ~p~n", [Id, Options]),
    Data = #{
        <<"type1">> => <<"value1">>,
        <<"typeN">> => <<"valueN">>
    },
    {ok, lists:foldl(fun apply_option/2, Data, Options)};

get(Id, Options) ->
    io:format("GET ~p => ~p~n", [Id, Options]),
    {error, not_found}.

delete(Id) -> delete(Id, []).

delete(Id = <<"user">>, Types) when is_list(Types) ->
    io:format("DELETE ~p => ~p~n", [Id, Types]);

delete(Id, Types) when is_list(Types) ->
    io:format("DELETE ~p => ~p~n", [Id, Types]),
    {error, not_found}.

apply_option({filter, Types}, Data) -> maps:with(Types, Data).
