-module(ghc_bome_rest_db).
-compile({no_auto_import, [get/1]}).

-export([
    put/2,
    patch/2,
    get/1, get/2,
    delete/1, delete/2
]).

put(Id = <<"user">>, Data) ->
    io:format("PUT ~p => ~p~n", [Id, Data]),
    {ok, modified};

put(Id, Data) ->
    io:format("PUT ~p => ~p~n", [Id, Data]),
    {ok, created}.

patch(Id = <<"user">>, Data) ->
    io:format("PATCH ~p => ~p~n", [Id, Data]);

patch(Id, Data) ->
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

    case lists:keyfind(<<"filter">>, 1, Options) of
        {<<"filter">>, TypesBinary} ->
            Types = binary:split(TypesBinary, <<",">>, [global]),
            Member = fun(Type, _Value) -> lists:member(Type, Types) end,
            {ok, maps:filter(Member, Data)};
        false -> {error, bad_options}
    end;

get(Id, Options) ->
    io:format("GET ~p => ~p~n", [Id, Options]),
    {error, not_found}.

delete(Id) -> delete(Id, []).

delete(Id = <<"user">>, Types) ->
    io:format("DELETE ~p => ~p~n", [Id, Types]);

delete(Id, Types) ->
    io:format("DELETE ~p => ~p~n", [Id, Types]),
    {error, not_found}.
