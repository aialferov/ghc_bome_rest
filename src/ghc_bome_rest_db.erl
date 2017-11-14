-module(ghc_bome_rest_db).
-compile({no_auto_import, [get/1]}).

-export([
    put/2,
    get/2, get/1,
    delete/2, delete/1
]).

put(User, {Type, Value}) ->
    io:format("PUT ~p => ~p: ~p~n", [btl(User), btl(Type), btl(Value)]).

get(User, undefined) -> get(User);
get(User, Type) ->
    io:format("GET ~p => ~p~n", [btl(User), btl(Type)]),
    {ok, <<"value">>}.

get(User) ->
    io:format("GET ~p~n", [btl(User)]),
    {ok, #{<<"type">> => <<"value">>}}.

delete(User, undefined) -> delete(User);
delete(User, Type) -> io:format("DELETE ~p => ~p~n", [btl(User), btl(Type)]).
delete(User) -> io:format("DELETE ~p~n", [btl(User)]).

btl(B) when is_binary(B) -> binary_to_list(B);
btl(T) -> T.
