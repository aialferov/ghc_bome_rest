-module(ghc_bome_rest_db).
-compile({no_auto_import, [get/1]}).

-export([
    put/2,
    patch/2,
    get/1, get/2,
    delete/1, delete/2
]).

put(UserId = <<"user1">>, Metrics) when is_map(Metrics) ->
    io:format("PUT ~p => ~p~n", [UserId, Metrics]),
    {ok, modified};

put(UserId, Metrics) when is_map(Metrics) ->
    io:format("PUT ~p => ~p~n", [UserId, Metrics]),
    {ok, created}.

patch(UserId = <<"user1">>, Metrics) when is_map(Metrics) ->
    io:format("PATCH ~p => ~p~n", [UserId, Metrics]);

patch(UserId, Metrics) when is_map(Metrics) ->
    io:format("PATCH ~p => ~p~n", [UserId, Metrics]),
    {error, not_found}.

get(UserId) -> get(UserId, []).

get(UserId = <<"user1">>, Options = []) ->
    io:format("GET ~p => ~p~n", [UserId, Options]),
    {ok, #{
        <<"metric_name1">> => <<"metric_value1">>,
        <<"metric_nameN">> => <<"metric_valueN">>
    }};

get(UserId = <<"user1">>, Options) ->
    io:format("GET ~p => ~p~n", [UserId, Options]),
    Metrics = #{
        <<"metric_name1">> => <<"metric_value1">>,
        <<"metric_nameN">> => <<"metric_valueN">>
    },
    {ok, lists:foldl(fun apply_option/2, Metrics, Options)};

get(UserId, Options) ->
    io:format("GET ~p => ~p~n", [UserId, Options]),
    {error, not_found}.

delete(UserId) -> delete(UserId, []).

delete(UserId = <<"user1">>, MetricNames) when is_list(MetricNames) ->
    io:format("DELETE ~p => ~p~n", [UserId, MetricNames]);

delete(UserId, MetricNames) when is_list(MetricNames) ->
    io:format("DELETE ~p => ~p~n", [UserId, MetricNames]),
    {error, not_found}.

apply_option({filter, MetricNames}, Metrics) -> maps:with(MetricNames, Metrics).
