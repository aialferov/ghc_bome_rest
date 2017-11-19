-module(ghc_bome_rest_SUITE).

-export([
    all/0, suite/0,

    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,

    put/1,
    patch/1,
    get/1,
    delete/1,
    bad/1
]).

-include_lib("common_test/include/ct.hrl").

-define(Endpoint, "http://localhost:~b/v1/users/~s~s").

-define(BadEndpoints, [
    "http://localhost:~b/v2/users/id",
    "http://localhost:~b/v1/user/id",
    "http://localhost:~b/v1/users/id/metric"
]).

-define(ContentType, "application/json").
-define(Options, [{body_format, binary}]).

all() -> [
    put,
    patch,
    get,
    delete,
    bad
].

suite() -> [].

init_per_suite(Config) ->
    {ok, _Apps} = ghc_bome_rest:start(),
    {ok, Port} = application:get_env(ghc_bome_rest, port),
    [{port, Port}|Config].

end_per_suite(Config) ->
    ok = ghc_bome_rest:stop(),
    Config.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, Config) -> Config.

put(Config) ->
    Metrics = jsx:encode(#{<<"metric_name1">> => <<"metric_value1">>}),

    {ok, {204, <<"">>}} = put("user1", Metrics, Config),
    {ok, {201, <<"">>}} = put("user2", Metrics, Config),

    {ok, {400, #{<<"reason">> := <<"malformed_json">>}}} =
        put("user1", <<"{">>, Config).

patch(Config) ->
    Metrics = jsx:encode(#{<<"metric_name1">> => <<"metric_value1">>,
                           <<"metric_nameN">> => <<"metric_valueN">>}),

    {ok, {204, <<"">>}} = patch("user1", Metrics, Config),
    {ok, {404, #{<<"user2">> := <<"not_found">>}}} =
        patch("user2", Metrics, Config),

    {ok, {400, #{<<"reason">> := <<"malformed_json">>}}} =
        patch("user1", <<"{">>, Config).

get(Config) ->
    {ok, {200, #{<<"metric_name1">> := <<"metric_value1">>,
                 <<"metric_nameN">> := <<"metric_valueN">>}}} =
        get("user1", [{"filter", "metric_name1,metric_nameN"}], Config),

    {ok, {200, #{<<"metric_nameN">> := <<"metric_valueN">>}}} =
        get("user1", [{"filter", "metric_nameN"}], Config),

    {ok, {404, #{<<"user2">> := <<"not_found">>}}} =
        get("user2", [{"filter", "metric_name1,metric_nameN"}], Config),

    {ok, {400, #{<<"reason">> := #{<<"unknown_option">> := <<"bad_filter">>}}}} =
        get("user1", [{"bad_filter", "metric_name1,metric_nameN"}], Config).

delete(Config) ->
    Metrics = jsx:encode([<<"metric_name1">>, <<"metric_nameN">>]),

    {ok, {204, <<"">>}} = delete("user1", Metrics, Config),
    {ok, {404, #{<<"user2">> := <<"not_found">>}}} =
        delete("user2", Metrics, Config),

    {ok, {400, #{<<"reason">> := <<"malformed_json">>}}} =
        delete("user1", <<"{">>, Config).

bad(Config) ->
    Port = port(Config),
    lists:foreach(fun(Endpoint) ->
        {ok, {400, _Usage}} = request(get, format(Endpoint, [Port]))
    end, ?BadEndpoints).

endpoint_fun(Config) ->
    Port = port(Config),
    fun(UserId, Ql) -> format(?Endpoint, [Port, UserId, qs(Ql)]) end.

qs([]) -> "";
qs(Ql) -> [$?|tl(lists:flatten([[$&|K] ++ [$=|V] || {K, V} <- Ql]))].

put(UserId, Metrics, Config) ->
    request(put, (endpoint_fun(Config))(UserId, []), Metrics).

patch(UserId, Metrics, Config) ->
    request(patch, (endpoint_fun(Config))(UserId, []), Metrics).

get(UserId, MetricNames, Config) ->
    request(get, (endpoint_fun(Config))(UserId, MetricNames)).

delete(UserId, Metrics, Config) ->
    request(delete, (endpoint_fun(Config))(UserId, []), Metrics).

request(Method, Endpoint) -> request(Method, Endpoint, []).
request(Method, Endpoint, Body) ->
    Request = case Method of
        get -> {Endpoint, ""};
        _Other -> {Endpoint, ?ContentType, [], Body}
    end,
    code_body(httpc:request(Method, Request, [], ?Options)).

code_body(Response) -> case Response of
    {ok, {{_Version, Code, _Reason}, _Headers, <<"">>}} -> {ok, {Code, <<"">>}};
    {ok, {{_Version, Code, _Reason}, Headers, Body}} ->
        {ok, case is_content_type_json(Headers) of
            true -> {Code, jsx:decode(Body, [return_maps])};
            false -> {Code, Body}
        end};

    {error, Reason} -> {error, Reason}
end.

is_content_type_json(Headers) ->
    proplists:get_value("content-type", Headers, "") == "application/json".

port(Config) -> proplists:get_value(port, Config).

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
