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
    Endpoint = endpoint_fun(Config),
    Body = jsx:encode(#{<<"metric_name1">> => <<"metric_value1">>}),

    {204, <<"">>} = request(put, Endpoint("user1", []), Body),
    {201, <<"">>} = request(put, Endpoint("user2", []), Body),

    ReasonMalformedJson = #{<<"reason">> => <<"malformed_json">>},
    {400, ReasonMalformedJson} = request(put, Endpoint("user1", []), <<"{">>).

patch(Config) ->
    Endpoint = endpoint_fun(Config),
    Body = jsx:encode(#{<<"metric_name1">> => <<"metric_value1">>,
                        <<"metric_nameN">> => <<"metric_valueN">>}),

    {204, <<"">>} = request(patch, Endpoint("user1", []), Body),
    {404, #{<<"user2">> := <<"not_found">>}} =
        request(patch, Endpoint("user2", []), Body),

    ReasonMalformedJson = #{<<"reason">> => <<"malformed_json">>},
    {400, ReasonMalformedJson} = request(patch, Endpoint("user1", []), <<"{">>).

get(Config) ->
    Endpoint = endpoint_fun(Config),

    {200, #{<<"metric_name1">> := <<"metric_value1">>,
            <<"metric_nameN">> := <<"metric_valueN">>}} =
        request(get, Endpoint("user1",
                              [{"filter", "metric_name1,metric_nameN"}])),

    {200, #{<<"metric_nameN">> := <<"metric_valueN">>}} =
        request(get, Endpoint("user1", [{"filter", "metric_nameN"}])),

    {404, #{<<"user2">> := <<"not_found">>}} =
        request(get, Endpoint("user2",
                              [{"filter", "metric_name1,metric_nameN"}])),

    {400, #{<<"reason">> := #{<<"unknown_option">> := <<"bad_filter">>}}} =
        request(get, Endpoint("user1",
                              [{"bad_filter", "metric_name1,metric_nameN"}])).

delete(Config) ->
    Endpoint = endpoint_fun(Config),
    Body = jsx:encode([<<"metric_name1">>, <<"metric_nameN">>]),

    {204, <<"">>} = request(delete, Endpoint("user1", []), Body),
    {404, #{<<"user2">> := <<"not_found">>}} =
        request(delete, Endpoint("user2", []), Body),

    ReasonMalformedJson = #{<<"reason">> => <<"malformed_json">>},
    {400, ReasonMalformedJson} = request(delete, Endpoint("user1", []), <<"{">>).

bad(Config) ->
    Port = port(Config),
    lists:foreach(fun(Endpoint) ->
        {400, _Usage} = request(get, format(Endpoint, [Port]))
    end, ?BadEndpoints).

endpoint_fun(Config) ->
    Port = port(Config),
    fun(UserId, Ql) -> format(?Endpoint, [Port, UserId, qs(Ql)]) end.

request(Method, Endpoint) -> request(Method, Endpoint, []).
request(Method, Endpoint, Body) ->
    Request = case Method of
        get -> {Endpoint, ""};
        _Other -> {Endpoint, ?ContentType, [], Body}
    end,
    code_body(httpc:request(Method, Request, [], ?Options)).

code_body(Response) -> case Response of
    {ok, {{_Version, Code, _Reason}, _Headers, <<"">>}} -> {Code, <<"">>};
    {ok, {{_Version, Code, _Reason}, Headers, Body}} ->
        case is_content_type_json(Headers) of
            true -> {Code, jsx:decode(Body, [return_maps])};
            false -> {Code, Body}
        end;

    {error, Reason} -> {error, Reason}
end.

qs([]) -> "";
qs(Ql) -> [$?|tl(lists:flatten([[$&|K] ++ [$=|V] || {K, V} <- Ql]))].

is_content_type_json(Headers) ->
    proplists:get_value("content-type", Headers, "") == "application/json".

port(Config) -> proplists:get_value(port, Config).

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
