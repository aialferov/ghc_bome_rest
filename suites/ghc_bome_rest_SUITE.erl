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
    "http://localhost:~b/v1/users/id/type"
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
    Body = jsx:encode(#{<<"type1">> => <<"value1">>}),

    {201, <<"">>} = request(put, Endpoint("user1", []), Body),
    {204, <<"">>} = request(put, Endpoint("user", []), Body),

    ReasonMalformedJson = #{<<"reason">> => <<"malformed_json">>},
    {400, ReasonMalformedJson} = request(put, Endpoint("user", []), <<"{">>).

patch(Config) ->
    Endpoint = endpoint_fun(Config),
    Body = jsx:encode(#{<<"type1">> => <<"value1">>,
                        <<"typeN">> => <<"valueN">>}),

    {204, <<"">>} = request(patch, Endpoint("user", []), Body),
    {404, #{<<"user1">> := <<"not_found">>}} =
        request(patch, Endpoint("user1", []), Body),

    ReasonMalformedJson = #{<<"reason">> => <<"malformed_json">>},
    {400, ReasonMalformedJson} = request(patch, Endpoint("user", []), <<"{">>).

get(Config) ->
    Endpoint = endpoint_fun(Config),

    {200, #{<<"type1">> := <<"value1">>, <<"typeN">> := <<"valueN">>}} =
        request(get, Endpoint("user", [{"filter", "type1,typeN"}])),

    {200, #{<<"typeN">> := <<"valueN">>}} =
        request(get, Endpoint("user", [{"filter", "typeN"}])),

    {404, #{<<"user1">> := <<"not_found">>}} =
        request(get, Endpoint("user1", [{"filter", "type1,typeN"}])),

    {400, #{<<"reason">> :=
          #{<<"malformed_query">> := #{<<"bad_filter">> := <<"type1,typeN">>}}}} =
        request(get, Endpoint("user", [{"bad_filter", "type1,typeN"}])).

delete(Config) ->
    Endpoint = endpoint_fun(Config),
    Body = jsx:encode([<<"type1">>, <<"typeN">>]),

    {204, <<"">>} = request(patch, Endpoint("user", []), Body),
    {404, #{<<"user1">> := <<"not_found">>}} =
        request(patch, Endpoint("user1", []), Body),

    ReasonMalformedJson = #{<<"reason">> => <<"malformed_json">>},
    {400, ReasonMalformedJson} = request(patch, Endpoint("user", []), <<"{">>).

bad(Config) ->
    Port = port(Config),
    lists:foreach(fun(Endpoint) ->
        {400, <<"">>} = request(get, format(Endpoint, [Port]))
    end, ?BadEndpoints).

endpoint_fun(Config) ->
    Port = port(Config),
    fun(Id, Ql) -> format(?Endpoint, [Port, Id, qs(Ql)]) end.

request(Method, Endpoint) -> request(Method, Endpoint, []).
request(Method, Endpoint, Body) ->
    Request = case Method of
        get -> {Endpoint, ""};
        _Other -> {Endpoint, ?ContentType, [], Body}
    end,
    code_body(httpc:request(Method, Request, [], ?Options)).

code_body(Response) -> case Response of
    {ok, {{_Version, Code, _Reason}, _Headers, <<"">>}} -> {Code, <<"">>};
    {ok, {{_Version, Code, _Reason}, _Headers, Body}} ->
        {Code, jsx:decode(Body, [return_maps])};

    {error, Reason} -> {error, Reason}
end.

qs([]) -> "";
qs(Ql) -> [$?|tl(lists:flatten([[$&|K] ++ [$=|V] || {K, V} <- Ql]))].

port(Config) -> proplists:get_value(port, Config).

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
