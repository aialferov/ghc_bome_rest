-module(ghc_bome_rest_app).
-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

-define(Listener, ghc_bome_http).

-define(HandlerOk, ghc_bome_rest_handler_ok).
-define(HandlerBadRequest, ghc_bome_rest_handler_bad_request).

-define(Path, "/v1/:user/[:type]").

start(_StartType, _StartArgs) ->
    {ok, Env} = application:get_key(env),
    Port = proplists:get_value(port, Env),
    DbModule = proplists:get_value(db_module, Env),

    Dispatch = cowboy_router:compile([
        {'_', [{?Path, ?HandlerOk, DbModule},
               {'_', ?HandlerBadRequest, []}]}
    ]),
    Opts = #{env => #{dispatch => Dispatch}},
    cowboy:start_clear(?Listener, [{port, Port}], Opts).

stop(_State) -> ok.

prep_stop(_State) -> cowboy:stop_listener(?Listener).
