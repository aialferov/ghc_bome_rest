-module(ghc_bome_rest_app).
-behaviour(application).

-export([
    start/2,
    stop/1, prep_stop/1
]).

-include("ghc_bome_rest_handler.hrl").

-define(Listener, ghc_bome_http).

-define(HandlerOk, ghc_bome_rest_handler_ok).
-define(HandlerBadRequest, ghc_bome_rest_handler_bad_request).

-define(ResourceUsers, "/v1/users/:id/").
-define(Resource, '_').

start(_StartType, _StartArgs) ->
    {ok, Env} = application:get_key(env),
    Port = proplists:get_value(port, Env),
    DbModule = proplists:get_value(db_module, Env),

    Dispatch = cowboy_router:compile([
        {'_', [
            {?ResourceUsers, ?HandlerOk, #state{db_module = DbModule}},
            {?Resource, ?HandlerBadRequest, []}
        ]}
    ]),
    Opts = #{env => #{dispatch => Dispatch}},
    cowboy:start_clear(?Listener, [{port, Port}], Opts).

stop(_State) -> ok.

prep_stop(_State) -> cowboy:stop_listener(?Listener).
