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
-define(Log, ghc_bome_rest_log).

-define(ResourceUsers, "/v1/users/:id/").
-define(Resource, '_').

start(_StartType, _StartArgs) ->
    {ok, Env} = application:get_key(env),

    Port = proplists:get_value(port, Env),
    DbModule = proplists:get_value(db_module, Env),

    LogFileName = proplists:get_value(log_file, Env),
    {ok, LogFile} = file:open(LogFileName, [append]),

    HandlerOkState = #state{db_module = DbModule, log_file = LogFile},
    HandlerBadRequestState = #state{log_file = LogFile},

    Dispatch = cowboy_router:compile([
        {'_', [
            {?ResourceUsers, ?HandlerOk, HandlerOkState},
            {?Resource, ?HandlerBadRequest, HandlerBadRequestState}
        ]}
    ]),
    Opts = #{env => #{dispatch => Dispatch}},
    {ok, Pid} = cowboy:start_clear(?Listener, [{port, Port}], Opts),

    ?Log:text(LogFile, "Listening on port: ~b", [Port]),
    {ok, Pid, LogFile}.

prep_stop(State) ->
    cowboy:stop_listener(?Listener),
    State.

stop(LogFile) ->
    file:close(LogFile).
