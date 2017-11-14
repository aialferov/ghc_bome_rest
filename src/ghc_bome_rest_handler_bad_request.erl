-module(ghc_bome_rest_handler_bad_request).
-export([init/2]).

-include("ghc_bome_rest.hrl").

init(Req0, State) ->
    Req = cowboy_req:reply(?CodeBadRequest, ?ContentTypeText, ?Usage, Req0),
    {ok, Req, State}.
