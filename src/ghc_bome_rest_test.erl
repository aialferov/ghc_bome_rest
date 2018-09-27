-module(ghc_bome_rest_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, ghc_bome_rest).

ghc_bome_rest_test() ->
    ?assertEqual(ok, ?M:ghc_bome_rest()).
