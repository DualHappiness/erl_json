-module(erl_json_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
    [encode_test, decode_test, performance_test].

test_case() ->
    [
        {<<"true">>, true},
        {<<"false">>, false},
        {<<"1.0">>, 1.0},
        {<<"null">>, undefined}
    ].

error_case() ->
    [
        {<<"ok">>, ok},
        {<<"1">>, 1}
    ].

encode_test(_Config) ->
    [
        ?assertEqual(A, EncodeJson)
        || {A, B} <- test_case(), {ok, EncodeJson} = erl_json:encode(B)
    ],
    ok.

decode_test(_Config) ->
    [?assertEqual(B, DecodeJson) || {A, B} <- test_case(), {ok, DecodeJson} = erl_json:decode(A)],
    ok.

performance_test(_Config) ->
    ok.
