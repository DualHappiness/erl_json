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
        {<<"null">>, undefined},
        {<<"\"string\"">>, <<"string">>},
        {<<"[1.0,2.0]">>, [1.0, 2.0]},
        {<<"{\"a\":1.0}">>, #{<<"a">> => 1.0}}
    ].

error_case() ->
    [
        {<<"ok">>, ok},
        {<<"1">>, 1},
        {<<"[string]">>, "string"},
        {<<"[1]">>, [1]},
        {<<"[1, null]">>, [1, undefined]}
    ].

encode_test(_Config) ->
    [
        begin
            EncodeJson = erl_json:encode(B),
            ?assertEqual(A, EncodeJson)
        end
        || {A, B} <- test_case()
    ],
    [?assertError(badarg, erl_json:encode(B)) || {_, B} <- error_case()],
    ok.

decode_test(_Config) ->
    [
        begin
            DecodeJson = erl_json:decode(A),
            ?assertEqual(B, DecodeJson)
        end
        || {A, B} <- test_case()
    ],
    ok.

performance_test_case() ->
    [
        #{
            <<"id">> => 1.0,
            <<"is_true">> => false,
            <<"first_name">> => <<"Jeanette">>,
            <<"last_name">> => <<"Penddreth">>,
            <<"email">> => <<"jpenddreth0@census.gov">>,
            <<"gender">> => <<"Female">>,
            <<"ip_address">> => [26.58, 193.2],
            <<"maps">> => #{
                <<"a">> => 1.0,
                <<"b">> => true,
                <<"c">> => [],
                <<"d">> => [1.0],
                <<"e">> => [<<"aa">>, <<"bb">>],
                % <<"f">> => undefined,
                <<"g">> => #{<<"aa">> => 1.0}
            }
        }
    ].

performance_test(_Config) ->
    Cases = performance_test_case(),
    Count = 10000,
    {Time1, _} = timer:tc(fun() ->
        [
            begin
                Encode = jsx:encode(Case),
                Decode = jsx:decode(Encode),
                ?assertEqual(Case, Decode)
            end
            || Case <- Cases, _ <- lists:seq(1, Count)
        ]
    end),
    {Time2, _} = timer:tc(fun() ->
        [
            begin
                Encode = erl_json:encode(Case),
                Decode = erl_json:decode(Encode),
                ?assertEqual(Case, Decode)
            end
            || Case <- Cases, _ <- lists:seq(1, Count)
        ]
    end),

    io:format("performance test result in ~p Count:~n", [Count]),
    io:format("jsx: ~p~n", [Time1]),
    io:format("erl_json: ~p~n", [Time2]),
    io:format("ratio is: ~p~n", [Time2 / Time1]),
    ok.
