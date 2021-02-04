-module(erl_json).

-on_load(init/0).

-export([init/0, encode/1, decode/1]).

init() ->
    PrivPath = code:priv_dir(erl_json),
    LibPath = filename:join(PrivPath, "liberl_json"),
    erlang:load_nif(LibPath, 0).

-spec decode(string()) -> JSON :: map().
decode(_String) ->
    error("nif not loaded").

-spec encode(JSON :: map()) -> string().
encode(_Json) ->
    error("nif not loaded").
