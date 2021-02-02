![Erlang CI](https://github.com/DualHappiness/erl_json/workflows/Erlang%20CI/badge.svg?branch=main)

# erl_json
=====

a native json parser

## WARN
number仅支持f64, string对应的是binary, atom仅有true, false, undefined可以使用

## Performance
```
performance test result in 10000 Count:

jsx: 644800

erl_json: 121598

ratio is: 0.18858250620347394
```

Build
-----

    $ rebar3 compile
