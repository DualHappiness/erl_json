[![Build Status](http://10.1.221.131:11180/api/badges/ErlangFramework/erl_json/status.svg)](http://10.1.221.131:11180/ErlangFramework/erl_json)
erl_json
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
