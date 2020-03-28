-module(unum_SUITE).
-compile(export_all).
-include("common_test/include/ct.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

all() ->
    [posit, unum, ubox].

posit(_Config) ->
    ok.

unum(_Config) ->
    unum:encode("1.0") == <<0:1, 0:1, 1:1, 0:1, 0:1, 0:1>>,
    unum:encode(1.0) == <<0:1, 0:1, 1:1, 0:1, 0:1, 0:1>>,
    unum:encode(1.0, [{fieldsizesize,4},{exponentsizesize,3}]) == <<0:1, 0:1, 1:1, 0:1, 0:3, 0:4>>,
    
    % <<0:0, >>
    % unum:encode("3.14") == <<>>,
    % unum:encode(3.14) == <<>>,
    % unum:encode(-3.14) == <<>>,
    % unum:encode("-3.14") == <<>>,
    % unum:encode("3.14", as_bitstring) == <<>>,
    % unum:encode("3.14", as_list) == [],
    % unum:encode("3.14", as_map) == [],

    unum:decode(<<0:1, 0:1, 0:1, 0:1, 0:2, 0:2>>) == <<"0">>,
    unum:decode(<<>>, as_string) == "",
    unum:decode(<<>>, as_map) == #{},
    ok.

unum_operation(_Config) ->
    ok.

ubox(_Config) ->
    ok.
