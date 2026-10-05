%% migration

%% This is an autogenerate file. Please adjust
%% Fails with error(test6_fail) when the erl_migrate application env
%% `fail_test6` is set to true, so tests can exercise the failure path.

-module(test6_erl_migration).
-behaviour(erl_migration).
-schema_name(schema_name_3).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).

init([]) -> ok.

get_current_rev() ->
    test6.

get_prev_rev() ->
    test5.

up() ->
    io:format("test6: up callled~n"),
    case application:get_env(erl_migrate, fail_test6, false) of
        true ->
            error(test6_fail);
        false ->
            ok
    end.

down() ->
   io:format("test6: down callled~n").