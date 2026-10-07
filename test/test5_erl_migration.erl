%% migration

%% This is an autogenerate file. Please adjust

-module(test5_erl_migration).
-behaviour(erl_migration).
-schema_name(schema_name_3).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).

init([]) -> ok.

get_current_rev() ->
    test5.

get_prev_rev() ->
    none.

up() ->
   io:format("test5: up callled~n").

down() ->
   io:format("test5: down callled~n").