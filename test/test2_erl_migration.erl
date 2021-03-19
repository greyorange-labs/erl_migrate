%% migration

%% This is an autogenerate file. Please adjust

-module(test2_erl_migration).
-behaviour(erl_migration).
-schema_name(schema_name).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).

init([]) -> ok.

get_current_rev() ->
    test2.

get_prev_rev() ->
    test1.

up() ->
   io:format("test2: up callled~n").

down() ->
   io:format("test2: down callled~n").
