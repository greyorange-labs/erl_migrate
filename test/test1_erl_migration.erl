%% migration

%% This is an autogenerate file. Please adjust

-module(test1_erl_migration).
-behaviour(erl_migration).
-schema_name(schema_name).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).

init([]) -> ok.

get_current_rev() ->
    test1.

get_prev_rev() ->
    none.

up() ->
   io:format("test1: up callled~n").

down() ->
   io:format("test1: down callled~n").
