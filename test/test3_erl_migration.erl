-module(test3_erl_migration).
-behaviour(erl_migration).
-schema_name(schema_name_2).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).

init([]) -> ok.

get_current_rev() ->
    test3.

get_prev_rev() ->
    none.

up() ->
   io:format("test3: up callled~n").

down() ->
   io:format("test3: down callled~n").