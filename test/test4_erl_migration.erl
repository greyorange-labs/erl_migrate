-module(test4_erl_migration).
-behaviour(erl_migration).
-schema_name(schema_name_2).
-export([up/0, down/0, get_current_rev/0, get_prev_rev/0, init/1]).

init([]) -> ok.

get_current_rev() ->
    test4.

get_prev_rev() ->
    test3.

up() ->
   io:format("test4: up callled~n").

down() ->
   io:format("test4: down callled~n").