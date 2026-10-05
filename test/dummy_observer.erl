%% Test observer used by erl_migrate_tests to assert the run_log_observer
%% callbacks are invoked. Records every call into the
%% `dummy_observer_calls` named ETS table.
-module(dummy_observer).

-export([
    init/0,
    on_revision_start/4,
    on_revision_ok/5,
    on_revision_failed/6,
    on_run_finished/4
]).

init() ->
    case ets:info(dummy_observer_calls) of
        undefined ->
            ets:new(dummy_observer_calls, [public, bag, named_table]);
        _ ->
            ok
    end,
    ok.

on_revision_start(SchemaName, Instance, Revision, _Args) ->
    init(),
    ets:insert(dummy_observer_calls, {start, SchemaName, Instance, Revision}),
    ok.

on_revision_ok(SchemaName, Instance, Revision, DurationMs, _Args) ->
    init(),
    ets:insert(dummy_observer_calls, {ok, SchemaName, Instance, Revision, DurationMs}),
    ok.

on_revision_failed(SchemaName, Instance, Revision, DurationMs, Error, _Args) ->
    init(),
    ets:insert(dummy_observer_calls, {failed, SchemaName, Instance, Revision, DurationMs, Error}),
    ok.

on_run_finished(SchemaName, Instance, Result, _Args) ->
    init(),
    ets:insert(dummy_observer_calls, {run_finished, SchemaName, Instance, Result}),
    ok.