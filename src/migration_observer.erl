-module(migration_observer).

%% Optional run-observability hook. Supplied via the `run_log_observer` key
%% in the Args map passed to erl_migrate:apply_upgrades/1 or
%% erl_migrate:apply_downgrades/2. All callbacks are best-effort: the engine
%% always writes its durable run-log table first, and never lets an observer
%% failure affect the migration itself.
%%
%% Any module implementing these callbacks (or a subset) can be passed. For
%% the canonical implementation used by butler_server, see the butler_shared
%% app's migration_observer_metrics module.

-callback on_revision_start(
    SchemaName :: any(),
    SchemaInstance :: any(),
    Revision :: atom(),
    Args :: map()
) -> ok.

-callback on_revision_ok(
    SchemaName :: any(),
    SchemaInstance :: any(),
    Revision :: atom(),
    DurationMs :: integer(),
    Args :: map()
) -> ok.

-callback on_revision_failed(
    SchemaName :: any(),
    SchemaInstance :: any(),
    Revision :: atom(),
    DurationMs :: integer(),
    Error :: {Class :: atom(), Reason :: any(), Stack :: list()},
    Args :: map()
) -> ok.

-callback on_run_finished(
    SchemaName :: any(),
    SchemaInstance :: any(),
    Result :: {ok, NewHead :: any(), RevList :: list()},
    Args :: map()
) -> ok.