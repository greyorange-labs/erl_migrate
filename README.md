# erl_migrate
A tool to upgrade/downgrade schema and migrate data of an erlang app's database(s)

[![Build Status](https://travis-ci.org/greyorange/erl_migrate.svg?branch=master)](https://travis-ci.org/greyorange/erl_migrate)

# Installation

* run `make deps` to install depedencies
* run `make` to compile code
* run `make eunit` to test UT's


# Usage
### Create migration file
```erlang
Args =
   #{
      schema_name => schema_name_1,
      migration_src_files_path => "apps/app_name/src/migrations",
      migration_beam_files_path => "_build/default/lib/{{AppName}}/ebin/
   },
erl_migrate:create_migration_file(Args).
```

### Apply Upgrade
```erlang
Args =
   #{
      schema_name => schema_name_1,
      schema_instance => schema_instance_1,
      migration_beam_files_dir_path => "path/of/app/ebin/"
   },
erl_migrate:apply_upgrades(Args).

```
### Apply Downgrade
```erlang
Args =
   #{
      schema_name => schema_name_1,
      schema_instance_1
   },
%% Num: # of migrations to downgrade
erl_migrate:apply_downgrades(Args, Num).
```

### Detect revision seq conflicts
```erlang
Args = #{schema_name => schema_name_1},
erl_migrate:detect_revision_sequence_conflicts(Args, Num).
```

### Run observability
Every revision attempt is recorded in the `erl_migration_runs` Mnesia table
(created alongside the head/history tables) with status `running -> ok | failed`,
direction, timestamps, error reason/stacktrace, node and an attempt key.

```erlang
%% Last attempt for a schema (record | none)
erl_migrate:get_last_migration_run(Args).

%% All recorded attempts for a schema (list of records, newest last)
erl_migrate:get_run_log(Args).
```

The head is advanced **per revision** during upgrades, so a mid-batch failure
resumes from the last applied revision on the next boot instead of re-running
already-applied revisions; the failing revision itself is re-attempted
unchanged. Failures are recorded and reported before the error is raised.

#### Custom observer (optional)

Pass `run_log_observer => Module` in `Args` to get callbacks for every
lifecycle event. The module may implement any subset of the
`migration_observer` behaviour callbacks; observer failures are never
allowed to affect the migration itself:

```erlang
Args = Args0#{run_log_observer => my_observer},
erl_migrate:apply_upgrades(Args).
```

Callbacks: `on_revision_start(SchemaName, SchemaInstance, Revision, Args)`,
`on_revision_ok(SchemaName, SchemaInstance, Revision, DurationMs, Args)`,
`on_revision_failed(SchemaName, SchemaInstance, Revision, DurationMs, {Class, Reason, Stack}, Args)`,
`on_run_finished(SchemaName, SchemaInstance, Result, Args)`.

### Configs
To enable print statements of library, add `{debug, true}` in sys.config under erl_migrate app config section, for example ->
```erlang
sys.config
[
   ....Other app configs
   {erl_migrate,[
      {debug, true}
   ]},
   ....Other app configs
]
```

# License

MIT License
