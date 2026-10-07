-module(erl_migrate_tests).

-include_lib("eunit/include/eunit.hrl").

-record(db_migration_runs, {
    id,
    tag,
    migration_name,
    direction,
    status,
    started_at,
    finished_at,
    error_reason,
    stacktrace,
    node
}).

-define(ARGS,
    #{
        schema_name => schema_name_1,
        schema_instance => schema_instance,
        migration_src_files_dir_path => "src/migrations/",
        migration_beam_files_dir_path => application:get_env(erl_migrate, migration_beam_dir, "ebin")
    }
).

create_migration_file_test_() ->
	[
        %{require, rm, unix},
	    {"check revision file creation",
            fun () ->
                {ok, Dir} = file:get_cwd(),
                code:add_path(Dir),
                %io:format("path: ~p~n", [Dir]),
                Filename = erl_migrate:create_migration_file(?ARGS),
                ?assertEqual(true, filelib:is_file(Filename)),
                ?assertCmd("rm " ++ Filename)
                %io:format("file: ~p~n", [Filename])
            end
        },
        {"check revision file creation and deletion",
            fun () ->
                {ok, Dir} = file:get_cwd(),
                code:add_path(Dir),
                Filename = erl_migrate:create_migration_file(?ARGS),
                ?assertCmd("rm " ++ Filename)
            end
        },
	    {"check copyright year, name, email",
            fun () ->
                {ok, Dir} = file:get_cwd(),
                code:add_path(Dir),
                Filename = erl_migrate:create_migration_file(?ARGS),
                ?assertEqual(true, filelib:is_file(Filename)),
                {ok, Bin} = file:read_file(Filename),
                {{Year, _, _}, _} = calendar:local_time(),
                Author =
                    case list_to_binary(string:trim(os:cmd("git config --get user.name"))) of
                        <<>> ->
                            <<"[ADD NAME HERE]">>;
                        <<_, _/binary>> = A ->
                            A
                    end,
                Email =
                    case list_to_binary(string:trim(os:cmd("git config --get user.email"))) of
                        <<>> ->
                            <<"[ADD EMAIL HERE]">>;
                        <<_, _/binary>> = E ->
                            E
                    end,
                ?assertNotEqual(nomatch, binary:match(Bin, <<"@copyright (C) ", (integer_to_binary(Year))/binary>>)),
                ?assertNotEqual(nomatch, binary:match(Bin, <<"@author ", Author/binary, " ", Email/binary>>)),
                ?assertCmd("rm " ++ Filename)
            end
        }
	].

migration_test_() ->
    {
        setup,
        fun() ->
            start_mnesia(),
            %% NOTE:
            %% We mock calendar:local_time/0 instead of erl_migrate:get_current_time/0
            %% because local self-calls may be inlined by the compiler and bypass meck.
            meck:new(calendar, [unstick, passthrough]),
            meck:expect(calendar, local_time,
                fun() ->
                    {{1994, 5, 10}, {11, 11, 11}}
                end
            )
        end,
        fun(_) ->
            meck:unload(),
            stop_mnesia()
        end,
        [
            {
                inorder,
                [
                    {"Init Migrations should not crash",
                        ?_assertEqual(ok, erl_migrate:init_db_tables())
                    },
                    {
                        "Test migration calculation functions",
                        fun() ->
                            Args = ?ARGS#{schema_instance => schema_instance_1},
                            ?assertEqual(none, erl_migrate:get_applied_head(Args)),
                            ?assertEqual(test1, erl_migrate:get_base_revision(Args)),
                            ?assertEqual(test2, erl_migrate:get_next_revision(test1, Args)),
                            ?assertEqual([test1, test2], erl_migrate:find_pending_migrations(Args))
                        end
                    },
                    {"Test apply upgrades / downgrades",
                        fun() ->
                            Args1 = ?ARGS#{schema_instance => schema_instance_1, schema_name => schema_name_1},
                            Args2 = ?ARGS#{schema_instance => schema_instance_2, schema_name => schema_name_1},
                            Args3 = ?ARGS#{schema_instance => schema_instance_1, schema_name => schema_name_2},
                            Args4 = ?ARGS#{schema_instance => schema_instance_2, schema_name => schema_name_2},
                            %% Test1: First time
                            ?assertEqual({ok, test2, [test1, test2]}, erl_migrate:apply_upgrades(Args1)),
                            %% Test2: When all migrations are already applied
                            ?assertEqual({ok, test2, []}, erl_migrate:apply_upgrades(Args1)),
                            %% Test3: Apply same schema migrations for different instance
                            ?assertEqual({ok, test2, [test1, test2]}, erl_migrate:apply_upgrades(Args2)),
                            ?assertEqual({ok, test2, []}, erl_migrate:apply_upgrades(Args2)),
                            %% Test4: Check if current head was updated for both instances
                            ?assertEqual(test2, erl_migrate:get_applied_head(Args1)),
                            ?assertEqual(test2, erl_migrate:get_applied_head(Args1)),
                            %% Test5: Downgrade test: Don't downgrade base migration 
                                %% 2 passed but only 1 migration will be downgraded as 2nd will be a base migration
                            ?assertEqual({ok, test1, [test2]}, erl_migrate:apply_downgrades(Args1, 2)), 
                            ?assertEqual(test1, erl_migrate:get_applied_head(Args1)),
                            %% Test6: Downgrade test: Don't downgrade base migration 
                                %% 2 passed: both migrations will be downgraded
                            ?assertEqual({ok, none, [test2, test1]}, erl_migrate:apply_downgrades(Args2#{down_base_migration => true}, 2)),
                            ?assertEqual(none, erl_migrate:get_applied_head(Args2)),
                            %% Test7: Existing setup: Pending migrations should get applied
                                %% For instance 1: base migration was already applied
                            ?assertEqual({ok, test2, [test2]}, erl_migrate:apply_upgrades(Args1)),
                            ?assertEqual(test2, erl_migrate:get_applied_head(Args1)),
                                %% For instance 2: all migration will be applied
                            ?assertEqual({ok, test2, [test1, test2]}, erl_migrate:apply_upgrades(Args2)), 
                            ?assertEqual(test2, erl_migrate:get_applied_head(Args1)),
                            %% For same instances a different schema migrations should get applied
                            ?assertEqual({ok, test4, [test3, test4]}, erl_migrate:apply_upgrades(Args3)),
                            ?assertEqual(test4, erl_migrate:get_applied_head(Args3)),
                            ?assertEqual({ok, test4, [test3, test4]}, erl_migrate:apply_upgrades(Args4)),
                            ?assertEqual(test4, erl_migrate:get_applied_head(Args4))
                        end
                    },
                    {"Test persistance",
                        fun() ->
                            SchemaMigration = 
                                [
                                    {erl_migrations,{schema_instance_1,schema_name_1},test2},
                                    {erl_migrations,{schema_instance_1,schema_name_2},test4},
                                    {erl_migrations,{schema_instance_2,schema_name_1},test2},
                                    {erl_migrations,{schema_instance_2,schema_name_2},test4}
                                ],
                            MigrationHistory =
                                [   
                                    {erl_migrations_history,1,test1,
                                                            {schema_instance_1,schema_name_1},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,2,test2,
                                                            {schema_instance_1,schema_name_1},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,3,test1,
                                                            {schema_instance_2,schema_name_1},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,4,test2,
                                                            {schema_instance_2,schema_name_1},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,5,test2,
                                                            {schema_instance_1,schema_name_1},
                                                            down,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,6,test2,
                                                            {schema_instance_2,schema_name_1},
                                                            down,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,7,test1,
                                                            {schema_instance_2,schema_name_1},
                                                            down,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,8,test2,
                                                            {schema_instance_1,schema_name_1},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,9,test1,
                                                            {schema_instance_2,schema_name_1},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,10,test2,
                                                            {schema_instance_2,schema_name_1},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,11,test3,
                                                            {schema_instance_1,schema_name_2},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,12,test4,
                                                            {schema_instance_1,schema_name_2},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,13,test3,
                                                            {schema_instance_2,schema_name_2},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}},
                                    {erl_migrations_history,14,test4,
                                                            {schema_instance_2,schema_name_2},
                                                            up,
                                                            {{1994,5,10},{11,11,11}}}
                                ],
                            {atomic, Result1} = 
                                mnesia:transaction(
                                    fun() ->
                                        mnesia:select(erl_migrations, [{'_', [], ['$_']}])
                                    end
                                ),
                            {atomic, Result2} = 
                                mnesia:transaction(
                                    fun() ->
                                        mnesia:select(erl_migrations_history, [{'_', [], ['$_']}])
                                    end
                                ),
                            ?assertEqual(SchemaMigration, lists:usort(Result1)),
                            ?assertEqual(MigrationHistory, lists:usort(Result2))
                        end
                    },
                    {"Test migration run log lifecycle (ok)",
                        fun() ->
                            Args5 = ?ARGS#{schema_instance => schema_instance_3, schema_name => schema_name_3},
                            ?assertEqual({ok, test6, [test5, test6]}, erl_migrate:apply_upgrades(Args5)),
                            Last = erl_migrate:get_last_migration_run(Args5),
                            ?assertEqual(test6, Last#db_migration_runs.migration_name),
                            ?assertEqual(ok, Last#db_migration_runs.status),
                            ?assertEqual(up, Last#db_migration_runs.direction),
                            Runs = erl_migrate:get_run_log(Args5),
                            %% One row per revision attempt (running row is updated in place)
                            ?assertEqual(2, length(Runs)),
                            ?assertEqual(
                                2, length([R || R <- Runs, R#db_migration_runs.status =:= ok])
                            ),
                            ?assertEqual(
                                0, length([R || R <- Runs, R#db_migration_runs.status =:= running])
                            )
                        end
                    },
                    {"Test run log on failure, per-revision head, no re-run of applied revisions",
                        fun() ->
                            Args5 = ?ARGS#{schema_instance => schema_instance_1, schema_name => schema_name_3},
                            application:set_env(erl_migrate, fail_test6, true),
                            ?assertError(test6_fail, erl_migrate:apply_upgrades(Args5)),
                            %% Per-revision head: only test6 remains pending (test5 applied)
                            ?assertEqual(test5, erl_migrate:get_applied_head(Args5)),
                            ?assertEqual([test6], erl_migrate:find_pending_migrations(Args5)),
                            %% Last run row is failed test6
                            Last = erl_migrate:get_last_migration_run(Args5),
                            ?assertEqual(test6, Last#db_migration_runs.migration_name),
                            ?assertEqual(failed, Last#db_migration_runs.status),
                            ?assertEqual({error, test6_fail}, Last#db_migration_runs.error_reason),
                            ?assertNotEqual(undefined, Last#db_migration_runs.stacktrace),
                            %% History has exactly one row: test5 (test6 failed before history write)
                            ?assertEqual(
                                1,
                                length(
                                    mnesia:dirty_match_object(erl_migrations_history,
                                        {erl_migrations_history, '_', '_',
                                            {schema_instance_1, schema_name_3}, '_', '_'})
                                )
                            ),
                            %% Retry: failing revision is re-attempted, applied revision is NOT re-run
                            HistoryCountBeforeRetry =
                                length(
                                    mnesia:dirty_match_object(erl_migrations_history,
                                        {erl_migrations_history, '_', '_',
                                            {schema_instance_1, schema_name_3}, '_', '_'})
                                ),
                            ?assertError(test6_fail, erl_migrate:apply_upgrades(Args5)),
                            ?assertEqual(test5, erl_migrate:get_applied_head(Args5)),
                            ?assertEqual(
                                HistoryCountBeforeRetry,
                                length(
                                    mnesia:dirty_match_object(erl_migrations_history,
                                        {erl_migrations_history, '_', '_',
                                            {schema_instance_1, schema_name_3}, '_', '_'})
                                )
                            ),
                            %% Fix the failure and continue from exactly test6
                            application:set_env(erl_migrate, fail_test6, false),
                            ?assertEqual({ok, test6, [test6]}, erl_migrate:apply_upgrades(Args5)),
                            ?assertEqual(test6, erl_migrate:get_applied_head(Args5)),
                            application:unset_env(erl_migrate, fail_test6)
                        end
                    },
                    {"Test observer callbacks",
                        fun() ->
                            dummy_observer:init(),
                            ArgsO = ?ARGS#{
                                schema_instance => schema_instance_2,
                                schema_name => schema_name_3,
                                run_log_observer => dummy_observer
                            },
                            ?assertEqual({ok, test6, [test5, test6]}, erl_migrate:apply_upgrades(ArgsO)),
                            ?assertEqual(
                                2,
                                length(
                                    ets:match_object(dummy_observer_calls,
                                        {start, schema_name_3, schema_instance_2, '_'})
                                )
                            ),
                            ?assertEqual(
                                1,
                                length(
                                    ets:match_object(dummy_observer_calls,
                                        {start, schema_name_3, schema_instance_2, test5})
                                )
                            ),
                            ?assertEqual(
                                1,
                                length(
                                    ets:match_object(dummy_observer_calls,
                                        {ok, schema_name_3, schema_instance_2, test5, '_'})
                                )
                            ),
                            ?assertEqual(
                                1,
                                length(
                                    ets:match_object(dummy_observer_calls,
                                        {ok, schema_name_3, schema_instance_2, test6, '_'})
                                )
                            ),
                            ?assertEqual(
                                1,
                                length(
                                    ets:match_object(dummy_observer_calls,
                                        {run_finished, schema_name_3, schema_instance_2, {ok, test6,
                                            [test5, test6]}})
                                )
                            ),
                            ets:delete_all_objects(dummy_observer_calls)
                        end
                    },
                    {"Test downgrade run log",
                        fun() ->
                            Args5 = ?ARGS#{schema_instance => schema_instance_3, schema_name => schema_name_3},
                            ?assertEqual({ok, test5, [test6]}, erl_migrate:apply_downgrades(Args5, 1)),
                            Last = erl_migrate:get_last_migration_run(Args5),
                            ?assertEqual(down, Last#db_migration_runs.direction),
                            ?assertEqual(ok, Last#db_migration_runs.status)
                        end
                    }
                ]
            }
        ]
    }.

%% Internal functions

start_mnesia() ->
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    {ok, [mnesia]} = application:ensure_all_started(mnesia),
    application:ensure_all_started(erl_migrate),
    EbinDir = code:lib_dir(erl_migrate) ++ "/test/",
    application:set_env(erl_migrate, migration_beam_dir, EbinDir).

stop_mnesia() ->
    application:stop(erl_migrate),
    application:stop(mnesia),
    mnesia:delete_schema([node()]).
