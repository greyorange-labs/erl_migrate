-module(erl_migrate_tests).

-include_lib("eunit/include/eunit.hrl").

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
        }
	].

migration_test_() ->
    {
        setup,
        fun() ->
            start_mnesia(),
            meck:expect(erl_migrate, get_current_time,
                fun() ->
                    {{1994,5,10},{11,11,11}}
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
                            Args1 = Args = ?ARGS#{schema_instance => schema_instance_1, schema_name => schema_name_1},
                            Args2 = Args = ?ARGS#{schema_instance => schema_instance_2, schema_name => schema_name_1},
                            Args3 = Args = ?ARGS#{schema_instance => schema_instance_1, schema_name => schema_name_2},
                            Args4 = Args = ?ARGS#{schema_instance => schema_instance_2, schema_name => schema_name_2},
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
