-module(erl_migrate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ARGS,
    #{
        schema_name => schema_name,
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
            start_mnesia()
        end,
        fun(_) ->
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
                            ?assertEqual(none, erl_migrate:get_applied_head(?ARGS)),
                            ?assertEqual(test1, erl_migrate:get_base_revision(?ARGS)),
                            ?assertEqual(test2, erl_migrate:get_next_revision(test1, ?ARGS)),
                            ?assertEqual([test1, test2], erl_migrate:find_pending_migrations(?ARGS))
                        end
                    },
                    {"Test apply upgrades",
                        fun() ->
                            ?assertEqual({ok, test2, [test1, test2]}, erl_migrate:apply_upgrades(?ARGS)),
                            ?assertEqual({ok, test2, []}, erl_migrate:apply_upgrades(?ARGS)),
                            ?assertEqual(test2, erl_migrate:get_applied_head(?ARGS)),
                            ?assertEqual({ok, test2, [test1, test2]}, erl_migrate:apply_upgrades(maps:put(schema_instance, schema_instance_2, ?ARGS))),
                            ?assertEqual(test2, erl_migrate:get_applied_head(maps:put(schema_instance, schema_instance_2, ?ARGS)))
                        end
                    },
                    {"Test apply downgrades",
                        fun() ->
                            ?assertEqual({ok, test1, [test2]}, erl_migrate:apply_downgrades(?ARGS, 2)),
                            ?assertEqual(test1, erl_migrate:get_applied_head(?ARGS)),
                            Args2 =
                                ?ARGS#{
                                    schema_instance => schema_instance_2,
                                    down_base_migration => true
                                },
                            ?assertEqual({ok, none, [test2, test1]}, erl_migrate:apply_downgrades(Args2, 2)),
                            ?assertEqual(none, erl_migrate:get_applied_head(Args2))
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
