-module(go_migrate_tests).

-include_lib("eunit/include/eunit.hrl").

create_migration_file_test_() ->
	[
        %{require, rm, unix},
	    {"check revision file creation",
            fun () ->
                {ok, Dir} = file:get_cwd(),
                code:add_path(Dir),
                %io:format("path: ~p~n", [Dir]),
                Filename = go_migrate:create_migration_file(
                    #{migration_src_files_dir_path => "src/migrations/"}),
                ?assertEqual(true, filelib:is_file(Filename)),
                ?assertCmd("rm " ++ Filename)
                %io:format("file: ~p~n", [Filename])
            end
        },
        {"check revision file creation and deletion",
            fun () ->
                {ok, Dir} = file:get_cwd(),
                code:add_path(Dir),
                Filename = go_migrate:create_migration_file(
                    #{migration_src_files_dir_path => "src/migrations/"}),
                ?assertCmd("rm " ++ Filename)
            end
        }
	].

mnesia_migration_test_() ->
    {
        setup,
        fun() ->
            start_mnesia()
        end,
        fun(_) ->
            % mnesia:transaction(
            %     fun() -> 
            %         mnesia:delete_table(go_schema_migrations)
            %     end
            % ),
            stop_mnesia()
        end,
        [
            {
                inorder,
                [
                    {"Init Migrations should not crash",
                        ?_assertEqual(ok, go_migrate:create_go_schema_migrations_table())
                    },
                    {
                        "Test migration calculation functions",
                        fun() ->
                            Options =
                            #{
                                app_name => butler_server,
                                app_db_name => mnesia,
                                migration_src_files_dir_path => "src/migrations/",
                                migration_beam_files_dir_path => application:get_env(go_migrate, migration_beam_dir, "ebin")
                            },
                            [
                                {inparallel,
                                    [
                                        ?_assertEqual(none, go_migrate:get_applied_head(Options)),
                                        ?_assertEqual(test1, go_migrate:get_base_revision(Options)),
                                        ?_assertEqual(test2, go_migrate:get_next_revision(test1, Options)),
                                        ?_assertEqual(test1, go_migrate:get_prev_revision(test2, Options)),
                                        ?_assertEqual([test1, test2],
                                                    go_migrate:find_pending_migrations(Options)),
                                        ?_assertEqual(1, go_migrate:get_count_between_2_revisions(
                                                        test1, test2, Options))
                                    ]
                                }
                            ]
                        end
                    },
                    {"Test apply upgrades",
                        fun() ->
                            Options =
                            #{
                                app_name => butler_server,
                                app_db_name => mnesia,
                                migration_src_files_dir_path => "src/migrations/",
                                migration_beam_files_dir_path => application:get_env(go_migrate, migration_beam_dir, "ebin")
                            },
                            [
                                ?_assertEqual({ok, applied}, go_migrate:apply_upgrades(Options)),
                                ?_assertEqual(test2, go_migrate:get_applied_head(Options))
                            ]
                        end
                    },
                    {"Test apply downgrades",
                        fun() ->
                            Options =
                            #{
                                app_name => butler_server,
                                app_db_name => mnesia,
                                migration_src_files_dir_path => "src/migrations/",
                                migration_beam_files_dir_path => application:get_env(go_migrate, migration_beam_dir, "ebin")
                            },
                            [
                                ?_assertEqual({error, wrong_number},
                                            go_migrate:apply_downgrades(5, Options)),
                                ?_assertEqual(ok, go_migrate:apply_downgrades(1, Options)),
                                ?_assertEqual(test1, go_migrate:get_applied_head(Options))
                            ]
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
    application:ensure_all_started(go_migrate),
    EbinDir = code:lib_dir(go_migrate) ++ "/test/",
    application:set_env(go_migrate, migration_beam_dir, EbinDir).

stop_mnesia() ->
    application:stop(go_migrate),
    application:stop(mnesia),
    mnesia:delete_schema([node()]).
