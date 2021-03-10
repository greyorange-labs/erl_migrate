-module(erl_migrate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ARGS,
    #{
        schema_name => {butler_server, mnesia},
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

mnesia_migration_test_() ->
    {
        setup,
        fun() ->
            start_mnesia()
        end,
        fun(_) ->
            % mnesia:transaction(
            %     fun() -> 
            %         mnesia:delete_table(go_erl_migrations)
            %     end
            % ),
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
                            [
                                {inparallel,
                                    [
                                        ?_assertEqual(none, erl_migrate:get_applied_head(?ARGS)),
                                        ?_assertEqual(test1, erl_migrate:get_base_revision(?ARGS)),
                                        ?_assertEqual(test2, erl_migrate:get_next_revision(test1, ?ARGS)),
                                        ?_assertEqual([test1, test2],
                                                    erl_migrate:find_pending_migrations(?ARGS))
                                    ]
                                }
                            ]
                        end
                    },
                    {"Test apply upgrades",
                        fun() ->
                            [
                                ?_assertEqual({ok, applied}, erl_migrate:apply_upgrades(?ARGS)),
                                ?_assertEqual(test2, erl_migrate:get_applied_head(?ARGS))
                            ]
                        end
                    },
                    {"Test apply downgrades",
                        fun() ->
                            [
                                ?_assertEqual({error, wrong_number},
                                            erl_migrate:apply_downgrades(5, ?ARGS)),
                                ?_assertEqual(ok, erl_migrate:apply_downgrades(1, ?ARGS)),
                                ?_assertEqual(test1, erl_migrate:get_applied_head(?ARGS))
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
    application:ensure_all_started(erl_migrate),
    EbinDir = code:lib_dir(erl_migrate) ++ "/test/",
    application:set_env(erl_migrate, migration_beam_dir, EbinDir).

stop_mnesia() ->
    application:stop(erl_migrate),
    application:stop(mnesia),
    mnesia:delete_schema([node()]).
