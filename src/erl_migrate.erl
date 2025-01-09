-module(erl_migrate).
-compile(export_all).
-compile(nowarn_export_all).

-define(TABLE_1, erl_migrations).
-define(TABLE_2, erl_migrations_history).

-record(erl_migrations, {
    id :: {SchemaInstance :: any(), SchemaName :: any()},
    current_head = none :: any()
}).

-record(erl_migrations_history, {
    operation_id :: integer(),
    migration_name :: atom(),
    id :: {SchemaInstance :: any(), SchemaName :: any()},
    operation :: up | down,
    timestamp :: calendar:local_time()
}).

get_current_time() ->
    calendar:local_time().

start_mnesia() ->
    mnesia:start().

init_db_tables() ->
    case lists:member(?TABLE_1, mnesia:system_info(tables)) of
        true ->
            ok;
        false ->
            print("Table ~p not found, creating...~n", [?TABLE_1]),
            Attr1 =
                [
                    {disc_copies, [node()]},
                    {attributes, record_info(fields, erl_migrations)}
                ],
            case mnesia:create_table(?TABLE_1, Attr1) of
                {atomic, ok} ->
                    print(" => created~n", []);
                {aborted, Reason1} ->
                    print("mnesia create table error: ~p~n", [Reason1]),
                    throw({error, Reason1})
            end
    end,
    case lists:member(?TABLE_2, mnesia:system_info(tables)) of
        true ->
            ok;
        false ->
            print("Table ~p not found, creating...~n", [?TABLE_2]),
            Attr2 =
                [
                    {disc_copies, [node()]},
                    {attributes, record_info(fields, erl_migrations_history)}
                ],
            case mnesia:create_table(?TABLE_2, Attr2) of
                {atomic, ok} ->
                    print(" => created~n", []);
                {aborted, Reason2} ->
                    print("mnesia create table error: ~p~n", [Reason2]),
                    throw({error, Reason2})
            end
    end,
    TimeOut = application:get_env(erl_migrate, table_load_timeout, 10000),
    ok = mnesia:wait_for_tables([?TABLE_1, ?TABLE_2], TimeOut).

%%
%%Functions related to migration info
%%

-spec get_base_revision(
    Args :: maps:map()
) -> BaseRev :: none | atom().
get_base_revision(Args) ->
    BeamFilesPath = get_migration_beam_filepath(Args),
    SchemaName = maps:get(schema_name, Args),
    Modulelist = filelib:wildcard(BeamFilesPath ++ "*_erl_migration.beam"),
    Res =
        lists:filter(
            fun(Filename) ->
                Modulename = list_to_atom(filename:basename(Filename, ".beam")),
                case is_erl_migration_module(Modulename, SchemaName) of
                    true -> Modulename:get_prev_rev() =:= none;
                    false -> false
                end
            end,
            Modulelist
        ),
    case Res of
        [] ->
            print("No Base Rev module, Args = ~p~n ", [Args]),
            none;
        _ ->
            BaseModuleName = list_to_atom(filename:basename(Res, ".beam")),
            print("Base Rev module is ~p~n", [BaseModuleName]),
            BaseModuleName:get_current_rev()
    end.

-spec get_current_head(
    Args :: maps:map()
) -> CurrentHead :: none | atom().
get_current_head(Args) ->
    BaseRev = get_base_revision(Args),
    get_current_head(BaseRev, Args).

-spec get_next_revision(
    RevId :: atom(),
    Args :: maps:map()
) -> [] | atom().
get_next_revision(RevId, Args) ->
    SchemaName = maps:get(schema_name, Args),
    BeamFilesPath = get_migration_beam_filepath(Args),
    Modulelist = filelib:wildcard(BeamFilesPath ++ "*_erl_migration.beam"),
    Res =
        lists:filter(
            fun(Filename) ->
                Modulename = list_to_atom(filename:basename(Filename, ".beam")),
                case is_erl_migration_module(Modulename, SchemaName) of
                    true -> Modulename:get_prev_rev() =:= RevId;
                    false -> false
                end
            end,
            Modulelist
        ),
    case Res of
        [] ->
            [];
        _ ->
            ModuleName = list_to_atom(filename:basename(Res, ".beam")),
            ModuleName:get_current_rev()
    end.

-spec get_current_head(
    RevId :: atom(),
    Args :: maps:map()
) -> none | atom().
get_current_head(RevId, Args) ->
    case get_next_revision(RevId, Args) of
        [] ->
            RevId;
        NextRevId ->
            get_current_head(NextRevId, Args)
    end.

-spec get_revision_tree(
    Args :: maps:map()
) -> RevList :: list().
get_revision_tree(Args) ->
    BaseRev = get_base_revision(Args),
    List1 = [],
    RevList = append_revision_tree(List1, BaseRev, Args),
    print("RevList ~p~n", [RevList]),
    RevList.

-spec get_applied_head(
    Args :: maps:map()
) ->
    AppliedHead :: atom().
get_applied_head(Args) ->
    SchemaName = maps:get(schema_name, Args),
    SchemaInstance = maps:get(schema_instance, Args),
    Id = {SchemaInstance, SchemaName},
    {atomic, KeyList} =
        mnesia:transaction(
            fun() ->
                mnesia:read(?TABLE_1, Id)
            end
        ),
    Head =
        case KeyList of
            [] ->
                none;
            [Rec | _Empty] ->
                Rec#erl_migrations.current_head
        end,
    print("current applied head is : ~p~n", [Head]),
    Head.

-spec find_pending_migrations(
    Args :: maps:map()
) -> RevList :: list().
find_pending_migrations(Args) ->
    RevList =
        case get_applied_head(Args) of
            none ->
                case get_base_revision(Args) of
                    none -> [];
                    BaseRevId -> append_revision_tree([], BaseRevId, Args)
                end;
            Id ->
                case get_next_revision(Id, Args) of
                    [] -> [];
                    NextId -> append_revision_tree([], NextId, Args)
                end
        end,
    print("Revisions needing migration : ~p~n", [RevList]),
    RevList.

%%
%% Functions related to migration creation
%%
-spec create_migration_file(
    Args :: maps:map()
) -> Path :: string().
create_migration_file(Args) ->
    erlydtl:compile(code:lib_dir(erl_migrate) ++ "/schema.template", migration_template),
    NewRevisionId = "a" ++ string:substr(uuid:to_string(uuid:uuid4()), 1, 8),
    OldRevisionId = get_current_head(Args),
    SchemaName = maps:get(schema_name, Args),
    Filename = NewRevisionId ++ "_erl_migration",
    {ok, Data} = migration_template:render(
        [
            {new_rev_id, NewRevisionId},
            {old_rev_id, OldRevisionId},
            {modulename, Filename},
            {tabtomig, []},
            {schema_name, SchemaName}
        ]
    ),
    SrcFilesPath = get_migration_source_filepath(Args),
    FilePath = SrcFilesPath ++ Filename ++ ".erl",
    ok = file:write_file(FilePath, Data),
    io:format("Migration file created at ~ts~n", [FilePath]).

%%
%% Functions related to applying migrations
%%

-spec apply_upgrades(
    Args :: maps:map()
) ->
    Output :: {ok, NewHead :: atom(), RevList :: list()}.
apply_upgrades(Args) ->
    ok = init_db_tables(),
    RevList = find_pending_migrations(Args),
    CurrHead = get_current_head(Args),
    case RevList of
        [] ->
            print("No pending revision found ~n", []),
            {ok, CurrHead, RevList};
        _ ->
            NewHead =
                lists:foldl(
                    fun(RevId, _Acc) ->
                        ModuleName = list_to_atom(atom_to_list(RevId) ++ "_erl_migration"),
                        print("ModuleName = ~p, RevId = ~p~n", [ModuleName, RevId]),
                        print("Running upgrade ~p -> ~p ~n", [
                            ModuleName:get_prev_rev(), ModuleName:get_current_rev()
                        ]),
                        ModuleName:up(),
                        update_history(RevId, Args, up),
                        RevId
                    end,
                    CurrHead,
                    RevList
                ),
            update_head(NewHead, Args),
            print("all upgrades successfully applied.~n", []),
            {ok, NewHead, RevList}
    end.

-spec apply_downgrades(
    Args :: maps:map(),
    DownNum :: integer()
) ->
    Output :: {ok, NewHead :: atom(), RevList :: list()}.
apply_downgrades(Args, DownNum) ->
    CurrHead = get_applied_head(Args),
    {NewHead, DownRevList} = downgrade(CurrHead, Args, DownNum, []),
    update_head(NewHead, Args),
    print("all downgrades successfully applied.~n", []),
    {ok, NewHead, DownRevList}.

-spec downgrade(
    CurrHead :: none | atom(),
    Args :: maps:map(),
    DownNum :: integer(),
    DownRevList :: list()
) ->
    Output :: {NewHead :: none | atom(), NewDownRevList :: list()}.
downgrade(CurrHead, _Args, 0, DownRevList) ->
    {CurrHead, DownRevList};
downgrade(none, _Args, _DownNum, NewDownRevList) ->
    {none, NewDownRevList};
downgrade(CurrHead, Args, DownNum, NewDownRevList) ->
    ModuleName = list_to_atom(atom_to_list(CurrHead) ++ "_erl_migration"),
    ParentHead = ModuleName:get_prev_rev(),
    CurrentHead = ModuleName:get_current_rev(),
    DownGradeBaseRevision = maps:get(down_base_migration, Args, false),
    if
        ParentHead == none andalso (not DownGradeBaseRevision) ->
            print("Not downgrading base revision ~p ~n", [CurrentHead]),
            {CurrentHead, NewDownRevList};
        true ->
            print("Running downgrade ~p -> ~p ~n", [CurrentHead, ParentHead]),
            ModuleName:down(),
            update_history(CurrHead, Args, down),
            downgrade(ModuleName:get_prev_rev(), Args, DownNum - 1, NewDownRevList ++ [CurrHead])
    end.

-spec append_revision_tree(
    List1 :: list(),
    RevId :: atom(),
    Args :: maps:map()
) -> List2 :: list().
append_revision_tree(List1, RevId, Args) ->
    case get_next_revision(RevId, Args) of
        [] ->
            List1 ++ [RevId];
        NewRevId ->
            List2 = List1 ++ [RevId],
            append_revision_tree(List2, NewRevId, Args)
    end.

-spec update_head(
    Head :: atom(),
    Args :: maps:map()
) -> any().
update_head(Head, Args) ->
    SchemaName = maps:get(schema_name, Args),
    SchemaInstance = maps:get(schema_instance, Args),
    Id = {SchemaInstance, SchemaName},
    mnesia:transaction(
        fun() ->
            case mnesia:wread({?TABLE_1, Id}) of
                [] ->
                    mnesia:write(?TABLE_1, #erl_migrations{id = Id, current_head = Head}, write);
                [CurrRec] ->
                    mnesia:write(CurrRec#erl_migrations{current_head = Head})
            end
        end
    ).

-spec update_history(
    RevId :: atom(),
    Args :: maps:map(),
    Operation :: up | down
) -> any().
update_history(RevId, Args, Operation) ->
    SchemaName = maps:get(schema_name, Args),
    SchemaInstance = maps:get(schema_instance, Args),
    Id = {SchemaInstance, SchemaName},
    mnesia:transaction(
        fun() ->
            OperationId =
                case mnesia:all_keys(?TABLE_2) of
                    [] -> 1;
                    CurrentOperations -> lists:max(CurrentOperations) + 1
                end,
            NewRecord =
                #erl_migrations_history{
                    operation_id = OperationId,
                    migration_name = RevId,
                    id = Id,
                    operation = Operation,
                    timestamp = erl_migrate:get_current_time()
                },
            mnesia:write(?TABLE_2, NewRecord, write)
        end
    ).

%%
%% helper functions
%%

-spec is_erl_migration_module(
    Modulename :: atom(),
    SchemaName :: any()
) -> boolean().
is_erl_migration_module(Modulename, SchemaName) ->
    case catch Modulename:module_info(attributes) of
        {'EXIT', {undef, _}} ->
            false;
        Attributes ->
            Cond1 =
                case lists:keyfind(behaviour, 1, Attributes) of
                    {behaviour, BehaviourList} ->
                        lists:member(erl_migration, BehaviourList);
                    false ->
                        false
                end,
            Cond2 =
                case lists:keyfind(schema_name, 1, Attributes) of
                    {schema_name, SchemaNameList} ->
                        lists:member(SchemaName, SchemaNameList);
                    false ->
                        false
                end,
            Cond1 andalso Cond2
    end.

-spec get_migration_source_filepath(
    Args :: maps:map()
) -> Path :: string().
get_migration_source_filepath(Args) ->
    case maps:get(migration_src_files_dir_path, Args, undefined) of
        undefined ->
            Val = application:get_env(erl_migrate, migration_source_dir, "src/migrations/"),
            ok = filelib:ensure_dir(Val),
            Val;
        SrcFilesPath ->
            ok = filelib:ensure_dir(SrcFilesPath),
            SrcFilesPath
    end.

-spec get_migration_beam_filepath(
    Args :: maps:map()
) -> Path :: string().
get_migration_beam_filepath(Args) ->
    Path =
        case maps:get(migration_beam_files_dir_path, Args, undefined) of
            undefined ->
                {ok, AppName} = application:get_application(),
                code:lib_dir(AppName, ebin) ++ "/";
            InputPath ->
                InputPath
        end,
    ok = filelib:ensure_dir(Path),
    Path.

print(Statement, Arg) ->
    case application:get_env(erl_migrate, verbose, false) of
        true -> io:format(Statement, Arg);
        false -> ok
    end.

detect_revision_sequence_conflicts(Args) ->
    SchemaName = maps:get(schema_name, Args, undefined),
    Tree = get_revision_tree(Args),
    Modulelist = filelib:wildcard(get_migration_beam_filepath(Args) ++ "*_erl_migration.beam"),
    ConflictId =
        lists:filter(
            fun(RevId) ->
                Res =
                    lists:filter(
                        fun(Filename) ->
                            Modulename = list_to_atom(filename:basename(Filename, ".beam")),
                            case is_erl_migration_module(Modulename, SchemaName) of
                                true -> Modulename:get_prev_rev() =:= RevId;
                                false -> false
                            end
                        end,
                        Modulelist
                    ),
                case length(Res) > 1 of
                    true ->
                        print("Conflict detected at revision id ~p~n", [RevId]),
                        true;
                    false ->
                        false
                end
            end,
            Tree
        ),
    ConflictId.

print_history(SchemaName) ->
    Data = mnesia:dirty_select(erl_migrations_history, [{'_', [], ['$_']}]),
    Header = ["OperationId", "Timestamp", "SchemaInstance", "OperationType", "MigrationName"],
    ok = print_row(Header),
    DashList = lists:duplicate(5, "---------------------"),
    ok = print_row(DashList),
    %% Print each row
    lists:foreach(
        fun(
            #erl_migrations_history{
                operation_id = OperationId,
                migration_name = MigrationName,
                id = {SchemaInstance, SchemaName1},
                operation = OperationType,
                timestamp = {{Year, Month, Day}, {Hour, Min, Sec}}
            }
        ) ->
            case SchemaName1 of
                SchemaName ->
                    print_row(
                        [
                            integer_to_list(OperationId),
                            io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [
                                Year, Month, Day, Hour, Min, Sec
                            ]),
                            io_lib:format("~p", [SchemaInstance]),
                            io_lib:format("~p", [OperationType]),
                            io_lib:format("~p", [MigrationName])
                        ]
                    );
                _ ->
                    ok
            end
        end,
        lists:sort(Data)
    ).

print_row(RowData) ->
    lists:foreach(fun(V) -> io:format("~-*s | ", [30, V]) end, RowData),
    io:format("~n").

get_dangling_migrations() ->
    %% TODO: implement this function
    [].
