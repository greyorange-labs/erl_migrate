%% @author Gaurav Kumar <gauravkumar552@gmail.com>

-module(go_db_migration).

-export(
    [
        start_mnesia/0,
        create_go_schema_migrations_table/0,
        get_base_revision/1,
        get_current_head/1,
        create_migration_file/1,
        find_pending_migrations/1,
        apply_upgrades/1,
        get_revision_tree/1,
        get_applied_head/1,
        apply_downgrades/2,
        detect_revision_sequence_conflicts/1
	]
).

-ifdef(TEST).
-export(
    [
        get_migration_source_filepath/1,
        get_migration_beam_filepath/1,
        get_next_revision/2,
        get_prev_revision/2,
        get_count_between_2_revisions/3
	]
).
-endif.

-define(TABLE, go_schema_migrations).

-record(go_schema_migrations, 
    {
        prime_key               :: {AppName :: atom(), AppDBName :: atom()},
        current_head = null     :: any()
    }
).


start_mnesia() ->
	mnesia:start().

create_go_schema_migrations_table() ->
    case lists:member(?TABLE, mnesia:system_info(tables)) of
        true ->
            ok;
        false ->
            print("Table schema_migration not found, creating...~n", []),
            Attr = 
                [
                    {disc_copies, [node()]}, 
                    {attributes, record_info(fields, go_schema_migrations)}
                ],
            case mnesia:create_table(?TABLE, Attr) of
                {atomic, ok} -> 
                    print(" => created~n", []);
                {aborted, Reason} -> 
                    print("mnesia create table error: ~p~n", [Reason]),
				    throw({error, Reason})
            end
    end,
    TimeOut = application:get_env(go_migrate, table_load_timeout, 10000),
    ok = mnesia:wait_for_tables([?TABLE], TimeOut).


%%
%%Functions related to migration info
%%

get_base_revision(Options) ->
    BeamFilesPath = get_migration_beam_filepath(Options),
    Modulelist = filelib:wildcard(BeamFilesPath ++ "*_migration.beam"),
    Res = 
        lists:filter(
            fun(Filename) ->
                Modulename = list_to_atom(filename:basename(Filename, ".beam")),
                case has_migration_behaviour(Modulename) of
	                true -> Modulename:get_prev_rev() =:= none;
	                false -> false
	            end
            end,
            Modulelist
        ),
    case Res of
        [] -> 
            print("No Base Rev module, Options = ~p~n ", [Options]),
            none;
	    _ -> 
            BaseModuleName = list_to_atom(filename:basename(Res, ".beam")),
            print("Base Rev module is ~p~n", [BaseModuleName]),
            BaseModuleName:get_current_rev()
    end.

get_current_head(Options) ->
    BaseRev = get_base_revision(Options),
    get_current_head(BaseRev, Options).

get_revision_tree(Options) ->
    BaseRev = get_base_revision(Options),
    List1 = [],
    RevList = append_revision_tree(List1, BaseRev, Options),
    print("RevList ~p~n", [RevList]),
    RevList.

get_down_revision_tree(Options) ->
    BaseRev = get_applied_head(Options),
    List1 = [],
    RevList = append_down_revision_tree(List1, BaseRev, Options),
    print("RevList ~p~n", [RevList]),
    RevList.

find_pending_migrations(Options) ->
   RevList = 
        case get_applied_head(Options) of
            none -> 
                case get_base_revision(Options) of
                    none -> [] ;
                    BaseRevId -> append_revision_tree([], BaseRevId, Options)
			     end;
            Id -> 
                case get_next_revision(Id, Options) of
			       [] -> [] ;
			       NextId -> append_revision_tree([], NextId, Options)
			   end
        end,
   print("Revisions needing migration : ~p~n", [RevList]),
   RevList.

%%
%% Functions related to migration creation
%%
create_migration_file(Options) ->
    erlydtl:compile(code:lib_dir(go_migrate) ++ "/schema.template", migration_template),
    NewRevisionId = "a" ++ string:substr(uuid:to_string(uuid:uuid4()),1,8),
    OldRevisionId = get_current_head(Options),
    Filename = NewRevisionId ++ "_migration" ,
    {ok, Data} = migration_template:render([{new_rev_id , NewRevisionId}, {old_rev_id, OldRevisionId},
					  {modulename, Filename}, {tabtomig, []},
					  {commitmessage, "migration"}]),
    SrcFilesPath = get_migration_source_filepath(Options),
    file:write_file(SrcFilesPath ++ Filename ++ ".erl", Data),
    print("New file created ~p~n", [Filename ++ ".erl"]),
    SrcFilesPath ++ Filename ++ ".erl".

%%
%% Functions related to applying migrations
%%

apply_upgrades(Options) ->
    RevList = find_pending_migrations(Options),
    CurrHead = get_current_head(Options),
    case RevList of
        [] -> 
            print("No pending revision found ~n", []),
            {ok, CurrHead};
        _ ->
            NewHead =
                lists:foldl(
                    fun(RevId, _Acc) -> 
                        ModuleName = list_to_atom(atom_to_list(RevId) ++ "_migration") ,
                        print("Running upgrade ~p -> ~p ~n",[ModuleName:get_prev_rev(), ModuleName:get_current_rev()]),
                        ModuleName:up(),
                        RevId
                    end,
                    CurrHead, RevList
                ),
            update_head(NewHead, Options),
            print("all upgrades successfully applied.~n", []),
            {ok, NewHead}
    end.

apply_downgrades(Options, DownNum) ->
    CurrHead = get_applied_head(Options),
    Count = get_count_between_2_revisions(get_base_revision(Options), CurrHead, Options),
    case DownNum =< Count of
        false -> 
            print("Wrong number for downgrade ~n", []),
            {error, wrong_number};
        true -> 
            RevList = get_down_revision_tree(Options),
            SubList = lists:sublist(RevList, 1, DownNum),
            case SubList of
                [] -> 
                    print("No down revision found ~n", []),
                    {ok, CurrHead};
                _ ->
                    NewHead =
                        lists:foldl(
                            fun(RevId, _Acc) -> 
                                ModuleName = list_to_atom(atom_to_list(RevId) ++ "_migration") ,
                                print("Running downgrade ~p -> ~p ~n",[ModuleName:get_current_rev(), ModuleName:get_prev_rev()]),
                                ModuleName:down(),
                                ModuleName:get_prev_rev()
                            end,
                            CurrHead, SubList
                        ),                    
                    update_head(NewHead, Options),
                    print("all downgrades successfully applied.~n", []),
                    {ok, NewHead}
            end
    end.

append_revision_tree(List1, RevId, Options) ->
    case get_next_revision(RevId, Options) of
        [] -> 
            List1 ++ [RevId];
	    NewRevId ->
		    List2 = List1 ++ [RevId],
	        append_revision_tree(List2, NewRevId, Options)
    end.

append_down_revision_tree(List1, RevId, Options) ->
    case get_prev_revision(RevId, Options) of
        [] -> 
            List1 ++ [RevId];
	    NewRevId ->
		    List2 = List1 ++ [RevId],
	        append_down_revision_tree(List2, NewRevId, Options)
    end.

get_applied_head(Options) ->
    AppName = maps:get(app_name, Options, undefined),
    AppDBName = maps:get(app_db_name, Options, undefined),
    {atomic, KeyList} = mnesia:transaction(fun() -> mnesia:read(?TABLE, {AppName, AppDBName}) end),
    Head = 
        case KeyList of
            [] -> 
                none;
            [Rec | _Empty] ->
               Rec#go_schema_migrations.current_head
           end,
    print("current applied head is : ~p~n", [Head]),
    Head.

update_head(Head, Options) ->
    AppName = maps:get(app_name, Options, undefined),
    AppDBName = maps:get(app_db_name, Options, undefined),
    mnesia:transaction(
        fun() ->
            case mnesia:wread({?TABLE, {AppName, AppDBName}}) of
                [] ->
                    mnesia:write(?TABLE, #go_schema_migrations{prime_key = {AppName, AppDBName}, current_head = Head}, write);
                [CurrRec] ->
                    mnesia:write(CurrRec#go_schema_migrations{current_head = Head})
            end
        end
    ).

% %%
% %% Post migration validations
% %%

% -spec detect_conflicts_post_migration([{tuple(), list()}]) -> list().
% detect_conflicts_post_migration(Models) ->
%     ConflictingTables = [TableName || {TableName, Options} <- Models , proplists:get_value(attributes, Options) /= mnesia:table_info(TableName, attributes)],
%     print("Tables having conflicts in structure after applying migrations: ~p~n", [ConflictingTables]),
%     ConflictingTables.


%%
%% helper functions
%%

has_migration_behaviour(Modulename) ->
    case catch Modulename:module_info(attributes) of
        {'EXIT', {undef, _}} ->
            false;
        Attributes ->
            case lists:keyfind(behaviour, 1, Attributes) of
                {behaviour, BehaviourList} ->
                    lists:member(migration, BehaviourList);
                false ->
                    false
            end
    end.

get_prev_revision(RevId, Options) ->
    CurrModuleName = list_to_atom(atom_to_list(RevId) ++ "_migration"),
    Modulelist = filelib:wildcard(get_migration_beam_filepath(Options) ++ "*_migration.beam"),
    Res = 
        lists:filter(
            fun(Filename) ->
                Modulename = list_to_atom(filename:basename(Filename, ".beam")),
                case has_migration_behaviour(Modulename) of
	                true -> Modulename:get_current_rev() =:= CurrModuleName:get_prev_rev();
	                false -> false
	            end
            end,
            Modulelist
        ),
    case Res of
        [] -> [];
        _ -> 
            ModuleName = list_to_atom(filename:basename(Res, ".beam")), 
            ModuleName:get_current_rev()
    end.

get_next_revision(RevId, Options) ->
    Modulelist = filelib:wildcard(get_migration_beam_filepath(Options) ++ "*_migration.beam"),
    Res = 
        lists:filter(
            fun(Filename) ->
                Modulename = list_to_atom(filename:basename(Filename, ".beam")),
                case has_migration_behaviour(Modulename) of
                    true -> Modulename:get_prev_rev() =:= RevId;
                    false -> false
                end
            end,
            Modulelist
        ),
    case Res of
        [] -> [];
        _ -> 
            ModuleName = list_to_atom(filename:basename(Res, ".beam")), 
            ModuleName:get_current_rev()
    end.

get_current_head(RevId, Options) ->
    case get_next_revision(RevId, Options) of
	    [] -> 
            RevId ;
        NextRevId -> 
            get_current_head(NextRevId, Options)
    end.

get_migration_source_filepath(Options) ->
    case maps:get(migration_src_files_dir_path, Options, undefined) of
        undefined ->
            Val = application:get_env(go_migrate, migration_source_dir, "src/migrations/"),
            ok = filelib:ensure_dir(Val),
            Val;
        SrcFilesPath ->
            SrcFilesPath
    end.

get_migration_beam_filepath(Options) ->
    case maps:get(migration_beam_files_dir_path, Options, undefined) of
        undefined ->
            Val = application:get_env(go_migrate, migration_beam_dir, "ebin/"),
            ok = filelib:ensure_dir(Val),
            Val;
        BeamFilesPath ->
            BeamFilesPath
    end.

get_count_between_2_revisions(RevStart, RevEnd, Options) ->
    RevList = get_revision_tree(Options),
    Count = string:str(RevList, [RevEnd])  - string:str(RevList, [RevStart]),
    Count.

print(Statement, Arg) ->
    case application:get_env(go_migrate, verbose, false) of
        true -> io:format(Statement, Arg);
        false -> ok
    end.

detect_revision_sequence_conflicts(Options) ->
    Tree = get_revision_tree(Options),
    Modulelist = filelib:wildcard(get_migration_beam_filepath(Options) ++ "*_migration.beam"),
    ConflictId = 
        lists:filter(
            fun(RevId) ->
                Res = 
                    lists:filter(
                        fun(Filename) ->
                            Modulename = list_to_atom(filename:basename(Filename, ".beam")),
                            case has_migration_behaviour(Modulename) of
                                true -> Modulename:get_prev_rev() =:= RevId;
                                false -> false
                            end
                        end,
                        Modulelist
                    ),
                case length(Res) > 1 of
                    true -> print("Conflict detected at revision id ~p~n", [RevId]),
                            true ;
                    false -> false
                end
	        end,
            Tree
        ),
    ConflictId.
