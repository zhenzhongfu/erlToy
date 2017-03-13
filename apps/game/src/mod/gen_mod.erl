-module(gen_mod).

-include("common.hrl").
-include("tab.hrl").

-export([init/0]).
-export([mod_list/0, event_list/0, default_event_list/0]).
-export([init/1, terminate/1, load/1, cross_daily/1, event/3]).

%%====================
%% behaviour
%%====================
-callback prepare() -> ok | {ok, Priority :: integer()} | {ok, Priority :: integer(), EventList :: list()}.
-callback init(Role :: #role{}) -> #role{}.
-callback terminate(Role :: #role{}) -> #role{}.
-callback load(Role :: #role{}) -> #role{}.
-callback cross_daily(Role :: #role{}) -> #role{}.
-callback handle_c2s(Req :: term(), Role :: #role{}) -> ok | {ok, Role :: #role{}}.
-callback handle_timeout(Event :: term(), Role :: #role{}) -> {ok, #role{}}.

% 模块初始化返回 {ok, Priority, [{event1, Data}, {event2, Data2}]}
init() ->
    % 1. 整理模块
    ModList = make_modlist(),
    ?INFO("mod ~p~n", ModList),
    % 2. 整理事件
    _EventList = make_event(ModList),   
    ?INFO("event ~p~n", _EventList),
    % TODO 调用排序后列表模块的init
    ok.

make_modlist() ->
    Mods = filelib:wildcard(game_conf:get(ebin_dir) ++ "/mod_*.beam"),
    Pbs = filelib:wildcard(game_conf:get(ebin_dir) ++ "/mod_*_pb.beam"),
    PbMods = [list_to_atom(filename:basename(Pb, ".beam")) || Pb <- Pbs],

    PriorityLowest = 99,
    ModF = fun(FileName, List) -> 
                Mod = list_to_atom(filename:basename(FileName, ".beam")),
                case lists:member(Mod, PbMods) of
                    true ->
                        List;
                    false ->
                        try
                            case Mod:prepare() of
                                ok ->
                                    [{Mod, PriorityLowest, []} | List];
                                {ok, Priority} ->
                                    [{Mod, Priority, []} | List];
                                {ok, Priority, EventList} ->
                                    [{Mod, Priority, EventList} | List]
                            end
                        catch 
                            T:R ->
                                ?ERROR2("module:~p init failed ~p:~p", [Mod, T, R]),
                                exit(normal)
                        end
                end
        end,
    L = lists:foldl(ModF, [], Mods),
    ModF2 = fun({_, P1, _}, {_, P2, _}) ->
                 P1 =< P2
         end,
    ModList = lists:sort(ModF2, L),
    game_conf:put(gen_mod_module_list, ModList),
    ModList.

make_event(ModList) ->
    L = lists:append([[{Event, Mod} || Event <- ModEvent] || {Mod, _, ModEvent} <- ModList]),
    Dict =
    lists:foldl(
          fun({Event, Mod}, Acc) ->
                              dict:append(Event, Mod, Acc)
                                  end, dict:new(), L),
    L2 = dict:to_list(Dict),
    game_conf:put(gen_mod_event_list, L2),
    L2.

mod_list() ->
    game_conf:get(gen_mod_module_list, []).

event_list() ->
    game_conf:get(gen_mod_event_list, []).

get_event(Event) ->
    case catch event_list() of
        undefined ->
            [];
        [] ->
            [];
        L ->
            case lists:keyfind(Event, 1, L) of
                false ->
                    [];
                {Event, EventList} ->
                    EventList
            end
    end.

init(Role) ->
    F = fun({Mod, _, _}, Acc) ->
                try
                    case Mod:init(Acc) of
                        {ok, #role{} = Acc2} ->
                            Acc2;
                        #role{} = Acc2 ->
                            Acc2;
                        _ ->
                            Acc
                    end
                catch
                    T:R ->
                        ?ERROR2("role:~p module ~p on init failed. ~p:~p", [Role#role.id, Mod, T, R]),
                        exit(R)
                end
        end,
    lists:foldl(F, Role, mod_list()).

terminate(Role) ->
    F = fun({Mod, _, _}, Acc) ->
                try 
                    case Mod:terminate(Acc) of
                        {ok, #role{} = Acc2} ->
                            Acc2;
                        #role{} = Acc2 ->
                            Acc2;
                        _ ->
                            Acc
                    end
                catch 
                    T:R ->
                        ?ERROR2("role:~p module ~p on terminate failed. ~p:~p", [Role#role.id, Mod, T, R])
                        %exit(R)
                end
        end,
    lists:foldl(F, Role, lists:reverse(mod_list())).

load(Role) ->
    F = fun({Mod, _, _}, Acc) ->
                try 
                    case Mod:load(Acc) of
                        {ok, #role{} = Acc2} ->
                            Acc2;
                        #role{} = Acc2 ->
                            Acc2;
                        _ ->
                            Acc
                    end
                catch 
                    T:R ->
                        ?ERROR2("role:~p module ~p on load failed. ~p:~p", [Role#role.id, Mod, T, R])
                        %exit(R)
                end
        end,
    lists:foldl(F, Role, lists:reverse(mod_list())).

cross_daily(Role) ->
    F = fun({Mod, _, _}, Acc) ->
                try 
                    case Mod:cross_daily(Acc) of
                        {ok, #role{} = Acc2} ->
                            Acc2;
                        #role{} = Acc2 ->
                            Acc2;
                        _ ->
                            Acc
                    end
                catch 
                    T:R ->
                        ?ERROR2("role:~p module ~p on cross daily failed. ~p:~p", [Role#role.id, Mod, T, R])
                        %exit(R)
                end
        end,
    lists:foldl(F, Role, mod_list()).

event(Role, Event, Data) ->
    F = fun(Mod, Acc) ->
                try 
                    case Mod:Event(Acc, Data) of
                        {ok, #role{} = Acc2} ->
                            Acc2;
                        #role{} = Acc2 ->
                            Acc2;
                        _ ->
                            Acc
                    end
                catch 
                    T:R ->
                        ?ERROR2("role:~p module ~p on event failed. ~p:~p", [Role#role.id, Mod, T, R]),
                        Acc
                end
        end,
    lists:foldl(F, Role, get_event(Event)).

default_event_list() ->
    [
     on_login
    ].
