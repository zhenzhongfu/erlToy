-module(game_conf).

-include("common.hrl").

-export([init/0, reload/0, init/1, reload/1, get/1,  get/2, put/2, list/0]).

init() ->
    do_load(true).

reload() ->
    do_load(false).

init(ConfName) ->
    do_load(true, ConfName).

reload(ConfName) ->
    do_load(false, ConfName).

list() ->
    global_conf:list().

get(K) ->
    global_conf:get(K, undefined).

get(K, Default) ->
    global_conf:get(K, Default).

put(K, V) ->
    global_conf:put(K, V).

do_load(_IsFirst) ->
    do_load(_IsFirst, os:getenv("CONF_NAME")).
do_load(_IsFirst, ConfName) ->
    RootDir = os:getenv("ROOT"),
    ConfDir = os:getenv("CONFDIR"),
    LogDir = os:getenv("LOGDIR"),
    EbinDir = os:getenv("EBINDIR"),
    ?NOTICE("Root:~p ConfDir:~p EbinDir:~p~n", [RootDir, ConfDir, EbinDir]), 
    L0 = [
        {root_dir, RootDir},
        {conf_dir, ConfDir},
        {log_dir, LogDir},
        {ebin_dir, EbinDir},
        {config_name, ConfName}
    ],
    case ConfName of
        false ->
            load_file(ConfDir, "game.config", L0);
        Name ->
            load_file(ConfDir, Name, L0)
    end.

load_file(Dir, File, L0) ->
    ConfFile = filename:join(Dir, File),
    case catch file:consult(ConfFile) of
        {ok, L} ->
            L2 = lists:append(L0 ,L),
            [?MODULE:put(K, V) || {K, V} <- L2],
            ?INFO("load ~p ok~n", [ConfFile]),
            ok;
        {error, Reason} ->
            ?ERROR2("load ~p error:~p~n", [ConfFile, Reason]),
            exit({error, Reason})
    end.

