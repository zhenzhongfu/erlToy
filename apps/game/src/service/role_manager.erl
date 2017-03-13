%% 玩家管理
%% ets存储在线玩家列表

-module(role_manager).
-include("common.hrl").
-include("tab.hrl").

-export([init/0]).
-export([list/0, id_list/0, online_list/0, pid_list/0, count/0, add/1, delete/1, get_name_by_id/1, get_pid_by_id/1]).
-export([broadcast/2]).

%% 以玩家id做key
-define(TABLE_ID, tab_role_online_id).
%% 玩家id列表与玩家pid列表
-define(TABLE_LIST, tab_role_online_list).

-record(role_online, {
        id,             % 玩家id
        name,           % 角色名
        pid,		% 关联的进程id
        account         % 账号
    }).

init() ->
    ok = create_table(),
    true = ets:insert(?TABLE_LIST, {id, []}),
    true = ets:insert(?TABLE_LIST, {pid, []}),
    ok.

list() ->
    ets:tab2list(?TABLE_ID).

online_list() ->
    L = list(),
    L2 = [{RoleId, node()} || #role_online{id = RoleId} <- L],
    L2.

id_list() ->
    ets:lookup_element(?TABLE_LIST, id, 2).

pid_list() ->
    ets:lookup_element(?TABLE_LIST, pid, 2).

count() ->
    ets:info(?TABLE_ID, size).

add(#role{id = Id, name = Name, pid = Pid, accname = Account}) 
    when is_integer(Id), is_binary(Name), is_pid(Pid) ->
    true = ets:insert(?TABLE_ID, #role_online{id = Id, name = Name, pid = Pid, account = Account}),
    true = ets:insert(?TABLE_LIST, {id, [Id | id_list()]}),
    true = ets:insert(?TABLE_LIST, {pid, [Pid | pid_list()]}),
    ok.

delete(#role{id = Id}) ->
    case ets:lookup(?TABLE_ID, Id) of
        [#role_online{pid = Pid}] ->
            true = ets:delete(?TABLE_ID, Id),
            true = ets:insert(?TABLE_LIST, {id, lists:delete(Id, id_list())}),
            true = ets:insert(?TABLE_LIST, {pid, lists:delete(Pid, pid_list())}),
            ok;
        [] ->
            ok
    end.

get_name_by_id(Id) ->
    case catch ets:lookup_element(?TABLE_ID, Id, #role_online.name) of
        {'EXIT', {badarg, _}} ->
            ?NONE;
        Name ->
            Name
    end.

get_pid_by_id(Id) ->
    case catch ets:lookup_element(?TABLE_ID, Id, #role_online.pid) of
        {'EXIT', {badarg, _}} ->
            ?NONE;
        Pid ->
            Pid
    end.

%% 广播操作
broadcast(Order, _Data) ->
    case Order of
        _Other ->
            ok
    end.

%%------------------------------
%%  Interval API
%%------------------------------

create_table() ->
    ?TABLE_ID = ets:new(?TABLE_ID, [named_table, set, public, 
            {keypos, #role_online.id}, {read_concurrency, true}]),
    ?TABLE_LIST = ets:new(?TABLE_LIST, [named_table, set, public, 
            {keypos, 1}, {read_concurrency, true}]),
    ok.

