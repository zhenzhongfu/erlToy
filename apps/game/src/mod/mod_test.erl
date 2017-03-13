-module(mod_test).
-include("common.hrl").
-include("tab.hrl").
-include("proto/mod_test_pb.hrl").

-behaviour(gen_mod).

-export([prepare/0, init/1, terminate/1, load/1, cross_daily/1, handle_c2s/2, handle_timeout/2]).

-export([on_login/2]).

on_login(Role, Data) ->
    ?INFO("event ~p~n", [Data]),
    Role.

prepare() ->
    ?INFO("on prepare~n"),
    {ok, 1, gen_mod:default_event_list()}.

init(Role) ->
    ?INFO("on init~n"),
    Role.

terminate(Role) ->
    ?INFO("on terminate~n"),
    Role.

load(Role) ->
    Role.

cross_daily(Role) ->
    ?INFO("on cross daily~n"),
    Role.

handle_c2s(#mod_test_c2s_login{id = Id, cmd = Cmd, name = Name, list = List}, #role{} = Role) ->
    ?ALERT("id:~p cmd:~p name:~p list:~p~n", [Id, Cmd, Name, List]),
    Role2 = role_server:do_login(Role, Id),
    gen_mod:event(Role2, on_login, "on login event"),
    role_server:start_check_heart_timer(),
    % TODO load data from redis
    case catch db_server:load("role"++?N2S(Id)) of
        {ok, Values} ->
            % TODO 更新,将values的值替换role里的字段
            Role3 = db_server:convert_data_to_role(Role2, Values),
            Role4 = gen_mod:load(Role3),
            {ok, Role4};
        {error, Reason} ->
            ?ERROR("load role error:~p~n", [Reason]),
            {ok, Role2}
    end;
handle_c2s(#mod_test_c2s_heart{}, Role) ->
    anti_cheat:check_heart_speedup(?HEART_CHECK_TIME),
    Msg = #mod_test_s2c_heart{time = util:now_sec()},
    %TODO 两段式发送避免调用进程被挂起 http://blog.yufeng.info/archives/336
    % 1. erlang:port_command(Port, Bin, [force]), 如果对应Port busy，会立即返回，将Bin数据放入cache
    % 2. 定时顺序发送cache内数据
    ?SEND(Role, Msg),
    {ok, Role};
handle_c2s(Req, Role) ->
    ?WARN("unknow proto ~p~n", [Req]),
    {ok, Role}.

%% 计时器回调
handle_timeout(?CHECK_HEART_EVENT, Role) ->
    anti_cheat:check_heart_timeout(Role),
    {ok, Role};
handle_timeout(_Msg, Role) ->
    {ok, Role}.

%%==================
%%
%%==================

