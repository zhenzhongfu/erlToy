%% 防作弊
%% 检测心跳, 加速，恶意洪水包

-module(anti_cheat).
-include("common.hrl").
-include("tab.hrl").

-export([check_heart_speedup/1, check_heart_timeout/1, check_dump_msg/1]).
-export([clear_heart_last_value/0]).

%% 最大心跳超时次数
-define(MAX_HEART_TIMEOUT_COUNT, 3).
%% 最大恶意行为次数
-define(MAX_HEART_SPAWN_MSG, 3).
%% 上次收到心跳时间
-define(HEART_LAST_TIME, 'heart_last_time').
%% 当前加速检测次数
-define(HEART_SPEEDUP_COUNT, 'heart_speedup_count').
%% 当前恶意行为检测次数
-define(HEART_SPAWN_MSG_COUNT, 'heart_spawn_msg_count').
%% 上次心跳包数
-define(HEART_LAST_PACKET_COUNT, 'heart_last_pack_count').
%% 上次心跳超时检测次数
-define(HEART_LAST_TIMEOUT_COUNT, 'heart_last_timeout_count').

%% 心跳加速检测
check_heart_speedup(Interval) ->
    % 校验上次心跳包到达时间
    LastTime = get_heart_last_value(?HEART_LAST_TIME),
    SpeedupCount = get_heart_last_value(?HEART_SPEEDUP_COUNT),
    Now = util:now_ms(),
    case LastTime of
        0 ->
            SpeedupCount2 = SpeedupCount;
        _ ->
            Diff = Now - LastTime,
            % 用差值计算加速,加速50%算作弊
            Factor = 50,
            SpeedupCount2 = ?IF(Diff < Interval * Factor div 100, SpeedupCount + 1, SpeedupCount),
            case SpeedupCount2 > ?MAX_HEART_TIMEOUT_COUNT of
                true ->
                    % 心跳太快判定加速,超过次数强制离线
                    role_server:cast(self(), {stop, exit_anti_cheat_speedup}),
                    ok;
                false ->
                    ok
            end
    end,
    % 记录本次心跳
    set_heart_last_value(?HEART_LAST_TIME, Now),
    set_heart_last_value(?HEART_SPEEDUP_COUNT, SpeedupCount2),
    ok.

%% 超时检测
check_heart_timeout(#role{socket = Sock} = _Role) ->
    HeartCount = get_heart_last_value(?HEART_LAST_PACKET_COUNT),
    % TODO 这里inet:getstat有两个用途
    % 1.判断前后两次收包数量是否一样，用来检测心跳
    % 2.判断前后两次收包数量是否猛增，用来限制client发包频率
    % 限制流量发送流量可以设置高低水位,使得Port busy
    % inet:setopts(Socket, [{high_watermark, 131072}]).
    % inet:setopts(Socket, [{low_watermark, 65536}]).
    case inet:getstat(Sock, [recv_cnt]) of
        {error, einval} ->
            role_server:cast(self(), {stop, tcp_einval});
        {ok, [{recv_cnt, RecvCnt}]} ->
            case HeartCount =/= RecvCnt of
                true ->
                    HeartT2 = 0,
                    set_heart_last_value(?HEART_LAST_PACKET_COUNT, RecvCnt), 
                    set_heart_last_value(?HEART_LAST_TIMEOUT_COUNT, 0);
                false ->
                    HeartT = get_heart_last_value(?HEART_LAST_TIMEOUT_COUNT),
                    HeartT2 = HeartT + 1,
                    set_heart_last_value(?HEART_LAST_TIMEOUT_COUNT, HeartT2)
            end,
            case HeartT2 >= ?MAX_HEART_TIMEOUT_COUNT of
                true ->
                    % 达到最大超时次数
                    role_server:cast(self(), {stop, exit_heart_timeout});
                false ->
                    ok
            end
    end.

%% 流量检测
check_dump_msg(_Role) ->
    SpawnCount = get_heart_last_value(?HEART_SPAWN_MSG_COUNT),
    SpawnCount2 = SpawnCount + 1,
    % 恶意行为超过N次，踢掉
    case SpawnCount2 > ?MAX_HEART_SPAWN_MSG of
        true ->
            role_server:cast(self(), {stop, exit_anti_cheat_dumpmsg}),
            set_heart_last_value(?HEART_SPAWN_MSG_COUNT, 0);
        false ->
            set_heart_last_value(?HEART_SPAWN_MSG_COUNT, SpawnCount2)
    end,
    ok.
%%---------
%% 
%%---------

get_heart_last_value(Type) ->
    case erlang:get(Type) of
        undefined ->
            0;
        N ->
            N
    end.

set_heart_last_value(Type, V) ->
    erlang:put(Type, V),
    ok.

clear_heart_last_value() ->
    erlang:erase(?HEART_LAST_TIME),
    erlang:erase(?HEART_SPEEDUP_COUNT),
    ok.

