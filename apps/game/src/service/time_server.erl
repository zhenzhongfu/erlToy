%% 时间cache，定时set到ets
%% 1. 处理时间突变
%% 2. 提升获取时间的性能

-module(time_server).

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------
-include("common.hrl").

% --------------------------------------------------------------------
% External exports
-export([start_link/1, i/0, call/1]).
-export([now_sec/0, now_ms/0, day/0, day_of_week/0, update/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        interval    % 时间采样间隔
    }).

-define(TAB_CACHE_TIME, 'tab_cache_time').
-define(TAB_CACHE_DATE, 'tab_cache_date').

%% Interval cache的时间间隔,单位ms
start_link(Interval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Interval], []).

i() ->
    [].

now_sec() ->
    do_get(?TAB_CACHE_TIME, now_sec).

now_ms() ->
    do_get(?TAB_CACHE_TIME, now_ms).

day() ->
    do_get(?TAB_CACHE_DATE, day).

day_of_week() ->
    do_get(?TAB_CACHE_DATE, dayofweek).

update() ->
    gen_server:cast(self(), update).

% --------------------------------------------------------------------
% Function: init/1
% --------------------------------------------------------------------
init([Interval]) ->
    erlang:process_flag(trap_exit, true),
    erlang:process_flag(priority, high),
    ?TAB_CACHE_TIME = ets:new(?TAB_CACHE_TIME, [set, public, named_table, {read_concurrency, true}]),
    ?TAB_CACHE_DATE = ets:new(?TAB_CACHE_DATE, [set, public, named_table, {read_concurrency, true}]),
    % timer
    ok = do_start_time_timer(Interval),
    ok = do_start_date_timer(),
    % update
    do_update_time(),
    do_update_date(),
    {ok, #state{interval = Interval}}.

% --------------------------------------------------------------------
% Function: handle_call/3
% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% --------------------------------------------------------------------
handle_cast(update, State) ->
    do_update(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% --------------------------------------------------------------------
handle_info(update_time, #state{interval = Interval} = State) ->
    ok = do_update_time(),
    ok = do_start_time_timer(Interval),
    {noreply, State};
handle_info(update_date, #state{interval = _Interval} = State) ->
    {ok, T} = do_update_date(),
    ok = do_start_date_timer(T),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% call调用
call(Req) ->
    gen_server:call(?MODULE, Req, ?TIMEOUT_5).

%%---------------------------------------
%% Internal API
%%---------------------------------------

do_update() ->
    ok = do_update_time(),
    {ok, _T} = do_update_date(),
    ok.

%% 启动time计时器
do_start_time_timer(Interval) ->
    erlang:send_after(Interval, self(), update_time),
    ok.

%% 启动date计时器
do_start_date_timer() ->
    do_start_date_timer(60000).
do_start_date_timer(Interval) ->
    erlang:send_after(Interval, self(), update_date),
    ok.

%% 更新time
do_update_time() ->
    % http://erlang.org/doc/apps/erts/time_correction.html
    % 1. +C no_time_warp是默认的动作，开启时间校正模式。
    % 2. 在otp18.0以上的版本erlang:now/0是不受时间校正约束的，erts的bif源码里使用os的localtime_r获取系统时间，不管是否突变
    % 3. erlang:timestamp/0返回的是erts经过时间校正的结果,内部加锁保证一致性.
    % 具体体现在erts/emulator/beam/erl_time_sup.c
    % erlang会在时间突变时，缓慢的修正，在将来的某个时间点与突变的系统时间一致, 每次<1%的offset
    {MegaSec, Sec, Ms} = erlang:timestamp(),
    NowSec = MegaSec * 1000000 + Sec,
    NowMs = NowSec * 1000 + Ms div 1000,
    true = ets:insert(?TAB_CACHE_TIME, [{now_sec, NowSec}, {now_ms, NowMs}]),
    ok.

%% 更新date
%% 返回下个计时器启动的时间
do_update_date() ->
    % 由于时间校正,使用erlang:timestamp/0代替erlang:localtime/0
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(erlang:timestamp()),
    % 7表示星期天
    DayOfWeek = calendar:day_of_the_week(Year, Month, Day), 
    true = ets:insert(?TAB_CACHE_DATE, [{day,Day}, {dayofweek, DayOfWeek}]),
    T = 
    case Hour =:= 23 andalso Minute =:= 59 of 
        true ->
            60 - Second;
        _ ->
            60
    end,
    {ok, T}.

do_get(Table, K) ->
    case ets:lookup(Table, K) of
        [] ->
            none;
        [{_, V}] ->
            V
    end.

