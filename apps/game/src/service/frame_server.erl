%% 全局帧事件管理

-module(frame_server).

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------
-include("common.hrl").

-define(INTERVAL, 1000).
-define(FRAME_EVENT, frame_event).

% --------------------------------------------------------------------
% External exports
% --------------------------------------------------------------------
-export([start_link/0, call/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% --------------------------------------------------------------------
% Function: init/1
% --------------------------------------------------------------------
init(_Type) ->
    erlang:process_flag(trap_exit, true),
    erlang:process_flag(priority, high),
    ok = do_start_timer(),
    {ok, #state{}}.

% --------------------------------------------------------------------
% Function: handle_call/3
% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% --------------------------------------------------------------------
handle_info(?FRAME_EVENT, State) ->
    % 驱动帧事件
    do_frame(),
    ok = do_start_timer(),
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

% --------------------------------------------------------------------
% Internal API
% --------------------------------------------------------------------
%% 启动timer
do_start_timer() ->
    erlang:send_after(?INTERVAL, self(), ?FRAME_EVENT),
    ok.

%% 执行全局帧
do_frame() ->
    [role_server:cast(Pid, {frame_event}) || Pid <- role_manager:pid_list()],
    % TODO 检查跨天
    case util:cross_day(util:now_sec()) of
        true ->
            [role_manager:cast(Pid, {cross_daily}) || Pid <- role_manager:pid_list()];
        false ->
            ok
    end,
    ok.
