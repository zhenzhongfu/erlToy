-module(role_server).
-include("common.hrl").
-include("ecode.hrl").
-include("tab.hrl").

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% API.
-export([start_link/4]).

%% gen_server
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([call/2, cast/2]).

-export([do_login/2]).
-export([start_check_heart_timer/0, start_check_dump_timer/1]).
%% 延迟发送
-export([cache_packet/2, merge_send/1, start_async_send_timer/0]).

-define(CLOSE_TIMEOUT, 30000).      % 超时无消息关闭时间
-define(SOCKET_DUMP_MSG_COUNT, 300).
-define(HEART_CHECK_TIMER, 'heart_check_timer').
-define(CHECK_DUMP_MSG_INTEVAL, 60000).
-define(PACKET_CACHE_KEY, 'packet_cache_key').
-define(SOCKET_SEND_TIME, 100).

start_link(Ref, Socket, Transport, Opts) ->
        ?INFO("module name:~p start. ref:~p socket:~p transport:~p opts:~p~n", [?MODULE, Ref, Socket, Transport, Opts]),
        {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

init({Ref, Socket, Transport, _Opts = []}) ->
    process_flag(trap_exit, true),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [
                                    binary,
                                    {packet, 4},                  % 包头长度4
                                    {packet_size, 65535},         % 允许的单个包大小 64k
                                    {reuseaddr, true},
                                    {delay_send, true},
                                    {active, ?SOCKET_DUMP_MSG_COUNT}              % 流量控制用N, 用计时器去release passive mode
                                   ]),
    % client流量检测
    start_check_dump_timer(Socket),
    % 异步socket发送
    start_async_send_timer(),
    gen_server:enter_loop(?MODULE, [],
                          #role{pid = self(), socket=Socket, transport=Transport}, ?CLOSE_TIMEOUT).

%%--------------------------------------------------------------------
% Function: handle_call/3
%%--------------------------------------------------------------------
handle_call(_Request, _From, Role) ->
    Reply = ok,                       
    {reply, Reply, Role}.            

%%--------------------------------------------------------------------
% Function: handle_cast/2
%%--------------------------------------------------------------------
%% 帧逻辑
handle_cast({frame_event}, Role) ->
    {ok, Role2} = proc_time_manager:on_timeout_loop(Role),
    {ok, Role3} = do_role_frame(Role2),
    {noreply, Role3};
%% 跨天操作
handle_cast({cross_daily}, Role) ->
    gen_mod:cross_daily(Role),
    {noreply, Role};
handle_cast({stop, exit_anti_cheat_speedup}, Role) ->
    ?ALERT("stop role server: exit_anti_cheat_speedup~n"),
    {stop, normal, Role};
handle_cast({stop, exit_heart_timeout}, Role) ->
    ?ALERT("stop role server: exit_heart_timeout~n"),
    {stop, normal, Role};
handle_cast({stop, tcp_einval}, Role) ->
    ?ALERT("stop role server: tcp_einval~n"),
    {stop, normal, Role};
handle_cast(_Msg, Role) ->
    {noreply, Role}.      

%%--------------------------------------------------------------------
% Function: handle_info/2
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, #role{socket = Socket} = Role) when byte_size(Data) >= 1 ->
    % 先处理并包包头
    case do_parse_pack(Data, Role) of
        {ok, State2} ->
            {noreply, State2};
        {error, State2} ->
            {stop, normal, State2}
    end;
handle_info(?CHECK_HEART_EVENT = Event, Role) ->
    ?ALERT("check heart event~n"),
    mod_test:handle_timeout(Event, Role),
    start_check_heart_timer(),
    {noreply, Role};
handle_info({release_passive, Sock}, Role) ->
    inet:setopts(Sock, [{active, ?SOCKET_DUMP_MSG_COUNT}]),
    % TODO passive
    start_check_dump_timer(Sock),
    {noreply, Role};
handle_info({async_send}, Role) ->
    merge_send(Role),
    start_async_send_timer(),
    {noreply, Role};
handle_info({tcp_closed, _Socket}, Role) ->
    ?ALERT("socket closed.~n"),
    {stop, normal, Role};
handle_info({inet_reply, _Rsp, ok}, Role) ->
    % erlang:command消息推送成功的通知
    {noreply, Role};
handle_info(_Info, Role) ->
    ?ALERT("unknown info info:~p~n", [_Info]),
    {stop, normal, Role}.

%%--------------------------------------------------------------------
% Function: terminate/2
%%--------------------------------------------------------------------
terminate(_Reason, #role{id = Id} = Role) ->
    gen_mod:terminate(Role),
    DataPairs = db_server:convert_role_to_data(Role),
    db_server:save("role"++?N2S(Id), DataPairs),
    ?ALERT("id:~p data:~p~n", [Id, DataPairs]),
    ok;
terminate(_Reason, _State) ->
    ok.                      

%%--------------------------------------------------------------------
% Function: code_change/2
%%--------------------------------------------------------------------
code_change(_OldVsn, Role, _Extra) ->
    {ok, Role}.                      

%%------------------------                                                                                      
%% handle c2s                                                                                                   
%%------------------------                                                                                      
                                                                                                               
handle_c2s(Data, #role{id = Id} = Role) ->
    {ok, Uniq, Mod, MsgType, Request} = proto:unpack(Data),
    ?DEBUG("-------------UID:[~p] uniq:~p handle_c2s: [~p:~p] request:~p~n", [Id, Uniq, Mod, MsgType, Request]),
    % 这里只为了适配客户端需要一问一答缓存的uniq
    Role2 = Role#role{uniq = Uniq},
    handle_c2s_reply(catch Mod:handle_c2s(Request, Role2), Role2).

handle_c2s_reply({ok, Role}, _OldState) ->
    {ok, Role};
handle_c2s_reply({'EXIT', Reason}, Role) ->
    ?ERROR("role:~p error:\n~p", [Role#role.id, Reason]),
    {stop, Role};
handle_c2s_reply({error, ?E_UNKNOW_PROTO}, Role) ->
    ?ERROR("role:~p unknow proto~n", [Role#role.id]),
    {ok, Role};
handle_c2s_reply({error, Reason}, Role) ->
    ?ERROR("role:~p error:\n~p", [Role#role.id, Reason]),
    {ok, Role};
handle_c2s_reply(Other, Role) ->
    ?ERROR("role:~p error:\n~p", [Role#role.id, Other]),
    {ok, Role}.

%%-----------------------------------
%% cast
%%-----------------------------------
cast(#role{pid = Pid}, Req) ->
    gen_server:cast(Pid, Req);
cast(Id, Req) when is_integer(Id) ->
    gen_server:cast(util:role_pid_name(Id), Req);
cast(Id, Req) when is_pid(Id) ->
    gen_server:cast(Id, Req).

%%-----------------------------------
%% call
%%-----------------------------------
call(Id, Req) ->
    call(Id, Req, ?TIMEOUT_5).
call(#role{pid = Pid}, Req, Timeout) ->
    call(Pid, Req, Timeout);
call(Id, Req, Timeout) when is_integer(Id) ->
    gen_server:call(util:role_pid_name(Id), Req, Timeout);
call(Id, Req, Timeout) when is_pid(Id) ->
    gen_server:call(Id, Req, Timeout).

%%------------------------                                                                                      
%% Internal API
%%------------------------                                                                                      
%% 拆解协议头
do_parse_pack(<<>>, Role) -> {ok, Role};
do_parse_pack(<<Len:32, Rest/binary>> = _Data, #role{id = _Id} = Role) ->
    % TODO 如果是前端上行包，要处理长度校验
    ?DEBUG("parse proto Data:~p pack: Len:~p Rest:~p~n", [_Data, Len, Rest]),
    <<Data:Len/binary, Rest2/binary>> = Rest,
    case catch handle_c2s(Data, Role) of
        {ok, State2} ->
            do_parse_pack(Rest2, State2);
        _ ->
            {error, Role}
    end.

%% role登录入口
do_login(Role, Id) ->
    erlang:register(util:role_pid_name(Id), self()),
    Role2 = gen_mod:init(Role),
    Role2#role{id = Id}.

%% 逻辑帧
do_role_frame(Role) ->
    Role.

%% 在线心跳                                                                          
start_check_heart_timer() ->
    Timer = erlang:send_after(?HEART_CHECK_TIME, self(), ?CHECK_HEART_EVENT),
    erlang:put(?HEART_CHECK_TIMER, Timer).
                                                                                     
%% 流量限制
start_check_dump_timer(Sock) ->
    erlang:send_after(?CHECK_DUMP_MSG_INTEVAL, self(), {release_passive, Sock}).

%% 缓存数据包
cache_packet(_Role, Msg) ->
    {packed, Bin, _Size} = proto:pack(proto:get_uniq(), Msg),
    case erlang:get(?PACKET_CACHE_KEY) of
        undefined ->
            erlang:put(?PACKET_CACHE_KEY, [Bin]);
        [_|_] = L ->
            L2 = [Bin|L],
            erlang:put(?PACKET_CACHE_KEY, [L2])
    end.
%% 合并发送
merge_send(#role{id = Id, socket = Sock} = _Role) ->
    case erlang:get(?PACKET_CACHE_KEY) of
        undefined ->
            ok;
        [_|_] = L ->
            L2 = lists:reverse(L),
            case erlang:port_command(Sock, L2, [force]) of
                true ->
                    ?ALERT("send msg:~p to sock:~p~n", [L2, Sock]),
                    erlang:erase(?PACKET_CACHE_KEY),
                    ok;
                _Reason ->
                    ?WARN("role ~p send data busy:~p~n", [Id, _Reason]),
                    error
            end
    end.

%% 定时异步发送
start_async_send_timer() ->
    erlang:send_after(?SOCKET_SEND_TIME, self(), {async_send}).
