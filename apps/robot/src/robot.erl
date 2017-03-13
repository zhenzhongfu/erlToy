-module(robot).
-include("../../game/include/common.hrl").
-include("../../game/include/proto/mod_test_pb.hrl").

-behaviour(gen_server).

-export([i/0, start_link/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TCP_OPTION,[                
                    binary,             
                    {active,once},      
                    {reuseaddr, true},  
                    {delay_send, true}, 
                    {nodelay, true},    
                    {packet, 4},        
                    {send_timeout, 5000}
                   ]).                 
-define(IP, "localhost").           
-define(PORT, 8080).                

-record(state, {id, sock}).

%%--------------------------                                 
%% gen_server callback                                       
%%--------------------------                                 
i() ->
    ?INFO("module name:~p~n", [?MODULE]).
                                                             
start_link(Id) ->
    ?DEBUG("module name:~p start~n", [?MODULE]),
    gen_server:start_link(?MODULE, [Id], []).
                                                             
init([Id]) ->
    process_flag(trap_exit, true),
    Name = list_to_atom("robot"++integer_to_list(Id)),
    case catch erlang:register(Name, self()) of
        true ->
            util:seed(),
            case do_connect(Id) of
                {error, _Reason} ->
                    ?ERROR("connect server error:~p~n", [_Reason]),
                    exit({error, _Reason});
                {ok, Sock} ->
                    {ok, #state{id = Id, sock = Sock}}
            end;
        Other ->
            ?ALERT("regist badarg:~p:~p:~p~n", [Name, self(),Other]),
            exit({error, badarg})
    end.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(heart, #state{sock = Sock} = State) ->
    Msg = #mod_test_c2s_heart{},
    Bin = pack(99, mod_test_c2s_heart, Msg),
    case gen_tcp:send(Sock, Bin) of
        ok ->
            start_heart_timer(),
            ?INFO("client heart~n");
        {error, Reason} ->
            ?INFO("client heart error:~p~n", [Reason])
    end,
    {noreply, State};
handle_info({tcp, Sock, Bin}, State) ->
    ?INFO("client tcp data~n"),
    % 重设active
    ok = inet:setopts(Sock, [{active, once}]),
    % 数据包已到
    {_Uniq, _Mod, _MsgType, Data} = unpack(Bin),
    ?DEBUG(" -------robot client recv mod:~p msgtype:~p data::~p~n", [_Mod, _MsgType, Data]),
    case handle_msg(Data, State) of
        {ok, State2} ->
            ok;
        _ ->
            State2 = State,
            error
    end,
    {noreply, State2};
handle_info(_Info, State) ->
    ?ALERT("handle_info:~p~n", [_Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%------------------------------------------
do_connect(IId) ->
    {Host, Port} = game_conf:get(robot_game),
    ?ALERT("Host:~p Port:~p~n", [Host, Port]),
    T1 = unixtime(),
    case gen_tcp:connect(Host, Port, ?TCP_OPTION, infinity) of
        {ok, Sock} ->
            T2 = unixtime(),
            ?NOTICE("******************client connect:~p DT:~p~n",[IId, T2-T1]),
            % 第一个包一定要是mod_login_c2s_login  
            %Name = "role" ++ integer_to_list(IId),
            Msg = #mod_test_c2s_login{id = IId, cmd = 1, name = "ssssssssssssss", list = [1,2,3,4]},
            Bin = pack(99, mod_test_c2s_login, Msg),
            ?NOTICE("connect send:~p sock:~p~n", [Bin, Sock]),
            case gen_tcp:send(Sock, Bin) of
                ok ->
                    % TODO heart
                    start_heart_timer(),
                    ?INFO("client send msg to sock~n");
                {error, Reason} ->
                    ?INFO("client send msg error:~p~n", [Reason])
            end,
            {ok, Sock};
        {error, Reason2} ->
            ?ERROR("client connect error:~p~n", [Reason2]),
            {error, Reason2}
    end.

unixtime() ->                    
    {M, S, _L} = os:timestamp(),
    M * 1000000 + S.

pack(Uniq, Proto, Data) ->                                         
    {packed, IOList, _Size} = proto:pack(Uniq, Proto, Data, "c2s"),
    IOList.                                                        

unpack(Bin) ->
    % 数据包已到
    <<Len:32, Rest/binary>> = Bin,
    <<Msg:Len/binary, _Rest2/binary>> = Rest,
    {ok, Uniq, Mod, MsgType, Records} = proto:unpack(Msg, "s2c"),
    {Uniq, Mod, MsgType, Records}.

start_heart_timer() ->
    erlang:send_after(5000, self(), heart).

handle_msg(#mod_test_s2c_heart{time = Time}, State) ->
    ?ALERT("time:~p~n", [Time]),
    {ok, State};
handle_msg(_Req, State) ->
    ?ALERT("unknown req:~p~n", [_Req]),
    {ok, State}.
