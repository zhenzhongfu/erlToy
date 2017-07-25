-module(robot_ctl).
-include("../../game/include/common.hrl").

-behaviour(gen_server).

-export([start/0, i/0, start_link/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([command/1]).

command(Msg) ->
    ?DEBUG("robot_ctl command~n"),
    gen_server:call(robot_ctl, Msg, infinity).

%%--------------------------                                 
%% gen_server callback                                       
%%--------------------------                                 
start() ->
    start_link().
                                                             
i() ->
    ?INFO("module name:~p~n", [?MODULE]).
                                                             
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
                                                             
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, []}.

%% 登录单个指定client
handle_call({do_robot_1, Id}, _From, State) ->
    supervisor:start_child(robot_sup, [Id]),
    {reply, ok, State};
%% 批量登录N个随机client
handle_call({do_robot_n, N}, _From, State) ->
    util:seed(),
    Id = util:rand(16#ffffffff),
    [begin
         Id2 = Id + _X,
         {ok, _Pid} = supervisor:start_child(robot_sup, [Id2]),
         case _X rem 10 of
             0 ->
                 ?ALERT("10 ~n", []),
                 timer:sleep(1000);
             _ ->
                 ok
         end
     end|| _X <- lists:seq(1, N)],
    {reply, ok, State};
%% 随机登录1个账号
handle_call({do_robot, 1}, _From, State) ->
    Id = util:rand(16#ffff),
    {ok, _Pid} = supervisor:start_child(robot_sup, [Id]),
    Name = list_to_atom("robot"++integer_to_list(Id)),
    ?INFO("++++++++++++++++++++++++++ pid:~p~n", [Name]),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
