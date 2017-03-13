%%%-------------------------------------------------------------------
%% @doc game public API
%% @end
%%%-------------------------------------------------------------------

-module(game_app).
-include("common.hrl").

-behaviour(application).

%% Application callbacks
-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([set_log_level/0, set_log_level/1]).

start() ->
    ensure_app(),
    ensure_log(),
    game_conf:init(),
    application:start(?MAIN_APP). 
stop() ->
    application:stop(?MAIN_APP). 

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = game_sup:start_link(),
    
    % 全局服务
    {ok, _} = supervisor:start_child(Sup, {time_server, {time_server, start_link, [1000]}, permanent, brutal_kill, worker, [time_server]}),
    {ok, _} = supervisor:start_child(Sup, {frame_server, {frame_server, start_link, []}, permanent, brutal_kill, worker, [frame_server]}),
    {ok, _} = supervisor:start_child(Sup, {db_server, {db_server, start_link, []}, permanent, 5000, worker, [db_server]}),
    role_manager:init(),
    proc_timer_manager:init(),

    % web server
    {Host, Ip, Port} = game_conf:get(http),
    Opts = [{name, Host},
            {ip, Ip},
            {port, Port}
           ],
    {ok, _} = supervisor:start_child(Sup, {myhttpd, {myhttpd, start_link, [Opts, [?CONFIG(root_dir)], ?CONFIG(root_dir)++"/ebin"]},
                                           permanent, 5000, worker, [myhttpd]}),
    % test httpc
    case myhttpc:get("http://www.baidu.com") of
        {ok, {200, _}} ->
            ok;
        {error, Reason} ->
            ?ERROR("httpc error:~p~n", [Reason])
    end,

    % role_sup
    {ok, _} = supervisor:start_child(Sup, {role_sup, {role_sup, start_link, []}, permanent, 5000, supervisor, [role_sup]}),

    % gen_mod
    ok = gen_mod:init(),

    % ranch - connector pool
    ok = application:start(ranch),
    {ok, _} = ranch:start_listener(tcp_echo, 10, ranch_tcp, [
                                                              {port, game_conf:get(port)},  % 监听端口5555
                                                              {backlog, 1024},              % socket缓存队列长度
                                                              {delay_send, true},           % erlang自己的queue up
                                                              {nodelay, true},              % send immediately
                                                              {send_timeout, 5000},         % timeout
                                                              {max_connections, infinity}   % 默认无链接上限
                                                             ], role_server, []),
    ?INFO("game start. listen port:~p ~n", [5555]),

    {ok, Sup}.

%%--------------------------------------------------------------------
stop(_State) ->
    ranch:stop_listener(tcp_echo),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
ensure_app() ->
    application:start(sasl),
    application:start(inets),   % httpc|httpd
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),     % ssl protocol  ssl:listen/2 ssl:connect/[2,3]
    ok.
ensure_log() ->
    % TODO lager注意阻塞问题，必要时将log打印替换成ok之类的无效果代码
    ok = application:load(lager),
    L = [{lager_console_backend, debug},
         {lager_file_backend, [{file, "ErrFileName"}, {level, error}, {size, 20971520}, {date, "$D0"}, {count, 10}]},
         {lager_file_backend, [{file, "InfoFileName"}, {level, info}, {size, 20971520}, {date, "$D0"}, {count, 10}]}],
    EnvLager = lists:foldl(fun({Handler, Var} = Item, Acc) ->
                                   case Handler of
                                       lager_file_backend ->
                                           New = lists:keyreplace("ErrFileName", 2, Var, {file, "log/"++?NODE++"/error.log"}),
                                           New2 = lists:keyreplace("InfoFileName", 2, New, {file, "log/"++?NODE++"/console.log"}),
                                           [{Handler, New2} | Acc];
                                       _ ->
                                           [Item | Acc]
                                   end
                           end, [], L),
    application:set_env(lager, colored, true),             % log handler规则
    application:set_env(lager, handlers, EnvLager),             % log handler规则
    application:set_env(lager, async_threshold, 500000),        % 50w以下使用异步模式,超过则使用同步模式
    application:set_env(lager, async_threshold_window, 100000),  % 50w+滚回10w时,恢复异步模式
    ok = lager:start(),
    set_log_level(debug),

    ok.

set_log_level(Level) ->
    ok = lager:set_loglevel(lager_console_backend, Level),
    ok = lager:set_loglevel(lager_file_backend, "log/"++?NODE++"/console.log", Level).

-ifdef(TEST). 
    set_log_level() ->
        set_log_level(debug).
-else.
    set_log_level() ->
        set_log_level(notice).
-endif.
