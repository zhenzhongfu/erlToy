%%%-------------------------------------------------------------------
%% @doc robot public API
%% @end
%%%-------------------------------------------------------------------

-module(robot_app).
-include("../../game/include/common.hrl").

-behaviour(application).

%% Application callbacks
-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([set_log_level/1]).

%%====================================================================
%% API
%%====================================================================
start() ->
    ensure_app(),
    ensure_log(),
    game_conf:init(),
    application:start(robot). 
stop() ->
    application:stop(robot). 

start(_StartType, _StartArgs) ->
    time_server:start_link(1000),
    robot_ctl:start_link(),
    robot_sup:start_link().

stop(_State) ->
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

