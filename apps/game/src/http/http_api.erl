%% http服务的接口
-module(http_api).
-include("common.hrl").
-include("http.hrl").

-export([handle/2]).
-export([get_str/2, get_str_raw/2, get_binary/2, get_int/2]).

handle(Req, Method) ->
    try 
        handle_req(Req, Method)
    catch
        throw:{error, Error} ->
            ?HTTP_RETURN_JSON(Error);
        T:R ->
            erlang:raise(T, R, erlang:get_stacktrace())
    end.

handle_req(Req, Method) ->
    Qs = 
    case Method of
        'GET' ->
            Req:parse_qs();
        'POST' ->
            Req:parse_post();
        _ ->
            []
    end,
    "/api" ++ Cmd = ?HTTP_GET_PATH(Req),
    ?INFO("http_api_req ==== Cmd:~p Qs:~p", [Cmd, Qs]), 
    do_handle(Cmd, Qs).

%%---------------------------------------
%TODO string要屏蔽掉非法字符,主要是防js注入
get_str(Key, Qs) ->
    V = ?KV_GET(Key, Qs, ""),
    re:replace(V, "[^a-zA-Z0-9_-]", "", [global, {return, list}]).

get_str_raw(Key, Qs) ->
    V = ?KV_GET(Key, Qs, ""),
    V.

get_binary(Key, Qs) ->
    V = ?KV_GET(Key, Qs, ""),
    list_to_binary(V).

get_int(Key, Qs) ->
    V = ?KV_GET(Key, Qs, ""),
    V2 = re:replace(V, "[^0-9]", "", [global, {return, list}]),
    ?IF(V2 =:= "", throw({error, ?PARAM_ERROR(Key)}), ok),
    list_to_integer(V2).

%%-----------------------------------------
% usage: curl "http://127.0.0.1:8888/api/test?testString=arg1&time=2017-01-01&testBin=Kdjjd"
do_handle("/test" ++ _Rest, Qs) ->
    Str = get_str("testString", Qs),
    Int = get_int("time", Qs),
    Bin = get_binary("testBin", Qs),
    ?INFO("~p ~p ~p~n", [Str, Int, Bin]),
    ?HTTP_RETURN_JSON(?SUCC);
do_handle(_Cmd, _Qs) ->
    ?HTTP_RETURN_JSON(?PARAM_ERROR(_Cmd)).
