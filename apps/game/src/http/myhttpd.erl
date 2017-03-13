%% http server,基于mochiweb,http1.1
-module(myhttpd).
-include("common.hrl").

-export([start_link/3]).
-export([handle_request/2]).

-define(MAX_MOD_LEN, 255).
%% macros for get query string
-define(QS_GET(K, QS), proplists:get_value(K, QS)).
-define(QS_GET(K, QS, D), proplist:get_value(K, QS, D)).

-define(HTTP_BADARG, http_badarg).
-define(RETURN_HTTP_BADARG, exit(http_badarg)).

-define(HTTP_CONTENT_TYPE, <<"Content-Type">>).

%% start the http server
-type httpd_options() ::
{name, string()} |
{ip, string()} |
{port, integer()} |
{allow_ip, list()}.
-spec start_link(Opts :: [httpd_options()],
                 HandlerDirs :: [file:filename()],
                 file:filename()) -> any().
start_link(Opts, HandlerDirs, DocRoot) ->
    load_mods_name(HandlerDirs),
    Loop = fun(Req) -> ?MODULE:handle_request(Req, DocRoot) end, 
    mochiweb_http:start([{loop, Loop} | Opts]).

%% handle the http request
-spec handle_request(Req :: any(), DocRoot :: string()) -> 'ok'.
handle_request(Req, DocRoot) ->
    Method = Req:get(method),
    Path = "/" ++ Path0 = Req:get(path),

    {Main, Minor} = Req:get(version),
    ?INFO("~s ~s HTTP ~B.~B~n", [Method, Path, Main, Minor]),
    Resp = 
    case get_handle_mod(convert_path(Path0)) of
        {ok, Mod} ->
            {Code, Headers, Data} = 
            try
                case Mod:handle(Req, Method) of
                    {_Code, _Headers, _Data} = Rsp ->
                        Rsp;
                    {ok, Rsp} ->
                        {200, [], Rsp};
                    {json, Json} ->
                        {200, [], mochijson2:encode(Json)};
                    Bin when is_binary(Bin) ->
                        {200, [], Bin}
                end
            catch 
                Error:_Reason ->
                    ?ERROR2("~p handle request ~p error:~p~p~n", [Mod, Path0, Error, _Reason]),
                    error_to_rsp(Error)
            end,
            send_respond(Req, Code, Headers, Data);
        {error, _} ->
            % 找不到合适接口，渲染原生html
            ?INFO("render raw html: ~p ~p~n", [Path0, DocRoot]),
            Req:serve_file(Path0, DocRoot)
    end,

    ?INFO("response ~s - ~s ~s ~b~n", [Req:get(peer), 
                                     Req:get(method),
                                     Path,
                                     Resp:get(code)]).

%%--------------------------------
%% Internal API
%% url请求格式
%% http://ip:port/module/action?key=value
%%--------------------------------

convert_path("") ->
    "index";
convert_path(P) ->
    % 这里是为了找二层路径
    case string:str(P, "/") of
        0 ->
            P;
        Idx ->
            string:sub_string(P, 1, Idx-1)
    end.

get_handle_mod(Path) ->
    case get_handle_mod(Path, ["http"], 0) of
        {ok, ModStr} ->
            case catch erlang:list_to_existing_atom(ModStr) of
                {'EXIT', {badarg, _}} ->
                    {error, no_handle_mod};
                Mod ->
                    {ok, Mod}
            end;
        Other ->
            Other
    end.

get_handle_mod(_Path, _Acc, AccLen) when AccLen >= ?MAX_MOD_LEN ->
    {error, max_mod_len};
get_handle_mod("", Acc, _AccLen) ->
    {ok, lists:append(lists:reverse(Acc))};
get_handle_mod(Path, Acc, AccLen) ->
    {Part, Rest} = mochiweb_util:path_split(Path),
    Acc2 = [Part, "_" | Acc],
    AccLen2 = AccLen + length(Part) + 1,
    get_handle_mod(Rest, Acc2, AccLen2).

error_to_rsp(?HTTP_BADARG) ->
    ?DEBUG("error is~p", [?HTTP_BADARG]),
    {400, "", <<"400 bad request (badarg)">>};
error_to_rsp(_Error) ->
    ?DEBUG("error is ~p", [_Error]),
    {500, "", [<<"500 Internal Error">>, "</br>",
               io_lib:format("~w</br>~p", [_Error, erlang:get_stacktrace()])
              ]}.

send_respond(Req, Code, Headers, Data) ->
    DefaultHeaders = [
                      {?HTTP_CONTENT_TYPE, set_content_type(Req, Headers)},
                      {<<"Cache-Control">>, <<"must-revalidate">>}
                     ] ++ server_header(),
    Req:respond({Code, Headers ++ DefaultHeaders, Data}).

set_content_type(Req, Headers) ->
    case proplists:get_value(?HTTP_CONTENT_TYPE, Headers) of
        undefined ->
            negotiate_content_type(Req);
        Type ->
            Type
    end.

negotiate_content_type(Req) ->
    AcceptedTypes = 
    case Req:get_header_value("Accept") of
        undefined ->
            [];
        AcceptHeader ->
            string:tokens(AcceptHeader, ", ")
    end,
    case lists:member("application/json", AcceptedTypes) of
        true -> "application/json";
        false ->
            case AcceptedTypes of 
                [] ->
                    "text/plain;charset=utf-8";
                [AType|_] ->
                    AType
            end
    end.

server_header() ->
    OTPVersion = "R" ++ integer_to_list(erlang:system_info(compat_rel)) ++ "B",
    [{<<"Server">>, ["(Erlang OTP/", OTPVersion, ")"]}].

load_mods_name(HandlerDirs) ->
    Self = code:which(?MODULE),
    Ebin = filename:dirname(Self),
    Dirs = [Ebin] ++ HandlerDirs,

    Mods = 
    [
     [begin
          ModStr = filename:basename(F, ".beam"),
          list_to_atom(ModStr)
      end || F <- filelib:wildcard("http_*.beam", Dir)]
     || Dir <- Dirs],
    ?INFO("http handler mods:~p~n", [lists:append(Mods)]),
    ok.

