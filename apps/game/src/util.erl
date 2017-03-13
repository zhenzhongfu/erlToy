-module(util).
-include("common.hrl").

%% atom,string转换与拼接
-export([concat_to_atom/2, concat_to_atom/3, role_pid_name/1]).
%% term转换
-export([encode_term/1, decode_term/1, row_to_record/1, record_to_row/1, expr/2]).
%% 时间
-export([now_sec/0, now_ms/0, set_now_sec/1, set_now_ms/1, timer/2, 
         cross_day/1, cross_mon/1, cross_week/1, get_date/0, datetime_to_timestamp/1]).
%% 网络
-export([ip_ntoa/1, ip_aton/1]).
%% misc
-export([for/3, ceil/1, floor/1, seed/0, rand/1, rand/2, rand_from_list/2, rand_from_list/3]).
%% 文本
-export([is_name_valid/1]).
%%
-export([any_to_iodata/1, any_to_list/1, md5_string_uppercase/1, md5_string_lowercase/1]).
%% 
-export([b2s/1, s2b/1]).
%% 
-export([split_list_to_tuplelist/2]).
%%
-export([none/1, none/2]).

%% 组合生成atom
concat_to_atom(Prefix, N) when is_list(Prefix), is_integer(N) ->
    case catch list_to_existing_atom(Prefix ++ ?N2S(N)) of
        {'EXIT', _} -> list_to_atom(Prefix ++ ?N2S(N));
        Atom -> Atom
    end.
concat_to_atom(Prefix, N1, N2) when is_list(Prefix), is_integer(N1), is_integer(N2) ->
    case catch list_to_existing_atom(Prefix ++ [$_ | ?N2S(N1)] ++ [$_ | ?N2S(N2)]) of
        {'EXIT', _} -> list_to_atom(Prefix ++ [$_ | ?N2S(N1)] ++ [$_ | ?N2S(N2)]);
        Atom -> Atom
    end.

%% role_server进程名
role_pid_name(Id) ->
    concat_to_atom("role", Id).

%% TODO 获取当前时间
now_sec() ->
    case erlang:get('now_sec_cache') of
        undefined ->
            time_server:now_sec();
        Sec ->
            Sec
    end.
now_ms() ->
    case erlang:get('now_ms_cache') of
        undefined ->
            time_server:now_ms();
        Ms ->
            Ms
    end.

set_now_sec(Sec) ->
    erlang:put('now_sec_cache', Sec).
set_now_ms(Ms) ->
    erlang:put('now_ms_cache', Ms).

get_date() ->
    NowSec = util:now_sec(),
    {{NowYear,NowMonth,NowDay},_} =  calendar:gregorian_seconds_to_datetime(NowSec + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}})),
    {NowYear, NowMonth, NowDay}.

cross_day(Sec) ->
    NowSec = util:now_sec(),
    case NowSec > Sec of
        true ->
            {{NowYear,NowMonth,NowDay},_} =  calendar:gregorian_seconds_to_datetime(NowSec + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}})),
            {{Year,Month,Day},_} = calendar:gregorian_seconds_to_datetime(Sec + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}})),
            YearParam = ?IF(NowYear > Year, 1, 0), 
            MonthParam = ?IF(NowMonth > Month, 1, 0),
            DayParam = ?IF(NowDay > Day, 1, 0),
            ?IF(YearParam + MonthParam+DayParam >= 1, true, false);
        false ->
            false
    end.

cross_mon(Sec) ->
    NowSec = util:now_sec(),
    case NowSec > Sec of
        true ->
            {{NowYear,NowMonth,_},_} =  calendar:gregorian_seconds_to_datetime(NowSec + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}})),
            {{Year,Month,_},_} = calendar:gregorian_seconds_to_datetime(Sec + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}})),
            YearParam = ?IF(NowYear > Year, 1, 0), 
            MonthParam = ?IF(NowMonth > Month, 1, 0),
            ?IF(YearParam + MonthParam >= 1, true, false);
        false ->
            false
    end.

cross_week(Sec) ->
    {Date,_} = calendar:gregorian_seconds_to_datetime(Sec + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}})),
    Week = calendar:iso_week_number(Date),
    {NowDate, _Time} = calendar:local_time(),
    NowWeek = calendar:iso_week_number(NowDate),
    ?IF(Week == NowWeek, false, true).

% 时间转时间戳，格式：{{2013,11,13}, {18,0,0}}  
datetime_to_timestamp(DateTime) ->  
    calendar:datetime_to_gregorian_seconds(DateTime) -  
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}}).  

encode_term(Term) ->
    term_to_binary(Term).

decode_term(Bin) when is_binary(Bin) ->
    binary_to_term(Bin).

% recode转成行数据
record_to_row({Key, Val}) when is_integer(Key) ->
    [Key, encode_term(Val)].

%% 一行数据转成record
row_to_record([Key, ValStr]) when is_integer(Key) ->
    Val = decode_term(ValStr),
    {Key, Val}.

%% ip tuple转换成string
ip_ntoa(Ip) ->
    inet_parse:ntoa(Ip).

%% ip string转换成tuple
ip_aton(Ip) ->
    {ok, Addr} = inet_parse:address(Ip),
    Addr.

%% for
for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

%% 简易计时器
timer(Time, Fun) -> proc_lib:spawn(fun() -> receive after Time -> ?MODULE:Fun() end end).

%% 向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%% 向下取整
floor(X) when X < 0 ->   
    T = trunc(X),   
    case (X - T) =:= 0 of       
        true -> T;
        false -> T - 1   
    end;
floor(X) ->    trunc(X).

%% 字符串表达式转term,字符串以"."结尾                             
%% http://www.cnblogs.com/me-sa/archive/2011/12/15/erlang0021.html
expr(Str,Binding) ->                                              
    {ok,Ts,_} = erl_scan:string(Str),                             
    Ts1 =                                                     
    case lists:reverse(Ts) of                                 
        [{dot,_}|_] -> Ts;                                    
        TsR -> lists:reverse([{dot,1} | TsR])                 
    end,                                              
    {ok,Expr} = erl_parse:parse_exprs(Ts1),               
    {value, Term , []} = erl_eval:exprs(Expr, Binding),   
    Term.                                                 

seed() ->
    <<A:32,B:32,C:32>> = crypto:strong_rand_bytes(12),
    random:seed({A, B, C}).

%% [1,N]
rand(0) -> 0;
rand(N) when N > 0 ->
    random:uniform(N).

%% [Min, Max]
rand(Min, Min) ->
    Min;
rand(Min, Max) ->
    rand(Max - Min) + Min.

%% 列表随机N个
rand_from_list(Num, List) ->
    rand_from_list(Num, List, []).
rand_from_list(0, _List, Ret) ->
    Ret;
rand_from_list(_Num, [], Ret) ->
    Ret;
rand_from_list(Num, List, Ret) ->
    Len = length(List),
    N = rand(Len),
    Obj = lists:nth(N, List), 
    rand_from_list(Num - 1, lists:delete(Obj, List), [Obj | Ret]).

%% 检查名字是否合法(不能包含特殊ASCII字符)
is_name_valid(Name) when is_binary(Name) ->
   NameLen = erlang:byte_size(Name),
   case re:run(Name, "[\x{4e00}-\x{9fff}a-zA-Z0-9]+", [unicode]) of
       {match, [{Idx, Len}]} ->
           ?IF(Idx =:= 0 andalso Len =:= NameLen, true, false);
       _ ->
           false
   end.

any_to_iodata(Var) when is_atom(Var)->
    atom_to_list(Var);
any_to_iodata(Var) when is_integer(Var) ->
    integer_to_list(Var, 10);
any_to_iodata(Var) when is_list(Var) ->
    Var;
any_to_iodata(Var) ->
    Var.

any_to_list(Var) when is_atom(Var)->
    atom_to_list(Var);
any_to_list(Var) when is_integer(Var) ->
    integer_to_list(Var, 10);
any_to_list(Var) when is_binary(Var) ->
    binary_to_list(Var);
any_to_list(Var) when is_list(Var) ->
    Var;
any_to_list(Var) ->
    Var.

b2s(<<>>) ->
    " ";
b2s(Bin) when is_binary(Bin) ->
    ?B2S(Bin);
b2s(Bin) ->
    Bin.

s2b([]) ->
    <<" ">>;
s2b(Str) when is_list(Str) ->
    ?S2B(Str);
s2b(Str) ->
    Str.

%% md5 && upper
md5_string_uppercase(Str) ->
    Str2 = lists:flatten([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(Str))]),
    string:to_upper(Str2).

md5_string_lowercase(Str) ->
    Str2 = lists:flatten([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(Str))]),
    string:to_lower(Str2).

%% 将list里的元素拆分成固定长度的tuple列表
-spec split_list_to_tuplelist(list(), integer()) -> list().
split_list_to_tuplelist(L, Size) ->
    split_list_to_tuplelist(L, [], Size).
split_list_to_tuplelist([], Acc, _Size) ->
    Acc;
split_list_to_tuplelist(L, Acc, Size) ->
    {L1, L2} = lists:split(Size, L),
    Tuple = list_to_tuple(L1),
    Acc2 = [Tuple|Acc],
    split_list_to_tuplelist(L2, Acc2, Size).

%% 空操作
none(_A) ->
    ok.
none(_A,_B) ->
    ok.

