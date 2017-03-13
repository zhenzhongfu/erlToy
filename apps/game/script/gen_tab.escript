#!/usr/bin/env escript
%% ====================================================
%% -*- erlang -*-
%%! -smp enable 
%% 根据tab.conf生成各个tab的record定义
%% ====================================================

-mode(compile).

-define(ERROR(D), io:format("[error]" ++ D ++"\n")).
-define(ERROR(F, D), io:format("[error]" ++ F ++"\n", D)).

-define(HEAD_FILE, "tab.hrl").
-define(HELPER_FILE, "db_helper.erl").

-record(col, {name, type, sync, def, loop}).

main([ConfFile, HrlDir, ErlDir]) ->
    {ok, List} = file:consult(ConfFile),
    TabList = convert_col(List, []),
    % 生成头文件
    gen_head_file(TabList, HrlDir),
    % 生成序列化操作文件
    gen_helper_file(TabList, ErlDir),
    ok;
main(_) ->
    ?ERROR("usage: ~s 配置文件 头文件路径 代码路径", [escript:script_name()]).

%% 列转化
convert_col([{db_mod, _}|T], Acc) ->
    convert_col(T, Acc);
convert_col([{Tab, FieldList}|T], Acc) ->
    convert_col([{Tab, FieldList, undefined}|T], Acc);
convert_col([{Tab, FieldList, CallBack}|T], Acc) ->
    FieldList2 = 
    lists:map(
        fun(Field) ->
            case Field of
                {l, Name, Type} -> Sync = true, Loop = true, Def = [];
                {l, Name, Type, Sync} -> Loop = true, Def = [];
                {l, Name, Type, Sync, Def} -> Loop = true;
                {Name, Type} -> Sync = true, Loop = false, Def = get_default_by_type(Type);
                {Name, Type, Sync} -> Loop = false, Def = get_default_by_type(Type);
                {Name, Type, Sync, Def} -> Loop = false
            end,
            #col{name = Name, type = Type, sync = Sync, def = Def, loop = Loop}
        end, FieldList),
    convert_col(T, [{Tab, FieldList2, CallBack} | Acc]);
convert_col([_|T], Acc) ->
    convert_col(T, Acc);
convert_col([], Acc) ->
    Acc.

%% 生成头文件
gen_head_file([], _OutDir) ->
    ok;
gen_head_file(TabList, OutDir) ->
    gen_head_file(TabList, OutDir, []).
gen_head_file([{Tab, FieldList, _CallBack}|T], OutDir, Acc) ->
    FieldList2 = [io_lib:format("~p = ~p", [Name, Def]) || #col{name = Name, def = Def} <- FieldList],
    FieldStr = string:join(FieldList2, ", \n    "),
    Str = io_lib:format("-record(~p, {\n    ~s}).\n\n", [Tab, FieldStr]),
    gen_head_file(T, OutDir, [Str|Acc]);

gen_head_file([], OutDir, Acc) ->
    HeadStr = get_head_str(),
    FileName = filename:join([OutDir, ?HEAD_FILE]),
    ok = file:write_file(FileName, [unicode:characters_to_binary(HeadStr)] ++ Acc),
    ok.

get_default_by_type(int) ->
    0;
get_default_by_type(bool) ->
    false;
get_default_by_type(string) ->
    <<>>;
get_default_by_type(_) ->
    undefined.

get_head_str() ->
    "%% 自动生成，请勿编辑 \n". 

gen_helper_file(TabList, OutDir) ->
    HeadStr = get_head_str(),
    Prefix = "-module(db_helper).\
-include(\"common.hrl\"). \
-include(\"tab.hrl\").    \
-export([serialize/1, deserialize/2]).  \
serialize(Role) ->                      \ 
    term_to_binary(Role).               \
deserialize(Role, Data) ->              \
    Role0 = binary_to_term(Data),       \
    #role{",

    BodyStr = 
    case lists:keyfind(role, 1, TabList) of
        false ->
            "} = Role0, \
            Role2 = Role0, \
            Role2.";
        {role, FieldList, _} ->
            StrL = [io_lib:format("~p = ~p", [Name, string:to_upper(erlang:atom_to_list(Name))]) || #col{name = Name, sync = Sync} <- FieldList, Sync =:= true],
            Fun = fun(S, Acc) ->
                          lists:concat([S, ",", Acc])
                  end,
            lists:foldr(Fun, "", StrL)
    end,
    BodyStr2 = re:replace(BodyStr, "\"", "", [global, {return, list}]),
    BodyStr3 = lists:droplast(BodyStr2),

    Affix = "} = Role0, \
    Role2 = Role#role{",
    Affix2 = "}, \
    Role2.",
    Str = lists:concat([Prefix, BodyStr3, Affix, BodyStr3, Affix2]),

    FileName = filename:join([OutDir, ?HELPER_FILE]),
    ok = file:write_file(FileName, [unicode:characters_to_binary(HeadStr)] ++ Str).
