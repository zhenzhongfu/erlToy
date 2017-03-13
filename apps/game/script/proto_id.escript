#!/usr/bin/env escript
%% -*- erlang -*- 

%% 为节约协议长度，将protobuf的映射字符串转成id
%% package_id,message_id 长度分别是8,解析demo.proto应该生成如下项:
%%   demo=3
%%   demo.c2s_addressbook=769     % 3:8,1:8
%% 这样能支持255个package

-mode(compile).

main([ProtoDir, TxtOutDir0, ErlOutDir0]) ->
    RootDir = root_dir(),
    ProtoFiles = filelib:wildcard(filename:join([RootDir, ProtoDir, "*.proto"])),
    TxtOutDir = filename:join([RootDir, TxtOutDir0]),
    ErlOutDir = filename:join([RootDir, ErlOutDir0]),
    try 
        % txt
        FileName = filename:join([TxtOutDir, "proto_id.txt"]),
        {ok, TxtFd} = file:open(FileName, write),

        % erl
        FileName2 = filename:join([ErlOutDir, "proto_id.erl"]),
        {ok, ErlFd} = file:open(FileName2, write),
        write(ErlFd, "%% warning: this file is auto-generated.\n\n"),
        write(ErlFd, "-module(proto_id).\n\n"),
        write(ErlFd, "-compile([export_all]).\n\n"),

        init_package_info(),

        [begin 
                    parse_proto(TxtFd, ErlFd, File),
                    write(TxtFd, "\n"),
                    write(ErlFd, "\n")
            end || File <- ProtoFiles],

        get_package_info(TxtOutDir),

        % txt
        file:close(TxtFd),

        % erl
        write(ErlFd, "proto_id_convert(_Arg) -> io:format(\"invalid arg:~p\", [_Arg]), error(invalid_proto_id_convert)."),
        file:close(ErlFd),
        io:format("\n\033\[1;42mSUCCESS\033[0m: gen proto_id.erl success.\n")
    catch
        T:R ->
            io:format("\n\033\[1;41mERROR\033[0m: gen proto_id.erl failed. ~p:~p\n", [T, R]),
            error
    end.

root_dir() ->                                                  
    Path = escript:script_name(),                              
    filename:dirname(filename:dirname(Path)).

write(Fd, Info) ->
    file:write(Fd, Info).

parse_proto(TxtFd, ErlFd, ProtoFile) ->
    % 取得注释中的id定义
    {ok, Fd} = file:open(ProtoFile, read),
    Desc = get_desc(Fd, []),
    ok = file:close(Fd),

    % 描述转term
    L = lists:foldl(
        fun(Str, Acc) ->
            Term = expr(Str, []),
            [Term | Acc]
        end, [], Desc),

    % TODO
    % 1. 按tuple读package和message，生成k=v这样的写入txt文件.这里还要校验下package的编号与其他文件的编号是否有冲突
    % 2. get(k) -> v. get(v) -> k. 生成erl代码写入文件

    % package
    case gen_package_prop(L) of
        {{TxtContext1, ErlContext1}, PackageName, PackageId, Rest} ->
            % message
            {TxtContext2, ErlContext2} = gen_message_prop(Rest, PackageId, PackageName),

            TxtContext = TxtContext1 ++ TxtContext2,
            write(TxtFd, TxtContext),

            ErlContext = ErlContext1 ++ ErlContext2,
            file:write(ErlFd, ErlContext),
            io:format("\n\033\[1;42mSUCCESS\033[0m: gen id_proto success. file:~p\n", [ProtoFile]),
            ok;
        false ->
            io:format("\n\033\[1;41mERROR\033[0m: gen id_proto failed. file:~p\n", [ProtoFile]),
            error
    end,
    ok.

%% 按行解析文件,得到proto注释里的自定义id描述
get_desc(Fd, Acc) ->
    case io:get_line(Fd,"") of
        eof ->
            Acc;
        Line ->
            Head = 
            case catch string:sub_string(Line, 3, 10) of
                {'EXIT', _} -> 
                    "";
                Other ->
                    Other
            end,
            Acc2 = 
            case Head =:= "{package" orelse Head =:= "{message" of
                true ->
                    Line2 = string:sub_string(Line, 3),
                    Line3 = string:strip(Line2, right, $\n),
                    [Line3 | Acc];
                false ->
                    Acc
            end,
            get_desc(Fd, Acc2)
    end.

%% 生成package的配置项
%% 需要在message之前生成，message_id基于package_id
gen_package_prop(L) when is_list(L) ->
    case catch lists:keytake(package, 1, L) of
        {value, Tuple, Rest} ->
            {package, PropName, PropId} = Tuple,
            % 抽package的value出来排序
            cache_package_info({PropName, PropId}), 
            TxtNewLine = convert_txt_newline(PropName, PropId),
            ErlNewLine = convert_erl_newline(PropName, PropId),
            %io:format("******** TxtLine:~p ErlNewLine:~p Rest:~p~n", 
            %    [TxtNewLine, ErlNewLine, Rest]),
            {{TxtNewLine, ErlNewLine}, PropName, PropId, Rest};
        _Other ->
            false
    end.

%% 生成message的配置项,txt的文本项和erl的文本项
gen_message_prop(L, BaseId, BaseName) when is_list(L) and is_integer(BaseId) ->
    lists:foldl(
        fun
            ({message, PropName, PropId}, {TxtAcc, ErlAcc}) ->
                PropId2 = convert_propid(PropId, BaseId),
                TxtNewLine = convert_txt_newline(PropName, PropId2, BaseName),
                %ErlNewLine = convert_erl_newline(PropName, PropId2, BaseName),
                ErlNewLine = convert_erl_newline(PropName, PropId2),
                {TxtNewLine ++ TxtAcc, ErlNewLine ++ ErlAcc};
            (_Other, {TxtAcc, ErlAcc}) ->
                {TxtAcc, ErlAcc}
        end, {"", ""}, L).

%%--------------------------------------------------------
%%
%%--------------------------------------------------------

%% 根据package_id生成message_id
convert_propid(PropId, BaseId) ->
    <<PropId2:16>> = <<BaseId:8, PropId:8>>,
    PropId2.

%% package=id
convert_txt_newline(PropName, PropId) ->
    atom_to_list(PropName) ++ "=" ++ integer_to_list(PropId) ++ "\n".
%% package.message=id
convert_txt_newline(PropName, PropId, BaseName) ->
    atom_to_list(BaseName) ++ "." ++ atom_to_list(PropName) ++ "=" ++ integer_to_list(PropId) ++ "\n".

%% proto_id_convert(id) -> msg
%% proto_id_convert(msg) -> id 
convert_erl_newline(PropName, PropId) ->
    "proto_id_convert(" ++ atom_to_list(PropName) ++ ") -> " ++ integer_to_list(PropId) ++ ";\n"
    ++
    "proto_id_convert(" ++ integer_to_list(PropId) ++ ") -> " ++ atom_to_list(PropName) ++ ";\n".

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

%% term转字符表达式
%term_to_expr(Term) ->
%    lists:flatten(io_lib:write(Term)).

%%-------------------------
%%将package id写到单独的文件，方便命名时查看id顺延
init_package_info() ->
    erlang:put(package_info, []).

cache_package_info(Value) ->
    L = erlang:get(package_info),
    L2 = [Value | L],
    erlang:put(package_info, L2).

get_package_info(Dir) ->
    L = lists:keysort(2, erlang:get(package_info)),
    File = filename:join([Dir, "tmpid.txt"]),
    {ok, Fd} = file:open(File, write),
    [
     begin
        write(Fd, atom_to_list(Mod) ++ "=" ++ integer_to_list(V)++"\n")
     end ||
        {Mod, V} <- L
    ],
    file:close(Fd).
%%--------------------------
