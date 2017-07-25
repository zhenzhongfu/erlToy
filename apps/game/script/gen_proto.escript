#!/usr/bin/env escript
%% ====================================================
%% -*- erlang -*-
%%! -smp enable 
%% 生成protobuf message对应的code/beam
%% TODO 生成的代码太多，还要对erlang-protobuffers的模板进行裁剪
%% ====================================================

-mode(compile).

-define(EXIT(Code), init:stop(Code)).

%% GenType: "code" | "beam"
main([GenType, ProtoDir, OutIncludeDir, OutSrcDir, ProtoEbinDir, OutputEbinDir]) ->
    io:format("rootdir:~p\nProtoDir:~p \nOutIncludeDir:~p\nOutSrcDir:~p\nProtoEbinDir:~p\nOutputEbinDir:~p\n", 
              [root_dir(), ProtoDir, OutIncludeDir, OutSrcDir, ProtoEbinDir, OutputEbinDir]),
    Dir = filename:join([root_dir(), ProtoEbinDir]),
    OutEbinDir = filename:join([root_dir(), OutputEbinDir]), 
    ProtoFiles = filelib:wildcard(filename:join([ProtoDir, "*.proto"])),
    true = code:add_path(Dir), 
    GenFun = fun (ProtoFile, Acc) ->
                            Path = filename:absname(ProtoFile),
                            Message = filename:basename(ProtoFile, ".proto"),
                            io:format("~n=== gen proto message ~p===~n", [Message]),
                            if 
                                % gen source code
                                GenType =:= "code" ->
                                    case catch(protobuffs_compile:generate_source(Path,
                                                     [{imports_dir,
                                                        [filename:join([ProtoDir, "import"])]},
                                                     {output_include_dir, OutIncludeDir}, 
                                                     {output_src_dir, OutSrcDir}
                                                     ])) of
                                    ok ->
                                        Acc andalso true;
                                    Other ->
                                        io:format("gen code: error:~p~n", [Other]),
                                        false
                                    end;

                                % gen beam
                                GenType =:= "beam" ->
                                    case catch(protobuffs_compile:scan_file(Path,
                                                      [{imports_dir,
                                                          [filename:join([ProtoDir, "import"])]},
                                                      {output_include_dir, OutIncludeDir}, 
                                                      {output_ebin_dir, OutEbinDir}])) of
                                    ok ->
                                        Acc andalso true;
                                    Other ->
                                        io:format("gen beam: error:~p~n", [Other]),
                                        false
                                    end;
                                true ->
                                    false
                           end
                    end,

    case lists:foldl(GenFun, true, ProtoFiles) of
      true -> 
        % 生成proto.hrl,include所有的*_pb.hrl
        L = filelib:wildcard(filename:join([OutIncludeDir, "*.hrl"])),
        HeadStr = lists:foldl(
                fun(Name, Acc) -> 
                    BaseName = filename:basename(Name),  
                    Acc 
                    ++
                    io_lib:format("-include(\"proto/~s\").\n", [BaseName])
            end, "%% Warning: this file is auto-generated.\n-ifndef(PROTO_PB_H).\n-define(PROTO_PB_H, true).\n", 
            L),
        HeadStr2 = HeadStr ++ "\n-endif.",
        FileName = filename:join([OutIncludeDir, "../proto_pb.hrl"]),
        file:write_file(FileName, HeadStr2),
        io:format("\n\033\[1;42mSUCCESS\033[0m: gen proto success\n");
      _ -> 
          io:format("\n\033\[1;41mERROR\033[0m:*** One or more property test cases failed\n"),
          ?EXIT(1)
    end;
main(_) ->
    io:format("\033\[1;41m[Usage]\033[0m~s proto_dir include_dir ebin_dir\n", [escript:script_name()]),
    ?EXIT(0).

root_dir() ->
    Path = escript:script_name(),
    filename:dirname(filename:dirname(Path)).
