-module(proto).
-include("common.hrl").
-include("proto.hrl").
-include("proto_pb.hrl").

-export([unpack/1, unpack/2, pack/2, pack/3, pack/4]).
-export([get_uniq/0, set_uniq/1, check_uniq/1, clear_uniq/0]).

-define(PREFIX_C2S, "c2s").
-define(PREFIX_S2C, "s2c").

%% 解析protobuf协议
%% 1. uniq只需要前端上传，用于校验
%% 2. 通过cmd找到映射的协议"Module_Message",Message以c2s开头
%% 3. 调用Module:decode
unpack(Packet) ->
    unpack(Packet, ?PREFIX_C2S).
unpack(<<Uniq:?UNIQ_SIZE, Cmd:?CMD_SIZE, Rest/binary>>, Prefix) ->
    ?ALERT("uniq:~p cmd:~p rest:~p~n", [Uniq, Cmd, Rest]),
    case check_uniq(Uniq) of
        ok ->
            try 
                set_uniq(Uniq + 1),
                Proto = proto_id:proto_id_convert(Cmd),
                {Mod, ModPb, Type} = get_mod_and_type(Proto, Prefix),
                case ModPb:decode(Type, Rest) of
                    {error, _Reason} ->
                        ?ERROR("unpack proto error:~p~n", [_Reason]),
                        error(proto_decode_error);
                    {'EXIT', _Reason} ->
                        ?ERROR("unpack proto error:~p~n", [_Reason]),
                        error(proto_decode_error);
                    Record ->
                        % 回调的是业务模块，不是pb部分，所以返回mod
                        {ok, Uniq, Mod, Type, Record}
                end
            catch 
                T:R ->
                    ?ERROR("unpack proto. cmd:~p ~p:~p~n", [Cmd, T, R]),
                    error(unpack_failed)
            end;
        _Other ->
            error(proto_cmd_error)
    end;
unpack(_Packet, _Prefix) ->
    ?ERROR("_Packet:~p~n", [_Packet]),
    error(packet_broken).


%% 编码protobuf协议，返回iolist
%% Proto直接传atom，如demo.s2c_heart,就传demo_s2c_heart
%% Record就是需要传递的数据,一般是record,如果是个record数组,对应的decode要改用delimited_decode
pack(Uniq, Record) ->
    ProtoName = erlang:element(1, Record), 
    pack(Uniq, ProtoName, Record).
pack(Uniq, ProtoName, Record) when is_atom(ProtoName) ->
    pack(Uniq, ProtoName, Record, ?PREFIX_S2C).
pack(Uniq, ProtoName, Record, Prefix) when is_atom(ProtoName) ->
    try
        % 找到mf对应的id作为cmd
        Cmd = proto_id:proto_id_convert(ProtoName),
        % encode
        {_Mod, ModPb, _Type} = get_mod_and_type(ProtoName, Prefix),
        Bin = iolist_to_binary(ModPb:encode(Record)),
        % 校验bin的长度,不能超过0xffffffff-?CMD_SIZE-LEN(4),否则socket option的{packet,4}表示不了
        % 这里size表示打包数据的长度,cmd+bin,用于merge
        Len = erlang:iolist_size(Bin) + 4,
        % 这里减去的是Len的4个字节
        ?ASSERT(Len =< 32#ffffffff - 4),
        {packed, [<<Len:32, Uniq:?UNIQ_SIZE, Cmd:?CMD_SIZE>>, Bin], Len}
    catch 
        T:R ->
            ?ERROR("pack proto ~p:~p ProtoName:~p record:~p~n", [T, R, ProtoName, Record]),
            exit(pack_failed)
    end.

%% UNIQ ID
-define(PROTO_UNIQ_ID, proto_uniq_id).
get_uniq() ->
    get(?PROTO_UNIQ_ID).

set_uniq(Uniq) ->
    % 数是随机给定的
    New = ?IF(Uniq > 65535, 13, Uniq),
    put(?PROTO_UNIQ_ID, New).

check_uniq(Uniq) ->
    case get_uniq() of
        undefined ->
            ok;
        Uniq ->
            ok;
        _Other ->
            %?ERROR("[check uniq]: _Other:~p uniq:~p~n", [_Other, Uniq]),
            %TODO for TEST
            %error
            ok
    end.

clear_uniq() ->
    erase(?PROTO_UNIQ_ID).

%% 获得Module和MessageType的atom
%% e.g. Proto = atom() = demo_c2s_addressbook
%%      Mod = atom() = demo
%%      ModPb = atom() = demo_pb
%%      MessageType = atom() = mod_login_c2s_heart
get_mod_and_type(Proto, Prefix) ->
    % proto字符串
    ProtoLiteral = ?A2S(Proto),

    % function以"c2s_"/"s2c"开头,"_pb"结尾
    StartIdx = string:str(ProtoLiteral, Prefix),
    ModLiteral = string:sub_string(ProtoLiteral, 1, StartIdx-2),

    % 这里要保证拆分出来module和message能正确，以免转换出来的atom爆掉
    Module = 
    case list_to_existing_atom(ModLiteral) of
        badarg ->
            throw({error, module_nexist}),
            ?NONE;
        _M ->
            _M
    end,
    MessageType = Proto,
    % pb的蛋疼后缀
    ModPbLiteral = ModLiteral ++ "_pb",
    ModulePb = ?S2A(ModPbLiteral),
    {Module, ModulePb, MessageType}.
