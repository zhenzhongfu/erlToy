-ifndef(HTTP_H).
-define(HTTP_H, true).

-define(API_KEY, "test1234").

%% json返回
-define(SUCC, [{<<"ret">>, <<"0">>}, {<<"msg">>, <<"success">>}]).
-define(SUCC_PARAM(Param), [{<<"ret">>, <<"0">>}, {<<"msg">>, http_api:to_binary(Param)}]).
-define(PARAM_ERROR(Param), [{<<"ret">>, <<"100">>}, {<<"msg">>, http_api:to_binary(Param)}]).
-define(TIME_EXPIRED, [{<<"ret">>, <<"101">>}, {<<"msg">>, <<"time_expired">>}]).
-define(FLAG_ERROR, [{<<"ret">>, <<"200">>}, {<<"msg">>, <<"flag_erorr">>}]).
-define(FAILED, <<"failed">>).

-define(PERCENT, 37).
-define(FULLSTOP, 46).
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

%%%%%%%%%
-define(HTTP_RETURN_JSON(Data), {json, Data}).
-define(HTTP_GET_PATH(Req), Req:get(path)).
-define(HTTP_GET_STR(Key, Qs), http_api:get_str(Key, Qs)).
-define(HTTP_GET_STR_RAW(Key, Qs), http_api:get_str_raw(Key, Qs)).
-define(HTTP_GET_INT(Key, Qs), http_api:get_int(Key, Qs)).
-define(HTTP_GET_BINARY(Key, Qs), http_api:get_binary(Key, Qs)).


-endif.
