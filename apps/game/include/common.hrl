-ifndef(COMMON_H).
-define(COMMON_H, true).

-include("log_server.hrl").

-define(MAIN_APP, game).
-define(NODE, ?A2S(node())).

%% 读配置
-define(CONFIG(K), game_conf:get(K)).
-define(CONFIG(K, D), game_conf:get(K, D)).

-ifndef(IF).
-define(IF(C, T, F), (case (C) of true -> (T); false -> (F) end)).
-endif.

%% 断言
-ifndef(ASSERT).
-define(ASSERT(Condition), (?IF(Condition, ok, error({assert, Condition})))).
-endif.

%% 数据结构转换
-define(B2S(B), (binary_to_list(B))).
-define(S2B(S), (list_to_binary(S))).
-define(N2S(N), integer_to_list(N)).
-define(S2N(S), list_to_integer(S)).
-define(N2B(N), ?S2B(integer_to_list(N))).
-define(B2N(B), list_to_integer(?B2S(B))).
-define(A2S(A), atom_to_list(A)).
-define(S2A(S), list_to_atom(S)).
-define(S2EA(S), list_to_existing_atom(S)).
-define(T2B(T), term_to_binary(T)).
-define(B2T(B), binary_to_term(B)).

%% other
-define(PRINT(Format, Args), io:format(Format, Args)).
-define(C2SERR(R), throw({error, R})).
-define(NONE, none).

%% 超时时间
-define(TIMEOUT_5, 5000).

%% 心跳检测时间
-define(HEART_CHECK_TIME, 5000).
-define(CHECK_HEART_EVENT, 'check_heart_event').

%% 属性列表操作
-define(KV_GET(K, L), proplists:get_value(K, L)).
-define(KV_GET(K, L, Def), proplists:get_value(K, L, Def)).

%% 
-define(SEND(Role, Msg), role_server:cache_packet(Role, Msg)).

-endif.
