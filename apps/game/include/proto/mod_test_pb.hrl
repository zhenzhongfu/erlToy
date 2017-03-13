-ifndef(MOD_TEST_C2S_LOGIN_PB_H).
-define(MOD_TEST_C2S_LOGIN_PB_H, true).
-record(mod_test_c2s_login, {
    id = 0,
    cmd,
    name,
    list = []
}).
-endif.

-ifndef(MOD_TEST_S2C_OP1_PB_H).
-define(MOD_TEST_S2C_OP1_PB_H, true).
-record(mod_test_s2c_op1, {
    code
}).
-endif.

-ifndef(MOD_TEST_C2S_HEART_PB_H).
-define(MOD_TEST_C2S_HEART_PB_H, true).
-record(mod_test_c2s_heart, {
    
}).
-endif.

-ifndef(MOD_TEST_S2C_HEART_PB_H).
-define(MOD_TEST_S2C_HEART_PB_H, true).
-record(mod_test_s2c_heart, {
    time
}).
-endif.

