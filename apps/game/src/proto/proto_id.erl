%% warning: this file is auto-generated.

-module(proto_id).

-compile([export_all]).

proto_id_convert(mod_test) -> 1;
proto_id_convert(1) -> mod_test;
proto_id_convert(mod_test_s2c_heart) -> 260;
proto_id_convert(260) -> mod_test_s2c_heart;
proto_id_convert(mod_test_c2s_heart) -> 259;
proto_id_convert(259) -> mod_test_c2s_heart;
proto_id_convert(mod_test_s2c_login) -> 258;
proto_id_convert(258) -> mod_test_s2c_login;
proto_id_convert(mod_test_c2s_login) -> 257;
proto_id_convert(257) -> mod_test_c2s_login;

proto_id_convert(_Arg) -> io:format("invalid arg:~p", [_Arg]), error(invalid_proto_id_convert).