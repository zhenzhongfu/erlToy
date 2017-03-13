%% http client

-module(myhttpc).
-include("common.hrl").

-export([get/1, get/2, post/2, post/3]).
-define(TIMEOUT, 2000).

-spec get(Url :: string()) ->
    {'ok', any()} | {'error', any()}.
get(Url) ->
    get(Url, ?TIMEOUT).

get(Url, Timeout) ->
    HttpOpts = [{timeout, Timeout}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    httpc:request(get, {Url, []}, HttpOpts, Opts).

-spec post(Url :: string(), Data :: iodata()) ->
    {'ok', any()} | {'error', any()}.
post(Url, Data) ->
    post(Url, "application/x-www-form-urlencoded", Data).

-spec post(Url :: string(), Type :: string(), Data :: iodata()) ->
    {'ok', any()} | {'error', any()}.
post(Url, Type, Data) ->
    HttpOpts = [{timeout, ?TIMEOUT}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    httpc:request(post, {Url, [], Type, iolist_to_binary(Data)}, HttpOpts, Opts).

