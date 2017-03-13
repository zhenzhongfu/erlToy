-module(role_sup).
-include("common.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> 
    ?DEBUG("module ~p start~n", [?MODULE]),
    {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    {ok, Sup}.

init(_Args) ->
    {ok, {{one_for_one, 10, 10}, []}}.
