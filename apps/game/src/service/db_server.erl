%% redis handler
-module(db_server).
-behaviour(gen_server).

-include("common.hrl").
-include("tab.hrl").

-export([start_link/0, call/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([load/1, save/2]).
-export([convert_role_to_data/1, convert_data_to_role/2]).

-record(state, {db_handler}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Type) ->
    erlang:process_flag(trap_exit, true),
    erlang:process_flag(priority, high),
    {Host, Port, Database, Password} = game_conf:get(redis),
    {ok, C} = eredis:start_link(Host, Port, Database, Password),
    {ok, #state{db_handler = C}}.

handle_call({load, Key}, _From, #state{db_handler = C} = State) ->
    {ok, Values} = eredis:q(C, ["GET", Key]),
    Reply = {ok, Values},
    {reply, Reply, State};
handle_call({save, Key, Values}, _From, #state{db_handler = C} = State) ->
    eredis:q(C, ["SET", Key, Values]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

call(Req) ->
    gen_server:call(?MODULE, Req, ?TIMEOUT_5).
    
%% 
load(Key) ->
    call({load, Key}).

save(Key, Values) ->
    call({save, Key, Values}).

%%
convert_data_to_role(#role{} = Role, Data) ->
    case Data of
        undefined ->
            Role2 = Role;
        _ ->
            Role2 = db_helper:deserialize(Role, Data)
    end,
    Role2.

convert_role_to_data(Role) ->
    db_helper:serialize(Role).
