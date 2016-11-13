%% Genserver for keep epush4 data. 
%% Slots, Chunks, Pools

-module(epush4_data).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([add_pool/1, del_pool/1, stop_pools/0]).

-export([state/0]).

-include("epush4.hrl").

start_link() ->
  ?INF("Epush4 data server start", self()),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #{pools => []}}.
 
terminate(_Reason, #{pools := Pools}) -> 
  %% Stop Pools
  Pools,
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
handle_info(Msg, S) -> io:format("Unk msg ~p~n", [Msg]), {noreply, S}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
%%casts
handle_cast(_Req, S)              -> ?INF("Unknown cast", _Req), {noreply, S}.
%%calls                           
handle_call({add_pool, P}, _F, S) -> add_pool_(S, P);
handle_call({del_pool, P}, _F, S) -> del_pool_(S, P);
handle_call(stop_pools, _F, S)    -> stop_pools_(S);
handle_call(state, _From,  S)     -> {reply, S, S};
handle_call(Req, _From, S)        -> {reply, ?e(unknown_command, Req), S}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

state() ->
  gen_server:call(?MODULE, state).


add_pool(Pool) -> 
  gen_server:call(?MODULE, {add_pool, Pool}).
add_pool_(S = #{pools := Pools}, Pool) ->
  NewPools = [Pool|lists:delete(Pool, Pools)],
  {reply, ok, S#{pools := NewPools}}.


del_pool(Pool) ->
  gen_server:call(?MODULE, {del_pool, Pool}).
del_pool_(S = #{pools := Pools}, Pool) ->
  {reply, ok, S#{pools := lists:delete(Pool, Pools)}}.


stop_pools() ->
  gen_server:call(?MODULE, stop_pools).
stop_pools_(S = #{pools := Pools}) ->
  [ers:stop(Pool) || Pool <- Pools],
  {reply, ok, S#{pools := []}}.


  
