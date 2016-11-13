%%
-module(epush4_android_worker).

-behaviour(gen_server).

-export([start/1, stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("epush4.hrl").

-define(TIMEOUT, 10 * 60 * 1000).

start(Args) -> 
  gen_server:start(?MODULE, Args, []).

stop(Pid) ->
  gen_server:stop(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
init(Args = #{key := Key}) when is_list(Key) ->
  %?INF("Start", self()),
  {ok, Args, ?TIMEOUT}.
%
terminate(_Reason, _S) -> 
  ok.
%
code_change(_OldVersion, S, _Extra) -> 
  {ok, S, ?TIMEOUT}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
handle_info(timeout, S)             -> {stop, normal, S};
handle_info(Msg, S)                 -> io:format("Unk msg ~p~n", [{self(), Msg}]), {noreply, S, ?TIMEOUT}.
%%casts
handle_cast(Msg, S)                 -> io:format("Unk msg ~p~n", [{?p, Msg}]), {noreply, S, ?TIMEOUT}.
%%calls
handle_call(get_key,_From, S)       -> get_key_(S);
handle_call(Req,_From, S)           -> {reply, {err, unknown_command, ?p(Req)}, S, ?TIMEOUT}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
get_key_(S = #{key := Key}) -> {reply, {ok, Key}, S, ?TIMEOUT}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
