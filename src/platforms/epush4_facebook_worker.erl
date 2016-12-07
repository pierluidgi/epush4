%%
-module(epush4_facebook_worker).

-behaviou(gen_server).

-export([start/1, stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../../include/epush4.hrl").

-define(TIMEOUT, 10 * 60 * 1000).

start(Args) -> 
  gen_server:start(?MODULE, Args, []).

stop(Pid) ->
  gen_server:stop(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
init(Args) ->
  %?INF("Start", self()),
  {ok, Args}.
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
handle_info(timeout, S)             -> {noreply, S};
handle_info(Msg, S)                 -> io:format("Unk msg ~p~n", [{self(), Msg}]), {noreply, S}.
%%casts
handle_cast(Msg, S)                 -> io:format("Unk msg ~p~n", [{?p, Msg}]), {noreply, S}.
%%calls
handle_call(get_key,_From, S)       -> get_key_(S);
handle_call(Req,_From, S)           -> {reply, {err, unknown_command, ?p(Req)}, S}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
get_key_(S = #{key := Key}) -> {reply, {ok, Key}, S}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

