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

-include("../../include/epush4.hrl").

-define(TIMEOUT, 10 * 60 * 1000).

start(Args) -> 
  gen_server:start(?MODULE, Args, []).

stop(Pid) ->
  gen_server:stop(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
init(#{key := {Url, {M,F,A}}}) when is_list(Url) ->
  %?INF("Start", self()),
  case apply(M,F,A) of
    {ok, #{access_token := AccessToken, ttl := Ttl}} -> 
      erlang:send_after(erlang:round((Ttl*0.5)*1000), self(), update_access_token),
      {ok, #{access_token => AccessToken, url => Url, mfa => {M,F,A}} };
    Else -> ?e(init_error, Else)
  end.

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
handle_info(update_access_token, S) -> update_access_token_(S);
handle_info(Msg, S)                 -> io:format("Unk msg ~p~n", [{self(), Msg}]), {noreply, S, ?TIMEOUT}.
%%casts
handle_cast(Msg, S)                 -> io:format("Unk msg ~p~n", [{?p, Msg}]), {noreply, S, ?TIMEOUT}.
%%calls
handle_call(get_key, _From, S)      -> get_key_(S);
handle_call(Req,_From, S)           -> {reply, {err, unknown_command, ?p(Req)}, S, ?TIMEOUT}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
get_key_(S = #{url := Url, access_token := Key}) -> {reply, {ok, {Url, Key}}, S, ?TIMEOUT}.

update_access_token_(S = #{mfa := {M,F,A}}) ->
  Res = 
    try 
      apply(M,F,A)
    catch 
      E:R -> ?e(E, {R, erlang:get_stacktrace()})
    end,
  
  NewS = 
    case Res of
      {ok, #{access_token := AccessToken, ttl := Ttl}} ->
        erlang:send_after(erlang:round((Ttl*0.8)*1000), self(), update_access_token),
        S#{access_token => AccessToken};
      Else -> 
        erlang:send_after(5000, self(), update_access_token),
        ?INF("Update access token error", Else), S
  end,
  {noreply, NewS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
