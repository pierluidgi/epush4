
-module(epush4_windows_worker).

-behaviour(gen_server).

-export([start/1, stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/epush4.hrl").

-define(TIMEOUT, 10 * 60 * 1000). %% 10min

start(Args) -> 
  gen_server:start(?MODULE, Args, []).

stop(Pid) ->
  gen_server:stop(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
init(Args) ->
  case req_key(Args) of
    {ok, Key, Expire} -> 
      Now = ?now,
      TryExpire = erlang:trunc((Expire/10) * 8),
      FinalExpire = case TryExpire > 600 of
        true  -> Now + TryExpire;
        false -> Now + Expire
      end,
      {ok, Args#{key => Key, expire => FinalExpire}, (FinalExpire - Now) * 1000};
    Else -> {stop, Else}
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
handle_info(Msg, S)                 -> io:format("Unk msg ~p~n", [{self(), Msg}]), {noreply, S, ttl(S)}.
%%casts
handle_cast(Msg, S)                 -> io:format("Unk msg ~p~n", [{?p, Msg}]), {noreply, S, ttl(S)}.
%%calls
handle_call(get_new_key,_From, S)   -> get_new_key(S);
handle_call(get_key,_From, S)       -> get_key(S);
handle_call(Req,_From, S)           -> {reply, {err, unknown_command, ?p(Req)}, S, ttl(S)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



ttl(#{expire := Expire}) -> 
  Now = ?now,
  case Expire - Now of
    Ttl when Ttl =< 0 -> 0;
    Ttl -> Ttl * 1000
  end.


%% 
-define(IBROWSE_OPTIONS(ContentType), [  % Ibrowse options ContentType = "text/html"
          {max_sessions, 1000},
          {max_pipeline_size, 500},
          {connect_timeout, 2000},
          {inactivity_timeout, 4000},
          {ssl_options, [{versions, ['tlsv1.2']}]},
          {content_type, ContentType}]).
-define(SEND_TIMEOUT, 5000).

-define(ACCESS_BASEURL, "https://login.live.com/accesstoken.srf").
-define(SCOPE, "notify.windows.com").

req_key(#{client_id := ClientID, client_key := ClientKey}) ->
  Options = ?IBROWSE_OPTIONS("application/x-www-form-urlencoded"),
  Body =
    lists:append([
      "grant_type=",      "client_credentials",
      "&client_id=",      http_uri:encode(ClientID),
      "&client_secret=",  http_uri:encode(ClientKey),
      "&scope=",          http_uri:encode(?SCOPE)]),
  Headers = [{"Content-Type", "application/x-www-form-urlencoded"},
             {"Content-Length", integer_to_list(length(Body))}],
  case ibrowse:send_req(?ACCESS_BASEURL, Headers, post, Body, Options, ?SEND_TIMEOUT) of
    {ok, "200", _RetHeaders, BodyData} ->
      Res = case jsx:decode(list_to_binary(BodyData), [return_maps]) of
        #{<<"access_token">> := AccessToken,
          <<"expires_in">>   := Expire,
          <<"token_type">>   := <<"bearer">>} ->
            Key = binary_to_list(<<"Bearer ", AccessToken/binary>>),
            {ok, Key, Expire};
        _ -> ?e(wrong_response)
      end,
      %?INF("req_key", Res),
      Res;
    Else ->
      ?e(get_access_fail, Else)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
get_key(S = #{key := Key}) -> {reply, {ok, Key}, S, ttl(S)}.
get_new_key(S) ->
  case req_key(S) of
    {ok, Key, Expire} -> 
      Now = ?now,
      TryExpire = erlang:trunc((Expire/10) * 8),
      FinalExpire = case TryExpire > 600 of
        true  -> Now + TryExpire;
        false -> Now + Expire
      end,
      {reply, {ok, Key}, S#{key := Key, expire := FinalExpire}, FinalExpire - Now};
    Else -> {stop, Else, Else, S}
  end. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
