
-module(epush4_windows_worker).

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
init(Args) ->
  {ok, Key} = req_key(Args),
  %?INF("Start", self()),
  {ok, Args#{key => Key}, ?TIMEOUT}.
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
handle_call(get_new_key,_From, S)   -> get_new_key(S);
handle_call(get_key,_From, S)       -> get_key(S);
handle_call(Req,_From, S)           -> {reply, {err, unknown_command, ?p(Req)}, S, ?TIMEOUT}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
-define(IBROWSE_OPTIONS(ContentType), [  % Ibrowse options ContentType = "text/html"
          {max_sessions, 1000},
          {max_pipeline_size, 500},
          {connect_timeout, 2000},
          {inactivity_timeout, 4000},
          {content_type, ContentType}]).
-define(SEND_TIMEOUT, 3000).

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
  Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
  case ibrowse:send_req(?ACCESS_BASEURL, Headers, post, Body, Options, ?SEND_TIMEOUT) of
    {ok, "200", _RetHeaders, BodyData} ->
      Json = jsx:decode(list_to_binary(BodyData)),
      {_AccessType, AccessToken} = {
          proplists:get_value(<<"token_type">>, Json),
          proplists:get_value(<<"access_token">>, Json)},
      AuthValue = binary_to_list(<<"Bearer ", AccessToken/binary>>),
      {ok, AuthValue};
    Else ->
      ?INF("Windows access error", Else),
      io:format("~w:~w microsoft err response ~p~n", [?MODULE, ?LINE, Else]),
      ?e(get_access_fail)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
get_key(S = #{key := Key}) -> {reply, {ok, Key}, S, ?TIMEOUT}.
get_new_key(S) ->
  case req_key(S) of
    {ok, Key} -> {reply, {ok, Key}, S#{key := Key}, ?TIMEOUT};
    Else      -> {stop, Else, Else, S}
  end. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
