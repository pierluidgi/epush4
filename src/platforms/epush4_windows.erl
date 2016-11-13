%%
%% Windows push sending
%%

%% DOCS
%% http://developer.android.com/google/gcm/gcm.html              %% Old
%% https://firebase.google.com/docs/cloud-messaging/send-message %% New
%% Old and new a same except the URL

-module(epush4_windows).

-include("epush4.hrl").

-export([push/4]).



%% 
-define(IBROWSE_OPTIONS(ContentType), [  % Ibrowse options ContentType = "text/html"
          {max_sessions, 1000},
          {max_pipeline_size, 500},
          {connect_timeout, 2000},
          {inactivity_timeout, 4000},
          {content_type, ContentType}]).
-define(SEND_TIMEOUT, 5000).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% push
push(Conn, Token, Payload, Key) ->
  FunS = #{
    status   => ok,
    conn     => Conn,
    key      => Key,
    payload  => Payload,
    token    => Token,
    err      => ?e(unknown),
    tries    => 2,
    res      => []},
  FunList = [
    fun push_check_tries/1,
    fun push_key/1,
    fun push_send/1
  ],

  %% Case recursion tru FunList
  #{status := Status, key := NewKey} = epush4_misc:c_r(FunList, FunS),
  {Status, NewKey}.

push_check_tries(FunS = #{tries := Tries}) when Tries > 0 -> FunS#{tries := Tries - 1};
push_check_tries(FunS = #{err   := Err}) -> FunS#{status := Err}.

push_key(FunS = #{conn := Conn, key := Key}) when Key == empty; Key == wrong_key ->
  Cmd = case Key of empty -> get_key; wrong_key -> get_new_key end,
  case gen_server:call(Conn, Cmd) of
    {ok, Key} -> FunS#{key    := Key};
    Else      -> FunS#{status := Else}
  end;
push_key(FunS) -> FunS.

push_send(FunS = #{key := Key, payload := Payload, token := Token}) ->
  Options = ?IBROWSE_OPTIONS("application/json"),
  Headers = [
    {"Authorization", Key},
    {"Content-Type",  "text/xml"},
    {"X-WNS-Type",    "wns/toast"}, % X-WNS-Type: wns/toast | wns/badge | wns/tile | wns/raw
    {"X-WNS-RequestForStatus", "true"}],
  
  try
    case ibrowse:send_req(binary_to_list(Token), Headers, post, Payload, Options, 3000) of
      {ok, "200", _RetHeaders, _BodyData} -> FunS;
      {ok, "401", _RetHeaders, _BodyData} -> push_check_tries(FunS#{key := wrong_key, err := ?e(wrong_key)});
      {ok, "410", _RetHeaders, _BodyData} -> FunS#{status := ?e(not_registered)};
      {ok, Status, RetHeaders,  BodyData} ->
          ?INF("Windows push error", {?p, {Status, RetHeaders, BodyData}}),
          FunS#{status := ?e(unknown_response_error)};
      Else ->
        ?INF("Windows push error", {?p, Else}),
        FunS#{status := ?e(unknown_response_error)}
    end
  catch
    E:R ->
      ?INF("Windows crash request", {E,R, Token, Payload}),
      FunS#{status := ?e(unknown_response_error)}
  end.
