%%
%% Send pushes by time according players time zone
%%
%%


-module(epush4_policy_simple_tz).

-include("../../include/epush4.hrl").

-export([timeout/1, add/2, send/1]).


% Send pushes by packs size in 100 pushes
-define(SEND_PUSHES_NUM, 100). 
% Max Time for send pack
-define(CONN_LEASE_TIME, 250000).


%% 
timeout(S = #{state := sent}) ->
  {stop, send_not_sent, S};

%% Check datatime
timeout(S = #{state := free, tokens := Ts, slot_data := SD, push_tags := PT}) when is_list(Ts), Ts /= [] ->
  PD = maps:get(push_data, SD),
  SendTime = maps:get(<<"send_time">>, PD),
  TimeZone = maps:get(<<"tz">>, PT, -5), %% NY time zone by default
  Now = ?now,
  DeltaTZ = TimeZone * 60*60,
  %?INF("timeout TZ", {{SendTime, Now, DeltaTZ}, SendTime =< (Now + DeltaTZ), PD}),
  case SendTime =< (Now + DeltaTZ) of
    true ->
      %?INF("push_tags and slot_data", {PT, SD}),
      gen_server:cast(self(), send),
      {noreply, S#{state := sent}, 500};
    false ->
      {noreply, S, (12 + rand:uniform(6)) * 60 * 1000} %% Idle 12-18 min
  end;
%
timeout(S = #{state := free, tokens := [], last_add := LA}) ->
  case ?now - LA > 1200 of
    true  -> {stop, normal, S};          %% stop idle and shutdown
    false -> {noreply, S, 5 * 60 * 1000} %% Idle
  end;
%
timeout(S = #{state := {conn_timeout, Until}}) ->
  Now = ?now,
  {NewS, Timeout} = case Now >= Until of
    true ->
      gen_server:cast(self(), send),
      {S#{state := sent}, 500};
    false ->
      {S, 1000*(Until - Now)}
  end,
  {noreply, NewS, Timeout}.




add(S = #{tokens := OldTokens, state := State, slot_data := SD, push_tags := PT}, Tokens) ->
  case State of
    sent  ->
      S#{tokens := lists:append(OldTokens, Tokens), last_add := ?now};
    free ->
      PD = maps:get(push_data, SD),
      SendTime = maps:get(<<"send_time">>, PD),
      TimeZone = maps:get(<<"tz">>, PT, -5), %% NY time zone by default
      ?INF("Add simple TZ", {length(Tokens), TimeZone, PT}),
      Now = ?now,
      DeltaTZ = TimeZone * 60 * 60,
      case SendTime =< Now + DeltaTZ of
        true ->
          gen_server:cast(self(), send),
          S#{tokens := lists:append(OldTokens, Tokens), state := sent, last_add := Now};
        false ->
          S#{tokens := lists:append(OldTokens, Tokens), state := free, last_add := Now}
      end;
    {conn_timeout, Until} ->
      Now = ?now,
      case Now > Until of
        true -> 
          gen_server:cast(self(), send),
          S#{tokens := lists:append(OldTokens, Tokens), state := sent, last_add := Now};
        false ->
          S#{tokens := lists:append(OldTokens, Tokens), last_add := Now}
      end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DO SEND
send(S = #{pool      := Pool, 
           platform  := Platform,
           payload   := Payload,
           tokens    := Tokens}) ->
  
  TokenFun = 
    fun(Token) ->
      case Platform of <<"ios">> -> {Token, maps:get(apns_topic, S, u)}; _ -> Token end
    end,

  case get_conn(Pool) of
    {ok, ConnPid} ->
      case get_key(ConnPid, Platform) of
        {ok, Conn} ->
          %?INF("Send to conn", {Platform, Conn, Payload}),
          SendFun = fun
            (Fu, [T|Ts], N, Acc) when N > 0 ->
                case do_send(Platform, Conn, TokenFun(T), Payload) of
                  conn_error  -> {[T|Ts], [conn_error|Acc], err_timeout(conn_error)};
                  Res         -> Fu(Fu, Ts, N-1, [{T, Res}|Acc])
                end;
            (_F, Ts, N, Acc) when Ts == []; N =< 0 -> {Ts, Acc, 0}
          end,
          {NewTokens, ResultAcc, Timeout} = SendFun(SendFun, Tokens, ?SEND_PUSHES_NUM, []),
          ret_conn(Pool, ConnPid),
          manage_response(S, ResultAcc),
          {noreply, S#{state := free, tokens := NewTokens}, Timeout};
        Else -> 
          ?INF("Error", Else),
          {noreply, S#{state := free, tokens := []}, 0}
      end;
    timeout -> 
      %% TODO SOMETHING 
      ?INF("Get Conn timeout!", {self(), err_timeout(conn_timeout)}),
      {noreply, S#{state := free}, err_timeout(conn_timeout)};
    {err,{pool_not_found,_}} -> 
      %% TODO SOMETHING
      ?INF("pool_not_found", pool_not_found),
      {noreply, S#{state := free, tokens := []}, 0}
  end.


do_send(<<"ios">>,      C, T, P) -> epush4_ios:push(C, T, P);
do_send(<<"android">>,  C, T, P) -> epush4_android:push(C, T, P);
do_send(<<"windows">>,  C, T, P) -> epush4_windows:push(C, T, P);
do_send(<<"facebook">>, C, T, P) -> epush4_facebook:push(C, T, P).
%
err_timeout(conn_error)    -> 10000 + random_int(20000);
err_timeout(conn_timeout)  -> 10000 + random_int(20000).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%
manage_response(#{slot := Slot, fmfa := u}, _) -> ?INF("Empty feedback mfa", Slot), ok;
manage_response(_S,  []) -> ok;
manage_response(#{slot      := Slot,
                  platform  := Platform,
                  token_src := Src,
                  fmfa      := {M,F,A}},  Res) ->
  %?INF("Feedback", {M,F,A}),
  FeedbackFun = fun
    (Fu, [{_, ok}|Rs], Acc) -> Fu(Fu, Rs, orddict:update_counter(ok, 1, Acc));
    (Fu, [{_, {ok, Desc}}|Rs], Acc) -> Fu(Fu, Rs, orddict:update_counter({ok, Desc}, 1, Acc));
    (Fu, [{T, R} |Rs], Acc) -> 
      %% TODO add platfrom and token src to feedback
      FeedbackArgs = #{platform => Platform,
                       src      => Src,
                       token    => T,
                       result   => R},
      try erlang:apply(M, F, lists:append(A, [FeedbackArgs]))
      catch E:R -> ?INF("Feedback error", {E,R, {M, F, lists:append(A, [FeedbackArgs])}})
      end,
      case R of
        {err, {{new_token, _}, Desc}} ->  %% rewrite for stat android new token error
          NewR = {err, {new_token, Desc}},
          Fu(Fu, Rs, orddict:update_counter(NewR, 1, Acc));
        _ -> 
          Fu(Fu, Rs, orddict:update_counter(R, 1, Acc))
      end;
    (_F, [], Acc) -> Acc
  end,
  Stat = FeedbackFun(FeedbackFun, Res, orddict:new()),
  epush4_slot:set_stat(Slot, Stat),
  ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Conns
get_conn(Pool) ->
  ers:get_conn(Pool, _LeaseTime = ?CONN_LEASE_TIME, _Timeout = 1500). %% wait for free conection 10sec
ret_conn(Pool, Conn) ->
  ers:ret_conn(Pool, Conn).

      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Keys
get_key(ConnPid, Platform) ->
  case Platform of
    <<"ios">>       -> {ok, ConnPid};
    <<"windows">>   -> gen_server:call(ConnPid, get_key);
    <<"facebook">>  -> gen_server:call(ConnPid, get_key);
    <<"android">>   -> gen_server:call(ConnPid, get_key);
    _               -> ?e(wrong_platfrom)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SYS MISC
random_int(1) -> 1;
random_int(N) -> rand:uniform(N).

