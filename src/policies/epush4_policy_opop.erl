%%
%% one_push_one_payload policy with generate payload on each send
%%

-module(epush4_policy_opop).

-include("../../include/epush4.hrl").

-export([timeout/1, add/2, send/1]).

-define(CHUNK_TTL, 1000). %% Sec

%
timeout(S = #{state := sent}) ->
  {stop, send_not_sent, S};
%
timeout(S = #{state := free, tokens := Ts}) when is_list(Ts), Ts /= [] ->
  gen_server:cast(self(), send),
  {noreply, S#{state := sent}, 500};
%
timeout(S = #{state := free, tokens := [], last_add := LA}) ->
  case ?now - LA > ?CHUNK_TTL of      %% LA in seconds
    true  -> {stop, normal, S};       %% stop idle and shutdown
    false -> {noreply, S, 20*60*1000} %% Idle
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




add(S = #{slot := _Slot, platform := _Platform, tokens := OldTokens, state := State}, Tokens) ->
  case State of
    sent  ->
      S#{tokens := lists:append(OldTokens, Tokens), last_add := ?now};
    free ->
      gen_server:cast(self(), send),
      S#{tokens := lists:append(OldTokens, Tokens), state := sent, last_add := ?now};
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
           slot_data := SlotData,
           pmfa      := {M,F,A},
           tokens    := Tokens}) ->
  
  TokenFun =
    fun(Token) ->
      case Platform of <<"ios">> -> {Token, maps:get(apns_topic, S, u)}; _ -> Token end
    end,

  case get_conn(Pool) of
    {ok, ConnPid} ->
      case get_key(ConnPid, Platform) of
        {ok, Conn} ->
          SendFun = fun
            (Fu, [T|Ts], N, Acc) when N > 0 ->
                TryPayload = 
                  try erlang:apply(M, F, [T, SlotData])
                  catch E:R -> 
                    ?INF("Payload generate error", {E,R, {M, F, [A, T, SlotData]}}), 
                    ?e(gen_payload_error)
                  end,
                case TryPayload of
                  {ok, Payload} ->
                    case do_send(Platform, Conn, TokenFun(T), Payload) of
                      conn_error  -> {[T|Ts], [conn_error|Acc], err_timeout(conn_error)};
                      Res         -> Fu(Fu, Ts, N-1, [{T, Res}|Acc])
                    end;
                  Else -> Fu(Fu, Ts, N-1, [{T, Else}|Acc])
                end;
            (_F, Ts, N, Acc) when Ts == []; N =< 0 -> {Ts, Acc, 0}
          end,
          {NewTokens, ResultAcc, Timeout} = SendFun(SendFun, Tokens, 1000, []),
          ret_conn(Pool, ConnPid),
          manage_response(S, ResultAcc),
          {noreply, S#{state := free, tokens := NewTokens}, Timeout};
        Else ->
          ?INF("Error", Else),
          {noreply, S#{state := free, tokens := []}, 0}
      end;

    timeout -> 
      %% TODO SOMETHING
      {noreply, S#{state := free}, err_timeout(conn_timeout)};
    {err,{pool_not_found,_}} -> 
      %% TODO SOMETHING
      {noreply, S#{state := free, tokens := []}, 0}
  end.


%%
do_send(<<"ios">>,      Conn, Token, Payload) -> epush4_ios:push(Conn, Token, Payload);
do_send(<<"android">>,  Conn, Token, Payload) -> epush4_android:push(Conn, Token, Payload);
do_send(<<"windows">>,  Conn, Token, Payload) -> epush4_windows:push(Conn, Token, Payload);
do_send(<<"facebook">>, Conn, Token, Payload) -> epush4_facebook:push(Conn, Token, Payload).

%
err_timeout(conn_error)    -> ?now + 50000 + random_int(20000);
err_timeout(conn_timeout)  -> ?now + 50000 + random_int(20000).
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
    (Fu, [{_,   ok}|Rs],           Acc) -> Fu(Fu, Rs, orddict:update_counter(ok, 1, Acc));
    (Fu, [{_,  {ok, Desc}}|Rs],    Acc) -> Fu(Fu, Rs, orddict:update_counter({ok, Desc}, 1, Acc));
    (Fu, [{_, [{ok, Desc}]}|Rs],   Acc) -> Fu(Fu, Rs, orddict:update_counter({ok, Desc}, 1, Acc));
    (Fu, [{_,  [ok]}|Rs],          Acc) -> Fu(Fu, Rs, orddict:update_counter(ok, 1, Acc));
    (Fu, [{T, [{ok, Desc}|R]}|Rs], Acc) -> Fu(Fu, [{T, R}|Rs], orddict:update_counter({ok, Desc}, 1, Acc));
    (Fu, [{T,  [ok|R]}|Rs],        Acc) -> Fu(Fu, [{T, R}|Rs], orddict:update_counter(ok, 1, Acc));
    (Fu, [{T, R} |Rs],             Acc) -> 
      %% TODO add platfrom and token src to feedback
      FeedbackArgs = #{platform => Platform,
                       src      => Src,
                       token    => T,
                       result   => R},
      try erlang:apply(M, F, lists:append(A, [FeedbackArgs]))
      catch FE:FR -> ?INF("Feedback error", {self(), FE,FR, {M, F, lists:append(A, [FeedbackArgs])}})
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
  ers:get_conn(Pool, _LeaseTime = 10000, _Timeout = 500). %% wait for free conection 10sec
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
random_int(N) ->
  rand:seed(exs64, erlang:timestamp()),
  rand:uniform(N).

