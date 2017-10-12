-module(epush4).

-export([pool/2, slot/2, send/2, sync_send/2]).
-export([t_pool/0, t_slot/0, t_send/0]).


%%
%% Tokens = [binary()] | [#{<<"token">> => Token::binary(), <<"else_push_var1">> := ..., ...}]
%%

send(Tokens, PushTags) -> 
  Md5Fun = fun(Bin) -> base64:encode(crypto:hash(md5, Bin)) end,
  Key = Md5Fun(term_to_binary(PushTags)),
  epush4_chunk:cast(Key, {add, Tokens}, PushTags, force_start).

sync_send(Tokens, PushTags) ->
  Md5Fun = fun(Bin) -> base64:encode(crypto:hash(md5, Bin)) end,
  Key = Md5Fun(term_to_binary(PushTags)),
  Timeout = 50000, %% For wait while chunk doing sending job
  epush4_chunk:call(Key, {add, Tokens}, PushTags, force_start, Timeout).

pool(Pool, PoolData) ->
  epush4_data:add_pool(Pool),
  ers:start(Pool, PoolData).

slot(Slot, SlotData) -> 
  epush4_slot:set_data(Slot, SlotData#{slot => Slot}).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests

%%
t_pool()  ->
  Pool = test_android_push_pool,
  Args = #{
      cnum_min => 3,
      cnum_max => 5,
      free_min => 2,
      conn_start_mfa => {ers_worker_emulator, start, []},
      conn_stop_mfa  => {ers_worker_emulator, stop, []}
    },

  ers:start(Pool, Args).

%%
t_slot() ->
  Slot = <<"test">>,
  %Ios =     #{pool_name     => test_ios_push_pool,
  %            feedback_mfa  => {io, format, ["Ios feedback ~p~n"]}},
  Android = #{pool_name     => test_android_push_pool,
              key           => "test_app_android_key",    %% Take it from your app manager
              feedback_mfa  => {io, format, ["Android feedback ~p~n"]}},

  Data = #{
    %% PushData
    push_data => #{<<"text">> => #{<<"en">> => <<"test">>}},
    %% Platfroms
    platforms => #{%<<"ios">>     => Ios,
                   <<"android">> => Android}
  },
  epush4_slot:set_data(Slot, Data#{slot => Slot}).


%%
t_send() ->

  PushTags = #{<<"lang">> => <<"en">>, <<"platform">> => <<"windows">>, <<"slot">> => <<"test">>},
  %% Token = #{<<"from">> => Id::binary(), Token => Token::binary()} | Token::binary()

  Tokens = [
      <<"https://db5.notify.windows.com/?token=AwYAAAAC71bFbTnCHE1tDM8tfqwn2VnX4mnqNT1bVrCDKccHA55KFRPDq7i7ODnGusQcInSoGNW4HKTdFPR7ZNb3ib9qxWu3MfCktFcJEWAzoRjOOXNBUUeCoegTqM16v%2bYK30o%3d">>
    ],
  

  sync_send(Tokens, PushTags).

