%%
%% Ios push sending
%%


-module(epush4_ios).

-include("epush4.hrl").


-export([push/3]).



push(Conn, #{<<"token">> := Token}, Payload) ->
  push(Conn, Token, Payload);
push(Conn, Token, Payload) ->
  RequestHeaders = [
    {<<":method">>, <<"POST">>}, 
    {<<":path">>,   <<"/3/device/", Token/binary>>}],
  Res = h2_client:sync_request(Conn, RequestHeaders, Payload),
  res_parse(Res).



res_parse({ok, {Headers, [BodyJson]}}) ->
  BodyMap     = jsx:decode(BodyJson, [return_maps]),
  HeadersMap  = maps:from_list(Headers),
  case maps:merge(HeadersMap, BodyMap) of
    #{<<":status">> := <<"200">>} -> ok;
    #{<<":status">> := <<"400">>, <<"reason">> := <<"BadDeviceToken">>} -> {err, {bad_token, ?p}};
    Else ->
      io:format("IOS push result ~p~n", [{?p, Else}]),
      {err, {unknown_error, ?p}}
  end;

%%
res_parse(ElseRes) ->
  io:format("IOS push result ~p~n", [{?p, ElseRes}]),
  {err, {unknown_error, ?p}}.

