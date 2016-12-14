%%
%% Ios push sending
%%


-module(epush4_ios).

-include("../../include/epush4.hrl").


-export([push/3]).



push(Conn, #{<<"token">> := Token}, Payload) ->
  push(Conn, Token, Payload);
push(Conn, Token, Payload) ->
  RequestHeaders = [
    {<<":method">>, <<"POST">>}, 
    {<<":path">>,   <<"/3/device/", Token/binary>>}],
  Res = h2_client:sync_request(Conn, RequestHeaders, Payload),
  res_parse(Res).



res_parse({ok, {Headers, Body}}) ->
  BodyMap    = case Body of [] -> #{}; [BodyJson] -> jsx:decode(BodyJson, [return_maps]) end,
  HeadersMap = maps:from_list(Headers),
  case maps:merge(HeadersMap, BodyMap) of
    #{<<":status">> := <<"200">>} -> {ok, ?p};
    #{<<":status">> := <<"410">>} -> ?e(not_registered);
    Else ->
      ?INF("IOS push error:", Else),
      ?e(unknown_error)
  end;

%%
res_parse(ElseRes) ->
  ?INF("IOS push error:", ElseRes),
  ?e(unknown_error).

