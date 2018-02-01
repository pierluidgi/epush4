%%
%% Ios push sending
%%


-module(epush4_ios).

-include("../../include/epush4.hrl").


-export([push/3]).


push(Conn, {#{<<"token">> := Token}, ApnsTopic}, Payload) ->
  push(Conn, {Token, ApnsTopic}, Payload);
%
push(Conn, {Token, u}, Payload) ->
  RequestHeaders = [
    {<<":method">>, <<"POST">>}, 
    {<<":path">>,   <<"/3/device/", Token/binary>>}],
  %?INF("IOS push without apns:", RequestHeaders),
  Res = h2_client:sync_request(Conn, RequestHeaders, Payload),
  res_parse(Res);
push(Conn, {Token, ApnsTopic}, Payload) ->
  RequestHeaders = [
    {<<":method">>,    <<"POST">>}, 
    {<<":path">>,      <<"/3/device/", Token/binary>>},
    {<<"apns-topic">>, ApnsTopic}],
  %?INF("IOS push with apns:", RequestHeaders),
  Res = h2_client:sync_request(Conn, RequestHeaders, Payload),
  res_parse(Res).



res_parse({ok, {Headers, Body}}) ->
  BodyMap    = case Body of [] -> #{}; [BodyJson] -> jsx:decode(BodyJson, [return_maps]) end,
  HeadersMap = maps:from_list(Headers),
  case maps:merge(HeadersMap, BodyMap) of
    #{<<":status">> := <<"200">>} -> {ok, ?p};
    #{<<":status">> := <<"400">>,
      <<"reason">>  := _Reason} -> ?e(not_registered); %?e(err_400, _Reason);
    %% 400 The apns-topic header of the request was not specified and was
    %% required. The apns-topic header is mandatory when the client is connected
    %% using a certificate that supports multiple topics.
    %% ReasonMissingTopic = "MissingTopic"
    %% #{<<":status">> := <<"400">>,
    #{<<":status">> := <<"410">>} -> ?e(not_registered);
    Else ->
      ?INF("IOS push error:", Else),
      ?e(unknown_error)
  end;

%%
res_parse(ElseRes) ->
  ?INF("IOS push error:", ElseRes),
  ?e(unknown_error).

