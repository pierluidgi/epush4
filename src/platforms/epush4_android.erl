%%
%% Android push sending
%%

%% DOCS
%% http://developer.android.com/google/gcm/gcm.html              %% Old
%% https://firebase.google.com/docs/cloud-messaging/send-message %% New
%% Old and new a same except the URL

-module(epush4_android).

-include("../../include/epush4.hrl").

-export([push/3]).



%% 
-define(URL, "https://gcm-http.googleapis.com/gcm/send").  %% Old
%-define(URL, "https://fcm.googleapis.com/fcm/send").        %% New
-define(SEND_TIMEOUT, 5000).


-define(IBROWSE_OPTIONS(ContentType), [  % Ibrowse options ContentType = "text/html"
          {max_sessions, 1000},
          {max_pipeline_size, 500},
          {connect_timeout, 2000},
          {inactivity_timeout, 4000},
          {content_type, ContentType}]).




push(Key, #{<<"token">> := Token}, Payload) ->
  push(Key, Token, Payload);
push(Key, Token, Payload) ->
  %io:format("~w:~w Msg ~p~n", [?MODULE, ?LINE, Msg]),
  Options = ?IBROWSE_OPTIONS("application/json"),
  Headers = [
    {"Authorization", lists:append("key=", Key)},
    {"Content-Type", "application/json"}],
  AndroidMsg = jsx:encode(Payload#{<<"to">> => Token}),
  send_message(Options, Headers, Token, AndroidMsg).


%
send_message(Options, Headers, Token, AndroidMsg) ->
  case ibrowse:send_req(?URL, Headers, post, AndroidMsg, Options, ?SEND_TIMEOUT) of
    {ok, "200", _RetHeaders, BodyData} ->
      parse_answer(list_to_binary(BodyData));
    {ok, "401", _RetHeaders, _BodyData} -> ?e(invalid_key);
    {error,req_timedout} -> ?e(timeout);
    Else                 -> ?INF("Error google response!", {Token, Else, AndroidMsg}), ?e(unknown)
  end.




parse_answer(Json) ->

  ErrFun =
    fun%%
       (#{<<"message_id">> := _, 
          <<"registration_id">> := NewToken})        -> ?e({new_token, NewToken});
       (#{<<"message_id">> := _})                    -> ok;
       (#{<<"error">> := <<"Unavailable">>})         -> ?e(service_unavailable);   
       (#{<<"error">> := <<"InvalidRegistration">>}) -> ?e(invalid_registration);  %% Drop this error
       (#{<<"error">> := <<"NotRegistered">>})       -> ?e(not_registered);        %% Delete token
       (#{<<"error">> := <<"MismatchSenderId">>})    -> ?e(wrong_app_key);         %% Drop this error
       (Else)                                        -> ?e(unknown_response_error, Else) %% Drop unknown error
    end,

  case jsx:is_json(Json) of
    true ->
      case jsx:decode(Json, [return_maps]) of
        #{<<"results">> := [Result]} -> ErrFun(Result);
        _ -> ?e(unparseble_response)
      end;
    false -> ?e(unparseble_response)
  end.

