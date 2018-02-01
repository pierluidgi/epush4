%%
%% Facebook push notification sending
%% https://developers.facebook.com/docs/games/services/appnotifications
%%

-module(epush4_facebook).

-include("../../include/epush4.hrl").

-export([push/3]).

-define(IBROWSE_OPTIONS(ContentType), [  % Ibrowse options ContentType = "text/html"
          {max_sessions, 1000},
          {max_pipeline_size, 500},
          {connect_timeout, 2000},
          {inactivity_timeout, 4000},
          {content_type, ContentType}]).


push(Key, #{<<"token">> := Token}, Payload) ->
  push(Key, Token, Payload);
push(Key, Token, Payload) ->
  Options  = ?IBROWSE_OPTIONS("text/html"),
  Headers  = [{"Content-Type", "application/x-www-form-urlencoded"}],
  PostData = lists:append(["access_token=", binary_to_list(Key),
                           "&href=index.html?gift_id=123",
                           "&template=", binary_to_list(Payload)]),
  send_message(PostData, Options, Headers, Token).



-define(FACEBOOK_BASEURL, "https://graph.facebook.com/").
-define(FACEBOOK_IBROWSE_SEND_TIMEOUT, 3000). %% msec


%
send_message(PostData, Options, Headers, #{<<"nid">> := Token, <<"nw">> := <<"facebook">>}) ->
  Url = lists:append([?FACEBOOK_BASEURL, binary_to_list(Token), "/notifications"]),
  case ibrowse:send_req(Url, Headers, post, PostData, Options, ?FACEBOOK_IBROWSE_SEND_TIMEOUT) of
    {ok, Status, _RHeaders, Body} -> parse_answer(Status, list_to_binary(Body));
    {error,req_timedout}          -> ?e(timeout);
    Else                          -> ?INF("AAAA", {Url, Else}), ?e(unknown_response_error, Else)
  end;
send_message(PostData, Options, Headers, Token) ->
  Url = lists:append([?FACEBOOK_BASEURL, binary_to_list(Token), "/notifications"]),
  case ibrowse:send_req(Url, Headers, post, PostData, Options, ?FACEBOOK_IBROWSE_SEND_TIMEOUT) of
    {ok, Status, _RHeaders, Body} -> parse_answer(Status, list_to_binary(Body));
    {error,req_timedout}          -> ?e(timeout);
    Else                          -> ?INF("BBBB", {Url, Else}), ?e(unknown_response_error, Else)
  end.


parse_answer(Status, _Body) when Status == "200" -> 
  %?INF("parse_answer Body", Body),
  {ok, ?p};
parse_answer(_Status, Body) ->
  %?INF("parse_answer Status", Status),
  case jsx:is_json(Body) of
    true ->
      case jsx:decode(Body, [return_maps]) of
        #{<<"error">> := Error} -> parse_error(Error);
        Else                    -> ?e(unknown_response_error, Else)
      end; 
    false -> ?e(unknown_response_error)
  end.

parse_error(Error) ->
  case Error of
    #{<<"code">> := 100}         -> ?e(too_long_text);
    #{<<"code">> := 200}         -> ?e(not_registered);
    #{<<"code">> := 803}         -> ?e(not_registered, 803);
    E = #{<<"fbtrace_id">> := _} -> ?e(unknown_response_error, E#{<<"fbtrace_id">> := <<"">>});
    Else                         -> ?e(unknown_response_error, Else)
  end.
