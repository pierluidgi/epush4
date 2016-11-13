-module(epush4_payload).


-export([payload/3]).

-include("epush4.hrl").


%% TokenVars = #{<<"lang">> => <<"en">>,<<"platform">> => <<"ios">>}},
%% SlotData = #{<<"text">> => #{<<"en">> => <<"AAAA">>}}
%% PushData = []
payload(TokenData = #{<<"platform">> := Platform}, SlotData, PushData) when Platform == <<"ios">> ->
  ios_p(TokenData, SlotData, PushData);
payload(TokenData = #{<<"platform">> := Platform}, SlotData, PushData) when Platform == <<"android">> ->
  android_p(TokenData, SlotData, PushData).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IOS {{{
ios_p(TokenVars, SlotData, _PushVars) ->
  S = #{
    status  => ok,
    lang    => maps:get(<<"lang">>, TokenVars, <<"en">>),
    tz      => maps:get(<<"tz">>,   TokenVars, <<"5">>),
    push    => maps:get(push_data,  SlotData),
    aps     => #{},
    payload => #{}},

  Funs = [
    fun ios_p_alert/1,
    fun ios_p_sound/1,
    fun ios_p_extra/1,
    fun ios_p_paylo/1
  ],

  %% Case recursion tru FunList
  R = case epush4_misc:c_r(Funs, S) of
    #{status := ok, payload := P} -> {ok, jsx:encode(P)};
    #{status := Else} -> Else
  end,
  R.

ios_p_alert(S = #{push := Push, aps := Aps, lang := Lang}) ->
  Alert =
    case Push of
      #{<<"loc-key">> := LK, <<"loc-args">> := LA} ->
        #{<<"loc-key">> => LK, <<"loc-args">> => LA};
      #{<<"text">> := Texts} ->
        Default = maps:get(<<"en">>, Texts),
        maps:get(Lang, Texts, Default)
    end,
  S#{aps := Aps#{<<"alert">> => Alert}}.

ios_p_sound(S = #{push := Push, aps := Aps}) ->
  case Push of
    #{<<"sound">> := Sound} -> S#{aps := Aps#{<<"sound">> => Sound}};
    _ -> S
  end.

ios_p_extra(S= #{push := Push, payload := PayLoad}) ->
  case Push of
    #{<<"extra">> := Extra} -> S#{payload := PayLoad#{<<"extra">> => Extra}};
    _ -> S
  end.

ios_p_paylo(S = #{payload := PayLoad, aps := Aps}) ->
  S#{payload := PayLoad#{<<"aps">> => Aps}}.
%%}}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ANDROID {{{
android_p(TokenVars, SlotData, _PushVars) ->
  S = #{
    status  => ok,
    lang    => maps:get(<<"lang">>, TokenVars, <<"en">>),
    tz      => maps:get(<<"tz">>,   TokenVars, <<"5">>),
    push    => maps:get(push_data,  SlotData),
    msg     => #{},
    payload => #{}},

  Funs = [
    fun android_p_text/1,
    fun android_p_sound/1,
    fun android_p_extra/1,
    fun android_p_paylo/1
  ],

  %% Case recursion tru FunList
  R = case epush4_misc:c_r(Funs, S) of
    #{status := ok, payload := P} -> {ok, P};
    #{status := Else} -> Else
  end,
  R.


android_p_text(S = #{push := Push, lang := Lang, msg := Msg}) ->
  %?INF("android_p_text", Push),
  Text =
    begin
        #{<<"text">> := Texts} = Push,
        Default = maps:get(<<"en">>, Texts),
        maps:get(Lang, Texts, Default)
    end,
  S#{msg := Msg#{<<"text">> => Text}}.

android_p_sound(S = #{push := Push, msg := Msg}) ->
  case Push of
    #{<<"sound">> := Sound} -> S#{Msg := Msg#{<<"sound">> => Sound}};
    _ -> S
  end.

android_p_extra(S= #{push := Push, msg := Msg}) ->
  case Push of
    #{<<"extra">> := Extra} -> S#{msg := Msg#{<<"extra">> => Extra}};
    _ -> S
  end.

android_p_paylo(S = #{payload := PayLoad, msg := Msg}) ->
  S#{payload := PayLoad#{<<"data">> => Msg}}.
%%}}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
