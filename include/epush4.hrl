


-define(SLOT_CLUSTER,  push_slot_proceses_cluster).
-define(CHUNK_CLUSTER, push_proceses_cluster).



%% IF
-define(IF(Cond, TrueVal, FalseVal), case Cond of true -> TrueVal; false -> FalseVal end).
% NOW time in seconds
-define(now, erlang:system_time(seconds)).


%% Point
-define(p,      list_to_binary(io_lib:format("Mod:~w line:~w", [?MODULE,?LINE]))).
-define(p(Reason), list_to_binary(io_lib:format("Mod:~w line:~w ~100P", [?MODULE,?LINE, Reason, 300]))).

-define(e(ErrCode), {err, {ErrCode, ?p}}).
-define(e(ErrCode, Reason), {err, {ErrCode, ?p(Reason)}}).

%% Log messages
-define(INF(Str, Term), io:format("EPUSH4 INFO LOG: ~p:~p ~p ~100P~n", [?MODULE, ?LINE, Str, Term, 300])).





-define(PUSH_TPL, #{
    <<"name">>      => <<"push_tpl">>,
    <<"text">>      => <<"u">>,
    <<"templates">> => <<"u">>,
    <<"locales">>   => <<"u">>,
    <<"sound">>     => <<"u">>,
    <<"extra">>     => <<"u">>,
    <<"mfa">> =>    => <<"u">>        %% MFA for generate PUSH from PUSH_TPL and PUSH_TOKEN_DATA
  }).

-define(PUSH, #{
    <<"name">>      => <<"push">>,
    <<"token">>     => <<"u">>,
    <<"payload">>   => <<"u">>
  }).


-define(PUSH_TO, #{
    <<"name">>      => <<"push_to">>,
    <<"nid">>       => <<"u">>,
    <<"nw">>        => <<"u">>,
    <<"mfa">>       => <<"u">>          %% MFA to resolve token and token data
  }).

-define(PUSH_TOKEN_DATA, {
    <<"name">>      => <<"push_token_data">>,
    <<"lang">>      => <<"en">>,
    <<"tz">>        => 0 %% TODO US tz
  }).
