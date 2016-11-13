%%
%% Send chunk gen server
%% https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/ApplePushService.html
%%


-module(epush4_chunk).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([call/4, cast/4, cast/5]).

-export([state/1]).

-include("epush4.hrl").


-define(TIMEOUT, 3*60 + random_int(50)).  %% + random for smooth
-define(ITTL, 1 * 1 * 20 * 1000).        %% Idle process time to live




start_link(#{start := true, args := Args = #{slot := Slot, 
                                             token_vars := #{<<"platform">> := Platform}}}) ->
  ?INF("start_link", {Slot, Platform}),
  gen_server:start_link(?MODULE, Args, []);
%
start_link(#{start := false}) ->
  not_started;
%
start_link(_) ->
  {err, {wrong_args, <<"wrong_args">>}}.




init(Args) ->
  %erlang:process_flag(trap_exit, true),
  Slot        = maps:get(slot, Args),
  TokenVars   = maps:get(token_vars, Args),
  PushVars    = maps:get(push_vars, Args),
  Platform    = maps:get(<<"platform">>, TokenVars),
  

  SlotData    = epush4_slot:get_data(Slot),
  Feedback    = maps:get(feedback, SlotData, u),
  PlatformData = case SlotData of
    #{platforms := #{Platform := Value}} -> Value;
    _ -> erlang:error(wrong_platform)
  end,
  Pool        = maps:get(pool_name, PlatformData),
  
  %?INF("slot data", SlotData),

  Policy  = maps:get(policy, Args, #{<<"name">> => <<"simple">>}),
  Payload = case epush4_payload:payload(TokenVars, SlotData, PushVars) of
    {ok, Value1} -> Value1;
    Else -> erlang:error(Else)
  end,

  Now        = ?now, 
  Timeout    = ?TIMEOUT,
  S = #{
    slot        => Slot,
    token_src   => maps:get(<<"token_src">>, SlotData, <<"db">>),
    pool        => Pool,
    policy      => Policy,    %% send policy for example  #{tz := 4, try_num := 3, send_rate := exponent};
    state       => free,      %% if command send is sent
    platform    => Platform,
    last_add    => ?now,
    until       => Now + Timeout,
    tokens      => [],
    payload     => Payload,
    fmfa        => Feedback,  %% {M,F,A} for feedback
    stat        => [],
    log         => []
  },
  {ok, S, Timeout}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
handle_info(timeout, State) -> timeout_(State);
handle_info(Msg, S) -> io:format("Unk msg ~p~n", [Msg]), {noreply, S, 0}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
%%casts
handle_cast(send, S)              -> send_(S);
handle_cast(_Req, S)              -> 
  ?INF("Unknown cast", _Req),
  {noreply, S, 0}.
%%calls                           
handle_call({add, Args},_From, S) -> add_(S, Args);
handle_call(ping,  _From,  S)     -> {reply, {pong, self()},  S, 0};
handle_call(log,   _From,  S)     -> {reply, log_(S),         S, 0};
handle_call(state, _From,  S)     -> {reply, state_(S),       S, 0};
handle_call({set_state, S},_F, _) -> {reply, ok,              S, 0};
%% Sys calls                       
%                                  
handle_call(Req, _From, S)        -> {reply, {err, unknown_command, ?p(Req)}, S, 0}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Call, Cast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Options = #{
%%    start => true, %% Start process if not exists
%%    init  => true  %% Init profile if not exists in database
%% }. 
-define(OPT(Mode, Args),
  case Mode of
    force_create -> #{args => Args, start => true,  init => true};
    force_start  -> #{args => Args, start => true,  init => false};
    info         -> #{args => Args, start => false, init => false}
  end).

%-define(CHUNK_CLUSTER, push_proceses_cluster).

%
call(Key, Msg, Args, Mode) ->
  ecld:call(?CHUNK_CLUSTER, Key, Msg, ?OPT(Mode, Args)).
%
cast(Key, Msg, Args, Mode) ->
  spawn(fun() -> ecld:call(?CHUNK_CLUSTER, Key, Msg, ?OPT(Mode, Args)) end).
%
cast(Key, Msg, Args, Mode, Delay) ->
  spawn(fun() -> timer:sleep(Delay), ecld:call(?CHUNK_CLUSTER, Key, Msg, ?OPT(Mode, Args)) end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%











%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SYS MISC
random_int(1) -> 1;
random_int(N) ->
  {A,B,C} = erlang:timestamp(),
  random:seed(A,B,C),
  random:uniform(N).

log_(_S = #{log := Log}) -> Log.
state(Key) ->
  ecld:call(?CHUNK_CLUSTER, Key, state, ?OPT(info, empty_args)).
state_(S = #{tokens := Tokens}) -> S#{tokens := length(Tokens)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server api
%% @doc Token = binary() || #{progress := Progress}
%% Progress = map() #{tries := 1, last_try := Time}
%
add_(S = #{policy := #{<<"name">> := PName}}, Msg) when PName == <<"simple">> ->
  NewS = epush4_policy_simple:add(S, Msg),
  {reply, ok, NewS, 500}.

%
send_(S = #{policy := #{<<"name">> := PName}}) when PName == <<"simple">> ->
  epush4_policy_simple:send(S).

%
timeout_(S = #{policy := #{<<"name">> := PName}}) when PName == <<"simple">> ->
  epush4_policy_simple:timeout(S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

