%%
%% gen_server with ets table with push channels data
%% i.e. pool, pushdata, etc
%%

-module(epush4_slot).

-behaviour(gen_server).

-export([start_link/1, stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([get_data/1, set_data/2, get_date/1]).
-export([get_stat/1, set_stat/2, get_and_flush_stat/1]).

-include("../include/epush4.hrl").


%
start_link(_Args = #{start := true, init := Init, args := Args}) ->
  Slot = maps:get(slot, Args),
  %% Try to load profile
  case get_saved_data(Slot) of
    {ok, Data} -> gen_server:start_link(?MODULE, [Slot, Data], []);
    not_exists ->
      case Init of
        true ->
          %% Init profile
          case init_data(Slot, Args) of
            {ok, Data} -> gen_server:start_link(?MODULE, [Slot, Data], []);
            Else          -> Else
          end;
        false -> not_exists
      end;
    Else -> Else
  end;
start_link(#{start := false}) ->
  not_started;
start_link(_) ->
  {err, {wrong_args, <<"wrong_args">>}}.

stop(Slot) ->
  call(Slot, stop, info).
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get datas
get_saved_data(_Slot) -> 
  not_exists.
%
init_data(Slot, SlotArgs) ->
  SlotData = #{
    slot => Slot, 
    from => ?p, 
    data => SlotArgs, 
    stat => orddict:new(), 
    policy => maps:get(<<"policy">>, SlotArgs, <<"simple">>),
    date => ?now},
  {ok, SlotData}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%
init([Slot, Data]) ->
  ?INF("Start", Slot),
  {ok, Data}.

%
terminate(_Reason, _S) -> 
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
handle_info(Msg, S) -> 
  ?INF("Unk msg:", Msg),
  case S of #{timeout := Timeout} -> {noreply, S, Timeout}; _ -> {noreply, S} end.
code_change(_OldVersion, S, _E) -> 
  case S of #{timeout := Timeout} -> {ok, S, Timeout}; _ -> {ok, S} end.
%%casts
handle_cast(Msg, S) -> 
  ?INF("Unk msg:", Msg),
  case S of #{timeout := Timeout} -> {noreply, S, Timeout}; _ -> {noreply, S} end.
%%calls
handle_call({set, Data}, _F, S)         -> set_(S, Data);
handle_call(get, _F, S)                 -> get_(S);
handle_call(get_date, _F, S)             -> get_date_(S);
handle_call({set_stat, AddStat}, _F, S) -> set_stat_(S, AddStat);
handle_call(get_stat, _F, S)            -> get_stat_(S);
handle_call(get_and_flush_stat, _F, S)  -> get_and_flush_stat_(S);
handle_call(stop, _F, S)                -> {stop, normal, ok, S};
handle_call(Req,_From, S) -> 
  Reply = {err, unknown_command, ?p(Req)},
  case S of #{timeout := Timeout} -> {reply, Reply, S, Timeout}; _ -> {reply, Reply, S} end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(OPT(Mode, Args),
  case Mode of
    force_create -> #{args => Args, start => true,  init => true};
    force_start  -> #{args => Args, start => true,  init => false};
    info         -> #{args => Args, start => false, init => false}
  end).


%-define(SLOT_CLUSTER, push_slot_proceses_cluster).


call(Slot, Msg, Mode) ->
  Args = #{slot => Slot},
  ecld:call(?SLOT_CLUSTER, Slot, Msg, ?OPT(Mode, Args)).



get_data(Slot) ->
  call(Slot, get, info).
get_(S = #{data := Data}) when is_map(Data) ->
  case S of 
    #{timeout := Timeout} -> {reply, Data#{pid => self()}, S, Timeout}; 
    _                     -> {reply, Data#{pid => self()}, S} 
  end;
get_(S) ->
  case S of 
    #{timeout := Timeout} -> {reply, ?e(no_data), S, Timeout}; 
    _                     -> {reply, ?e(no_data), S} 
  end.


get_date(Slot) ->
  call(Slot, get_date, info).
get_date_(S) ->
  Date = maps:get(date, S, 0),
  case S of 
    #{timeout := Timeout} -> {reply, {ok, Date}, S, Timeout}; 
    _                     -> {reply, {ok, Date}, S} 
  end.



set_data(Slot, Data) ->
  call(Slot, {set, Data}, force_create).
set_(S, Data) ->
  case S of
    #{timeout := Timeout} -> {reply, ok, S#{data := Data, date := ?now}, Timeout};
    _                     -> {reply, ok, S#{data := Data, date := ?now}}
  end.


set_stat(Slot, AddStat) ->
  call(Slot, {set_stat, AddStat}, force_start).
set_stat_(S = #{stat := Stat}, AddStat) ->
  Fun = fun(K, V, Acc) -> orddict:update_counter(K, V, Acc) end,
  NewStat = 
    try 
      orddict:fold(Fun, Stat, AddStat)
    catch
      E:R -> 
        ?INF("Set stat error", {E,R, Stat, AddStat}),
        Stat
    end,
  case S of
    #{timeout := Timeout} -> {reply, ok, S#{stat := NewStat}, Timeout};
    _                     -> {reply, ok, S#{stat := NewStat}}
  end.

get_stat(Slot) ->
  call(Slot, get_stat, force_start).
get_stat_(S = #{stat := Stat}) ->
  case S of 
    #{timeout := Timeout} -> {reply, {ok, Stat}, S, Timeout};
    _                     -> {reply, {ok, Stat}, S} 
  end.     


get_and_flush_stat(Slot) ->
 call(Slot, get_and_flush_stat, force_start).
get_and_flush_stat_(S = #{stat := Stat}) ->
  case S of  
    #{timeout := Timeout} -> {reply, Stat, S#{stat := orddict:new()}, Timeout};
    _                     -> {reply, Stat, S#{stat := orddict:new()}}                  
  end.
