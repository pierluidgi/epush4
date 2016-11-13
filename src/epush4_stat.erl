%%
%% Push stat module
%% 

%%
%% StatData = [
%%  {{PushCode, Pool}, [{success, N}, {err, N}, ...]::orddict()}
%%


-module(epush4_stat).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([set_stat/1, get_stat/0, get_stat/1]).


start_link() ->
    gen_server:start_link(?MODULE, [], []).

%
init([]) ->
  %erlang:process_flag(trap_exit, true),
  State = #{ data => [] },
  {ok, State}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
handle_info(Msg, S) -> io:format("Unk msg ~p~n", [Msg]), {noreply, S}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%%cast
handle_cast({set_stat, Stat}, S) -> set_stat_(S, Stat);
handle_cast(_Req, S)             -> {noreply, S}.
%%calls
handle_call(_Req, _From, S)      -> {reply, unknown_command, S}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_stat() -> ok.
get_stat(_Name) -> ok.


set_stat(Stat) ->
  gen_server:cast(?MODULE, {set_stat, Stat}).

set_stat_(S = #{data := Data}, _Stat) -> 
  {noreply, S#{data := Data}}.

