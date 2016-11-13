%%%-------------------------------------------------------------------
%% @doc epush4 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('epush4_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    Ch = #{id       => epush4_data,
           start    => {epush4_data, start_link, []},
           restart  => permanent,
           shutdown => 10000,
           type     => worker,
           modules  => [epush4_data]},

    {ok, { {one_for_all, 0, 1}, [Ch]} }.

%%====================================================================
%% Internal functions
%%====================================================================
