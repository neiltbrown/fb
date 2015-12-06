%%%-------------------------------------------------------------------
%% @doc fb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(fb_sup).

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
    CommandBus = {fb_command_bus, {fb_command_bus, start_link, []}, permanent, 1000, worker},
    MatchSup = {fb_match_sup, {fb_match_sup, start_link, []}, permanent, 1000, supervisor},
    {ok, { {one_for_all, 5, 10}, [CommandBus, MatchSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================
