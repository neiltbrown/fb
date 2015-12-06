-module(fb_match_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_match/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_match(Id) ->
    supervisor:start_link(?MODULE, [Id]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 100, 1},
        [{fb_match, {fb_match, start_link, []},
          transient, brutal_kill, worker, [fb_match]}]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
