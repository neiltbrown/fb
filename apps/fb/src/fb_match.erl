-module(fb_match).

-behaviour(gen_server).

-include("fb_match.hrl").

%% API
-export([start_link/1, apply_command/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {match_id, event_manager, event_mgr_ref}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Id) ->
    gen_server:start_link({local, {?SERVER, Id}}, ?MODULE, Id, []).

apply_command(MatchRef, Command) ->
    gen_server:cast(MatchRef, Command).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Id) ->
    {ok, EventMgr} = gen_event:start_link(),
    EventMgrRef = erlang:monitor(process, EventMgr),
    {ok, #state{match_id      = Id,
                event_manager = EventMgr,
                event_mgr_ref = EventMgrRef}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(#command{id = Id, type = create_match, data = _Data}, State) ->
    MatchCreatedEvt = #match_created{id = Id,
                                     home = "H",
                                     away = "A",
                                     start_time = calendar:universal_time()},
    apply_event(State#state.event_manager, MatchCreatedEvt),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_event(EventManager, Event) ->
    ok = store(Event),
    self ! Event,
    ok = gen_event:notify(EventManager, Event).

store(_Event) ->
    % store some event
    ok.
