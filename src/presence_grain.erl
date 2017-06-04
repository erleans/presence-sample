%%%-------------------------------------------------------------------
%% @doc Presence Grain
%% @end
%%%-------------------------------------------------------------------
-module(presence_grain).

-behaviour(erleans_grain).

-export([heartbeat/1]).

-export([placement/0]).

-export([init/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         deactivate/1]).

-include_lib("erleans/include/erleans.hrl").

heartbeat(Blob) ->
    GrainRef = erleans:get_grain(?MODULE, presence),
    erleans_grain:call(GrainRef, {heartbeat, Blob}).

placement() ->
    {stateless, 20}.

init(_GrainRef, State) ->
    {ok, State, #{}}.

handle_call({heartbeat, Blob}, _From, State) ->
    Status = handle_heartbeat(Blob),
    {reply, Status, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

deactivate(State) ->
    {save, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_heartbeat(#{game   := GameId,
                   status := Status}) ->
    GameRef = erleans:get_grain(game_grain, GameId),
    game_grain:update_game_status(GameRef, Status);
handle_heartbeat(Blob) when is_binary(Blob) ->
    handle_heartbeat(jsx:decode(Blob, [return_maps, {labels, existing_atom}])).
