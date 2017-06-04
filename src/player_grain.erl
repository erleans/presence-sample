%%%-------------------------------------------------------------------
%% @doc Player Grain
%% @end
%%%-------------------------------------------------------------------
-module(player_grain).

-behaviour(erleans_grain).

-export([get_current_game/1,
         join_game/2,
         leave_game/2]).

-export([placement/0,
         provider/0]).

-export([init/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         deactivate/1]).

-include_lib("erleans/include/erleans.hrl").

-type state() :: #{current_game := erleans:grain_ref() | undefined}.

-spec get_current_game(erleans:grain_ref()) -> erleans:grain_ref() | undefined.
get_current_game(PlayerGrain) ->
    erleans_grain:call(PlayerGrain, get_current_game).

-spec join_game(erleans:grain_ref(), erleans:grain_ref()) -> ok.
join_game(PlayerGrain, GameGrain) ->
    erleans_grain:call(PlayerGrain, {join_game, GameGrain}).

-spec leave_game(erleans:grain_ref(), erleans:grain_ref()) -> ok.
leave_game(PlayerGrain, GameGrain) ->
    erleans_grain:call(PlayerGrain, {leave_game, GameGrain}).

placement() ->
    prefer_local.

provider() ->
    erleans_config:get(default_provider).

-spec init(erleans:grain_ref(), state() | #{}) -> {ok, state(), #{}}.
init(_GrainRef, State) ->
    {ok, State, #{}}.

handle_call(get_current_game, _From, State=#{current_game := CurrentGame}) ->
    {reply, CurrentGame, State};
handle_call({join_game, GameGrain}, _From, State) ->
    lager:info("Player ~p joined game ~p", [get(grain_ref), GameGrain]),
    {reply, ok, State#{current_game => GameGrain}};
handle_call({leave_game, GameGrain}, _From, State) ->
    lager:info("Player ~p left game ~p", [get(grain_ref), GameGrain]),
    {reply, ok, State#{current_game => undefined}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

deactivate(State) ->
    {save, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
