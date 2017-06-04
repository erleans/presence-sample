%%%-------------------------------------------------------------------
%% @doc Game Grain
%% @end
%%%-------------------------------------------------------------------
-module(game_grain).

-behaviour(erleans_grain).

-export([update_game_status/2]).

-export([placement/0,
         provider/0]).

-export([init/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         deactivate/1]).

-include_lib("erleans/include/erleans.hrl").

-type state() :: #{status  := #{players := sets:set(),
                                score   := string()},
                   players := sets:set()}.

update_game_status(GameRef, Status) ->
    erleans_grain:call(GameRef, {update_game_status, Status}).

placement() ->
    prefer_local.

provider() ->
    erleans_config:get(default_provider).

-spec init(erleans:grain_ref(), state() | #{}) -> {ok, state(), #{}}.
init(_GrainRef, State=#{players := _}) ->
    {ok, State, #{}};
init(_GrainRef, _) ->
    {ok, #{status => #{players => sets:new(),
                       score => ""},
           players => sets:new()}, #{}}.

handle_call({update_game_status, Status=#{players := PlayersUpdate}}, _From, State=#{players := Players}) ->
    State1 = State#{status => Status},
    PlayersUpdateSet = sets:from_list(PlayersUpdate),

    %% Check for new players that joined since last update
    NewPlayers = sets:subtract(PlayersUpdateSet, Players),
    Players1 = lists:foldl(fun(Player, Acc) ->
                               try
                                   ok = player_grain:join_game(erleans:get_grain(player_grain, Player), get(grain_ref)),
                                   sets:add_element(Player, Acc)
                               catch
                                   C:T ->
                                       lager:error("failure to join ~p ~p ~p", [C, T]),
                                       Acc
                               end
                           end, Players, sets:to_list(NewPlayers)),

    %% Check for players that left since last update
    LeftPlayers = sets:subtract(Players, PlayersUpdateSet),
    Players2 =  lists:foldl(fun(Player, Acc) ->
                                try
                                    ok = player_grain:leave_game(erleans:get_grain(player_grain, Player), get(grain_ref)),
                                    sets:del_element(Player, Acc)
                                catch
                                    C:T ->
                                        lager:error("failure to leave ~p ~p", [C, T]),
                                        Acc
                                end
                            end, Players1, sets:to_list(LeftPlayers)),

    {reply, ok, State1#{players => Players2}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

deactivate(State) ->
    {save, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
