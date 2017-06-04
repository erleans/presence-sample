%%%-------------------------------------------------------------------
%% @doc presence top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(presence_sup).

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
    Port = application:get_env(presence, port, 8080),

    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},

    ElliOpts = [{callback, presence_elli_callback}, {port, Port}],
    ChildSpecs = [#{id => elli_server,
                    start => {elli, start_link, [ElliOpts]},
                    restart => permanent,
                    shutdown => 5000}],

    {ok, {SupFlags, ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
