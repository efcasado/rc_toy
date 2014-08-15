-module(rc_toy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(VNODE_MASTER(Name, Module),
        {Name, {riak_core_vnode_master, start_link, [Module]}, permanent, 5000,
         worker, [Module]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?VNODE_MASTER(rc_toy_vnode_master, rc_toy_vnode)]} }.
