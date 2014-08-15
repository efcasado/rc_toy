-module(rc_toy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case rc_toy_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register(rc_toy, [{vnode_module, rc_toy_vnode}]),
            ok = riak_core_node_watcher:service_up(rc_toy, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
