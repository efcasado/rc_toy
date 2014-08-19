%%%=============================================================================
%%% File: rc_toy.erl
%%%
%%% API module for interacting with the rc_toy application.
%%%
%%%
%%% Author: Enrique Fernandez <efcasado@gmail.com>
%%%=============================================================================
-module(rc_toy).


%% API
-export(
   [
    ping/0,
    put/2,
    get/1
   ]).


%% ===================
%%  Macro definitions
%% ===================

-define(MASTER, rc_toy_vnode_master).



%% =====
%%  API
%% =====

ping() ->
    %% Pick a random node
    IdxNode = rnd_node(),

    %% There are three possible ways of sending a command:
    %%   a) command: asynchronous
    %%   b) sync_command: synchronous (blocks the vnode)
    %%   c) sync_spawn_command: synchronous (does not block the vnode)
    riak_core_vnode_master:sync_spawn_command(IdxNode, ping, ?MASTER).

put(Key, Value) ->
    IdxNode = find_node({data, Key}),
    riak_core_vnode_master:sync_spawn_command(IdxNode, {put, Key, Value}, ?MASTER).

get(Key) ->
    IdxNode = find_node({data, Key}),
    riak_core_vnode_master:sync_spawn_command(IdxNode, {get, Key}, ?MASTER).


%% =================
%%  Local functions
%% =================

rnd_node() ->
    Key     = rnd_key(),
    find_node(Key).

rnd_key() ->
    Bucket = base64:encode(crypto:strong_rand_bytes(32)),
    Key    = base64:encode(crypto:strong_rand_bytes(32)),
    {Bucket, Key}.

find_node(Key) ->
    %% Key must be of the {Bucket, Key} type.
    CHash     = riak_core_util:chash_key(Key),
    [IdxNode] = riak_core_apl:get_apl(CHash, 1, rc_toy),
    IdxNode.
