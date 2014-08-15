%%%=============================================================================
%%% File: rc_toy_vnode.erl
%%%
%%% Toy riak_core vnode implementation.
%%%
%%% It responds to ping queries with  information about the partition and the
%%% node of the vnode who handled the request.
%%%
%%%
%%% Author: Enrique Fernandez <efcasado@gmail.com>
%%%=============================================================================
-module(rc_toy_vnode).

-behaviour(riak_core_vnode).

%% API
-export(
   [
    start_vnode/1
   ]).

%% riak_core_vnode callbacks
-export(
  [
   init/1,
   terminate/2,
   handle_exit/3,
   handle_command/3,
   handle_coverage/4,
   is_empty/1,
   delete/1,
   handoff_starting/2,
   handoff_cancelled/1,
   encode_handoff_item/2,
   handle_handoff_data/2,
   handle_handoff_command/3,
   handoff_finished/2
  ]).


%% ====================
%%  Record definitions
%% ====================

-record(state,
        {
          partition
        }).



%% =====
%%  API
%% =====

start_vnode(I) ->
    riak_core_vnode_master:start_vnode(I, ?MODULE).


%% =====================
%%  riak_core callbacks
%% =====================

%%-----------------%%
%% basic callbacks %%
%%-----------------%%

%% Initialize the state of the vnode.
init([Partition]) ->
    {ok, #state{ partition = Partition }}.

%% Used to clean up any resources held by the vnode.
terminate(_Reason, _State) ->
    ok.

%% This callback is called when a process linked to the vnode dies.
%% Pid is refers to the crashed process.
%% Two possibilities:
%%   a) {stop, NewState} if the crashed process is crucial for
%%      the vnode.
%%   b) {noreply, NewState} if the crashed process is not vital
%%      for the vnode.
handle_exit(Pid, Reason, State) ->
    ok.

%% All incoming requests targeting a vnode end up here. Similar to
%% gen_server's handle_call/3.
handle_command(ping, _From, #state{partition = Partition} = State) ->
    Node = node(),
    {reply, {pong, {Partition, Node}}, State};
handle_command(_Request, _From, State) ->
    {noreply, State}.

%% TO-DO: handle_coverage/4
handle_coverage(_Request, _KeySpaces, _From, State) ->
    {stop, not_implemented, State}.

%%---------------------------%%
%% handoff-related callbacks %%
%%---------------------------%%

%% Used to determine if there is any data that needs to be
%% transfered. When a vnode is deemed empty, the delete/3
%% callback is called.
is_empty(State) ->
    {true, State}.

%% Used to perform a preemtive cleanup of vnode resources.
delete(State) ->
    {ok, State}.

%% Called by riak_core when it determines that a handoff
%% must occur. Return true to continue and false to cancel.
handoff_starting(TargetNode, State) ->
    {true, State}.

%% Called when a handoff is cancelled. Used this callback
%% to undo changes made in handoff_starting/2.
handoff_cancelled(State) ->
    {ok, State}.

%% Used to encode the data before sending it to the target
%% node. Considerations:
%%   a) K and V encoded together
%%   b) must return a binary
%%
%% see handle_handoff_data/2
encode_handoff_item(K, V) ->
    term_to_binary({K, V}).

%% Used to reconstruct a vnode's state from the 'BinObj'.
handle_handoff_data(_BinObj, State) ->
    {reply, ok, State}.

%% Similar to handle_command/3 but invoked when a command
%% is received during a handoff.
%% Two additional return types can be used:
%%   a) forward: will send the request to the target node.
%%   b) drop: similar to noreply
handle_handoff_command(Request, _From, State) ->
    {drop, State}.

%% Called when all data has been successfully handed off to
%% the target node.
handoff_finished(TargetNode, State) ->
    ok.
