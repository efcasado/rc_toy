### Usage

Build 3 Erlang nodes running the `rc_toy` application and start them.
```sh
make devrel; make dev-start
```

Attach to the nodes and create a cluster.
```sh
devrel/rc_toy1/rc_toy1/bin/rc_toy1 attach
```
```erlang
riak_core:join('rc_toy2@127.0.0.1').
```

```sh
devrel/rc_toy3/rc_toy3/bin/rc_toy3 attach
```
```erlang
riak_core:join('rc_toy1@127.0.0.1').
```

Join to one of the nodes and start issuing pings (i.e. `rc_toy:ping()`) randomly.
Look at the at the responses you get. You will note that `pong` replies come from
`vnodes` from all 3 nodes we have deployed in the cluster.


### Useful functions

Join a `riak_core` cluster using `Node` as a bootstrapping node.
```erlang
riak_core:join(Node)
```

```erlang
riak_core_status:ringready()
```

```erlang
riak_core_ring_manager:get_my_ring()
```
