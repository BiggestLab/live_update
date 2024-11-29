# Connecting Erlang systems together

Connecting between VMs on the same host seems somewhat problematic, but it might be possible using different localhost addresses.

When setting up you need to decide if you're using "short names" (host names on their own) or "fully qualified names" (host names with a domain). Once one is picked then all machines seem to need to use the same option.

There is also a security token for the running VM which is often called "cookie", but other places called "token". Although many pages on the Internet say you need the same token throughout, this isn't true.

BEAM is BEAM, but Erlang is not Elixir.


## Erlang

Start using the `-sname` option to give the instance a short name. This is a short name and means that all connections must be between VMs started with the short name. The same goes for fully qualified names -- those can only connect to VMs started with fully qualified names.

It's very easy to end up in a position where the connections aren't possilbe. Use `is_alive/0` to make sure things are running. The usual connection you'll see is `net_adm:ping/1`, but it will always just print `pang` no matter what. Use `net_kernel:connect_node/1` instead (`net_kernel:connect_node('idfe@lowry').`). It will report `ignored` if the node isn't alive (for some reason this is not documented anywhere) and it will return `false` if the connection can't be established (presumably networking related).

A cookie for a specific node can be set using `erlang:set_cookie(Node2, DiffCookie)`, e.g. `erlang:set_cookie('idfe@lowry', 'QAEVKAFZBIMGYIYTIRFG').`. The default cookie can be found the user's home directory at `~/.elrang.cookie`.

The documentation about distributed Erlang is a bit confusing. The important point is that both nodes using the _same cookie_ for each other. The simplest way of doing this is to use `set_cookie` on each machine to set a cookie for the other.

On idfe@lowry:

    > erlang:set_cookie('idfe@indecline', 'idfecookie').
    true

And on idfe@indecline:

    > erlang:set_cookie('idfe@lowry', 'idfecookie').
    true

Note that `idfecookie` is not the default cookie on either node. The connection can now be initiated from either side. E.g. on `idfe@indecline`:

    > net_kernel:connect_node('idfe@lowry').
    true


## Elixir

With `iex` use the options `--name` or `--sname` together with `--cookie` to set the name and the cookie. The command `Node.alive?()` will tell you if the network is set up properly. To connect the `Node.connect/1` function can be used, but note that the atom syntax is different: `Node.connect(:'idfe@lowry')`. `Node.list/0` can be used to list the connections between nodes.

