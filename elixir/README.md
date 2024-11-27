# Elixir upgrades

* [Rebar3 releases](https://www.rebar3.org/docs/deployment/releases/)
  - [relflow](https://github.com/RJ/relflow/blob/master/README.md)
  - [erlup](https://github.com/soranoba/erlup)
* [systools:make_relup](https://www.erlang.org/doc/apps/sasl/systools#make_relup/3)
* [Appup cookbook](https://www.erlang.org/doc/system/appup_cookbook.html)
* [Release handling: installing](https://www.erlang.org/doc/system/release_handling#installing-a-release)
* [sasl relup](https://www.erlang.org/doc/apps/sasl/relup)


## Deploy v1

From the `elixir/v1` folder, build a tarball for deployment:

```bash
rebar3 as prod tar
```

Move the tarball to a likely location and then untar it.

```bash
cp _build/prod/rel/luex/luex-1.0.0.tar.gz /tmp/
cd /tmp/
mkdir luex
cd luex
tar xvf ../luex-1.0.0.tar.gz
./bin/luex foreground
```

The terminal should display some information to show that it is running.

    $ ./bin/luex foreground
    Exec: /tmp/luex/erts-13.2.2.5/bin/erlexec -noinput +Bd -boot /tmp/luex/releases/1.0.0/start -mode embedded -boot_var SYSTEM_LIB_DIR /tmp/luex/lib -config /tmp/luex/releases/1.0.0/sys.config -args_file /tmp/luex/releases/1.0.0/vm.args -- foreground
    Root: /tmp/luex
    /tmp/luex

We can connect to the running system from another terminal:

```bash
cd /tmp/luex
./bin/luex remote_console
```

From this shell we can try out the functions in the example code:

    (luex@lowry)1> example_library:foo().
    1
    (luex@lowry)2> counter:current_value().
    0
    (luex@lowry)3> counter:increment().
    ok
    (luex@lowry)4> counter:current_value().
    1

Use ctrl-c twice to break out.


## Upgrade to v2

From the `elixir/v2` folder build a new tarball as before:


```bash
rebar3 as prod tar
```

Now we need to place this in the releases folder in the deployed code. On a real system you'll be using something like `rsync` to push to the node running the old version, but for the demonstration we can use `cp`:

```bash
cp _build/prod/rel/luex/luex-2.0.0.tar.gz /tmp/luex/releases/
```

On the machine we're deploying to we can now unpack the version:

    $ ./bin/luex unpack 2.0.0
    Release 2.0.0 not found, attempting to unpack releases/luex-2.0.0.tar.gz
    Unpacked successfully: "2.0.0"
    $ ./bin/luex versions
    Installed versions:
    * 2.0.0 unpacked
    * 1.0.0 permanent

The "permanent" version is that one that will be started when the VM is run again. You can go back into the remote console and check that it's still running the old version.

    Attempting to do the upgrade just leads to errors though:
    $ ./bin/luex upgrade 2.0.0
    Release 2.0.0 not found, attempting to unpack releases/luex-2.0.0.tar.gz
    Unpacked successfully: "2.0.0"
    ERROR: release_handler:install_release failed: {enoent,
                                                    "/tmp/luex/releases/1.0.0/relup"}

It doesn't make sense for it to be looking for a relup file in 1.0.0, because at the point of doing the 1.0.0 we don't know how the upgrade will work, so the appup file must be in the 2.0.0 code.
