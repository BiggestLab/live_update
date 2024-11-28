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

Move the tarball to a likely location and then untar it. On a real deployment this will likely be `rsync`ed to the target machine in a suitable directory, but for the purposes of this demo, we can `cp` to the `/tmp` folder and work from there.

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

Assuming you didn't keep the v1 release files, you'll need to rebuild v1. Normally this would be done by `git checkout` of the commit tag that was installed, but we have it in a directory, so from the `v1` directory:


```bash
rm -rf _build
rebar3 as prod release
```

Now you would checkout the commit that you wish to use for the next version **keeping the _build directory from v1**. We'll simulate this with the following:

```bash
cd ../v2
rm -rf _build
mv ../v1/_build .
```

Note that the v2 release must include the `ebin/live_update.appup` file (the name is formed using the application name, which in our case is `live_udate`). This is used to describe the modules (`.erl` files) that need to be upgraded.

In order to create the correct tarball we have to also use a rebar3 plugin called `rebar3_appup_plugin` (this goes in the `project_plugins`, see the [./v2/rebar3.config](./v2/rebar3.config) file -- note that this is **not** present in the v1 code). Substitute the correct relx release name, and the correct version number you're upgrading to in the `relup` command.

```bash
rebar3 as prod release
rebar3 as prod appup generate
rebar3 as prod relup -n luex -v 2.0.0
rebar3 as prod tar
```

Now we need to place this in the releases folder in the deployed code. On a real system you'll be using something like `rsync` to push to the node running the old version, but for the demonstration we can use `cp`:

```bash
cp _build/prod/rel/luex/luex-2.0.0.tar.gz /tmp/luex/releases/
```

On the machine we're deploying to we can now unpack the version:

    $ ./bin/luex release 2.0.0
    Release 2.0.0 not found, attempting to unpack releases/luex-2.0.0.tar.gz
    Unpacked successfully: "2.0.0"
    Installed Release: 2.0.0
    Made release permanent: "2.0.0"
    $ ./bin/luex versions
    Installed versions:
    * 2.0.0 permanent
    * 1.0.0 old

The "permanent" version is that one that will be started when the VM is run again. You can go back into the remote console and check that it's upgraded.

    $ ./bin/luex remote_console
    Erlang/OTP 25 [erts-13.2.2.5] [source] [64-bit] [smp:32:32] [ds:32:32:10] [async-threads:1] [jit:ns]

    Eshell V13.2.2.5  (abort with ^G)
    (luex@lowry)1> example_library:foo().
    2
    (luex@lowry)2> counter:current_value().
    2
    (luex@lowry)3> counter:increment().
    ok
    (luex@lowry)4> counter:current_value().
    3

The `counter:current_value().` should print whatever the last value was when you tested the remote console on v1. This shows that the library has upgraded and the gen server has been upgraded without losing the gen server state.
