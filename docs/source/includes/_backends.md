# Backends

lhc supports a handfule of backends, with more future ones planned.

Current backends are:

* [httpc](http://erlang.org/doc/man/httpc.html)
* [lhttpc](http://github.com/billosys/lhttpc)
* [dlhttpc](https://github.com/ferd/dlhttpc)

Planned backends currently include the following:

* [hackney](https://github.com/benoitc/hackney)
* [ibrowse](https://github.com/cmullaparthi/ibrowse)
* [shotgun](https://github.com/inaka/shotgun)
* [fusco](https://github.com/esl/fusco)

## Selecting

> Edit the backend value in the ``lhc`` section of your ``lfe.config`` file.
> See the lhc ``lfe.config`` for an example of this. Once edited, running
> ``start`` will use that value:

```lfe
> (lhc:start)
(#(inets ok) #(ssl ok) #(lhttpc ok) #(lhc ok))
> (lhc:get-backend)
lhttpc
>
```

> You can also start lhc with your preferred backend:

```lfe
> (lhc:start 'httpc)
(#(inets ok) #(ssl ok) #(lhc ok))
>
```

> You can change backends at any time:

```lfe
> (lhc:change-backend 'lhttpc)
(#(backend #(previous httpc) #(current lhttpc)))
>
```

Bakends my be selected one of three ways:

 * setting the ``backend`` value in the ``lhc`` section of the ``lfe.config``
   file
 * Starting lhc with a particular bacend.
 * Changing the backend after lhc has been started.

<aside class="danger">
If you wish to use a backend, you <strong>must</strong> have the desired HTTP
client library installed, as well as all of its dependencies. Attempting to
start a backend that hasn't been installed (or whose libraries are not present
in <code>ERL_LIBS</code>) will result in errors upon startup.
</aside>

## Backend Information

> Find out which backend you are using:

```lfe
> (lhc:get-backend)
lhttpc
>
```

> Find out which backend is configured in your ``lfe.config`` file:

```lfe
> (lhc:get-backend-cfg)
lhttpc
```

> Find out which module holds the backend functions:

```lfe
> (lhc:get-backend-module)
lhc-backend
```

Several convenience functions are provided in support of lhc backends.

See examples to the right.
