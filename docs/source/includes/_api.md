# The API

Each of the API functions that map to an HTTP verb have at least one arity
that supports setting lhc options which are passed to ``lhc:request``.
These options may have zero or more of the following:

* ``return`` - return type. Can be one of ``body`` (default), ``status``,
  ``headers`` or ``all``
* ``callback`` - the function that gets called once a result is obtained
  from the ``lhttpc`` client library

## ``start``

> It is required to start lhc before using the API:

```lfe
> (lhc:start)
(#(inets ok) #(ssl ok) #(lhttpc ok) #(lhc ok))
```

> If you attempt to use the API without having first started it,
> you will see errors like the following:

```lfe
> (lhc:get "http://google.com")
#(error
  #(exit
    #(noproc
      #(gen_server call
        (undefined #(socket <0.35.0> "google.com" 80 false) infinity)))
    (#(lhttpc_manager ensure_call 6
       (#(file "src/lhttpc_manager.erl") #(line 234)))
     #(lhttpc_client execute 9 (#(file "src/lhttpc_client.erl") #(line 158)))
     #(lhttpc_client request 9 (#(file "src/lhttpc_client.erl") #(line 99)))
     #(proc_lib init_p_do_apply 3 (#(file "proc_lib.erl") #(line 239))))))
```

This function simply starts the lhc LFE HTTP client. This is required for any
use of the lhc client library.

<aside class="danger">
If you do not start lhc, calling the API functions will result in errors.

lhc depends upon other applications which need to be started (e.g., the
Erlang networking services, SSL support, etc.)
</aside>

## ``get``

```lfe
> (lhc:get "http://google.com/")
"<HTML><HEAD><meta http-equiv=\"content-type\" ... </HTML>\r\n"
>
```

* ``lhc:get/1`` - takes a URL
* ``lhc:get/2`` - takes a URL and lhc options
* ``lhc:get/2`` - takes a URL, list of headers, and lhc options

Depending upon the ``return`` option (default being ``body``) and  ``callback``
option, each of these will return a parsed result iof the content obtained by
``lhttpc``.

## ``head``

```lfe
> (lhc:head "http://google.com/")
(#("X-Frame-Options" "SAMEORIGIN")
 #("X-Xss-Protection" "1; mode=block")
 #("Content-Length" "219")
 #("Server" "gws")
 #("Cache-Control" "public, max-age=2592000")
 #("Expires" "Sat, 26 Sep 2015 04:38:28 GMT")
 #("Date" "Thu, 27 Aug 2015 04:38:28 GMT")
 #("Content-Type" "text/html; charset=UTF-8")
 #("Location" "http://www.google.com/"))
>
```

* ``lhc:head/1`` - takes a URL
* ``lhc:head/2`` - takes a URL and lhc options
* ``lhc:head/2`` - takes a URL, list of headers, and lhc options

Returns just the parsed headers of the result from ``lhttpc``. By defualt, this
is a list of tuples, each being a key/value pair of header name and head value.

## ``post``

[Code ready, need docs]

## ``put``

[Code ready, need docs]

## ``delete``

TBD

## ``trace``

TBD

## ``options``

TBD

## ``connect``

TBD

## ``patch``

TBD

## ``request``

[Code ready, need docs]

## ``parse-results``

[Code ready, need docs]

# Custom Results Parsing

[Code ready, need docs]
