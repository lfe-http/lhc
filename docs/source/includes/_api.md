# The API

Each of the API functions that map to an HTTP verb have at least one arity
that supports setting lhc options which are passed to ``lhc:request``.
These options may have zero or more of the following:

* ``return`` - return type. Can be one of ``body`` (default), ``status``,
  ``headers`` or ``all``
* ``callback`` - the function that gets called once a result is obtained
  from the ``lhttpc`` client library

The lhc examples below are run against a YAWS REST server demo store. You can
download and run the "store" yourself when you
[clone the repo](https://github.com/lfex/yaws-rest-starter).

For POST'ing data, we use the [ljson](https://github.com/lfex/ljson) LFE
library. To make this available to your project, simply add it to your
``rebar.config`` file, per the instructions on the ljson project README.

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
> (lhc:get "http://localhost:8000/demos/store3/orders")
"{\"result\": \"You got a list of orders.\"}"
>
```

* ``lhc:get/1`` - takes a URL
* ``lhc:get/2`` - takes a URL and lhc options
* ``lhc:get/3`` - takes a URL, list of headers, and lhc options

Depending upon the ``return`` option (default being ``body``) and  ``callback``
option, each of these will return a parsed result iof the content obtained by
``lhttpc``.

## ``head``

```lfe
> (lhc:head "http://localhost:8000/demos/store3/")
(#("Content-Type" "application/json")
 #("Date" "Thu, 27 Aug 2015 14:39:31 GMT")
 #("Server" "Yaws 2.0"))
>
```

* ``lhc:head/1`` - takes a URL
* ``lhc:head/2`` - takes a URL and lhc options
* ``lhc:head/3`` - takes a URL, list of headers, and lhc options

Returns just the parsed headers of the result from ``lhttpc``. By defualt, this
is a list of tuples, each being a key/value pair of header name and head value.

## ``post``

> To post, we first create a payload. The demo REST API we're testing against
> can take any string value, but many applications will expect JSON data. Let's
> use JSON here:

```lfe
> (set payload (ljson:encode '(#(make "Volvo") #(model "P1800"))))
#B(123 34 109 97 107 101 34 58 91 56 54 44 49 49 49 44 49 48 56 ...)
>
```

> With our payload in hand, we can now ``POST``:

```lfe
> (lhc:post "http://localhost:8000/demos/store/order" payload)
"{\"result\": \"You made a new order.\"}"
>
```

* ``lhc:post/1`` - takes a URL, making an empty data ``POST``
* ``lhc:post/2`` - takes a URL and ``POST`` data
* ``lhc:post/3`` - takes a URL, ``POST`` data, and lhc options
* ``lhc:post/4`` - takes a URL, ``POST`` data, a list of headers, and lhc
  options

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
