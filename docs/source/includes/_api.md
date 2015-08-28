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

For ``POST``ing data, we use the [ljson](https://github.com/lfex/ljson) LFE
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
> (lhc:get "http://localhost:8000/demos/store3/orders")
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

> Perform a simple ``GET``:

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

> To just get the headers from the server:

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

> To ``POST``, we first create a payload. The demo REST API we're testing against
> can take any string value, but many applications will expect JSON data. Let's
> use JSON here:

```lfe
> (set payload (ljson:encode '(#(make #"Volvo") #(model #"P1800"))))
#"{"make":"Volvo","model":"P1800"}"
>
```

> With our payload in hand, we can now ``POST`` to create a new order:

```lfe
> (lhc:post "http://localhost:8000/demos/store3/order" payload)
"{\"result\": \"{\"order-id\": 124}\"}"
>
```

* ``lhc:post/1`` - takes a URL, making an empty data ``POST``
* ``lhc:post/2`` - takes a URL and ``POST`` data
* ``lhc:post/3`` - takes a URL, ``POST`` data, and lhc options
* ``lhc:post/4`` - takes a URL, ``POST`` data, a list of headers, and lhc
  options

## ``put``

> To ``PUT``, we first create a payload similar to what we did for ``POST`` but
> with the modified data we want:

```lfe
> (set payload (ljson:encode '(#(make #"Volvo") #(model #"2015 P1800"))))
#"{"make":"Volvo","model":"2015 P1800"}"
>
```

> We can now ``PUT`` to update our order:

```lfe
> (lhc:put "http://localhost:8000/demos/store3/order/124" payload)
"{\"result\": \"You updated all of order 124.\"}"
>
```

* ``lhc:put/1`` - takes a URL, making an empty data ``PUT``
* ``lhc:put/2`` - takes a URL and ``PUT`` data
* ``lhc:put/3`` - takes a URL, ``PUT`` data, and lhc options
* ``lhc:put/4`` - takes a URL, ``PUT`` data, a list of headers, and lhc
  options

## ``delete``

> ``DELETE``ing is as simple as passing the appropriate URL:

```lfe
> (lhc:delete "http://localhost:8000/demos/store3/order/124")
"{\"result\": \"You deleted order 124.\"}"
>
```

* ``lhc:delete/1`` - takes a URL
* ``lhc:delete/2`` - takes a URL and lhc options
* ``lhc:delete/3`` - takes a URL, list of headers, and lhc options

## ``trace``

Note that ``TRACE`` isn't implemented in YAWS so we don't have example code to
share. Likewise, ``lhc:trace`` is untested against a server implementing
``TRACE`` -- please
[let us know](https://github.com/lfex/lhc/issues/new)
if you run into usage problems with this function.

* ``lhc:trace/1`` - takes a URL
* ``lhc:trace/2`` - takes a URL and data
* ``lhc:trace/3`` - takes a URL, data, and lhc options
* ``lhc:trace/4`` - takes a URL, data, a list of headers, and lhc options

## ``options``

> lhc provides support for requesting the allowed methods for a given URL:

```lfe
> (lhc:options "http://localhost:8000/demos/store3/order/124")
"{\"result\": \"You got the allowed method for order/124: GET, PUT, POST, DELETE, and OPTIONS.\"}"
```

* ``lhc:options/1`` - takes a URL
* ``lhc:options/2`` - takes a URL and lhc options
* ``lhc:options/3`` - takes a URL, list of headers, and lhc options


## ``connect``

TBD

(Note that this HTTP verb is not supported by YAWS, so we'll likely save it for
very last.)

## ``patch``

> Let's define the partial data we want to send:

```lfe
> (set payload (ljson:encode '(#(model #"2015 P1800"))))
#"{"model":"2015 P1800"}"
>
```

> We can now ``PATCH`` our order, updating only the part that we wanted to
> change:

```lfe
> (lhc:patch "http://localhost:8000/demos/store3/order/124" payload)
"{\"result\": \"You updated part of order 124.\"}"
>
```

``PATCH`` is useful for large data sets where you only one a part of the data
updated. You can use ``PATCH`` to update just the bits you're interested in,
without having to send the entire payload like is recommended with ``PUT``.

* ``lhc:patch/1`` - takes a URL, making an empty data ``PATCH``
* ``lhc:patch/2`` - takes a URL and ``PATCH`` data
* ``lhc:patch/3`` - takes a URL, ``PATCH`` data, and lhc options
* ``lhc:patch/4`` - takes a URL, ``PATCH`` data, a list of headers, and lhc
  options


## ``request``

[Code ready, need docs]

## ``parse-results``

[Code ready, need docs]

# Custom Results Parsing

[Code ready, need docs]

# From Erlang

> Basic usage from Erlang is straight-forward:

```erlang
1> lhc:start().
[{inets,ok},{ssl,ok},{lhttpc,ok},{lhc,ok}]
2> lhc:get("http://localhost:8000/demos/store3/orders").
"{\"result\": \"You got a list of orders.\"}"
3> Payload = ljson:encode([{make,<<"Volvo">>},{model,<<"P1800">>}]).
<<"{\"make\":\"Volvo\",\"model\":\"P1800\"}">>
4> lhc:post("http://localhost:8000/demos/store3/order", Payload).
"{\"result\": \"{\"order-id\": 124}\"}"
```

Using lhc from Erlang is very straight-forward; there aren't even hypens in
most module or function names, so no need to escape any atoms!

Why would you want to, you ask? Well, it might be convenient to use a library
that offers consistent usage patterns (and function calls) across a selection
of HTTP clients in the Erlang ecosystem. You and your developers could write
your HTTP code once, and then change backend clients as the needs arose,
without having to change any code (just some small configuration settings).

Take a look at the sample usage to the right to get a sense of using lhc from
Erlang.

# User Agent

> The lhc user agent string:

```
"LFE Little HTTP Client (lhc/LFE)/0.1.0 (+http://github.com/lfex/lhc)"
```

The lhc user agent will show up in any HTTP server log files where client user
agent strings are recorded. To the right is an example of the lhc user agent
string.
