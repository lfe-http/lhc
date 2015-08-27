# The API


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

[Code ready, need docs]

## ``head``

[Code ready, need docs]

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
