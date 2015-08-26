# Wrapping erlsom

lxml only makes one call to erlsom: in the private function
``lxml:parse-body-raw``. It is through this function that all of lxml's exported
parsing functions ultimately pass. As referenced in the introduction, lxml only
supports erlsom's simple form, and this is what ``parse-body-raw`` calls.

``erlsom:simple_form/1`` takes either string or binary data and upon a
successful parse, returns an LFE data structure which represents the original
XML. Bot the inputs and outputs of ``lxml:parse`` are covered in more detail
below.


## Inputs

> To demonstrate string and binary inputs, let's define some data:

```cl
> (set data-1 "<xml>data</xml>")
"<xml>data</xml>"
> (set data-2 (binary "<xml>data</xml>"))
#B(60 120 109 108 62 100 97 116 97 60 47 120 109 108 62)
```

> Now we can parse these and show that lxml (via erlsom) handles both string and
> binary input the same:

```cl
> (=:= (lxml:parse data-1)
       (lxml:parse data-2))
true
```

lxml handles the following types of input:

* string XML data
* binary XML data
* a file whose data is XML
* a URL that points to XML data

For the first two, you simply pass lxml the data you want to parse. If you want
to parse an XML file or URL, you'll need to pass a tuple to ``parse`` with the
appropriate option set.

For more information on the ``parse`` function, see the "API" section.


## Options

When calling ``parse`` you can simply pass the data (or URL or filename), or
you can also pass a second argument: an "options" property list. The supported
options are:

* ``result-type`` - can be the atom ``raw``. When this is the case, no
  processing is done on the results; they are simply returned as obtained
  from erlsom. Without the ``#(result-type raw)`` option set, lxml will return
  keys as atoms (with it, they are left as-is: strings).

## Outputs

TBD

