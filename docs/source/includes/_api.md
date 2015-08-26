# The API


## ``start``

> Starting up lxml for parsing URLs and following media links:

```cl
> (lxml:start)
(#(inets ok) #(ssl ok) #(lhttpc ok) #(lhc ok) #(lxml ok))
```

This function simply starts the lhc LFE HTTP client. If you plan on parsing XML
from URLs or if you want to be able to call the ``get-linked`` function, you
will first need to call ``start``.


## ``parse``

> For this and subsequent API functions, we will use the following
sample data, assumed to be saved to ``./data.xml``:

```xml
<life>
  <bacteria division="domain">
    <bacterium>spirochetes</bacterium>
    <bacterium>proteobacteria</bacterium>
    <bacterium>cyanobacteria</bacterium>
  </bacteria>
  <archaea division="domain">
    <archaum></archaum>
  </archaea>
  <eukaryota division="domain">
    <eukaryotum>slime molds</eukaryotum>
    <eukaryotum>fungi</eukaryotum>
    <eukaryotum>plants</eukaryotum>
    <eukaryotum>animals</eukaryotum>
  </eukaryota>
</life>
```

> Parse the data normally:

```cl
> (lxml:parse #(file "data.xml"))
(#(tag "life")
 #(attr ())
 #(content
   #(life ()
     (#(bacteria
        (#(division "domain"))
        (#(bacterium () ("spirochetes"))
         #(bacterium () ("proteobacteria"))
         #(bacterium () ("cyanobacteria"))))
      #(archaea (#(division "domain")) (#(archaum () ())))
      #(eukaryota
        (#(division "domain"))
        (#(eukaryotum () ("slime molds"))
         #(eukaryotum () ("fungi"))
         #(eukaryotum () ("plants"))
         #(eukaryotum () ("animals")))))))
 #(tail "\n"))
```

> Parse the data, requesting a raw result:

```cl
> (lxml:parse #(file "data.xml") '(#(result-type raw)))
#(ok
  #("life"
    ()
    (#("bacteria"
       (#("division" "domain"))
       (#("bacterium" () ("spirochetes"))
        #("bacterium" () ("proteobacteria"))
        #("bacterium" () ("cyanobacteria"))))
     #("archaea" (#("division" "domain")) (#("archaum" () ())))
     #("eukaryota"
       (#("division" "domain"))
       (#("eukaryotum" () ("slime molds"))
        #("eukaryotum" () ("fungi"))
        #("eukaryotum" () ("plants"))
        #("eukaryotum" () ("animals"))))))
  "\n")
```

When parsing the data normally, a 4-element property list is returned.
The keys and values of the proplist are as follows:

* ``tag`` - the top-level tag of the parsed XML data
* ``attr`` - the attributes of the top-level tag
* ``content`` - the content of the top-level tag; this includes the full
  XML result, minus any trailing characters
* ``tail`` - any trailing characters

Note that XML tags with no attributes simply have an empty list in the
"attrributes" portion of the parsed results.

Normal parsing also converts nested tag names from strings to ataom.

When asking for raw results, the XML data is passed directly to erlsom and the
result is returned without any filtering. The data from erlsom is returned as
a 2-tuple where the elements of the tuple are as follows:

* status - either ``ok``or ``error``
* result - either the parsed data structure or an error message


## ``get-data``

> By default, ``get-data`` operates on a parsed result set:

```cl
> (lxml:get-data (lxml:parse #(file "data.xml")))
#(life ()
  (#(bacteria
     (#(division "domain"))
     (#(bacterium () ("spirochetes"))
      #(bacterium () ("proteobacteria"))
      #(bacterium () ("cyanobacteria"))))
   #(archaea (#(division "domain")) (#(archaum () ())))
   #(eukaryota
     (#(division "domain"))
     (#(eukaryotum () ("slime molds"))
      #(eukaryotum () ("fungi"))
      #(eukaryotum () ("plants"))
      #(eukaryotum () ("animals"))))))
```

> However, by passing options, one may call ``get-data`` in the following,
more concise manner:

```cl
> (lxml:get-data #(file "data.xml"))
#(life ()
  (#(bacteria ...)))
```

> Or:

```cl
> (lxml:get-data #(url "http://example.com/data.xml"))
#(life ()
  (#(bacteria ...)))
```

> Or:

```cl
> (lxml:get-data #(xml "<life> ... </life>"))
#(life ()
  (#(bacteria ...)))
```

The ``get-data`` function is provided as a convenience, as more often than not,
one cares about the ``#(content ...)`` tuple in the parsed property list, and
this is what ``get-data`` returns.

``get-data`` also provides a convenience wrapper around parse: if you provide
a tuple with any of the following keys, the parse command is called under
the covers, alleviating the user from any need to make that call. The value
keys are:

* ``file``
* ``url``
* ``xml``


## ``get-in``

> First let's set some data:

```cl
> (set data (lxml:get-data #(file "data.xml")))
#(life ()
```

> Using ``get-in`` trivially, with one key:

```cl
> (lxml:get-in '(life) data)
(#(bacteria
   (#(division "domain"))
   (#(bacterium () ("spirochetes"))
    #(bacterium () ("proteobacteria"))
    #(bacterium () ("cyanobacteria"))))
 #(archaea (#(division "domain")) (#(archaum () ())))
 #(eukaryota
   (#(division "domain"))
   (#(eukaryotum () ("slime molds"))
    #(eukaryotum () ("fungi"))
    #(eukaryotum () ("plants"))
    #(eukaryotum () ("animals")))))
```

> Using ``get-in`` with two keys:

```cl
> (lxml:get-in '(life bacteria) data)
(#(bacterium () ("spirochetes"))
 #(bacterium () ("proteobacteria"))
 #(bacterium () ("cyanobacteria")))
```

> Using ``get-in`` with three:

```cl
> (lxml:get-in '(life bacteria bacterium) data)
"spirochetes"
```

> Or, to get the third bacterium:

```cl
> (lxml:get-in '(life bacteria 3))
"cyanobacteria"
```

> For each of these, we could also have used ``file``, ``url``, or ``xml``:

```cl
> (lxml:get-in '(life bacteria) #(file "data.xml"))
(#(bacterium () ("spirochetes"))
 #(bacterium () ("proteobacteria"))
 #(bacterium () ("cyanobacteria")))
```

``get-in`` is inspired by the Clojure function of the same name and provide
an easy means of extracting data nested to any depth. A list of keys is
provided, as well as parsed XML data. It is expected that each subsequent key
frerences a child element. The parsed XML tree is walked until the last
key is reached, at which point the data at that node is returned.

Keys may either be atoms (keys in the proplist) or integers (1-based indices).
This allows for situations when sibling elements have the same name and
can only be distinguished by index.

As you may have guessed, ``get-in`` provides the same optional use as that
of ``get-data`` and supports the same tuple keys:

* ``file``
* ``url``
* ``xml``


## ``get-attr-in``

> If we wanted to get the nested value for an attribute, we use ``get-attr-in``
  passing the nested list of tags and then the attribute after the final tag:

```cl
> (lxml:get-attr-in '(archaea domain) data)
```

``get-attr-in`` is very similar to ``get-attr``: it traverses a tree of parsed
XML data, but instead of the last item in the list of keys pointing to the
content of an element, it points to an element's attribute.


## ``get-linked``

TBD


## ``map``

> Get the raw data of our sample XML file, without any of lxml's
  post-parse-processing:

```cl
> (set `#(ok ,data ,_) (lxml:parse #(file "data.xml") '(#(result-type raw))))
#(ok
  #("life"
    ()
    (#("bacteria"
       (#("division" "domain"))
       (#("bacterium" () ("spirochetes"))
        #("bacterium" () ("proteobacteria"))
        #("bacterium" () ("cyanobacteria"))))
     #("archaea" (#("division" "domain")) (#("archaum" () ())))
     #("eukaryota"
       (#("division" "domain"))
       (#("eukaryotum" () ("slime molds"))
        #("eukaryotum" () ("fungi"))
        #("eukaryotum" () ("plants"))
        #("eukaryotum" () ("animals"))))))
  "\n")
>
```

> Using ``lxml:map/2``, we can modify the content of all tags. For example,
  the following will uppercase all the content data:

```cl
> (lxml:map #'string:to_upper/1 data)
#("life"
  ()
  (#("bacteria"
     (#("division" "domain"))
     (#("bacterium" () ("SPIROCHETES"))
      #("bacterium" () ("PROTEOBACTERIA"))
      #("bacterium" () ("CYANOBACTERIA"))))
   #("archaea" (#("division" "domain")) (#("archaum" () ())))
   #("eukaryota"
     (#("division" "domain"))
     (#("eukaryotum" () ("SLIME MOLDS"))
      #("eukaryotum" () ("FUNGI"))
      #("eukaryotum" () ("PLANTS"))
      #("eukaryotum" () ("ANIMALS"))))))
```

> In the raw data we got back, all the tags and attirbute keys are all strings.
  What if we'd like to convert those to atoms? We can use ``lxml:map/4`` and
  just use the identify function for processing the content, since we don't
  want that modified:

```cl
> (lxml:map #'list_to_atom/1 #'lxml:key->atom/1 #'lxml:ident/1 data)
#(life ()
  (#(bacteria
     (#(division "domain"))
     (#(bacterium () ("spirochetes"))
      #(bacterium () ("proteobacteria"))
      #(bacterium () ("cyanobacteria"))))
   #(archaea (#(division "domain")) (#(archaum () ())))
   #(eukaryota
     (#(division "domain"))
     (#(eukaryotum () ("slime molds"))
      #(eukaryotum () ("fungi"))
      #(eukaryotum () ("plants"))
      #(eukaryotum () ("animals"))))))
```

``map`` functions typically operate over lists of arbitrary data; the function
passed to the ``map`` function is what is expected to know something about the
list data. The ``map`` functions in lxml are significantly different than
standard ``map`` functions in the following ways:

* They operate on nested data,
* They understand that the lists will contain either the 3-tuple of parsed XML
  data, or a list of attribute typles, and
* There are potentially three functions one may pass: on to operate on the tag,
  one on the attributes, and another on the contents.

As a result of the last bullet point, ``lxml:map`` comes in three arities:

* ``lxml:map/2`` - takes a content-manipulating function and parsed XML data
* ``lxml:map/3`` - takes an attribute-manipulating function,
    a content-manipulating function, and parsed XML data
* ``lxml:map/4`` - takes s tag-manipulating function,
    an attribute-manipulating function,
    a content-manipulating function, and parsed XML data
