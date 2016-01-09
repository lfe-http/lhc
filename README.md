# lhc

*Little (LFE) HTTP Client -- A light-weight LFE wrapper around lhttpc*

<a href="resources/images/lhc.jpg"><img src="resources/images/lhc-small.jpg" /></a>

##### Table of Contents

* [Introduction](#introduction-)
* [Installation](#installation-)
* [Usage](#usage-)
* [Backends](#backends-)


## Introduction [&#x219F;](#table-of-contents)

This is simply meant to be a dead-simple HTTP client for LFE projects. That's
all there is to it. It uses lhttpc under the hood, which can be a little
cumbersome for those new to Erlang when used directly.


## Installation [&#x219F;](#table-of-contents)

Just add it to your ``rebar.config`` deps:

```erlang
  {deps, [
    ...
    {lhc, ".*",
      {git, "git@github.com:lfex/lhc.git", "master"}}
      ]}.
```

And then do the usual:

```bash
    $ rebar compile
```


## Usage [&#x219F;](#table-of-contents)

Usage information is provided in the [documentation](http://lfex.github.io/lhc/).


## Backends [&#x219F;](#table-of-contents)

lhc is not an HTTP client in its own right, rather a wrapper for HTTP clients. These "backends" are selectable using a directive in the ``lfe.config`` file. For more information, see the [backend documentation](http://lfex.github.io/lhc/current/#backends).
