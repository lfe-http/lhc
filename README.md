# lhc [![Build Status][travis-badge]][travis]

[![LHC project logo][logo]][logo-large]

*Little (LFE) HTTP Client -- A light-weight LFE wrapper around lhttpc*


#### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Installation](#installation-)
* [Usage](#usage-)
* [Backends](#backends-)


## Introduction [&#x219F;](#contents)

This is simply meant to be a dead-simple HTTP client for LFE projects. That's
all there is to it. It can use one of several possible HTTP clients under the hood. Some of those can can be a little cumbersome for those new to Erlang when used directly, so lhc can be quite helpful for them.


## Dependencies [&#x219F;](#contents)

As of version 0.2.0, this project assumes that you have
[rebar3](https://github.com/rebar/rebar3) installed somwhere in your ``$PATH``.
It no longer uses the old version of rebar. If you do not wish to use rebar3,
you may use the most recent rebar2-compatible release of lhc: 0.1.0.


## Installation [&#x219F;](#contents)

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


## Usage [&#x219F;](#contents)

Usage information is provided in the [documentation](http://lfex.github.io/lhc/).


## Backends [&#x219F;](#contents)

lhc is not an HTTP client in its own right, rather a wrapper for HTTP clients. These "backends" are selectable using a directive in the ``lfe.config`` file. For more information, see the [backend documentation](http://lfex.github.io/lhc/current/#backends).


<!-- Named page links below: /-->

[travis]: https://travis-ci.org/lfex/lhc
[travis-badge]: https://travis-ci.org/lfex/lhc.png?branch=master
[logo]: priv/images/lhc-small.jpg
[logo-large]: priv/images/lhc.jpg
