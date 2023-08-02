# lhc

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][version]
[![Tags][github tags badge]][github tags]
[![Downloads][hex downloads]][hex package]

*Little (LFE) HTTP Client -- A light-weight LFE wrapper around lhttpc*

[![LHC project logo][logo]][logo-large]


#### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Installation](#installation-)
* [Usage](#usage-)
* [Backends](#backends-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

This is simply meant to be a dead-simple HTTP client for LFE projects. That's
all there is to it. It can use one of several possible HTTP clients under the hood. Some of those can can be a little cumbersome for those new to Erlang when used directly, so lhc can be quite helpful for them.


## Dependencies [&#x219F;](#contents)

As of version 0.2.0, this project assumes that you have
[rebar3](https://github.com/rebar/rebar3) installed somwhere in your `$PATH`.
It no longer uses the old version of rebar. If you do not wish to use rebar3,
you may use the most recent rebar2-compatible release of lhc: 0.1.0.


## Installation [&#x219F;](#contents)

Just add it to your `rebar.config` deps:

```erlang
{deps, [
  ...
  {lhc, "0.4.0"}
]}.
```

And then do the usual:

```bash
$ rebar compile
```


## Usage [&#x219F;](#contents)

Usage information is provided in the [documentation](http://lfe-http.github.io/lhc/).


## License [&#x219F;](#contents)

Apache Version 2 License

Copyright (c) 2015 BilloSystems, Ltd. Co.

Copyright (c) 2015-2023 Duncan McGreggor <oubiwann@gmail.com>


<!-- Named page links below: /-->

[logo]: priv/images/lhc-small.jpg
[logo-large]: priv/images/lhc.jpg
[org]: https://github.com/lfex
[github]: https://github.com/lfex/lhc
[gitlab]: https://gitlab.com/lfex/lhc
[gh-actions-badge]: https://github.com/lfex/lhc/actions/workflows/cicd.yml/badge.svg
[gh-actions]: https://github.com/lfex/lhc/actions
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.2.1-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-19%20to%2026-blue.svg
[version]: https://github.com/lfex/lhc/blob/master/.github/workflows/cicd.yml
[github tags]: https://github.com/lfex/lhc/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/lhc.svg
[github downloads]: https://img.shields.io/github/downloads/lfex/lhc/total.svg
[hex badge]: https://img.shields.io/hexpm/v/lhc.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/lhc
[hex downloads]: https://img.shields.io/hexpm/dt/lhc.svg
