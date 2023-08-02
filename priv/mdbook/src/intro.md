# Introduction

*Little (LFE) HTTP Client -- A light-weight LFE wrapper around lhttpc*

<a href="images/lhc.jpg"><img src="images/lhc-small.jpg" /></a>

[lhc](http://github.com/lfex/lhc) is library is meant to be a dead-simple HTTP client for LFE projects. That's all there is to it. It uses lhttpc under the hood, which can be a little cumbersome for those new to Erlang when used directly; as such, lhc will likely provide a welcome alternative.

<aside class="success">
The following API functions are ready to use:
<ul>
<li><code>get/1</code>, <code>get/2</code>, <code>get/3</code></li>
<li><code>head/1</code>, <code>head/2</code>, <code>head/3</code></li>
<li><code>post/1</code>, <code>post/2</code>, <code>post/3</code>, <code>post/4</code></li>
<li><code>put/1</code>, <code>put/2</code>, <code>put/3</code>, <code>put/4</code></li>
<li><code>delete/1</code>, <code>delete/2</code>, <code>delete/3</code></li>
<li><code>trace/1</code>, <code>trace/2</code>, <code>trace/3</code>, <code>trace/4</code></li>
<li><code>options/1</code>, <code>options/2</code>, <code>options/3</code></li>
<li><code>patch/1</code>, <code>patch/2</code>, <code>patch/3</code>, <code>patch/4</code></li>
<li><code>request/3</code>, <code>request/4</code>, <code>request/5</code>, <code>request/6</code>, <code>request/7</code></li>
</ul>
</aside>

<aside class="caution">
Note that the following have not been tested extensively (or possibly at all)
against compliant servers:
<ul>
<li><code>trace/1</code>, <code>trace/2</code>, <code>trace/3</code>, <code>trace/4</code></li>
</ul>
</aside>

<aside class="danger">
Note that the following have not yet been implemented:
<ul>
<li>Functions for <code>CONNECT</code></li>
</ul>
</aside>


# Dependencies

To use lhc, you need to have the following:

* Erlang
* ``rebar3``

Since version 0.2.0, the lhc library has moved to using ``rebar3`` and no longer supports the old version of rebar. If, for whatever reason, your project cannot use ``rebar3``, you'll need to use version 0.1.0 of the lhc library.


# Installation

> To use the ``rebar3`` LFE plugin, update your ``rebar.config`` file like the following:

```erlang
{plugins, [
   {'lfe-compile', ".*", {git, "https://github.com/lfe-rebar3/compile.git", {tag, "0.2.0"}}}
  ]}.
{provider_hooks, [
   {pre, [{compile, {lfe, compile}}]}
  ]}.
```

> Then update the ``deps`` directive in your ``rebar.config`` to pull down the latest lhc:

```erlang
{deps, [
...
{lhc, ".*",
  {git, "git@github.com:lfex/lhc.git", "0.2.0"}}
  ]}.
```

> Once your project has added the lhc dependency, you're ready to  execute the ``rebar3`` command for compiling LFE projects:

```bash
$ rebar3 lfe compile
```

> If you want to compile both LFE and any Erlang code you have in your project, you can do both with one command:

```bash
$ rebar3 compile
```

Since lhc now uses ``rebar3``, you'll need to configure your project's ``rebar.config`` file to pull down the necessary ``rebar3`` plugins. A good example of this is the [rebar.config file](https://github.com/lfex/lcfg/blob/master/rebar.config) for the lcfg project. In particular, you'll want to have the same ``plugins`` and ``provider_hooks`` directives.

See notes to the right for specifics.
