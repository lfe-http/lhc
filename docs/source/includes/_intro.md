# Introduction

*Little (LFE) HTTP Client -- A light-weight LFE wrapper around lhttpc*

<a href="images/lhc.jpg"><img src="images/lhc-small.jpg" /></a>

This library is meant to be a dead-simple HTTP client for LFE projects. That's
all there is to it. It uses lhttpc under the hood, which can be a little
cumbersome for those new to Erlang when used directly; as such, lhc will likely
provide a welcome alternative.

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

# Installation


> Just add it to your ``rebar.config`` deps:

```erlang
  {deps, [
    ...
    {lhc, ".*",
      {git, "git@github.com:lfex/lhc.git", "master"}}
      ]}.
```

> Then execute the usual ``rebar`` command:

```bash
    $ rebar compile
```

See notes to the right.

