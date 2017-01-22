# ghc-mod: Happy Haskell Hacking
[![build status](https://gitlab.com/dxld/ghc-mod/badges/master/build.svg)](https://gitlab.com/dxld/ghc-mod/commits/master)

ghc-mod is a couple of different things depending on what you want to do, you
should read the corresponding section:

- for [all Haskell developers (Using ghc-mod in your development environment)](#using-ghc-mod-in-your-development-environment)
- for [people developing Haskell IDEs (Using ghc-mod as an IDE backend program)](#using-ghc-mod-as-an-ide-backend-program)
- for [developing Haskell tooling (Using ghc-mod as a library)](#using-ghc-mod-as-a-library)

## Overview

### Using ghc-mod in your Development Environment<a name="haskell-dev"></a>

To use `ghc-mod` in your development environment of choice you need two things:

  - The `ghc-mod` program included in the package of the same name, see [Installing](https://github.com/DanielG/ghc-mod/wiki/Installing)
  - A ghc-mod frontend to integrate it into your development environment, see [Frontend](https://github.com/DanielG/ghc-mod/wiki/Frontend)

### Using ghc-mod as an IDE Backend Program<a name="ide-dev"></a>

Directly using ghc-mod is while still supported for the time being
discouraged. You should look into working with
[`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine) instead.

The `ghc-mod` backend program is somewhat crusty and carries a lot of legacy
baggage so going forward we would like to see frontends use `haskell-ide-engine`
instead. There we're trying to get the design right from the beginning and fix
the fragmentation of the Haskell Tooling Ecosystem along the way.

### Using ghc-mod as a Library<a name="tool-dev"></a>

Internally ghc-mod uses the Glasgow Haskell Compilers's API to implement most of
it's functionality.

In order to provide a hassle free experience to users ghc-mod tries hard to
automatically, and correctly, detect and if needed tweak the environment GHC
needs. It also handles some of the more cumbersome parts of getting a working
compiler session up and running.

This functionality can be very useful to all kinds of Haskell development tools
therefore want to expose all the useful abstractions ghc-mod provides.

Right now the ghc-mod API is pretty messy a result major internal rewrites and
reorganization coupled with too little time for cleanups over the course of
almost 100 releases! We would like to make a cut during v6.0 or so and
completely re-do the API but we need more input from downstream tool writers to
do that properly, see [Library API Redesign](Library-API-Redesign.md).

Right now tools like
[The Haskell Refactorer (HaRe)](https://github.com/alanz/HaRe) use this
environment handling so they can concentrate on their core functionality instead
of worrying about environments.

Most recently the
[`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine) project
has sprung up and if you're planning to write any kind of tool that needs editor
integration eventually you should definetly look into that. `haskell-ide-engine`
uses `ghc-mod` at it's core so you'll want to be familliar with it either way.

API "documentation" is here:
[Hackage docs](https://hackage.haskell.org/package/ghc-mod#modules).

## IRC

If you have any problems, suggestions, comments swing by
[\#ghc-mod (web client)](https://kiwiirc.com/client/irc.freenode.org/ghc-mod) on
Freenode. If you're reporting a bug please also create an issue
[here](https://github.com/DanielG/ghc-mod/issues) so we have a way to contact
you if you don't have time to stay.

Do hang around for a while if no one answers and repeat your question if you
still haven't gotten any answer after a day or so. You're most likely to get an
answer during the day in GMT+1.
