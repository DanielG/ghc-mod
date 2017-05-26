# ghc-mod: Happy Haskell Hacking
[![build status](https://gitlab.com/dxld/ghc-mod/badges/master/build.svg)](https://gitlab.com/dxld/ghc-mod/commits/master)

ghc-mod provides editors/IDEs with support for Haskell compiler features, it
supports both Cabal and Stack based projects and integrations exist for Emacs,
Vim, Atom, IntelliJ and VSCode.

- for [all Haskell developers (Using ghc-mod in your development environment)](#using-ghc-mod-in-your-development-environment)
- for [people developing Haskell IDEs (Using ghc-mod as an IDE backend program)](#using-ghc-mod-as-an-ide-backend-program)
- for [developing Haskell tooling (Using ghc-mod as a library)](#using-ghc-mod-as-a-library)

## Overview

### Using ghc-mod in your Development Environment

To use `ghc-mod` in your development environment of choice you need two things:

  - The `ghc-mod` program included in the package of the same name, see [Installing](https://github.com/DanielG/ghc-mod/wiki/Installing)
  - A ghc-mod frontend to integrate it into your development environment, see [Frontend](https://github.com/DanielG/ghc-mod/wiki/Frontend)

### Using ghc-mod as an IDE Backend Program

We provide two modes of operation for frontends: interactive and single shot
mode. The former is accessed by calling `$ ghc-mod legacy-interactive` this will
sit and wait for you to type a command and exit when an empty line is
entered. Interactive mode is pretty much always faster than single shot mode
since it gives ghc-mod the ability to cache the compiler session between
commands on the other hand it needs more memory because it keeps these things
cached. 

Single shot mode is pretty much only there for (backwards) compatibility with
Vim since it only recently got the ability to talk to background processes
without installing some external plugin. You can use single-shot mode by simply
calling the sub-comamnds of the `ghc-mod` program. Since re-compiling large
projects can be really, really slow you really shouldn't use this and use
interactive mode instead.

As a rule of thumb all commands available in single shot mode are available in
interactive mode, a list of the former can be obtained by running 
`$ ghc-mod --help`.

If you're developing a new ghc-mod fronted we'd love to hear from you! Please
open an issue or e-mail the maintainer. Also we invite you to add installation
and configuration instructions to
[Frontend](https://github.com/DanielG/ghc-mod/wiki/Frontend).

### Using ghc-mod as a Library

Internally ghc-mod uses the Glasgow Haskell Compilers's API to implement most of
it's functionality.

In order to provide a hassle free experience to users ghc-mod tries hard to
automatically, and correctly, detect and if needed tweak the environment GHC
needs. It also handles some of the more cumbersome parts of getting a working
compiler session up and running.

This functionality can be very useful to all kinds of Haskell development tools
therefore want to expose all the useful abstractions ghc-mod provides.

Right now the ghc-mod API is pretty messy; a result of major internal rewrites
and reorganization coupled with too little time for cleanups over the course of
almost 100 releases! We would like to make a cut during v6.0 or so and
completely re-do the API but we need more input from downstream tool writers to
do that properly, see [Library API Redesign](Library-API-Redesign.md).

For example [The Haskell Refactorer (HaRe)](https://github.com/alanz/HaRe) uses
the build environment abstraction ghc-mod provides so it can concentrate on it's
core functionality instead of worrying about build environments and compiler
session setup.

Recently the
[`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine) project
has sprung up and if you're planning to write any kind of tool that needs editor
integration eventually you should definetly look into that. `haskell-ide-engine`
uses `ghc-mod` at it's core so you'll want to be familliar with it either way.

API "documentation" is here:
[Hackage docs](https://hackage.haskell.org/package/ghc-mod#modules).

## Reporting Bugs

Please report bugs on the GitHub issue tracker for ghc-mod:
https://github.com/DanielG/ghc-mod/issues

Including general environment information like the operating system
(distribution, version) you're using and the output of `$ ghc-mod debug` run in
your project directory is probably a good idea.

## IRC

If you have any problems, suggestions, comments swing by
[\#ghc-mod (web client)](https://kiwiirc.com/client/irc.freenode.org/ghc-mod) on
Freenode. If you're reporting a bug please also create an issue
[here (GitHub issue tracker)](https://github.com/DanielG/ghc-mod/issues) so we
have a way to contact you if you don't have time to stay.

Do hang around for a while if no one answers and repeat your question if you
still haven't gotten any answer after a day or so (the maintainer was probably
asleep). You're most likely to get an answer during the day in GMT+1.
