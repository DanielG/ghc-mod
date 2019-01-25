# ghc-mod: Happy Haskell Hacking
[![build status](https://gitlab.com/dxld/ghc-mod/badges/master/build.svg)](https://gitlab.com/dxld/ghc-mod/commits/master)

## Legacy

Please note that using ghc-mod as a user facing tool for IDE/Editor integration
is no longer supported or maintained. We are in the process of refocusing our
efforts around
[haskell-ide-engine](https://github.com/haskell/haskell-ide-engine) as the main
user/IDE facing program.

Currently haskell-ide-engine still uses ghc-mod as a library for GHC session
setup, so this part will still be maintained. However moving forward we're
hoping to move most of the functionality which remains in ghc-mod (the library)
into GHC upstream rendering ghc-mod unceccesary. Meanwhile ghc-mod (the library)
will remain.

If someone feels like taking over maintainership of ghc-mod as a standalone
development tool feel free to contact the maintainer. However I must warn you: a
mountain of legacy baggage and scattered Editor/IDE plugins awaits. Your time is
probably better spent improving haskell-ide-engine.

For more information on what ghc-mod used to be have a look at the
[old README](README_old.md).

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

Do hang around for a while if no one answers, and repeat your question if you
still haven't gotten any answer after a day or so (the maintainer was probably
asleep). You're most likely to get an answer during the day in GMT+1.
