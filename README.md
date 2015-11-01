# Happy Haskell Programming
[![Build Status](https://travis-ci.org/kazu-yamamoto/ghc-mod.png)](https://travis-ci.org/kazu-yamamoto/ghc-mod)

Please read: [http://www.mew.org/~kazu/proj/ghc-mod/](http://www.mew.org/~kazu/proj/ghc-mod/)

## Using the stable version

The Emacs front-end is available from
[*stable* MELPA](https://stable.melpa.org/). This package should
always be compatible with the latest version of ghc-mod from hackage.

To use stable *stable* MELPA add this to your `.emacs`:

```elisp
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://stable.melpa.org/packages/"))
(package-initialize)
```

With this configuration you can install the Emacs front end from MELPA (the
package is called `ghc` there, not `ghc-mod`) and install the
`ghc-mod`/`ghc-modi` binaries from hackage by doing:

```shell
% cabal update && cabal install ghc-mod
```

### Nix & NixOS

`ghc-mod` works fine for users of Nix who follow a recent version of the
package database such as the `nixos-15.09` or `nixos-unstable` channel. Just
include the package `ghc-mod` into your `ghcWithPackages` environment like any
other library. The [Nixpkgs Haskell User's
Guide](http://hydra.nixos.org/job/nixpkgs/trunk/manual/latest/download-by-type/doc/manual#users-guide-to-the-haskell-infrastructure)
covers this subject in great detail.

## Using the development version

The easiest way to hack on ghc-mod is compile it, then add `dist/build/ghc-mod`
and `dist/build/ghc-modi` to your `PATH` and add the `elisp/` directory to your
Emacs `load-path`.

Make sure you're not using the MELPA version of `ghc.el` otherwise you might get
all sorts of nasty conflicts.


## Custom ghc-mod cradle

To customize the package databases used by `ghc-mod`, put a file called `ghc-mod.package-db-stack` beside the `.cabal` file with the following syntax:

```
temp directory root
package db 1
...
package db n
```

each package database line is either a *path* to a package database, or `global` or `user`.

## IRC

If you have any problems, suggestions, comments swing by
[\#ghc-mod (web client)](https://kiwiirc.com/client/irc.freenode.org/ghc-mod) on
Freenode. If you're reporting a bug please also create an issue
[here](https://github.com/kazu-yamamoto/ghc-mod/issues) so we have a way to contact
you if you don't have time to stay.

Do hang around for a while if no one answers and repeat your question if you
still haven't gotten any answer after a day or so. You're most likely to get an
answer during the day in GMT+1.
