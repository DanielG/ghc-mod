# Happy Haskell Programming
[![Build Status](https://travis-ci.org/kazu-yamamoto/ghc-mod.png)](https://travis-ci.org/kazu-yamamoto/ghc-mod)

Please read: [http://www.mew.org/~kazu/proj/ghc-mod/](http://www.mew.org/~kazu/proj/ghc-mod/)

## Using the stable version

The Emacs front-end is available from
[*stable* MELPA](http://melpa-stable.milkbox.net/). This package should
always be compatible with the latest version of ghc-mod from hackage.

To use stable *stable* MELPA add this to your `.emacs`:

```elisp
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)
```

With this configuration you can install the Emacs front end from MELPA (the
package is called `ghc` there, not `ghc-mod`) and install the
`ghc-mod`/`ghc-modi` binaries from hackage by doing:

```shell
% cabal update && cabal install ghc-mod
```

## Using the development version

The easiest way to hack on ghc-mod is compile it, then add `dist/build/ghc-mod`
and `dist/build/ghc-modi` to your `PATH` and add the `elisp/` directory to your
Emacs `load-path`.

Make sure you're not using the MELPA version of `ghc.el` otherwise you might get
all sorts of nasty conflicts.


## IRC

If you have any problems, suggestions, comments swing by
[#ghc-mod](irc://chat.freenode.net/ghc-mod) on Freenode.
