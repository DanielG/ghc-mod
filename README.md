# Happy Haskell Programming
[![Build Status](https://travis-ci.org/kazu-yamamoto/ghc-mod.png)](https://travis-ci.org/kazu-yamamoto/ghc-mod)

## Using the stable version

Please read: [http://www.mew.org/~kazu/proj/ghc-mod/](http://www.mew.org/~kazu/proj/ghc-mod/)

Emacs front-end, which is consistent with binaries on Hackage, is available *stable* MELPA whose URL is http://melpa-stable.milkbox.net/packages/. So, your "~/.emacs" should be:

```elisp
(require 'package)
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)
```

With this configuration you can install the stable Emacs front end indicated by "ghc" from MELPA while you can install `ghc-mod`/`ghc-modi` binaries by:

```shell
% cabal update
% cabal install ghc-mod
```

## Using the develop version

You should install both Emacs front-end and binaries from this git repo. If you use the snapshot MELPA to install Emacs front-end, you would suffer from inconsistency between Emacs front-end and binaries.
