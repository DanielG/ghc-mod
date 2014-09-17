#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 VERSION" >&2
    exit 1
fi

VERSION=$1

cd $(dirname $0)/..

sed -i 's/(defconst ghc-version ".*")/(defconst ghc-version "'"$VERSION"'")/' \
    elisp/ghc.el

sed -r -i 's/^(Version:[[:space:]]*)[0-9.]+/\1'"$VERSION"'/' ghc-mod.cabal

( tac ChangeLog; echo "\n$(date '+%Y-%m-%d') $VERSION" ) | tac \
    > ChangeLog.tmp

mv ChangeLog.tmp ChangeLog

emacs -q -nw ChangeLog

git add ChangeLog elisp/ghc.el ghc-mod.cabal
git commit -m "Bump version to $VERSION"
