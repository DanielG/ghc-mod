#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 VERSION" >&2
    exit 1
fi

VERSION=$1

if ! echo $VERSION | grep "^[0-9.]"; then
    echo "invalid version";
    exit 1
fi

cd $(dirname $0)/..

sed -i 's/(defconst ghc-mod-version ".*")/(defconst ghc-mod-version "'"$VERSION"'")/' \
    elisp/ghc-mod.el

sed -i 's/(defconst ghc-mod-version ".*")/(defconst ghc-mod-version "'"$VERSION"'")/' \
    elisp-internal/ghc-mod-internal.el

sed -r -i 's/^(Version:[[:space:]]*)[0-9.]+/\1'"$VERSION"'/' ghc-mod.cabal

( tac ChangeLog; echo "\n$(date '+%Y-%m-%d') v$VERSION" ) | tac \
    > ChangeLog.tmp

mv ChangeLog.tmp ChangeLog

emacs -q -nw ChangeLog

git add ChangeLog elisp-internal/ghc.el elisp/ghc-mod.el ghc-mod.cabal
git commit -m "Bump version to $VERSION"
git tag "v$VERSION"
