#!/bin/sh

set -e

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

sed -i 's/(defconst ghc-version ".*")/(defconst ghc-version "'"$VERSION"'")/' \
    elisp/ghc.el

sed -r -i 's/^(Version:[[:space:]]*)[0-9.]+/\1'"$VERSION"'/' ghc-mod.cabal

git add elisp/ghc.el ghc-mod.cabal
git commit -m "Bump version to $VERSION"

git checkout release
#git merge master
git merge -s recursive -X theirs master

( tac ChangeLog; echo "\n$(date '+%Y-%m-%d') v$VERSION" ) | tac \
    > ChangeLog.tmp

mv ChangeLog.tmp ChangeLog

emacs -q -nw ChangeLog

git add ChangeLog
git commit -m "ChangeLog"


git tag "v$VERSION"
