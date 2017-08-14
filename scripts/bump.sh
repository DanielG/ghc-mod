#!/bin/sh

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 VERSION" >&2
    exit 1
fi

VERSION=$1

if ! echo $VERSION | grep -Eq "^[0-9.]*(-.+)?$"; then
    echo "invalid version";
    exit 1
fi

cd $(dirname $0)/..

git checkout release-$VERSION

sed -i 's/(defconst ghc-version ".*")/(defconst ghc-version "'"$VERSION"'")/' \
    elisp/ghc.el

sed -r -i 's/^(Version:[[:space:]]*)[0-9.]+/\1'"$VERSION"'/' ghc-mod.cabal

git add elisp/ghc.el ghc-mod.cabal

git update-index -q --ignore-submodules --refresh
# If there are uncommitted changes do the bump commit
if ! git diff-index --cached --quiet HEAD --ignore-submodules --
then
    git commit -m "Bump version to $VERSION" --allow-empty
fi

git checkout release
#git merge release-VER branch into 'release'
git merge -s recursive -X theirs release-$VERSION

( tac ChangeLog; echo "\n$(date '+%Y-%m-%d') v$VERSION" ) | tac \
    > ChangeLog.tmp

mv ChangeLog.tmp ChangeLog

emacs -q -nw ChangeLog

git add ChangeLog
git commit -m "ChangeLog"

git tag -f "v$VERSION"
