#!/bin/sh

(
    set -e

    cd $(dirname $0)/..

    echo "======== ghc-pkg list ========"
    ghc_ver=$(ghc --version | sed -r 's/.*[[:space:]]([0-9.]+)/\1/')
    ghc-pkg list -v \
        --global --user \
        --package-db .cabal-sandbox/*-ghc-$ghc_ver-packages.conf.d 2>&1

    echo "======== cabal reconfigure ========"
    cabal clean -v3
    cabal configure -v3 --enable-tests 2>&1
    echo "======== END cabal reconfigure ========"

    echo "======== cabal setup/config ========"
    cat dist/setup-config
    echo "\n======== END cabal setup/config ========"

    echo "======== cabal build ========"
    cabal build -v 2>&1
    echo "======== END cabal build ========"

    echo "======== spec ========"
    ./dist/build/spec/spec 2>&1
    echo "======== END spec ========"

    echo "======== doctest ========"
    ./dist/build/doctest/doctest 2>&1
    echo "======== END doctest ========"
) | tee /tmp/ghc-mod-debug-info.log
echo
echo
echo "Debug info written to: /tmp/ghc-mod-debug-info.log"
