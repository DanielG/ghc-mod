#!/bin/sh

set -e

rm cabal.sandbox.config
rm -r .cabal-sandbox

cabal sandbox init
