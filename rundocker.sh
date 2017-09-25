#!/bin/sh

# Start up a docker vm using the gitlab CI container

# docker run --rm -i -t  registry.gitlab.com/dxld/ghc-mod:ghc8.2.1-cabal-install2.0.0.0 /bin/bash
docker run --rm -i -t -v `pwd`:/root   registry.gitlab.com/dxld/ghc-mod:ghc8.2.1-cabal-install2.0.0.0 /bin/bash
