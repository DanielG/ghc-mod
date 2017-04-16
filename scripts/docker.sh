#!/bin/sh

namespace="registry.gitlab.com/dxld/ghc-mod"
target="$1"

tmpdir=$(mktemp -p "${TMPDIR:-/tmp/}" -d ghc-mod-docker-XXXX) || exit 1
trap 'rm -r '"$tmpdir" 0 2 15

while read ghc_rel ghc ghc_arch ghc_ext cabal image ghc_hash cabal_hash; do
    [ -n "$target" -a x"$ghc_rel" != x"$target" ] && continue

    ghc_url="https://downloads.haskell.org/~ghc/${ghc_rel}/ghc-${ghc}-${ghc_arch}.tar.${ghc_ext}"
    cabal_install_url="https://www.haskell.org/cabal/release/cabal-install-${cabal}/cabal-install-${cabal}.tar.gz"

    ADDITIONAL_COMMANDS=""
    WGET_OPTIONS=""

    if [ x"$image" = x"debian:squeeze" ]; then
        WGET_OPTIONS="--no-check-certificate"
        GHC_VERIFY_COMMANDS="sha256sum -c ghc.sha256sum &&"
        CABAL_VERIFY_COMMANDS="sha256sum -c cabal.sha256sum &&"
        ADDITIONAL_PACKAGES=" libncursesw5"
        ADDITIONAL_COMMANDS='COPY sources.list /etc/apt/
COPY 10-no-check-valid-until /etc/apt/apt.conf.d/
COPY *.sha256sum /root/
'

        echo "$ghc_hash  ghc-${ghc}-${ghc_arch}.tar.${ghc_ext}" \
            | cat > "$tmpdir"/ghc.sha256sum
        echo "$cabal_hash  cabal-install-${cabal}.tar.gz" \
            | cat > "$tmpdir"/cabal.sha256sum

        cat > "$tmpdir"/sources.list <<EOF
deb http://archive.debian.org/debian-archive/debian/ squeeze main
deb http://archive.debian.org/debian-archive/debian/ squeeze-lts main
#deb http://snapshot.debian.org/archive/debian-security/20160216T165545Z/ squeeze/updates main
EOF
        cat > "$tmpdir"/10-no-check-valid-until <<EOF
Acquire::Check-Valid-Until "0";
EOF
    fi

    cat > "$tmpdir"/Dockerfile <<EOF
FROM $image
ARG GHC_VER
ARG GHC_URL
ARG CABAL_INSTALL_URL

## ensure locale is set during build
ENV LANG C.UTF-8

WORKDIR /root

$ADDITIONAL_COMMANDS

RUN apt-get update && apt-get upgrade && \
    apt-get install -y --no-install-recommends \
      wget xz-utils gpgv ca-certificates build-essential libgmp3-dev zlib1g-dev $ADDITIONAL_PACKAGES && \
    apt-get clean
RUN wget -nv $WGET_OPTIONS "\$GHC_URL" && $GHC_VERIFY_COMMANDS \
    tar -xf ghc-*.tar.* && \
    cd ghc-* && ./configure --prefix=/usr/local && make install && cd .. && \
    rm -r ghc-*
RUN wget -nv $WGET_OPTIONS "\$CABAL_INSTALL_URL" && $CABAL_VERIFY_COMMANDS \
    tar -xf cabal-install-*.tar.* && \
    cd cabal-install-* && ./bootstrap.sh --global && cd .. && \
    rm -r cabal-*
EOF

    docker build \
           --build-arg GHC_URL="$ghc_url" \
           --build-arg CABAL_INSTALL_URL="$cabal_install_url" \
           -t "${namespace}:ghc${ghc}-cabal-install${cabal}" \
           "$tmpdir"
done <<EOF
8.2.1-rc1  8.2.0.20170404  x86_64-deb8-linux          xz  1.24.0.2  debian:jessie
8.0.2      8.0.2           x86_64-deb8-linux          xz  1.24.0.2  debian:jessie
7.10.3     7.10.3          x86_64-deb8-linux          xz  1.22.8.0  debian:jessie
7.8.4      7.8.4           x86_64-unknown-linux-deb7  xz  1.18.1.0  debian:jessie
7.6.3      7.6.3           x86_64-unknown-linux       bz2 1.18.1.0  debian:squeeze 398dd5fa6ed479c075ef9f638ef4fc2cc0fbf994e1b59b54d77c26a8e1e73ca0 d6abb6fef8204780a41aff2e93dfa297883673507cec557348aebf6b37843ae4
7.4.2      7.4.2           x86_64-unknown-linux       bz2 1.18.1.0  debian:squeeze da962575e2503dec250252d72a94b6bf69baef7a567b88e90fd6400ada527210 d6abb6fef8204780a41aff2e93dfa297883673507cec557348aebf6b37843ae4
7.2.2      7.2.2           x86_64-unknown-linux       bz2 1.18.1.0  debian:squeeze 43dd8b0f9bf713d51d2b1f3a3f5184add39d2a1375f7d3bec496e8a7520297f1 d6abb6fef8204780a41aff2e93dfa297883673507cec557348aebf6b37843ae4
7.0.4      7.0.4           x86_64-unknown-linux       bz2 1.18.1.0  debian:squeeze 29e122db0b7720a1604d9d3865fcc6d28b06acadcebd4f303f82fcce30cc8455 d6abb6fef8204780a41aff2e93dfa297883673507cec557348aebf6b37843ae4
EOF
