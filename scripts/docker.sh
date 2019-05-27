#!/bin/sh

# Usage: ./docker.sh [GHC_VER]
# Example: ./docker.sh 8.2.2
# Faster Example:
#     $ echo 8.6.3 8.4.4 8.2.2 8.0.2 7.10.3 | xargs -P4 -n1 -- ./docker.sh


namespace="registry.gitlab.com/dxld/ghc-mod"
target="$1"

tmpdir=$(mktemp -p "${TMPDIR:-/tmp/}" -d ghc-mod-docker-XXXX) || exit 1
[ -z "$DEBUG" ] && trap 'rm -r '"$tmpdir" 0 2 15

dldir="/tmp/ghc-mod-docker-dl"

while read -r ghc_rel ghc ghc_arch ghc_ext cabal cabal_rev image; do
    [ -n "$target" ] && [ x"$ghc_rel" != x"$target" ] && continue

    ghc_url="https://downloads.haskell.org/~ghc/${ghc_rel}/ghc-${ghc}-${ghc_arch}.tar.${ghc_ext}"
    cabal_url="https://hackage.haskell.org/package/cabal-install-${cabal}/cabal-install-${cabal}.tar.gz"
    cabal_meta_url="https://hackage.haskell.org/package/cabal-install-${cabal}/revision/${cabal_rev}.cabal"

    ghc_file="$(basename "$ghc_url")"
    cabal_file="$(basename "$cabal_url")"
    cabal_meta_file=cabal-install-"${cabal}"-"${cabal_rev}".cabal

    ADDITIONAL_PACKAGES=""
    ADDITIONAL_PURGE_PACKAGES=""
    ADDITIONAL_COMMANDS=""
    ADDITIONAL_BOOTSTRAP_SETUP="true"
    ADDITIONAL_BOOTSTRAP_TEARDOWN="true"

    if [ $ghc_rel = "7.10.3" ]; then
            cabal_boot=2.2.0.0
            cabal_bin_url="https://www.haskell.org/cabal/release/cabal-install-${cabal_boot}/cabal-install-${cabal_boot}-x86_64-unknown-linux.tar.gz"
            cabal_bin_file="$(basename "$cabal_bin_url")"
            wget -nv -nc -c "$cabal_bin_url"  && cp -l "$cabal_bin_file" "$tmpdir"
            ADDITIONAL_COMMANDS="${ADDITIONAL_COMMANDS}COPY $cabal_bin_file /root/
"
            ADDITIONAL_BOOTSTRAP_SETUP='tar -C /root -xf /root/'"$cabal_bin_file"' cabal && /root/cabal update && /root/cabal install --only-dependencies'
            ADDITIONAL_BOOTSTRAP_TEARDOWN='rm /root/cabal '"/root/$cabal_bin_file"
    fi

    mkdir -p "$dldir"
    d="$(pwd)"; cd "$dldir" || exit 1
    wget -nv -nc -c "$ghc_url"        && cp -l "$ghc_file" "$tmpdir"
    wget -nv -nc -c "$cabal_url"      && cp -l "$cabal_file" "$tmpdir"
    if [ ! -e "$dldir"/"${cabal_meta_file}" ]; then
        wget -nv "$cabal_meta_url" -O "$dldir"/"${cabal_meta_file}"
    fi && cp -l "$dldir"/"${cabal_meta_file}" "$tmpdir"

    ls -l "$tmpdir"
    cd "$d" || exit 1

    ADDITIONAL_COMMANDS="${ADDITIONAL_COMMANDS}COPY $ghc_file $cabal_file $cabal_bin_file $cabal_meta_file /root/
"

    cat > "$tmpdir"/Dockerfile <<EOF
FROM $image

## ensure locale is set during build
ENV LANG C.UTF-8

WORKDIR /root

$ADDITIONAL_COMMANDS

RUN apt-get update && apt-get upgrade -y && \
    apt-get install -y --no-install-recommends \
      alex happy wget git xz-utils gpgv ca-certificates build-essential libgmp3-dev libtinfo-dev zlib1g-dev netbase pkg-config $ADDITIONAL_PACKAGES && \
    apt-get clean
RUN tar -xf ghc-*.tar.* && \
    cd ghc-* && ./configure --prefix=/usr/local && make install && cd .. && \
    rm -r ghc-*
RUN tar -xf cabal-install-${cabal}.tar.* && \
    cd cabal-install-*/ && \
      cp /root/$cabal_meta_file cabal-install.cabal && \
      ${ADDITIONAL_BOOTSTRAP_SETUP} && \
      sh -x ./bootstrap.sh --no-doc --user && \
      cp ~/.cabal/bin/cabal /usr/local/bin && \
      rm -r ~/.cabal ~/.ghc && \
      ${ADDITIONAL_BOOTSTRAP_TEARDOWN} && \
    cd .. && \
    ls -l /root && \
    rm -r cabal-*
EOF

    docker build -t "${namespace}:ghc${ghc}-cabal-install${cabal}" "$tmpdir"
done <<EOF
8.6.5  8.6.5  x86_64-deb8-linux         xz  2.4.1.0   3 debian:jessie
8.6.3  8.6.3  x86_64-deb8-linux         xz  2.4.1.0   0 debian:jessie
8.4.4  8.4.4  x86_64-deb8-linux         xz  2.4.1.0   0 debian:jessie
8.2.2  8.2.2  x86_64-deb8-linux         xz  2.4.1.0   0 debian:jessie
8.0.2  8.0.2  x86_64-deb8-linux         xz  2.4.1.0   0 debian:jessie
7.10.3 7.10.3 x86_64-deb8-linux         xz  2.4.1.0   0 debian:jessie
EOF

# 7.8.4  7.8.4  x86_64-unknown-linux-deb7 xz  2.2.0.0   0 debian:jessie

# 8.0.2  8.0.2  x86_64-deb8-linux         xz  1.24.0.2  0 debian:jessie
# 7.10.3 7.10.3 x86_64-deb8-linux         xz  1.24.0.2  0 debian:jessie
# 7.8.4  7.8.4  x86_64-unknown-linux-deb7 xz  1.24.0.2  0 debian:jessie
# 7.6.3  7.6.3  x86_64-unknown-linux      bz2 1.24.0.2  0 debian:squeeze
# 7.4.2  7.4.2  x86_64-unknown-linux      bz2 1.24.0.2  0 debian:squeeze

# 7.2.2  7.2.2  x86_64-unknown-linux      bz2 1.24.0.2  0 debian:squeeze
# 7.0.4  7.0.4  x86_64-unknown-linux      bz2 1.24.0.2  0 debian:squeeze

# 7.10.3 7.10.3 x86_64-deb8-linux         xz  1.22.9.0  0 debian:jessie
# 7.8.4  7.8.4  x86_64-unknown-linux-deb7 xz  1.18.2.0  0 debian:jessie
# 7.6.3  7.6.3  x86_64-unknown-linux      bz2 1.18.2.0  0 debian:squeeze
# 7.4.2  7.4.2  x86_64-unknown-linux      bz2 1.18.2.0  0 debian:squeeze
# 7.2.2  7.2.2  x86_64-unknown-linux      bz2 1.18.2.0  0 debian:squeeze
# 7.0.4  7.0.4  x86_64-unknown-linux      bz2 1.18.2.0  0 debian:squeeze
