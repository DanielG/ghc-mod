#!/bin/sh

# Usage: ./docker.sh [GHC_VER...]
#
# Example: ./docker.sh        #< build all images
#
# Example: ./docker.sh 8.2.2  #< build just 8.2.2 image
#
# Faster Example:
#     $ SWAP=1 ./docker.sh tmux 8.6.5 8.4.4 8.2.2 8.0.2 7.10.3
#
# tmux cheatsheet:
#    quit: Ctrl-b & y
#    other-window: Ctrl-b o
#    scroll: Ctrl-b [

if [ -n "$SWAP" ] && ! [ -e /tmp/swap ]; then
        # We run out of memory with 5 concurrent builds on the Vultr 4 core
        # instance.
        swapoff /tmp/swap || true
        dd if=/dev/zero of=/tmp/swap bs=4k count=4M # 16GiB
        chmod 0600 /tmp/swap
        mkswap /tmp/swap
        swapon /tmp/swap
fi

if [ "$1" = tmux ]; then
        shift
        tmux kill-session -t docker || true
        exec tmux new -s docker sh -x "$0" do-tmux "$@"
elif [ "$1" = do-tmux ]; then
        shift
        myghc=$1; shift

        tmux set-option remain-on-exit on

        for ghc in "$@"; do
                tmux split-window sh -c "$0 $ghc 2>&1 | tee $ghc.log"
        done

        tmux select-layout even-vertical

        set -- $myghc
fi

namespace="registry.gitlab.com/dxld/ghc-mod"

tmpdir=$(mktemp -p "${TMPDIR:-/tmp/}" -d ghc-mod-docker-XXXX) || exit 1
if [ -z "$DEBUG" ]; then
        trap 'echo This is $ghc signing off>&2; rm -r '"$tmpdir" 0 2 15
fi

dldir="/tmp/ghc-mod-docker-dl"

GHC_BASE=https://downloads.haskell.org/~ghc
HACKAGE_BASE=https://hackage.haskell.org/package
STACK_BASE=https://github.com/commercialhaskell/stack/releases/download

while read -r ghc ghc_arch ghc_ext cabal cabal_rev stack image; do
    if [ $# -eq 0 ]; then cont=false; else cont=true; fi
    for target in "$@"; do
        if [ x"$ghc" = x"$target" ]; then cont=false; fi
    done
    if $cont; then continue; fi

    ghc_url="${GHC_BASE}/${ghc}/ghc-${ghc}-${ghc_arch}.tar.${ghc_ext}"

    cabal_base="${HACKAGE_BASE}/cabal-install-${cabal}"
    cabal_url="${cabal_base}/cabal-install-${cabal}.tar.gz"
    cabal_meta_url="${cabal_base}/revision/${cabal_rev}.cabal"

    stack_url="${STACK_BASE}/v${stack}/stack-${stack}-linux-x86_64-static.tar.gz"
    ghc_file="$(basename "$ghc_url")"
    cabal_file="$(basename "$cabal_url")"
    cabal_meta_file=cabal-install-"${cabal}"-"${cabal_rev}".cabal
    stack_file="$(basename "$stack_url")"

    ADDITIONAL_PACKAGES=""
    ADDITIONAL_COMMANDS=""
    ADDITIONAL_BOOTSTRAP_SETUP="true"
    ADDITIONAL_BOOTSTRAP_TEARDOWN="true"

    if [ $ghc = "7.10.3" ]; then
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
    wget -nv -nc -c "$stack_url"      && cp -l "$stack_file" "$tmpdir"

    ls -l "$tmpdir"
    cd "$d" || exit 1

    ADDITIONAL_COMMANDS="${ADDITIONAL_COMMANDS}COPY $ghc_file $cabal_file $cabal_bin_file $cabal_meta_file $stack_file /root/
"

    cat > "$tmpdir"/Dockerfile <<EOF
FROM $image

## ensure locale is set during build
ENV LANG C.UTF-8

WORKDIR /root

$ADDITIONAL_COMMANDS

RUN ( cd /usr/local/bin && \
      tar -xvf /root/${stack_file} \
          --strip-components 1 --wildcards '*/stack' && \
      cd && stack --help >/dev/null \
      )
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

    docker build -t "${namespace}:ghc${ghc}-cabal-install${cabal}-stack${stack}" "$tmpdir"
done <<EOF
   8.6.5  x86_64-deb8-linux xz   2.4.1.0  3   2.1.1   debian:jessie
   8.6.3  x86_64-deb8-linux xz   2.4.1.0  0   2.1.1   debian:jessie
   8.4.4  x86_64-deb8-linux xz   2.4.1.0  0   2.1.1   debian:jessie
   8.2.2  x86_64-deb8-linux xz   2.4.1.0  0   2.1.1   debian:jessie
   8.0.2  x86_64-deb8-linux xz   2.4.1.0  0   2.1.1   debian:jessie
   7.10.3 x86_64-deb8-linux xz   2.4.1.0  0   2.1.1   debian:jessie
EOF
#  ^-ver  ^-arch                 ^-ver rev^   ^-ver     ^-base img
# |--------------------------|   |-------|   |-----|    |-----------|
#                GHC             caba-inst    Stack         Docker

## Old versions:

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
