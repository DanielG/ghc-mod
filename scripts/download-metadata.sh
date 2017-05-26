#!/bin/sh
################################################################################
#                                                                              #
# Download package metadata for all versions on Hackage                        #
#                                                                              #
# Copyright (C)  2015  Daniel Gröber <dxld@darkboxed.org>                      #
#                                                                              #
# Copying and distribution of this file, with or without modification,         #
# are permitted in any medium without royalty provided the copyright           #
# notice and this notice are preserved.  This file is offered as-is,           #
# without any warranty.                                                        #
#                                                                              #
# Usage: ./download-metadata.sh PACKAGE OUTPUT_DIRECTORY                       #
#                                                                              #
################################################################################

PACKAGE=$1
OUTDIR=$2

versions="$(wget -O - https://hackage.haskell.org/package/"$PACKAGE"/preferred.json | jq '(."normal-version" + ."deprecated-version")[]' -r)"
#versions="$(wget -O - https://hackage.haskell.org/package/"$PACKAGE"/preferred.json | jq '."deprecated-version"[]' -r)"



mkdir -p "$OUTDIR"

for v in $versions; do

    wget https://hackage.haskell.org/package/"$PACKAGE-$v"/"$PACKAGE".cabal -O "$OUTDIR/${PACKAGE}-${v}.cabal" &

done
