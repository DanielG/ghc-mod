#!/bin/sh

PACKAGE=$1
OUTDIR=$2

versions="$(wget -O - https://hackage.haskell.org/package/"$PACKAGE"/preferred.json | jq '(."normal-version" + ."deprecated-version")[]' -r)"

mkdir -p "$OUTDIR"

for v in $versions; do
    date=$(wget -O - https://hackage.haskell.org/package/"$PACKAGE-$v" \
                  | w3m -dump -T text/html \
                  | grep Uploaded \
                  | sed -r 's/^\s+Uploaded\s+(.*) by .*$/\1/')

    date --date="$date" '+%s' > "$OUTDIR"/"$PACKAGE-$v".upload-date
done
