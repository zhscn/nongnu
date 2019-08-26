#!/bin/bash

# Dump the ghc flags that cabal-install uses to launch a repl session for
# all components into files named `.ghc.flags.component'.
#
# This is a partial workaround to https://github.com/haskell/cabal/issues/6203
#
# Note that this flushes the build plan cache and only supports the default
# build flags. If users wish to include test phases they must add tests: True
# to their cabal.project.local

# set -e -x -o pipefail

TMP="/tmp/$PWD/hack-cabal"
mkdir -p "$TMP" 2> /dev/null

# to ensure the json plan is in place
cabal v2-build -v0 :all --only-dependencies

if [ ! -d dist-newstyle ] ; then
    echo "dist-newstyle not found"
    exit 1
fi

GHC=$(cabal v2-exec -v2 ghc -- --numeric-version | tail -2 | head -1 | sed 's/ .*//')
GHC_PKG=$(echo "$GHC" | rev | sed 's/chg/gkp-chg/' | rev)

# ghc is called multiple times during the v2-repl startup.
# The only call that we're interested in is this one.
cat <<EOF > "$TMP/ghc"
#!/bin/bash
if [ "\$1" == "--interactive" ]; then
    echo -n "\${@:2}" >> "$TMP/out"
else
    exec "$GHC" "\$@"
fi
EOF
chmod 755 "$TMP/ghc"

cat <<EOF > "$TMP/ghc-pkg"
#!/bin/bash
exec "$GHC_PKG" "\$@"
EOF
chmod 755 "$TMP/ghc-pkg"

jq -c '(.["install-plan"][] | select(.["pkg-src"].type == "local") | select(.["component-name"] != null) | [ .["pkg-name"], .["component-name"], .["pkg-src"].path, .id ] )' dist-newstyle/cache/plan.json | while read LINE ; do
    NAME=$(echo "$LINE" | jq -r '.[0]')
    PART=$(echo "$LINE" | jq -r '.[1]')
    ROOT=$(echo "$LINE" | jq -r '.[2]')
    ID=$(echo "$LINE" | jq -r '.[3]')

    if [ "$PART" == "lib" ] ; then
        COMPONENT="lib:$NAME"
    else
        COMPONENT="$PART"
    fi

    rm "$TMP/out" 2> /dev/null
    cabal v2-repl -v0 -w "$TMP/ghc" "$NAME:$COMPONENT"

    # extract all the source directories that use these flags
    for D in $(cat "$TMP/out" | tr ' ' '\n' | grep '^-i' | sed 's/^-i//' | sed '/^$/d') ; do
        if [ -d "$D" ] ; then
            echo "writing $D/.ghc.flags"
            cat  "$TMP/out" > "$D/.ghc.flags"
        fi
    done
done

if [ -d "$TMP" ] ; then
  rm -rf "$TMP"
fi

# try our best to reset the cache to what the user expects
cabal v2-build -v0 :all --dry
