#!/usr/bin/env bash

# Dump the ghc flags that cabal-install uses to launch a repl session for
# all components into files named `.ghc.flags.component'.
#
# This is a partial workaround to https://github.com/haskell/cabal/issues/6203
#
# Note that this flushes the build plan cache and only supports the default
# build flags. If users wish to include test phases they must add tests: True
# to their cabal.project.local

set -e

TMP="/tmp/cabal-ghcflags-$USER/$PWD"
mkdir -p "$TMP" 2> /dev/null || true

# to ensure the json plan is in place
echo "Resolving dependencies"
# cabal v2-build -v0 :all --dry
# best just ensure deps are compiled otherwise the repl compiles them
cabal v2-build :all --only-dependencies

if [ ! -f dist-newstyle/cache/plan.json ] ; then
    echo "dist-newstyle/cache/plan.json not found"
    exit 1
fi

# seems to be a bug in cabal...
mkdir dist-newstyle/tmp 2> /dev/null || true

cabal v2-exec -v0 ghc -- --numeric-version > .ghc.version
GHC=$(cabal v2-exec -v2 ghc -- --numeric-version | tail -2 | head -1 | sed 's/ .*//')

# ghc is called multiple times during the v2-repl startup.
# The only call that we're interested in is this one.
cat <<EOF > "$TMP/ghc"
#!/usr/bin/env bash
if [ "\$1" == "--interactive" ]; then
    echo -n "\${@:2}" >> "\$OUTPUT"
else
    exec "$GHC" "\$@"
fi
EOF
chmod 755 "$TMP/ghc"

GHC_PKG=$(echo "$GHC" | rev | sed 's/chg/gkp-chg/' | rev)
cat <<EOF > "$TMP/ghc-pkg"
#!/usr/bin/env bash
exec "$GHC_PKG" "\$@"
EOF
chmod 755 "$TMP/ghc-pkg"

HSC2HS=$(echo "$GHC" | rev | sed 's/chg/sh2csh/' | rev)
cat <<EOF > "$TMP/hsc2hs"
#!/usr/bin/env bash
exec "$HSC2HS" "\$@"
EOF
chmod 755 "$TMP/hsc2hs"

create_ghcflags() {
    LINE="$1"

    NAME=$(echo "$LINE" | jq -r '.[0]')
    PART=$(echo "$LINE" | jq -r '.[1]')
    ROOT=$(echo "$LINE" | jq -r '.[2]')
    ID=$(echo "$LINE" | jq -r '.[3]')

    if [ "$PART" == "lib" ] ; then
        COMPONENT="lib:$NAME"
    else
        COMPONENT="$PART"
    fi

    echo "  $NAME:$COMPONENT $ID"

    export OUTPUT="$TMP/out.$NAME:$COMPONENT"

    rm "$OUTPUT" 2> /dev/null || true
    cabal v2-repl -v0 -w "$TMP/ghc" "$NAME:$COMPONENT"

    # TODO also need to provide the PATH, since it might include commands that
    # are referred to from compiler plugins (e.g. tasty-discover).

    # extract all the source directories that use these flags
    for D in $(cat "$OUTPUT" | tr ' ' '\n' | grep '^-i' | sed 's/^-i//' | sed '/^$/d') ; do
        if [[ "$D" != /* ]] ; then
            D="$ROOT/$D"
            if [ -d "$D" ] ; then
                # echo "    $D/.ghc.flags"
                cat  "$OUTPUT" > "$D/.ghc.flags"
            fi
        fi
    done
}

echo "Inspecting build plan"
for LINE in $(jq -c '(.["install-plan"][] | select(.["pkg-src"].type == "local") | select(.["component-name"] != null) | [ .["pkg-name"], .["component-name"], .["pkg-src"].path, .id ] )' dist-newstyle/cache/plan.json) ; do
    # NOTE: could be done in parallel, but I haven't measured it being faster
    create_ghcflags "$LINE"
done

wait

if [ -d "$TMP" ] ; then
  rm -rf "$TMP"
fi
