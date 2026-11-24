#!/usr/bin/env bash
# fetch_r8.sh - same as fetch_r8.ps1 for macOS/Linux helpers (e.g. a CI or analysis box).
set -euo pipefail
VERSION=9.0.32
EXPECTED=ce5b937b03efea9ce63bd5b534faed130e00ffe8e0faad4098c1bb436916c17b
DIR="$(cd "$(dirname "$0")" && pwd)/r8"; mkdir -p "$DIR"; JAR="$DIR/r8.jar"
[ -f "$JAR" ] || curl -fsSL -o "$JAR" "https://storage.googleapis.com/r8-releases/raw/$VERSION/r8.jar"
HASH=$( (sha256sum "$JAR" 2>/dev/null || shasum -a 256 "$JAR") | cut -d' ' -f1)
[ "$HASH" = "$EXPECTED" ] || { rm -f "$JAR"; echo "SHA-256 mismatch: $HASH" >&2; exit 1; }
java -cp "$JAR" com.android.tools.r8.R8 --version
echo "R8_JAR=$JAR SHA256=$HASH"
