#!/usr/bin/env bash
# smoke_test.sh - MANDATORY start test after every R8 build (dx_r8.py verify is not enough).
# Usage: smoke_test.sh <app.aab> <package.name> [seconds=32]
# Needs: java, bundletool.jar (BUNDLETOOL env or ./bundletool.jar), adb with ONE device/emulator attached.
# Signs the universal APK with a throwaway debug key - never with the upload/release keystore.
set -uo pipefail
AAB=$1; PKG=$2; WAIT=${3:-32}
BT=${BUNDLETOOL:-./bundletool.jar}
OUT=$(mktemp -d)
KS=${DEBUG_KEYSTORE:-$HOME/.android/debug.keystore}
if [ ! -f "$KS" ]; then
  mkdir -p "$(dirname "$KS")"
  keytool -genkeypair -keystore "$KS" -storepass android -keypass android -alias androiddebugkey \
    -keyalg RSA -keysize 2048 -validity 10000 -dname "CN=Android Debug,O=Android,C=US" >/dev/null 2>&1
fi
java -jar "$BT" build-apks --bundle="$AAB" --output="$OUT/u.apks" --mode=universal \
  --ks="$KS" --ks-pass=pass:android --ks-key-alias=androiddebugkey --key-pass=pass:android || exit 2
unzip -o -q "$OUT/u.apks" universal.apk -d "$OUT"
adb uninstall "$PKG" >/dev/null 2>&1
adb install -r "$OUT/universal.apk" || exit 3
adb logcat -c
adb shell monkey -p "$PKG" -c android.intent.category.LAUNCHER 1 >/dev/null 2>&1
sleep "$WAIT"
PID=$(adb shell pidof "$PKG" | tr -d '\r')
adb shell dumpsys activity activities | grep -E 'mResumedActivity|topResumedActivity' | head -2
adb exec-out screencap -p > "$OUT/screen.png"
adb logcat -d -v threadtime > "$OUT/logcat.txt"
PAT='ClassNotFoundException|NoClassDefFoundError|NoSuchMethod(Error|Exception)|NoSuchField(Error|Exception)|UnsatisfiedLinkError|JNI DETECTED ERROR|FATAL EXCEPTION'
HITS=$(grep -cE "$PAT" "$OUT/logcat.txt")
grep -E "$PAT" "$OUT/logcat.txt" | head -20
echo "pid=${PID:-none} hits=$HITS logcat=$OUT/logcat.txt screenshot=$OUT/screen.png"
[ -n "$PID" ] && [ "$HITS" -eq 0 ] && echo "SMOKE: PASS (now click through ads, consent, IAP, etc. by hand)" && exit 0
echo "SMOKE: FAIL"; exit 1
