---
name: delphi-android-r8
description: >
  Use for any Delphi / RAD Studio FMX Android app that ships to Google Play as an AAB when: Play Console
  warns about low or missing DEX code optimization, shrinking or obfuscation ("extremely low rate of dex
  optimization", obfuscation 0 %, missing deobfuscation/mapping file, sometimes mistranslated, e.g. "Glare");
  the DEX in the AAB is over 10 MB; you want to run R8 / ProGuard on a Delphi build; or classes.dex is tiny,
  lacks system JARs (androidx, play-services, fmx) or FMXNativeActivity and the app crashes on start.
  Installs a tested MSBuild hook (DX.R8.targets + dx_r8.py) that runs R8 inside the Delphi build, then
  measures, verifies and start-tests the result. Delphi 13.x, Android64.
---

# Delphi FMX Android: R8 / DEX optimization

Delphi 13.x never runs R8: it dexes with D8 and merges, so Play sees ~0 % shrinking/optimization/obfuscation
and a large DEX. `scripts/DX.R8.targets` hooks in after `BuildClassesDex`, rebuilds the DEX with R8 from the
original JARs (keep rules generated automatically), and puts `mapping.txt` + `r8.json` into the AAB before
Delphi signs it. Reference result: DEX 15.24 MB -> 5.49 MB (decimal), Play reports ~82 % on all three values.

## Hard rules

- Never upload to Play, roll out, publish or submit without the app owner's explicit OK.
- Work on an experiment branch. Keep the release branch unchanged until the owner approves adopting R8.
- Sign test builds only with a throwaway debug key. Never touch the upload/release keystore for tests.
- A build that passes `verify` can still crash. The device/emulator start test (step 7) is mandatory on every R8 build.
- On a crash add the ONE specific rule that is missing. Never add blanket keeps (`-keep class com.google.** { *; }`).

## 0. Preconditions

- RAD Studio / Delphi 13.x with Android SDK + NDK, platform **Android64**, build type **AppStore** (AAB).
- JDK 17+ (Delphi's `$(JAVA_TOOL)` is used in the build), Python 3.10+ (`py -3` on Windows; override with `/p:DX_R8_Python=...`).
- R8 **9.0.32 or newer** as `tools/delphi-r8/r8/r8.jar`: run `scripts/fetch_r8.ps1` (SHA-256 pinned). Do NOT use
  Delphi's bundled R8 (cmdline-tools 16.0 = 8.6.17): it fails with `Unknown option: --build-metadata-output`.
  Add `r8/` to `.gitignore` (55 MB).
- For analysis/smoke test (any OS): `bundletool` 1.18+, `adb`, a device or emulator (API 31 x86_64 Google APIs
  image works; it runs arm64 libs via translation), optionally `android analyze` (Android CLI) for Play's numbers.

## 1. Pre-flight: is the DEX even complete? (before any R8 work)

Build the AAB once without R8 and check it:

```
python scripts/dx_r8.py check-dex --aab MyApp.aab
```

It must list `com/embarcadero/firemonkey/FMXNativeActivity`. If it is missing or the DEX is only a few hundred KB,
the `.dproj` has no `<EnabledSysJars>` for Android64: Delphi 13 then packs **no** system JARs (fmx, androidx,
play-services) and the app cannot start. Fix: enable the needed libraries under Project > Libraries for
Android64 (or copy `EnabledSysJars`/`DisabledSysJars` from a working Delphi 13 sample), rebuild, re-check.
Add `--require <class>` for classes your app must have (e.g. `com.google.android.gms.ads.MobileAds`).

## 2. Install the hook

1. Copy `scripts/` to the app repo as `tools/delphi-r8/` (DX.R8.targets, dx_r8.py, delphi-base-rules.pro, fetch_r8.*, smoke_test.sh).
2. Add ONE line at the end of the `.dproj`, right before `</Project>` (adjust the relative path):
   ```xml
   <Import Project="..\tools\delphi-r8\DX.R8.targets" Condition="Exists('..\tools\delphi-r8\DX.R8.targets')" />
   ```
3. Run `tools/delphi-r8/fetch_r8.ps1`.
4. Optional app rules: `r8-rules.pro` next to the `.dproj` (empty is the normal case).

The hook is active only for Android64 + AppStore and is on by default; `/p:DX_R8=false` switches it off.

## 3. Build both variants

From a RAD Studio command prompt (`rsvars.bat`), same config both times:

```
msbuild MyApp.dproj /t:Build;Deploy /p:Config=Release /p:Platform=Android64 /p:BT_BuildType=AppStore /p:DX_R8=false
msbuild MyApp.dproj /t:Build;Deploy /p:Config=Release /p:Platform=Android64 /p:BT_BuildType=AppStore
```

Keep both AABs. The R8 log line must show a non-zero number of consumer rule files when the app uses AARs
(`DX.R8: 94 JARs, 535 JNI classes, 9 embedded and 6 consumer rule files, 1 DEX file(s)`). Work files land in
`$(APK_OutputDir)r8\` (`r8.log`, `generated-rules.pro`, `consumer-*.pro`, `jni-classes.txt`, `mapping.txt`, `r8.json`).
If you put the msbuild call into a `.cmd` file and pass lists with `%3B`, write `%%3B`.

## 4. Measure before/after

```
android analyze before.aab      android analyze after.aab
java -jar bundletool.jar get-size total --apks=<split .apks> --device-spec=assets/arm64.json
```

Expect: DEX size down 60-70 %, "R8 enabled", shrinking/optimization/obfuscation each well above 25 %, and the
"low rate of dex optimization" warning gone. Without `r8.json` Play only estimates obfuscation. Download size
drops less (~20 %) because the native `lib<App>.so` dominates Delphi apps.

## 5. Verify JNI names

```
python tools/delphi-r8/dx_r8.py verify --jni <APK_OutputDir>/r8/jni-classes.txt --aab after.aab
```

Must be `N/N`. Any `MISSING:` line = R8 renamed or removed a class Delphi calls by name. This catches only JNI
names from the `.so`, not reflection inside Java libraries.

## 6. Archive

Keep `mapping.txt` per version code (Play gets it via BUNDLE-METADATA; you need it for stack traces elsewhere).

## 7. MANDATORY start test on device/emulator

```
tools/delphi-r8/smoke_test.sh after.aab com.example.myapp
```

(universal APK via bundletool with a debug key, `adb install`, launch, wait ~30 s, logcat grep for
`ClassNotFound|NoClassDefFound|NoSuchMethod|NoSuchField|UnsatisfiedLink|JNI DETECTED ERROR|FATAL EXCEPTION`.)
Run it for the non-R8 build too, so you know which log lines are pre-existing. Then click through every
feature that loads Java code lazily: ads (banner, interstitial, rewarded, app open, with test ad units),
consent form (UMP, debug geography EEA), "Ad privacy choices", in-app purchase, push, maps, share, camera/gallery,
review prompt, deep links, rotate, background and resume. Reflection crashes appear seconds later on worker
threads (reference: UMP crashed after ~6 s in "Google consent worker #1").

## 8. If it crashes

1. Read the stack trace: class/field/method name and the library (obfuscated names like `zze` in `d2.m1` point
   to protobuf-lite/reflection in a Google SDK).
2. First check the library's own rules reached R8: `consumer-*.pro` in the work dir must contain that AAR's
   `proguard.txt`. If not, the AAR is not in `@(AarReference)` - fix the reference, not the rules.
3. Only if the library ships no rule: add the narrowest rule to `r8-rules.pro`
   (e.g. `-keepclassmembers class * extends com.vendor.internal.Foo { <fields>; }`), rebuild, re-run steps 4-7.
4. Never "fix" with blanket keeps or by turning off obfuscation; that defeats the Play metrics.

## 9. Report to the owner

Before/after table (DEX bytes, files, the three Play percentages, download size), verify result, smoke-test
result incl. what was NOT exercised, any rules you added. Ask for the OK before merging to the release branch
or uploading anything.

## References

- `references/pitfalls.md` - every trap found so far, with symptoms and fixes. Read before debugging.
- `references/reference-numbers.md` - full measurements of the reference app (A/B/B2/C).
- `references/how-it-works.md` - Delphi 13 DEX pipeline, hook points, keep-rule generation, post-aab route.
- `assets/AGENTS-snippet.md` - short pointer to paste into an app repo's AGENTS.md / CLAUDE.md.
- `assets/arm64.json` - device spec for `bundletool get-size`.
