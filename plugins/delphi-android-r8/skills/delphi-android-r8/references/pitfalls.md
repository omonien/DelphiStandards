# Pitfalls: R8 for Delphi FMX Android

Found while bringing R8 to a Delphi 13.1 FMX app with AdMob + UMP (Nov 2026). All fixes are in `scripts/`.

| # | Symptom | Cause | Fix |
|---|---|---|---|
| 1 | App starts, then crashes after ~6 s on a worker thread: `RuntimeException: Field zze for d2.m1 not found` / `NoSuchFieldException` (thread "Google consent worker #1"). `verify` was green. | Delphi extracts each AAR to `AndroidLibraries\<name>\` keeping only `AndroidManifest.xml`, `classes.jar`, `res.zip`, `R-prepended.txt`. **`proguard.txt` is dropped**, so the library's consumer rules never reach R8. | `DX.R8.targets` passes `@(AarReference->'%(FullPath)')`; `dx_r8.py` reads `proguard.txt` straight from every `.aar` (`consumer-NNN.pro`). Check the build line shows `N consumer rule files` > 0. |
| 2 | `WinError 123` / broken path in Python on the first R8 build. | `--work "...\r8\"`: the trailing backslash escapes the closing quote on Windows. | Pass `"$(DX_R8_Work)."` (trailing dot). |
| 3 | `Unknown option: --build-metadata-output`. | Delphi's bundled R8 (Android SDK cmdline-tools 16.0) is 8.6.17. The option exists in 9.0.32 but is not listed in `--help`. | Use R8 9.0.32+ (`fetch_r8.ps1`, SHA-256 pinned). Without `r8.json` Play only estimates obfuscation. |
| 4 | `MSB4023` / path too long, a single "file" named `activity-1.7.2.dex.jarBannotation...`. | Overriding a list property (`/p:EnabledSysJars=a%3Bb`) inside a `.cmd` file: cmd.exe reads `%3` as batch parameter 3 (empty), leaving `B`. | Write `%%3B` in `.cmd` files (single `%3B` on the command line / PowerShell). |
| 5 | `r8.log` contains dozens of `Missing class ...` warnings, build still succeeds. | `delphi-base-rules.pro` ships `-ignorewarnings` for the first round. Reference app: 47 missing classes (androidx.appcompat, androidx.biometric, play.core.hsdp, privacysandbox topics); none caused a runtime error. | `dx_r8.py` prints the count. For production review them and replace `-ignorewarnings` with targeted `-dontwarn` lines in `r8-rules.pro`. |
| 6 | `post-aab` route produces a crashing app. | Rewriting a finished AAB has no `@(AarReference)`, so no consumer rules. | Prefer the in-build hook. If you must use `post-aab`, pass `--consumer-rules <list of .aar>`; the script warns when you don't. |
| 7 | DEX tiny (~285 KB), no `FMXNativeActivity`, app cannot start - even without R8. | `.dproj` has no `<EnabledSysJars>` for Android64. Delphi 13 runs `GetPredexedSysJars` only when it is non-empty, so no system JARs (fmx, androidx, play-services) are packed. | Enable the libraries for Android64 in Project > Libraries (or copy `EnabledSysJars` from a working sample). Run `dx_r8.py check-dex` before every upload. |
| 8 | `verify` reports a few JNI names "missing" from the non-R8 build too (e.g. maps classes). | Delphi registers imports of all used units; classes it resolves lazily (maps) may be absent if the JAR is disabled. | Harmless as long as the feature is never used. If FMX resolved them at start-up the JAR would have to stay. |
| 9 | Disabling room breaks ads. | `play-services-ads` uses WorkManager, which needs room. | Keep `room-common`/`room-runtime` when disabling unused system JARs. |
| 10 | Emulator too slow / won't start. | No KVM access. | Give the user rw on `/dev/kvm`. API 31 x86_64 Google APIs images run arm64-only Delphi libs via native bridge translation. |
| 11 | UMP logs `Publisher misconfiguration ... no form(s) configured for the input app ID`. | AdMob account has no consent message for this Android app ID. Not R8-related (appears without R8 too). | Configure the message in AdMob > Privacy & messaging, otherwise the consent and ad paths cannot be tested. |
| 12 | Internal target names change after a Delphi update. | The hook depends on `BuildClassesDex`, `_AndroidCreateBaseModuleFile`, `_AndroidAppBundle`, `_AndroidSign`, `@(_ClasspathFiles)`, `@(AarReference)`. | Re-check `CodeGear.Common.Targets` / `CodeGear.Deployment.Targets` after each update; the script fails loudly when inputs are missing. |

Also: download size gains are modest (~20 %) because the native `.so` dominates; the real win is passing Play's
DEX optimization check and readable crash reports via `mapping.txt`.

Smoke-test noise: `UnsafeUtil: platform method missing - proto runtime falling back ... NoSuchMethodException:
sun.misc.Unsafe.copyMemory` is a harmless protobuf fallback warning, not an R8 problem. Judge by
`FATAL EXCEPTION`, by the process still running, and by comparing with the non-R8 build's log.
