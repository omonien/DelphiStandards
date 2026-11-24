# Reference measurements

App: Scary Recipes 2.0.1 (Delphi 13.1 / BDS 37.0, FMX, Android64, AAB, AdMob 25.4.0 + UMP 4.0.0 via Kastri),
R8 9.0.32, JDK 21. Measured 24 Nov 2026 with `android analyze`, `bundletool get-size` (arm64-v8a, SDK 34) and an
API 31 emulator.

| Value | A: no R8 (D8) | B: R8, hook without AAR rules | B2: R8, final hook | C: B2 minus 17 unused system JARs |
|---|---|---|---|---|
| DEX files | 2 | 1 | 1 | 1 |
| DEX bytes | 15,244,604 | 2,062,876 | 5,491,772 | 5,224,976 |
| DEX MB decimal / MiB | 15.24 / 14.54 | 2.06 / 1.97 | 5.49 / 5.24 | 5.22 / 4.98 |
| Change vs A | | -86.5 % | -64.0 % | -65.7 % |
| Shrinking (Play) | n/a | 65.61 % | 82.61 % | 83.58 % |
| Optimization (Play) | n/a | 65.60 % | 82.22 % | 83.17 % |
| Obfuscation (Play) | heuristic 2.54 % | 65.71 % | 82.64 % | 83.62 % |
| "low rate of dex optimization" warning | yes | no | no | no |
| R8 input JARs | | 94 | 94 | 77 |
| JNI classes from .so, `verify` | 535/535 | 535/535 | 535/535 | 467/467 |
| Consumer rule files applied | | 0 | 6 | 6 |
| Download arm64 (bytes) | 15,375,342 | 10,465,004 | 12,153,890 | 12,043,628 |
| Emulator start test | runs | **crash after 6 s** | runs + click test | runs + click test |

Notes
- Play reads `noShrinkingPercentage`/`noOptimizationPercentage`/`noObfuscationPercentage` from `r8.json`
  (BUNDLE-METADATA/com.android.tools/r8.json) and shows 100 minus these. Without `r8.json` only an obfuscation
  heuristic is shown.
- B is smaller but broken: without the AARs' consumer rules R8 stripped fields that UMP/AdMob read via reflection.
- AAB size grows because `proguard.map` sits in BUNDLE-METADATA; Play does not ship it to devices.
- No hand-written keep rules were needed; `r8-rules.pro` did not exist.
- Not exercised in the emulator: actual ad loading/rendering, consent form, "Ad privacy choices", review prompt
  (consent was not configured in AdMob for the test app ID).
