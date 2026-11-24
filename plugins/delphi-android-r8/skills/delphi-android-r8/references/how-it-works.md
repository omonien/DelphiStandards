# How it works

## Delphi 13 DEX pipeline (CodeGear.Common.Targets / CodeGear.Deployment.Targets, BDS 37.0)

1. `GetPredexedSysJars`: enabled system libraries (`EnabledSysJars`) exist in `$(BDS)\lib\android\release\` as
   pre-dexed `xyz.dex.jar` and plain `xyz.jar` (metadata `%(Jar)`). Runs only if `EnabledSysJars` is non-empty.
2. `GetProjectJars` / `_AndroidResolveAndroidLibJarFiles`: project JARs (`JavaReference`), `classes.jar` of each
   AAR (`AarReference`, extracted to `$(APK_OutputDir)AndroidLibraries\<name>\`, **without proguard.txt**), `R.jar`.
3. `BuildPredexedJar`: D8 (`dexer-1.0.1.jar`) per project JAR.
4. `BuildClassesDex`: `PAClient AndroidDexMerge` -> `$(ClassesDir)\classes*.dex`.
5. `_AndroidCreateBaseModuleFile` -> `BaseModule.zip`, `_AndroidAppBundle` (bundletool) -> unsigned AAB,
   `_AndroidSign` (jarsigner).
No R8/ProGuard anywhere; no `.pro` files in `lib\android`.

## The hook

- `DX_R8_Shrink` (AfterTargets `BuildClassesDex`, BeforeTargets `_AndroidCreateBaseModuleFile`): writes
  `@(_ClasspathFiles)` (non-dexed inputs), the `.so` list and the consumer-rule list (`@(AarReference)`), then runs
  `dx_r8.py shrink`, which replaces `classes*.dex` in `$(ClassesDir)`. (R8 produced a single DEX in the reference app; multi-DEX output from R8 is untested.)
- `DX_R8_BundleMetadata` (AfterTargets `_AndroidAppBundle`, BeforeTargets `_AndroidSign`): adds
  `BUNDLE-METADATA/com.android.tools.build.obfuscation/proguard.map` and `BUNDLE-METADATA/com.android.tools/r8.json`.
  Delphi then signs as usual; the keystore is never touched by the hook.

## Keep rules (generated, no hand-written keeps needed)

1. `delphi-base-rules.pro`: `com.embarcadero.**` fully, native methods, `R$*` fields, enums, Parcelable,
   Serializable, view constructors, `androidx.startup.Initializer`, Firebase `ComponentRegistrar`, attributes.
2. JNI names: Delphi stores every imported Java class as a string (`JavaSignature('com/google/.../AdView')`).
   `dx_r8.py` scans `lib*.so` for slash and dot names present in the input JARs and keeps them fully, plus their
   whole super-class/interface chain (Delphi calls inherited methods by name through the subclass).
3. Manifest entry points from `AndroidManifest-merged.xml` (incl. `com.google.firebase.components:<class>`).
4. Rules embedded in JARs (`META-INF/proguard/`, `META-INF/com.android.tools/`).
5. AAR consumer rules: `proguard.txt` read directly from each `.aar`.
6. Optional `r8-rules.pro` next to the `.dproj`.

R8 runs with `--release --min-api <MinSdk> --lib android.jar --pg-map-output --pg-conf-output
--build-metadata-output` via an `@argfile` (Windows command-line length).

## Route 2: `post-aab`

Rewrites a finished AAB (unpack, rebuild `base/dex/` with R8, repack, re-sign yourself). Emergency only, for CI
chains that may not touch the `.dproj`. Not tested end to end; needs `--consumer-rules` with the `.aar` list.
