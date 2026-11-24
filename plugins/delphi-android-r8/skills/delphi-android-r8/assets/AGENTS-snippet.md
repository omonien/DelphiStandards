## Delphi Android: R8 / Google Play DEX optimization

This is a Delphi FMX Android app. Before touching Android packaging, Play Console DEX/obfuscation warnings,
R8/ProGuard, `<EnabledSysJars>`, or an AAB with a DEX over 10 MB, read and follow the skill
`delphi-android-r8` (SKILL.md in https://github.com/omonien/DelphiStandards/tree/master/plugins/delphi-android-r8/skills/delphi-android-r8;
vendored copy, if present: `tools/delphi-r8/SKILL.md`).
Rules: run `dx_r8.py check-dex` before any upload; every R8 build needs a device/emulator start test;
never upload, roll out or publish without the owner's OK; keep the release branch unchanged until R8 is approved.
Claude Code: `/plugin marketplace add omonien/DelphiStandards` then `/plugin install delphi-android-r8`.
