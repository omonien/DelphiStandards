# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Purpose

This is a **documentation and tooling standards repository** for Delphi development — not a Delphi application itself. It contains:

- Style guides in English and German (`Delphi Style Guide EN.md` / `Delphi Style Guide DE.md`)
- Git templates (`Delphi GitIgnore.txt`, `Delphi GitAttributes.txt`)
- A universal Delphi build script for CI/CD use (`DelphiBuildDPROJ.ps1`)
- A WSL bridge to that script (`DelphiBuildDPROJ.sh`)
- Release tooling that converts Markdown guides to PDFs (`release-tools/`)

## Build Scripts

### Windows (PowerShell) — `DelphiBuildDPROJ.ps1`

```powershell
# Auto-detects latest Delphi from Windows Registry, builds Debug/Win32
.\DelphiBuildDPROJ.ps1 -ProjectFile "MyProject.dproj"

# Release build for Win64
.\DelphiBuildDPROJ.ps1 -ProjectFile "MyProject.dproj" -Config Release -Platform Win64

# Linux64 with full debug symbols and map file
.\DelphiBuildDPROJ.ps1 -ProjectFile "MyProject.dproj" -Platform Linux64 -LinuxMap
```

The script loads the Delphi environment by executing `rsvars.bat` from the detected installation path and then invokes MSBuild. Auto-detection reads `HKLM:\SOFTWARE\Embarcadero\BDS` (and the Wow6432Node variant).

### WSL — `DelphiBuildDPROJ.sh`

```bash
# Basic build (defaults: Debug / Win32)
./DelphiBuildDPROJ.sh -ProjectFile ./test/Project38.dproj

# Release build for Win64
./DelphiBuildDPROJ.sh -ProjectFile "./MyProject.dproj" -Config Release -Platform Win64

# Linux64 with debug symbols and map file
./DelphiBuildDPROJ.sh -ProjectFile "./MyProject.dproj" -Platform Linux64 -LinuxMap
```

This is a pure bridge: it converts WSL paths to Windows UNC paths via `wslpath -w` and delegates to `DelphiBuildDPROJ.ps1` via `powershell.exe -Command "& 'unc-path'"`. Requires WSL with `powershell.exe` accessible on the host and Delphi installed on Windows.

Parameters are identical to `DelphiBuildDPROJ.ps1` (minus `-IsWSL`, which is internal and no longer exists). Defaults (Debug / Win32) are applied by the PS1.

## Release Process

Releases bundle PDFs of the style guides with the git templates. Requires `pandoc` and `xelatex`.

```bash
# From project root
./release-tools/create-release.sh
```

Before running: update the version in `release-tools/release-config.yaml` and update the version/date header in both style guide files. After running, create a git tag (`git tag -a vX.Y.Z -m "..."`) and push.

## Bilingual Maintenance Rule

The style guide exists in both German and English. **Both files must be kept in sync.** Single-language PRs are acceptable — the maintainer handles the other language post-merge — but if you are editing one, note which language was updated so the gap is visible.

## Delphi Style Guide Conventions (Summary)

When editing the style guides, preserve these conventions (they are the content being documented):

| Element             | Convention                                       |
| ------------------- | ------------------------------------------------ |
| Local variables     | `L` prefix — `LCustomer: TCustomer`              |
| Parameters          | `A` prefix — `const AValue: string`              |
| Fields              | `F` prefix — `FName: string`                     |
| Record fields       | **No** `F` prefix (records differ from classes)  |
| Loop counters       | Lowercase, no prefix — `for var i := 0 to 10 do` |
| Classes / Records   | `T` prefix                                       |
| Interfaces          | `I` prefix                                       |
| Technical constants | `c` prefix — `cMaxRetries = 3`                   |
| String constants    | `sc` prefix — `scErrorMessage = 'Error'`         |

Other key rules documented in the guide:

- 2-space indentation, 120-char line limit
- `begin..end` always on separate lines
- `FreeAndNil(LObj)` instead of `LObj.Free`
- Inline variables (Delphi 10.3+), multiline strings (Delphi 12+)
- `TArray<T>` for fixed-size, `TList<T>` for dynamic, `TObjectList<T>` for owned objects
- Unit names follow namespace hierarchy: `Main.Form`, `Customer.Details.DM`
