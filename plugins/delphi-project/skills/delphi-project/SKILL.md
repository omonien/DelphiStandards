---
name: delphi-project
description: >
  Activate for ANY Delphi-related work: creating new projects, editing .pas/.dpr/.dpk/.dfm/.fmx
  files, touching DPROJ files, configuring build output, setting up git, encoding issues, naming
  conventions, DUnitX test projects, package/BPL creation, FMX or VCL applications, adding units
  to a project, managing search paths, version info, normalization before commit, or any question
  about Delphi project structure or coding standards.
---

# delphi-project Skill

You are working on a Delphi project. Apply the rules and procedures in this skill to **all** Delphi
work. No exceptions. Read the relevant reference documents when you need details.

## Non-Negotiable Rules

These rules are absolute. Never generate code or files that violate them.

**UTF-8 BOM:** Every `.pas`, `.dpr`, `.dpk` file must start with the UTF-8 BOM bytes `EF BB BF`.
Always write these files with `encoding='utf-8-sig'` or equivalent. A file without BOM will cause
the Delphi IDE to misread non-ASCII characters.

**CRLF line endings:** Every Delphi source file (`.pas`, `.dpr`, `.dpk`, `.dfm`, `.fmx`, `.dproj`)
must use CRLF (`\r\n`) line endings. LF-only files cause silent failures in the Delphi IDE.

**Output paths:** All DPROJ output paths must use `$(Platform)\$(Config)` for executables and
`$(Platform)\$(Config)\dcu` for DCU files. Never use hard-coded paths like `bin\` or `output\`.

**No absolute paths in DPROJ:** Search paths in the shared DPROJ file must be relative. Machine-
specific absolute paths belong in `.dproj.user` only (which is in `.gitignore`).

---

## 1. Project Bootstrap

When a user asks to create a new Delphi project, follow this procedure completely.

### 1.1 Gather Requirements

Before creating files, determine (ask if not clear):
- Project name (PascalCase, no spaces)
- Project type: `console`, `vcl`, `fmx`, `package`, `dunitx`, `library`
- Layout: `simple` (single `src/` folder) or `multi-layer` (Core/Data/VCL/FMX subfolders)
- Delphi version target: default **Delphi 13 Florence** (`ProjectVersion 20.4`) unless specified
- Whether DUnitX tests are wanted
- License (MIT or other) — needed for unit headers and copyright
- Author name — ask the user; fall back to `git config user.name`

### 1.2 Simple Project Layout

Use for standalone apps or simple utilities:

```
ProjectName/
├── src/
├── tests/              (if DUnitX requested)
├── libs/               (git submodules: DUnitX, etc.)
├── build/
│   └── DelphiBuildDPROJ.ps1
├── docs/
│   └── Delphi Style Guide EN.md
├── res/
├── .gitignore
├── .gitattributes
├── CLAUDE.md
├── delphi.md
├── ProjectName.dpr
├── ProjectName.dproj
└── ProjectName.groupproj    (if multiple projects)
```

### 1.3 Multi-Layer Project Layout

Use for applications with architectural separation:

```
ProjectName/
├── src/
│   ├── Core/           (business logic, domain, interfaces)
│   ├── Data/           (data access, repositories)
│   ├── VCL/            (VCL forms, frames)
│   └── FMX/            (FMX forms, if cross-platform)
├── tests/
├── libs/
│   └── DUnitX/
├── build/
│   └── DelphiBuildDPROJ.ps1
├── docs/
│   └── Delphi Style Guide EN.md
├── res/
├── demo/               (for library projects)
├── .gitignore
├── .gitattributes
├── CLAUDE.md
├── delphi.md
├── ProjectName.dpr
├── ProjectName.dproj
└── ProjectName.groupproj
```

### 1.4 Files to Generate

**`.gitignore`** — Copy the curated template `Delphi GitIgnore.txt` from the DelphiStandards repo root
(`../../../../Delphi GitIgnore.txt` relative to this skill when the repo is cloned, otherwise
`https://raw.githubusercontent.com/omonien/DelphiStandards/master/Delphi%20GitIgnore.txt`) and rename it to `.gitignore`.

**`.gitattributes`** — Same for `Delphi GitAttributes.txt` → `.gitattributes` (CRLF enforcement, binary markers;
`https://raw.githubusercontent.com/omonien/DelphiStandards/master/Delphi%20GitAttributes.txt`). Then run `git add --renormalize .`.

**`CLAUDE.md`** — Minimal file that references `delphi.md`:
```markdown
# Project Instructions

Read @delphi.md for Delphi-specific coding standards.

## Project Overview

<!-- Describe what this project does -->

## Build

Run `.\build\DelphiBuildDPROJ.ps1 -ProjectFile ProjectName.dproj` to build.

## Test

Open `tests\TestProjectName.dproj` in Delphi IDE and run, or use the build script.
```

**`delphi.md`** — Full Delphi rules for this project. Use the template in
`references/delphi-md-template.md`, filling in project-specific values.

**`.dpr` file** — See project type templates in `references/project-templates.md`.

**`.dproj` file** — See DPROJ templates in `references/project-templates.md`. Always:
- Generate a fresh GUID with `python -c "import uuid; print('{'+str(uuid.uuid4()).upper()+'}')"` or
  equivalent
- Set `ProjectVersion` to match the target Delphi version
- Use the canonical output path convention

**`build/DelphiBuildDPROJ.ps1`** — Fetch from `https://raw.githubusercontent.com/omonien/DelphiStandards/master/DelphiBuildDPROJ.ps1`
if internet is available; otherwise copy the version from the skill's assets if present.

### 1.5 DUnitX Integration

When tests are requested:
1. Add DUnitX as git submodule: `git submodule add https://github.com/VSoftTechnologies/DUnitX.git libs/DUnitX`
2. Generate `tests/TestProjectName.dpr` — console runner
3. Generate `tests/TestProjectName.dproj` with search path `../libs/DUnitX/Source`
4. Generate `tests/TestProjectNameTests.pas` — sample test unit demonstrating `[Test]`, `[Setup]`, `[TearDown]`

### 1.6 Verify Encoding After Creation

After creating all source files, run:
```bash
python scripts/encoding_check.py <project_dir>/src
```
Fix any issues found.

---

## 2. DPROJ Management

### 2.1 Creating a New DPROJ

Use the templates in `references/project-templates.md` as starting points. Key requirements:

| Property | Rule |
|----------|------|
| `ProjectGuid` | Fresh GUID, `{UPPERCASE-WITH-HYPHENS}` format |
| `ProjectVersion` | Match target Delphi: 11→`19.5`, 12→`20.2`, 13.0→`20.3`, 13.1→`20.4`, 13.2→`20.5` |
| `FrameworkType` | `VCL`, `FMX`, or `None` |
| `AppType` | `Application`, `Console`, `Package`, `Library` |
| `TargetedPlatforms` | Bitmask: Win32=1, Win64=2, Linux64=128, Android=16, Android64=32768 (full table in `references/dproj-format.md`) |
| `DCC_DcuOutput` | `.\$(Platform)\$(Config)\dcu` |
| `DCC_ExeOutput` | `.\$(Platform)\$(Config)` |
| `DCC_BplOutput` | `.\$(Platform)\$(Config)` (for packages) |
| `DCC_DcpOutput` | `.\$(Platform)\$(Config)\dcu` (for packages) |
| `Config` | Default: `Debug` with `Condition="'$(Config)'==''"` |
| `Platform` | Default: `Win32` with `Condition="'$(Platform)'==''"` |

DCC_Namespace values by framework:
- VCL: `System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell`
- FMX: `System;Xml;Data;Datasnap;Web;Soap;FMX;FMX.Surfaces;FMX.Import`
- None/Console: `System;Xml;Data;Datasnap;Web;Soap`

### 2.2 Modifying an Existing DPROJ

When adding or removing source files, read the DPROJ first, then make targeted edits.

**Add a plain unit:**
```xml
<DCCReference Include="UnitName.pas"/>
```

**Add a unit with VCL form:**
```xml
<DCCReference Include="UnitName.pas">
  <Form>TFormName</Form>
  <FormType>dfm</FormType>
</DCCReference>
```

**Add a data module:**
```xml
<DCCReference Include="DMName.pas">
  <Form>TDMName</Form>
  <DesignClass>TDataModule</DesignClass>
</DCCReference>
```

**Add to search path:** Find `DCC_UnitSearchPath` in the base `PropertyGroup` (no condition or
`'$(Base)'!=''` condition), append with semicolon. Use relative paths only.

**Update version info:** Change `VerInfo_MajorVer`, `VerInfo_MinorVer`, `VerInfo_Release`,
`VerInfo_Build` AND update `VerInfo_Keys` to keep `FileVersion` and `ProductVersion` consistent.

### 2.3 DPROJ Normalization

Run normalization before every VCS commit and whenever the DPROJ has been saved by the IDE.

```bash
python scripts/dproj_normalize.py ProjectName.dproj --in-place
```

The normalizer:
- Sorts PropertyGroup children into canonical order (alphabetical within each group)
- Removes duplicate DCCReference, search path, and define entries
- Removes PropertyGroups for platforms neither in `TargetedPlatforms` nor enabled in `<Platforms>`
- Normalizes path separators and removes trailing semicolons
- Warns about machine-specific absolute paths

**Idempotency check:** Running the normalizer twice must produce no diff. If it does, file a bug.

### 2.4 DPROJ Validation

```bash
python scripts/dproj_validate.py ProjectName.dproj
```

Fix all ERRORs before committing. Review WARNINGs.

### 2.5 Git Pre-Commit Hook

Offer to install this hook when setting up a new project:

```bash
#!/bin/sh
# .git/hooks/pre-commit
for dproj in $(git diff --cached --name-only --diff-filter=ACM | grep '\.dproj$'); do
    python scripts/dproj_normalize.py "$dproj" --in-place
    git add "$dproj"
done
```

---

## 3. Coding Standards

The authoritative rules are in the [Delphi Style Guide](../../../../Delphi%20Style%20Guide%20EN.md) at the repo root
(online: https://raw.githubusercontent.com/omonien/DelphiStandards/master/Delphi%20Style%20Guide%20EN.md). `references/coding-standards.md` is a digest for agents;
on any conflict the Style Guide wins. Summary of critical rules:

### 3.1 Unit Headers

Every `.pas` file must start with the XML Doc header (after the UTF-8 BOM). Template in
`assets/unit-header-template.txt`.

```pascal
/// <summary>
///   Brief one-line description of this unit.
/// </summary>
/// <remarks>
///   Additional details, design decisions, usage notes.
/// </remarks>
/// <author>{{AUTHOR}}</author>
/// <created>YYYY-MM-DD</created>
/// <modified>YYYY-MM-DD</modified>
/// <license>MIT</license>
unit My.Unit.Name;
```

### 3.2 Naming Conventions

| Element | Prefix | Example |
|---------|--------|---------|
| Class | `T` | `TCustomerRepository` |
| Interface | `I` | `ICustomerRepository` |
| Exception | `E` | `ECustomerNotFound` |
| Method parameter | `A` | `ACustomerName: string` |
| Local variable | `L` | `LCustomer: TCustomer` |
| Class field | `F` | `FCustomerList: TList<TCustomer>` |
| Constant | `c` | `cMaxRetries = 3` |
| String constant | `sc` | `scErrorMessage = 'Error'` |
| Enumeration type | `T` | `TOrderStatus` |
| Enum member | no prefix, `{$SCOPEDENUMS ON}` | `TOrderStatus.New` |
| Component | type prefix + name | `ButtonLogin`, `EditUserName` |

Unit names: dot-notation hierarchy, e.g. `Customer.Repository.pas`, `Main.Form.pas`.

### 3.3 Code Formatting

- 2-space indentation
- 120-character maximum line length
- `begin`/`end` on their own lines for `if`, `for`, `while`, `try`
- Methods: `begin` on same line as declaration (or next line — be consistent within the file)
- `const` parameters for string/interface parameters that are not modified
- `var`/`out` explicit for output parameters

### 3.4 DFM/FMX Files

Special characters in string properties must use `#decimal` notation:
```
Caption = 'Stra'#223'e'     // ß = #223
Caption = 'M'#252'nchen'    // ü = #252
```

Never embed raw UTF-8 bytes in DFM/FMX files.

---

## 4. CLAUDE.md and delphi.md Generation

### 4.1 Two-File Architecture

- **`CLAUDE.md`** — Minimal, project-specific. Contains non-Delphi context (API keys references,
  deployment notes, team conventions, architecture overview). Has `Read @delphi.md` at the top.
- **`delphi.md`** — All Delphi rules. Generated and maintained by this skill. Written as direct
  instructions to an AI agent (imperative mood: "Always...", "Never...", "Use...").

### 4.2 Updating delphi.md

When the skill is updated or new rules apply, regenerate `delphi.md` from
`references/delphi-md-template.md` without touching `CLAUDE.md`.

### 4.3 Tone for delphi.md

Write in imperative mood, directly addressing the agent:
- "Always use UTF-8 BOM for .pas files."
- "Never add absolute paths to DCC_UnitSearchPath."
- "Run dproj_normalize.py before every commit."

---

## 5. Configuration System

### 5.1 Project-Level Config File

When starting work on a Delphi project, check for `delphi-project.json` in the project root.
If found, read it and apply its settings as overrides to the skill defaults.

```bash
# Check for config
ls delphi-project.json 2>/dev/null && echo "Config found" || echo "Using defaults"
```

If not found, use the DelphiStandards defaults (all rules from `references/coding-standards.md`).

**Loading config in Python:**
```python
import json, os
config = {}
config_path = 'delphi-project.json'
if os.path.exists(config_path):
    with open(config_path, encoding='utf-8') as f:
        config = json.load(f)

# Access with fallbacks
local_prefix = config.get('naming', {}).get('localVarPrefix', 'L')
field_prefix  = config.get('naming', {}).get('fieldPrefix', 'F')
param_prefix  = config.get('naming', {}).get('paramPrefix', 'A')
indent_size   = config.get('formatting', {}).get('indentSize', 2)
max_line      = config.get('formatting', {}).get('maxLineLength', 120)
author        = config.get('header', {}).get('author', '{{AUTHOR}}')
license_id    = config.get('header', {}).get('license', 'MIT')
source_dir    = config.get('paths', {}).get('sourceDir', 'src')
test_dir      = config.get('paths', {}).get('testDir', 'tests')
```

### 5.2 Config Schema

The default config template is at `assets/delphi-project.json`. Copy it to the project root and
customize. The full schema:

```json
{
  "naming": {
    "classPrefix": "T",        "interfacePrefix": "I",
    "exceptionPrefix": "E",    "localVarPrefix": "L",
    "fieldPrefix": "F",        "paramPrefix": "A",
    "globalPrefix": "G",       "constantPrefix": "c",
    "stringConstantPrefix": "sc", "resourceStringPrefix": "rs"
  },
  "formatting": {
    "indentSize": 2,
    "maxLineLength": 120,
    "beginOnSameLine": true
  },
  "paths": {
    "outputPath": "$(Platform)/$(Config)",
    "dcuPath": "$(Platform)/$(Config)/dcu",
    "sourceDir": "src",   "testDir": "tests",
    "libsDir": "libs",    "buildDir": "build"
  },
  "header": {
    "author": "Team Name",
    "license": "MIT",
    "format": "xmldoc"
  },
  "testing": {
    "framework": "DUnitX",
    "asSubmodule": true,
    "submodulePath": "libs/DUnitX",
    "submoduleUrl": "https://github.com/VSoftTechnologies/DUnitX.git"
  },
  "delphi": {
    "defaultVersion": "13",
    "defaultProjectVersion": "20.4",
    "defaultPlatform": "Win32",
    "targetedPlatforms": 3
  }
}
```

### 5.3 What Is and Is Not Configurable

**Configurable:** naming prefixes, indentation, line length, output paths, source/test directories,
unit header author/license, test framework, DUnitX submodule settings, default Delphi version.

**NOT configurable (Delphi requirements):**
- UTF-8 BOM on `.pas`/`.dpr`/`.dpk` — the IDE requires it
- CRLF line endings — the Delphi IDE and form designer fail on LF-only files
- `$(Platform)\$(Config)` path structure — required for multi-platform compilation to work

When you encounter a `delphi-project.json` that sets `outputPath` to something other than a
`$(Platform)/$(Config)` pattern, warn the user — this will break multi-platform builds.

### 5.4 Generating delphi.md from Config

When generating `delphi.md` for a project that has `delphi-project.json`, substitute the
config values into the template from `references/delphi-md-template.md`. The generated file
reflects the actual project settings, not generic defaults.

---

## 6. Tool Reference

All scripts are in the `scripts/` directory relative to the skill root.

| Script | Purpose | Usage |
|--------|---------|-------|
| `dproj_normalize.py` | Normalize DPROJ for VCS | `python dproj_normalize.py <file> [--in-place] [--backup]` |
| `dproj_validate.py` | Validate DPROJ structure | `python dproj_validate.py <file> [--strict]` |
| `encoding_check.py` | Check/fix file encoding | `python encoding_check.py <dir> [--fix]` |

Always copy scripts to the target project's `scripts/` or `build/` folder so they can be invoked
from project-relative paths and used in git hooks.

---

## 6. Reference Documents

| Document | Contents |
|----------|---------|
| `references/dproj-format.md` | Complete DPROJ XML format, all properties, valid values |
| `references/dproj-normalization.md` | Normalization rules, canonical ordering, dedup logic |
| `references/project-templates.md` | DPROJ XML templates for all project types |
| `references/coding-standards.md` | Agent digest of the Delphi Style Guide (the Style Guide wins on conflict) |
| `Delphi Style Guide EN.md` / `DE.md` (repo root) | Authoritative style guide |
| `references/delphi-md-template.md` | Template for generating project `delphi.md` files |
| `Delphi GitIgnore.txt` (repo root) | Curated .gitignore template |
| `Delphi GitAttributes.txt` (repo root) | .gitattributes template with CRLF enforcement |
| `assets/unit-header-template.txt` | Unit header XML Doc template |

---

## 7. Common Workflows

### "Create a new VCL app" (and FMX, Console, Package)
1. Gather: name, Delphi version, layout, DUnitX wanted?
2. Create directory structure
3. Generate `.gitignore`, `.gitattributes` from assets
4. Generate `CLAUDE.md`, `delphi.md`
5. Generate `.dproj` from VCL template (fresh GUID, correct version)
6. Generate `.dpr` with `Application.Initialize/CreateForm/Run`
7. Generate main form unit: `Main.Form.pas` + `Main.Form.dfm`
8. Add build script
9. Run `encoding_check.py --fix` on all generated sources
10. Run `dproj_validate.py` on generated DPROJ
11. `git init` + initial commit

### "Normalize the DPROJ"
```bash
python scripts/dproj_normalize.py ProjectName.dproj --in-place --backup
python scripts/dproj_validate.py ProjectName.dproj
```

### "Add a unit to the project"
1. Create `.pas` file with UTF-8 BOM, CRLF, unit header
2. Add `<DCCReference>` to DPROJ ItemGroup
3. Add `uses` entry to `.dpr` if needed
4. Run normalization

### "Check encoding on all sources"
```bash
python scripts/encoding_check.py src/ --fix
```