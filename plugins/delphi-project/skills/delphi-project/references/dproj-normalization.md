# DPROJ Normalization Rules

## Why Normalization Is Necessary

Since Delphi 10 Seattle, the IDE uses dictionary-based internal storage. This causes XML element
order within PropertyGroups to be randomized every time the project is saved — even if no
functional change was made. The result:

- VCS diffs are enormous and meaningless (50+ changed lines for a simple property edit)
- Team code reviews become noise-heavy
- Merge conflicts occur on elements that were not actually changed
- History is polluted with IDE-generated churn

The normalizer (`scripts/dproj_normalize.py`) applies a canonical ordering and performs cleanup
operations to produce stable, minimal diffs.

**Idempotency requirement:** Running the normalizer twice on the same file must produce identical
output. This is tested by comparing the output of the first and second run byte-for-byte.

---

## Normalization Operations

### REQ-NORM-01: Element Ordering

Sort all children within each `PropertyGroup` into a canonical, deterministic order.

**Canonical order within a PropertyGroup** (see `CANONICAL_PROPERTY_ORDER` in the script):

1. Project identity: `ProjectGuid`, `ProjectVersion`, `FrameworkType`, `MainSource`, `Base`,
   `Config`, `Platform`, `TargetedPlatforms`, `AppType`, `SanitizedProjectName`
2. Compiler paths: `DCC_Namespace`, `DCC_DcuOutput`, `DCC_ExeOutput`, `DCC_BplOutput`,
   `DCC_DcpOutput`, `DCC_ObjOutput`, `DCC_UnitSearchPath`, `DCC_ResourcePath`, etc.
3. Compiler flags: `DCC_Define`, `DCC_Optimize`, `DCC_DebugInfoInExe`, `DCC_LocalDebugSymbols`,
   `DCC_SymbolReferenceInfo`, etc.
4. Version info: `IncludeVerInfo`, `VerInfo_MajorVer`, `VerInfo_MinorVer`, `VerInfo_Release`,
   `VerInfo_Build`, `VerInfo_AutoIncBuild`, `VerInfo_Keys`
5. Build events: `PreBuildEventUseInBuild`, `PostBuildEventUseInBuild`
6. All other elements: sorted alphabetically

Within `ItemGroup` elements, DCCReference entries are sorted by their `Include` attribute value
(case-insensitive).

---

### REQ-NORM-02: Duplicate Entry Removal

**DCCReference duplicates:** Remove all entries with an `Include` path that already appeared
earlier in the same `ItemGroup`. The first occurrence is kept; subsequent duplicates are removed.
Path comparison is case-insensitive; forward and back slashes are normalized before comparison.

**DCC_UnitSearchPath duplicates:** After splitting on `;` and normalizing each path segment,
remove entries whose lowercased, normalized form has already been seen. Empty entries (from
trailing or double semicolons) are always removed.

**DCC_Define duplicates:** After splitting on `;`, deduplicate case-insensitively. MSBuild
variable references (e.g., `$(DCC_Define)`) are treated as distinct entries and always kept.

**VerInfo_Keys duplicates:** When the same key name appears more than once (possible after
IDE rewrites), keep the most informative value using this scoring:
1. Non-empty value beats empty value
2. Literal value beats MSBuild macro (`$(...)`  value)
3. Otherwise keep the first occurrence

---

### REQ-NORM-03: Unused Platform Cleanup

Remove `PropertyGroup` elements whose `Condition` attribute references a platform that is neither
set in the `TargetedPlatforms` bitmask nor enabled (`True`) in `<BorlandProject><Platforms>`.
Unknown platform names are always kept.

**Example:** If `TargetedPlatforms` is `3` (Win32 + Win64), remove any PropertyGroups whose
condition mentions `Android`, `iOSDevice64`, `Linux64`, `OSX64`, or `OSXARM64`.

**Platform extraction from Condition:** The normalizer recognizes these patterns:
- `'$(Platform)'=='Win32'` → platform is `Win32`
- `'$(Base_Android)'!=''` → platform is `Android`
- `'$(Cfg_1_Linux64)'!=''` → platform is `Linux64`

**Safety:** If the platform name is not in the known bitmask table, the PropertyGroup is kept
(unknown = safe default).

---

### REQ-NORM-04: Machine-Specific Path Detection

The normalizer **warns** (does not remove) when it detects machine-specific absolute paths in
search path properties:

- Windows drive letters: `C:\`, `D:\`, etc.
- Windows environment variable expansions: `%USERPROFILE%`, `%APPDATA%`, `%LOCALAPPDATA%`
- Unix absolute paths not starting with `$(`: `/home/`, `/usr/`, etc.

These settings belong in `.dproj.user` (which is gitignored). The warning message suggests
moving them there.

**`.dproj.user` settings that should NOT be in the main DPROJ:**
- `Debugger_*` properties
- `Excluded_Packages`
- Any `DCC_UnitSearchPath` entry with an absolute path pointing to a local library installation
- `DCC_AdditionalSwitches` containing machine-specific paths

---

### REQ-NORM-05: Search Path Normalization

For all search path properties (`DCC_UnitSearchPath`, `DCC_ResourcePath`, `DCC_ObjPath`,
`DCC_IncludePath`, `DCC_LibraryPath`):

1. Split on `;`
2. Strip whitespace from each part
3. Remove empty parts
4. For non-MSBuild-variable paths: convert `/` to `\`, collapse `\\` to `\`, strip trailing `\`
5. Deduplicate (case-insensitive key)
6. Rejoin with `;` (no trailing semicolon)

MSBuild variable references like `$(BDS)\lib` are preserved as-is (not path-normalized, as
the variable value is unknown at normalization time).

---

### REQ-NORM-06: Empty Group Removal

Remove `PropertyGroup` and `ItemGroup` elements that have:
- A `Condition` attribute (i.e., not the unconditional base PropertyGroup)
- Zero child elements after normalization

Rationale: The IDE sometimes creates PropertyGroups as placeholders for platform/config
combinations that have no actual settings. These produce noise in diffs and can be safely removed
since MSBuild treats missing sections as having no properties.

---

### REQ-NORM-07: Post-Normalization Validation

After normalization, the script reports warnings for:

- `DCC_ExeOutput` not matching `$(Platform)/$(Config)` pattern
- `DCC_DcuOutput` not matching `$(Platform)/$(Config)/dcu` pattern
- Machine-specific paths remaining in search path properties
- `DCCReference` Include files not found on disk (informational — may be generated)

These warnings do not prevent the file from being written; they alert the developer to potential
issues.

---

## ProjectMagician Compatibility

The normalization output is designed to be compatible with ProjectMagician by Uwe Raabe:

- Canonical element order does not conflict with ProjectMagician's normalization order
- Settings separated into `.dproj.user` match ProjectMagician's DProjSplitter conventions
- A project can use both tools without conflicts

**Feature overlap summary:**

| Feature | dproj_normalize.py | ProjectMagician | theSKULD |
|---------|--------------------|-----------------|----------|
| Element ordering | Yes | Yes | No |
| Duplicate removal | Yes (with scoring) | Limited | Yes (advanced) |
| Platform cleanup | Yes | Yes | No |
| Machine path separation | Warn | Yes (DProjSplitter) | No |
| Search path normalization | Yes | No | No |
| Version info management | Normalize keys | Set version | Deduplicate |
| Form type refresh | No | Yes | No |
| CLI integration | Yes | Yes | No (GUI only) |
| VCS pre-commit hook | Yes | Via CLI | No |

---

## Git Pre-Commit Hook

Install this hook to normalize automatically before every commit:

```sh
#!/bin/sh
# .git/hooks/pre-commit
# Normalize DPROJ files before committing

SCRIPT_DIR="$(git rev-parse --show-toplevel)/scripts"

for dproj in $(git diff --cached --name-only --diff-filter=ACM | grep '\.dproj$'); do
    if [ -f "$SCRIPT_DIR/dproj_normalize.py" ]; then
        python "$SCRIPT_DIR/dproj_normalize.py" "$dproj" --in-place
        git add "$dproj"
    fi
done
```

Make executable: `chmod +x .git/hooks/pre-commit`

---

## Running the Normalizer

```bash
# Preview changes to stdout (does not modify file)
python scripts/dproj_normalize.py MyProject.dproj

# Normalize in place
python scripts/dproj_normalize.py MyProject.dproj --in-place

# Normalize with backup
python scripts/dproj_normalize.py MyProject.dproj --in-place --backup

# Dry run: show what would change without writing
python scripts/dproj_normalize.py MyProject.dproj --dry-run
```

Exit codes:
- `0` = file unchanged (already normalized)
- `1` = file was changed
- `2` = error (file not found, parse failure)
