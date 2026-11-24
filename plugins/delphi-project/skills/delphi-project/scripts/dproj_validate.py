#!/usr/bin/env python3
"""
dproj_validate.py - Validate Delphi DPROJ files for correctness and convention compliance.

Checks:
  - Required elements present (ProjectGuid, ProjectVersion, AppType, MainSource, Import)
  - ProjectGuid is a valid GUID
  - ProjectVersion is a known Delphi version
  - Output paths follow $(Platform)/$(Config) convention
  - DCU output follows $(Platform)/$(Config)/dcu convention
  - VerInfo_Keys is consistent with VerInfo_MajorVer/MinorVer/Release/Build
  - No machine-specific absolute paths in search paths
  - DCCReference files exist on disk (warning if missing — may be generated)
  - TargetedPlatforms is a valid integer
  - Import for CodeGear.Delphi.Targets is present

Usage:
  python dproj_validate.py <file.dproj>           # Standard validation
  python dproj_validate.py <file.dproj> --strict  # Additional strict checks
  python dproj_validate.py <file.dproj> --quiet   # Errors and warnings only

Exit codes:
  0  Validation passed (no errors)
  1  Validation failed (one or more errors)
  2  File not found or XML parse error
"""

import sys
import os
import re
import argparse
import xml.etree.ElementTree as ET
from typing import Optional

MSBUILD_NS = 'http://schemas.microsoft.com/developer/msbuild/2003'
NS_PREFIX = '{' + MSBUILD_NS + '}'

# Maps ProjectVersion string to human-readable Delphi version name
KNOWN_PROJECT_VERSIONS: dict[str, str] = {
    '18.0': 'Delphi 10 Seattle',
    '18.1': 'Delphi 10 Seattle / 10.1 Berlin',
    '18.2': 'Delphi 10.1 Berlin / 10.2 Tokyo',
    '18.3': 'Delphi 10.2 Tokyo',
    '18.4': 'Delphi 10.2 Tokyo',
    '18.5': 'Delphi 10.3 Rio',
    '18.6': 'Delphi 10.3 Rio',
    '18.7': 'Delphi 10.3 Rio',
    '18.8': 'Delphi 10.3 Rio',
    '19.0': 'Delphi 10.4 Sydney',
    '19.1': 'Delphi 10.4 Sydney',
    '19.2': 'Delphi 10.4 Sydney',
    '19.3': 'Delphi 11 Alexandria',
    '19.4': 'Delphi 11 Alexandria',
    '19.5': 'Delphi 11 Alexandria',
    '20.0': 'Delphi 12 Athens',
    '20.1': 'Delphi 12 Athens',
    '20.2': 'Delphi 12.2 Athens',
    '20.3': 'Delphi 12.3 Athens / Delphi 13.0 Florence',
    '20.4': 'Delphi 13.1 Florence',
    '20.5': 'Delphi 13.2 Florence',
}

PLATFORM_FLAGS: dict[str, int] = {
    'Win32': 1,
    'Win64': 2,
    'OSX32': 4,
    'iOSSimulator': 8,
    'Android': 16,
    'iOSDevice32': 64,
    'Linux64': 128,
    'iOSDevice64': 1024,
    'OSX64': 4096,
    'Android64': 32768,
    'OSXARM64': 131072,
    'iOSSimARM64': 524288,
    'Win64x': 1048576,
    'WinARM64EC': 2097152,
}

VALID_APP_TYPES = {'Application', 'Console', 'Package', 'Library'}
VALID_FRAMEWORK_TYPES = {'VCL', 'FMX', 'None', ''}

GUID_RE = re.compile(
    r'^\{[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}'
    r'-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}\}$'
)
# Output path must contain $(Platform) and $(Config)
OUTPUT_PATH_RE = re.compile(r'\$\(Platform\)[/\\]\$\(Config\)', re.IGNORECASE)
DCU_PATH_RE = re.compile(r'\$\(Platform\)[/\\]\$\(Config\)[/\\]dcu', re.IGNORECASE)

MACHINE_PATH_RE = re.compile(
    r'[A-Za-z]:\\'
    r'|%(?:USERPROFILE|APPDATA|LOCALAPPDATA|HOMEPATH|PROGRAMFILES)[^%]*%',
    re.IGNORECASE
)


class ValidationResult:
    """Accumulates validation findings into errors, warnings, and informational notes."""

    def __init__(self) -> None:
        self.errors: list[str] = []
        self.warnings: list[str] = []
        self.notes: list[str] = []

    def error(self, msg: str) -> None:
        self.errors.append(f'ERROR:   {msg}')

    def warning(self, msg: str) -> None:
        self.warnings.append(f'WARNING: {msg}')

    def note(self, msg: str) -> None:
        self.notes.append(f'INFO:    {msg}')

    @property
    def ok(self) -> bool:
        return len(self.errors) == 0

    def print_all(self, quiet: bool = False) -> None:
        for line in self.errors:
            print(line)
        for line in self.warnings:
            print(line)
        if not quiet:
            for line in self.notes:
                print(line)


def strip_ns(tag: str) -> str:
    if tag.startswith(NS_PREFIX):
        return tag[len(NS_PREFIX):]
    return tag


def find_property(root: ET.Element, name: str) -> str:
    """Return the text of the first matching property across all PropertyGroups."""
    for pg in root.findall(f'{NS_PREFIX}PropertyGroup'):
        elem = pg.find(f'{NS_PREFIX}{name}')
        if elem is not None and elem.text:
            return elem.text.strip()
    return ''


def validate_dproj(filepath: str, strict: bool = False) -> ValidationResult:
    result = ValidationResult()
    dirpath = os.path.dirname(os.path.abspath(filepath))

    # Parse XML
    try:
        tree = ET.parse(filepath)
    except ET.ParseError as exc:
        result.error(f'XML parse error: {exc}')
        return result

    root = tree.getroot()

    # Root element must be <Project>
    if strip_ns(root.tag) != 'Project':
        result.error(f'Root element must be <Project>, found <{strip_ns(root.tag)}>')
        return result

    # --- Required properties ---

    guid = find_property(root, 'ProjectGuid')
    if not guid:
        result.error('Missing required property: ProjectGuid')
    elif not GUID_RE.match(guid):
        result.error(f'ProjectGuid has invalid format: {guid}  (expected {{XXXXXXXX-...}})')

    version = find_property(root, 'ProjectVersion')
    if not version:
        result.error('Missing required property: ProjectVersion')
    elif version in KNOWN_PROJECT_VERSIONS:
        result.note(f'Target Delphi version: {KNOWN_PROJECT_VERSIONS[version]} (ProjectVersion {version})')
    else:
        result.warning(
            f'ProjectVersion {version!r} is not a recognized Delphi version. '
            f'Known versions: {", ".join(sorted(KNOWN_PROJECT_VERSIONS))}'
        )

    main_source = find_property(root, 'MainSource')
    if not main_source:
        result.error('Missing required property: MainSource')
    else:
        main_source_path = os.path.join(dirpath, main_source)
        if not os.path.exists(main_source_path):
            result.warning(f'MainSource file not found on disk: {main_source}')

    app_type = find_property(root, 'AppType')
    if not app_type:
        result.warning('AppType not set (expected: Application, Console, Package, Library)')
    elif app_type not in VALID_APP_TYPES:
        result.warning(f'Unusual AppType: {app_type!r}')

    framework_type = find_property(root, 'FrameworkType')
    if framework_type not in VALID_FRAMEWORK_TYPES:
        result.warning(f'Unusual FrameworkType: {framework_type!r}')

    # --- Output path conventions ---

    dcu_output = find_property(root, 'DCC_DcuOutput')
    if not dcu_output:
        result.warning('DCC_DcuOutput not set — DCU files will go to the default location')
    elif not DCU_PATH_RE.search(dcu_output):
        result.error(
            f'DCC_DcuOutput does not follow $(Platform)/$(Config)/dcu convention: {dcu_output!r}'
        )

    exe_output = find_property(root, 'DCC_ExeOutput')
    if not exe_output:
        result.warning('DCC_ExeOutput not set — executables will go to the default location')
    elif not OUTPUT_PATH_RE.search(exe_output):
        result.error(
            f'DCC_ExeOutput does not follow $(Platform)/$(Config) convention: {exe_output!r}'
        )

    bpl_output = find_property(root, 'DCC_BplOutput')
    if bpl_output and not OUTPUT_PATH_RE.search(bpl_output):
        result.error(
            f'DCC_BplOutput does not follow $(Platform)/$(Config) convention: {bpl_output!r}'
        )

    # --- VerInfo consistency ---

    major = find_property(root, 'VerInfo_MajorVer')
    minor = find_property(root, 'VerInfo_MinorVer')
    release_v = find_property(root, 'VerInfo_Release')
    build = find_property(root, 'VerInfo_Build')
    ver_keys = find_property(root, 'VerInfo_Keys')

    if major and minor and release_v and build:
        expected = f'{major}.{minor}.{release_v}.{build}'
        if ver_keys:
            for key_name in ('FileVersion', 'ProductVersion'):
                m = re.search(rf'{re.escape(key_name)}=([^;]+)', ver_keys)
                if m:
                    found = m.group(1).strip()
                    if found and found != expected:
                        result.warning(
                            f'VerInfo_Keys {key_name} ({found!r}) does not match '
                            f'version numbers ({expected})'
                        )
        result.note(f'Version: {expected}')

    # --- Machine-specific paths ---

    for pg in root.findall(f'{NS_PREFIX}PropertyGroup'):
        for child in pg:
            tag = strip_ns(child.tag)
            if tag in ('DCC_UnitSearchPath', 'DCC_ResourcePath', 'DCC_LibraryPath'):
                text = child.text or ''
                if MACHINE_PATH_RE.search(text):
                    snippet = text[:100] + ('...' if len(text) > 100 else '')
                    result.warning(
                        f'Machine-specific absolute path in {tag}: {snippet!r}\n'
                        f'         Move to .dproj.user (which is in .gitignore)'
                    )

    # --- DCCReference files exist ---

    for ig in root.findall(f'{NS_PREFIX}ItemGroup'):
        for ref in ig.findall(f'{NS_PREFIX}DCCReference'):
            include = ref.get('Include', '')
            if include and not include.startswith('$('):
                ref_path = os.path.join(dirpath, include.replace('\\', os.sep))  # .dproj paths use backslashes
                if not os.path.exists(ref_path):
                    result.warning(
                        f'DCCReference not found on disk: {include} '
                        f'(may be generated — not auto-removed)'
                    )

    # --- TargetedPlatforms ---

    targeted_str = find_property(root, 'TargetedPlatforms')
    if targeted_str:
        try:
            targeted = int(targeted_str)
            active = [name for name, flag in PLATFORM_FLAGS.items() if targeted & flag]
            unknown = targeted & ~sum(PLATFORM_FLAGS.values())
            result.note(f'Targeted platforms: {", ".join(active) if active else "none (value=0)"}'
                        + (f' (+ unknown bits {unknown})' if unknown else ''))
            enabled = sorted({e.get('value') for e in root.iter()
                              if e.tag.endswith('Platform') and e.get('value')
                              and (e.text or '').strip().lower() == 'true'})
            if enabled:
                result.note(f'Platforms enabled in IDE: {", ".join(enabled)}')
        except ValueError:
            result.error(f'TargetedPlatforms is not a valid integer: {targeted_str!r}')

    # --- Import for CodeGear.Delphi.Targets ---

    has_import = any(
        'CodeGear.Delphi.Targets' in (imp.get('Project', ''))
        for imp in root.findall(f'{NS_PREFIX}Import')
    )
    if not has_import:
        result.error('Missing <Import Project="$(BDS)\\Bin\\CodeGear.Delphi.Targets"> element')

    # --- Strict checks ---

    if strict:
        if not find_property(root, 'SanitizedProjectName'):
            result.warning('SanitizedProjectName not set (recommended for Delphi 12+)')

        include_ver = find_property(root, 'IncludeVerInfo')
        if include_ver.lower() != 'true':
            result.warning(
                f'IncludeVerInfo is {include_ver!r} — version info will not be embedded in the binary'
            )

        if not find_property(root, 'DCC_Namespace'):
            result.warning('DCC_Namespace not set — default namespace prefixes may differ from expectations')

        # Check for duplicate DCCReference entries
        all_refs: list[str] = []
        for ig in root.findall(f'{NS_PREFIX}ItemGroup'):
            for ref in ig.findall(f'{NS_PREFIX}DCCReference'):
                include = ref.get('Include', '').lower()
                if include:
                    all_refs.append(include)
        duplicates = {r for r in all_refs if all_refs.count(r) > 1}
        for dup in sorted(duplicates):
            result.warning(f'Duplicate DCCReference: {dup} — run dproj_normalize.py to fix')

    return result


def main() -> None:
    parser = argparse.ArgumentParser(
        description='Validate Delphi DPROJ files for correctness and convention compliance.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exit codes:
  0  Validation passed (no errors; warnings may exist)
  1  Validation failed (one or more errors)
  2  File not found or XML parse error
        """,
    )
    parser.add_argument('file', help='DPROJ file to validate')
    parser.add_argument(
        '--strict', '-s',
        action='store_true',
        help='Enable additional strict checks (SanitizedProjectName, IncludeVerInfo, duplicates)',
    )
    parser.add_argument(
        '--quiet', '-q',
        action='store_true',
        help='Suppress informational notes; show only errors and warnings',
    )
    args = parser.parse_args()

    if not os.path.isfile(args.file):
        print(f'ERROR: File not found: {args.file}', file=sys.stderr)
        sys.exit(2)

    result = validate_dproj(args.file, strict=args.strict)
    result.print_all(quiet=args.quiet)

    base = os.path.basename(args.file)
    print()
    if result.ok:
        suffix = f' ({len(result.warnings)} warning(s))' if result.warnings else ''
        print(f'PASSED: {base}{suffix}')
        sys.exit(0)
    else:
        print(
            f'FAILED: {base} — {len(result.errors)} error(s), {len(result.warnings)} warning(s)'
        )
        sys.exit(1)


if __name__ == '__main__':
    main()
