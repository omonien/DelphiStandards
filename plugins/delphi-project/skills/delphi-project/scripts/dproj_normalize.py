#!/usr/bin/env python3
"""
dproj_normalize.py - Normalize Delphi DPROJ files for clean VCS diffs.

Performs:
  - Canonical element ordering within PropertyGroups and ItemGroups
  - Duplicate entry removal (DCCReference, search paths, defines, VerInfo_Keys)
  - Unused platform cleanup based on TargetedPlatforms bitmask
  - Machine-specific path detection (warns; does not silently remove)
  - Search path normalization (separators, trailing semicolons, dedup)
  - Empty PropertyGroup / ItemGroup removal
  - Idempotent: running twice on the same file produces identical output

Usage:
  python dproj_normalize.py <file.dproj>               # Print normalized XML to stdout
  python dproj_normalize.py <file.dproj> --in-place    # Modify file in place
  python dproj_normalize.py <file.dproj> --in-place --backup  # Backup first
  python dproj_normalize.py <file.dproj> --dry-run     # Report changes without writing
"""

import sys
import os
import re
import argparse
import shutil
import xml.etree.ElementTree as ET
from xml.dom import minidom
from typing import Optional

# MSBuild namespace used by all DPROJ files
MSBUILD_NS = 'http://schemas.microsoft.com/developer/msbuild/2003'
NS_PREFIX = '{' + MSBUILD_NS + '}'

# TargetedPlatforms bitmask values. Derived from IDE-written .dproj files (Delphi XE3 .. 13.2),
# e.g. 32784 = Android + Android64, 693267 = Win32+Win64+Android+Android64+OSX64+OSXARM64+iOSDevice64+iOSSimARM64.
# Note: some Delphi 13 projects carry a TargetedPlatforms value that does not match the platforms enabled
# in <BorlandProject><Platforms>; treat that list as authoritative for "is this platform used".
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

# Canonical order for common PropertyGroup children.
# Elements not in this list are sorted alphabetically after the listed ones.
CANONICAL_PROPERTY_ORDER = [
    'ProjectGuid',
    'ProjectVersion',
    'FrameworkType',
    'MainSource',
    'Base',
    'Config',
    'Platform',
    'TargetedPlatforms',
    'AppType',
    'SanitizedProjectName',
    'DCC_Namespace',
    'DCC_DcuOutput',
    'DCC_ExeOutput',
    'DCC_BplOutput',
    'DCC_DcpOutput',
    'DCC_ObjOutput',
    'DCC_UnitSearchPath',
    'DCC_ResourcePath',
    'DCC_ObjPath',
    'DCC_IncludePath',
    'DCC_Define',
    'DCC_Optimize',
    'DCC_DebugInfoInExe',
    'DCC_LocalDebugSymbols',
    'DCC_SymbolReferenceInfo',
    'DCC_MapFile',
    'DCC_UsePackage',
    'DCC_BuildAllUnits',
    'DCC_Align',
    'DCC_IOChecks',
    'DCC_RangeChecking',
    'DCC_OverflowChecking',
    'DCC_AssertionLevel',
    'DCC_BoolEvalMode',
    'DCC_RemoteDebug',
    'IncludeVerInfo',
    'VerInfo_MajorVer',
    'VerInfo_MinorVer',
    'VerInfo_Release',
    'VerInfo_Build',
    'VerInfo_AutoIncBuild',
    'VerInfo_Keys',
    'Manifest_File',
    'PreBuildEventUseInBuild',
    'PreLinkEventUseInBuild',
    'PostBuildEventUseInBuild',
]

# Standard VerInfo_Keys ordering
VERINFO_KEY_ORDER = [
    'CompanyName', 'FileDescription', 'FileVersion', 'InternalName',
    'LegalCopyright', 'LegalTrademarks', 'OriginalFilename',
    'ProductName', 'ProductVersion', 'Comments',
]

# Regex patterns for machine-specific path detection
_MACHINE_PATH_RE = re.compile(
    r'[A-Za-z]:\\'           # Windows drive letters
    r'|%(?:USERPROFILE|APPDATA|LOCALAPPDATA|HOMEPATH|PROGRAMFILES)[^%]*%'  # Windows env vars
    r'|(?:^|;)/(?![$\(])',   # Unix absolute paths (not MSBuild variables)
    re.IGNORECASE
)


def strip_ns(tag: str) -> str:
    """Return element tag without the MSBuild namespace prefix."""
    if tag.startswith(NS_PREFIX):
        return tag[len(NS_PREFIX):]
    return tag


def get_targeted_platforms(root: ET.Element) -> int:
    """Extract the TargetedPlatforms bitmask from the base PropertyGroup."""
    for pg in root.findall(f'{NS_PREFIX}PropertyGroup'):
        condition = pg.get('Condition', '')
        # Base PropertyGroup has no condition or a condition referencing $(Base)
        if not condition or "'$(Base)'!=''" in condition or "'$(Base)'==''" not in condition:
            tp_elem = pg.find(f'{NS_PREFIX}TargetedPlatforms')
            if tp_elem is not None and tp_elem.text:
                try:
                    return int(tp_elem.text.strip())
                except ValueError:
                    pass
    return 3  # Default: Win32 (1) + Win64 (2)


def get_enabled_platforms(root: ET.Element) -> set[str]:
    """Platforms marked True in ProjectExtensions/BorlandProject/Platforms (what the IDE shows as enabled)."""
    enabled: set[str] = set()
    for elem in root.iter():
        if strip_ns(elem.tag) == 'Platform' and elem.get('value') and (elem.text or '').strip().lower() == 'true':
            enabled.add(elem.get('value'))
    return enabled


def is_platform_active(platform: str, targeted: int, enabled: Optional[set[str]] = None) -> bool:
    """Return True if the platform is targeted (bitmask) or enabled in the Platforms list.
    Never remove a platform the IDE lists as enabled, and keep unknown platforms."""
    if enabled and platform in enabled:
        return True
    bit = PLATFORM_FLAGS.get(platform, 0)
    if bit == 0:
        return True  # Unknown platform: keep it to be safe
    return bool(targeted & bit)


def extract_condition_platform(condition: str) -> Optional[str]:
    """
    Extract the platform name from a PropertyGroup Condition attribute.

    Handles patterns like:
      '$(Platform)'=='Win32'
      '$(Base_Win32)'!=''
      '$(Cfg_1_Win64)'!=''
    """
    # Match explicit platform comparison
    m = re.search(r"'\$\(Platform\)'\s*==\s*'([^']+)'", condition)
    if m:
        return m.group(1)
    # Match Base_<Platform> or Cfg_N_<Platform> variable references
    m = re.search(r"'\$\((?:Base|Cfg_\d+)_([A-Za-z0-9]+)\)'\s*!=\s*''", condition)
    if m:
        return m.group(1)
    return None


def normalize_win_path(path: str) -> str:
    """
    Normalize a single filesystem path:
    - Strip whitespace
    - Forward slashes → backslashes
    - Collapse double backslashes (except UNC \\server)
    - Strip trailing backslash
    """
    path = path.strip()
    path = path.replace('/', '\\')
    # Collapse double backslashes unless it's a UNC path
    while '\\\\' in path and not path.startswith('\\\\'):
        path = path.replace('\\\\', '\\')
    return path.rstrip('\\')


def normalize_search_path(value: str) -> str:
    """
    Normalize a semicolon-delimited search path:
    - Remove empty entries
    - Normalize each path segment
    - Deduplicate (case-insensitive)
    - Preserve MSBuild variable references unchanged
    """
    if not value:
        return value
    parts = value.split(';')
    seen: set[str] = set()
    result: list[str] = []
    for part in parts:
        part = part.strip()
        if not part:
            continue
        # Only normalize non-MSBuild paths
        normalized = normalize_win_path(part) if '$(' not in part else part
        key = normalized.lower()
        if key not in seen:
            seen.add(key)
            result.append(normalized)
    return ';'.join(result)


def normalize_ver_info_keys(value: str) -> str:
    """
    Normalize the VerInfo_Keys semicolon-delimited key=value string:
    - Deduplicate keys, keeping the most informative value
    - Sort into canonical key order
    """
    if not value:
        return value

    pairs: dict[str, str] = {}
    for part in value.split(';'):
        part = part.strip()
        if '=' not in part:
            continue
        key, val = part.split('=', 1)
        key, val = key.strip(), val.strip()
        if key not in pairs:
            pairs[key] = val
        else:
            # Keep the better value: non-empty > non-macro > existing
            existing = pairs[key]
            is_macro = lambda v: v.startswith('$')
            if (not existing and val) or (is_macro(existing) and not is_macro(val) and val):
                pairs[key] = val

    result: list[str] = []
    for k in VERINFO_KEY_ORDER:
        if k in pairs:
            result.append(f'{k}={pairs[k]}')
    for k, v in pairs.items():
        if k not in VERINFO_KEY_ORDER:
            result.append(f'{k}={v}')
    return ';'.join(result)


def normalize_define(value: str) -> str:
    """Remove duplicate conditional defines while preserving MSBuild variable references."""
    if not value:
        return value
    parts = [p.strip() for p in value.split(';') if p.strip()]
    seen: set[str] = set()
    result: list[str] = []
    for p in parts:
        key = p.lower()
        if key not in seen:
            seen.add(key)
            result.append(p)
    return ';'.join(result)


def property_sort_key(element: ET.Element) -> tuple:
    """
    Sort key for PropertyGroup child elements.
    Canonical-order elements sort first (by index), then alphabetically.
    """
    tag = strip_ns(element.tag)
    try:
        return (0, CANONICAL_PROPERTY_ORDER.index(tag), tag)
    except ValueError:
        return (1, 0, tag)


def remove_duplicate_dcc_references(itemgroup: ET.Element) -> int:
    """
    Remove duplicate DCCReference entries (same Include path).
    Returns the number of duplicates removed.
    """
    seen: set[str] = set()
    to_remove: list[ET.Element] = []
    for child in list(itemgroup):
        if strip_ns(child.tag) == 'DCCReference':
            key = child.get('Include', '').lower().replace('/', '\\')
            if key in seen:
                to_remove.append(child)
            else:
                seen.add(key)
    for elem in to_remove:
        itemgroup.remove(elem)
    return len(to_remove)


def normalize_dproj(root: ET.Element) -> dict:
    """
    Normalize the DPROJ XML element tree in-place.

    Returns statistics dict with counts of changes made.
    Warnings about machine-specific paths are printed to stderr.
    """
    stats = {
        'platforms_removed': 0,
        'empty_groups_removed': 0,
        'duplicates_removed': 0,
        'paths_normalized': 0,
        'machine_path_warnings': 0,
    }

    targeted = get_targeted_platforms(root)
    enabled = get_enabled_platforms(root)

    # Process all direct children of root in order
    children_to_remove: list[ET.Element] = []

    for child in list(root):
        tag = strip_ns(child.tag)

        if tag == 'PropertyGroup':
            condition = child.get('Condition', '')
            platform = extract_condition_platform(condition)

            # Remove PropertyGroups for platforms not in TargetedPlatforms
            if platform and not is_platform_active(platform, targeted, enabled):
                children_to_remove.append(child)
                stats['platforms_removed'] += 1
                continue

            # Normalize child elements
            for prop in list(child):
                prop_tag = strip_ns(prop.tag)
                text = prop.text or ''

                if prop_tag in ('DCC_UnitSearchPath', 'DCC_ResourcePath',
                                'DCC_ObjPath', 'DCC_IncludePath', 'DCC_LibraryPath'):
                    normalized = normalize_search_path(text)
                    if normalized != text:
                        if _MACHINE_PATH_RE.search(text):
                            print(
                                f'WARNING: Machine-specific path in {prop_tag} — '
                                f'consider moving to .dproj.user',
                                file=sys.stderr
                            )
                            stats['machine_path_warnings'] += 1
                        prop.text = normalized
                        stats['paths_normalized'] += 1

                elif prop_tag == 'VerInfo_Keys':
                    normalized = normalize_ver_info_keys(text)
                    if normalized != text:
                        prop.text = normalized

                elif prop_tag == 'DCC_Define':
                    normalized = normalize_define(text)
                    if normalized != text:
                        prop.text = normalized

            # Remove PropertyGroup if now empty (after potential child removal)
            if len(child) == 0 and child.get('Condition'):
                children_to_remove.append(child)
                stats['empty_groups_removed'] += 1
                continue

            # Sort children into canonical order
            sorted_children = sorted(list(child), key=property_sort_key)
            child[:] = sorted_children

        elif tag == 'ItemGroup':
            removed = remove_duplicate_dcc_references(child)
            stats['duplicates_removed'] += removed

            # Remove now-empty ItemGroups
            if len(child) == 0:
                children_to_remove.append(child)
                stats['empty_groups_removed'] += 1

    for elem in children_to_remove:
        root.remove(elem)

    return stats


def serialize_dproj(root: ET.Element) -> str:
    """
    Serialize the XML element tree to a formatted string.

    Uses minidom for pretty-printing. Ensures consistent 2-space indentation.
    Returns the XML string WITHOUT a leading <?xml?> declaration (DPROJ files
    use their own encoding declaration in the XML prolog).
    """
    ET.register_namespace('', MSBUILD_NS)
    raw = ET.tostring(root, encoding='unicode', xml_declaration=False)

    # Use minidom for indentation
    dom = minidom.parseString(f'<?xml version="1.0"?>{raw}'.encode('utf-8'))
    pretty = dom.toprettyxml(indent='  ', newl='\n', encoding=None)

    # Remove the <?xml version="1.0"?> line minidom adds
    lines = pretty.split('\n')
    if lines and lines[0].startswith('<?xml'):
        lines = lines[1:]

    # Remove blank lines introduced by minidom
    lines = [line for line in lines if line.strip()]

    return '\n'.join(lines)


def process_file(path: str, in_place: bool, dry_run: bool, backup: bool) -> bool:
    """
    Normalize a single DPROJ file.

    Returns True if the file was (or would be) changed.
    """
    ET.register_namespace('', MSBUILD_NS)

    try:
        tree = ET.parse(path)
    except ET.ParseError as exc:
        print(f'ERROR: Cannot parse {path}: {exc}', file=sys.stderr)
        return False

    root = tree.getroot()
    original_xml = ET.tostring(root, encoding='unicode')

    stats = normalize_dproj(root)
    normalized_xml = ET.tostring(root, encoding='unicode')

    changed = original_xml != normalized_xml

    # Report statistics
    base = os.path.basename(path)
    if any(v > 0 for v in stats.values()):
        print(f'{base}: ', end='')
        parts = []
        for key, val in stats.items():
            if val > 0:
                parts.append(f'{val} {key.replace("_", " ")}')
        print(', '.join(parts))
    elif changed:
        print(f'{base}: property order normalized')
    else:
        print(f'{base}: no changes needed')

    if not changed:
        return False

    if dry_run:
        print(f'[DRY RUN] Would modify {path}')
        return True

    if backup:
        shutil.copy2(path, path + '.bak')
        print(f'Backup: {path}.bak')

    output = serialize_dproj(root)

    if in_place:
        # Write UTF-8 with BOM, CRLF line endings (standard for DPROJ)
        with open(path, 'w', encoding='utf-8-sig', newline='\r\n') as fh:
            fh.write(output)
        print(f'Written: {path}')
    else:
        sys.stdout.write(output)
        sys.stdout.write('\n')

    return True


def main() -> None:
    parser = argparse.ArgumentParser(
        description='Normalize Delphi DPROJ files for clean VCS diffs.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python dproj_normalize.py App.dproj                       # Preview to stdout
  python dproj_normalize.py App.dproj --in-place            # Modify in place
  python dproj_normalize.py App.dproj --in-place --backup   # Backup then modify
  python dproj_normalize.py App.dproj --dry-run             # Report only

Exit codes:
  0  File unchanged or successfully written
  1  File was changed (useful in CI / pre-commit hooks)
  2  Error (file not found, parse failure)
        """,
    )
    parser.add_argument('file', help='DPROJ file to normalize')
    parser.add_argument(
        '--in-place', '-i',
        action='store_true',
        help='Modify the file in place (default: print to stdout)',
    )
    parser.add_argument(
        '--dry-run', '-n',
        action='store_true',
        help='Report what would change without writing anything',
    )
    parser.add_argument(
        '--backup', '-b',
        action='store_true',
        help='Create a .bak backup before modifying (use with --in-place)',
    )
    args = parser.parse_args()

    if not os.path.isfile(args.file):
        print(f'ERROR: File not found: {args.file}', file=sys.stderr)
        sys.exit(2)

    if not args.file.lower().endswith('.dproj'):
        print(f'WARNING: File does not have .dproj extension: {args.file}', file=sys.stderr)

    changed = process_file(args.file, args.in_place, args.dry_run, args.backup)
    sys.exit(1 if changed else 0)


if __name__ == '__main__':
    main()
