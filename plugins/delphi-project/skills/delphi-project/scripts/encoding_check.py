#!/usr/bin/env python3
"""
encoding_check.py - Check and optionally fix Delphi source file encoding.

Rules enforced:
  .pas, .dpr, .dpk       — must have UTF-8 BOM (EF BB BF) AND CRLF line endings
  .dfm, .fmx, .dproj     — must have CRLF line endings (no BOM requirement)

Usage:
  python encoding_check.py <dir>              # Check all files recursively
  python encoding_check.py <dir> --fix        # Fix all issues automatically
  python encoding_check.py <file.pas>         # Check a single file
  python encoding_check.py <file.pas> --fix   # Fix a single file
  python encoding_check.py src/ --summary     # Show totals only

Exit codes:
  0  All files OK (or all issues fixed when --fix is used)
  1  Issues found and not fixed (requires --fix to resolve)
"""

import sys
import os
import re
import argparse
from pathlib import Path
from typing import NamedTuple

UTF8_BOM = b'\xef\xbb\xbf'

# Extensions that require UTF-8 BOM
BOM_EXTENSIONS = frozenset({'.pas', '.dpr', '.dpk'})

# Extensions that require CRLF line endings
CRLF_EXTENSIONS = frozenset({'.pas', '.dpr', '.dpk', '.dfm', '.fmx', '.dproj'})

# All extensions this tool cares about
ALL_EXTENSIONS = BOM_EXTENSIONS | CRLF_EXTENSIONS


class FileStatus(NamedTuple):
    path: Path
    has_bom: bool
    has_crlf: bool
    has_lone_lf: bool     # LF not preceded by CR — indicates LF-only or mixed endings
    needs_bom: bool       # Whether BOM is required for this extension
    needs_crlf: bool      # Whether CRLF is required for this extension

    @property
    def bom_ok(self) -> bool:
        return not self.needs_bom or self.has_bom

    @property
    def crlf_ok(self) -> bool:
        return not self.needs_crlf or not self.has_lone_lf

    @property
    def ok(self) -> bool:
        return self.bom_ok and self.crlf_ok

    @property
    def issues(self) -> list[str]:
        problems = []
        if not self.bom_ok:
            problems.append('missing UTF-8 BOM')
        if not self.crlf_ok:
            problems.append('LF-only line endings (CRLF required)')
        return problems


def inspect_file(filepath: Path) -> FileStatus:
    """Read a file and determine its encoding status."""
    ext = filepath.suffix.lower()
    needs_bom = ext in BOM_EXTENSIONS
    needs_crlf = ext in CRLF_EXTENSIONS

    with open(filepath, 'rb') as fh:
        content = fh.read()

    has_bom = content.startswith(UTF8_BOM)

    # Detect line endings
    # has_lone_lf = True means there is at least one LF not preceded by CR
    has_crlf = b'\r\n' in content
    has_lone_lf = bool(re.search(b'(?<!\r)\n', content))

    return FileStatus(
        path=filepath,
        has_bom=has_bom,
        has_crlf=has_crlf,
        has_lone_lf=has_lone_lf,
        needs_bom=needs_bom,
        needs_crlf=needs_crlf,
    )


def fix_file(status: FileStatus) -> bool:
    """
    Fix encoding issues in a file.

    Returns True if the file was modified.
    Writes UTF-8 BOM if required and missing.
    Converts line endings to CRLF if required and LF-only/mixed.
    """
    if status.ok:
        return False

    with open(status.path, 'rb') as fh:
        content = fh.read()

    original = content

    # Add BOM if missing and required
    if status.needs_bom and not status.has_bom:
        content = UTF8_BOM + content

    # Normalize to CRLF if required and there are lone LFs
    if status.needs_crlf and status.has_lone_lf:
        # Remove existing BOMs before line-ending conversion to avoid duplication
        working = content
        had_bom = working.startswith(UTF8_BOM)
        if had_bom:
            working = working[len(UTF8_BOM):]

        # Step 1: normalize all CRLF → LF
        working = working.replace(b'\r\n', b'\n')
        # Step 2: normalize standalone CR → LF
        working = working.replace(b'\r', b'\n')
        # Step 3: convert all LF → CRLF
        working = working.replace(b'\n', b'\r\n')

        if had_bom:
            working = UTF8_BOM + working
        content = working

    if content != original:
        with open(status.path, 'wb') as fh:
            fh.write(content)
        return True

    return False


def collect_files(root: Path) -> list[Path]:
    """Collect all Delphi source files under a path."""
    if root.is_file():
        if root.suffix.lower() in ALL_EXTENSIONS:
            return [root]
        return []

    files: list[Path] = []
    for ext in ALL_EXTENSIONS:
        files.extend(root.rglob(f'*{ext}'))

    # Skip build output directories
    skip_dirs = {'__history', '__recovery', '.git', 'node_modules'}
    files = [
        f for f in files
        if not any(part in skip_dirs for part in f.parts)
    ]

    return sorted(files)


def main() -> None:
    parser = argparse.ArgumentParser(
        description='Check and fix Delphi source file encoding (UTF-8 BOM + CRLF).',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Rules:
  .pas / .dpr / .dpk    UTF-8 BOM required + CRLF required
  .dfm / .fmx / .dproj  CRLF required (no BOM requirement)

Examples:
  python encoding_check.py src/                    Check all files
  python encoding_check.py src/ --fix              Fix all issues
  python encoding_check.py Main.Form.pas --fix     Fix single file
  python encoding_check.py src/ --summary          Show totals only
        """,
    )
    parser.add_argument('path', help='File or directory to check')
    parser.add_argument(
        '--fix', '-f',
        action='store_true',
        help='Automatically fix encoding issues',
    )
    parser.add_argument(
        '--summary', '-s',
        action='store_true',
        help='Print summary line only (suppress per-file output)',
    )
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Show OK files in addition to failures',
    )
    args = parser.parse_args()

    target = Path(args.path)
    if not target.exists():
        print(f'ERROR: Path not found: {target}', file=sys.stderr)
        sys.exit(1)

    files = collect_files(target)
    if not files:
        print(f'No Delphi source files found under {target}')
        sys.exit(0)

    issues: list[FileStatus] = []
    fixed_count = 0

    for filepath in files:
        try:
            status = inspect_file(filepath)
        except OSError as exc:
            print(f'ERROR: Cannot read {filepath}: {exc}', file=sys.stderr)
            continue

        if not status.ok:
            issues.append(status)
            if not args.summary:
                label = '[FAIL]'
                print(f'{label} {filepath}')
                for issue in status.issues:
                    print(f'       {issue}')

            if args.fix:
                changed = fix_file(status)
                if changed:
                    fixed_count += 1
                    if not args.summary:
                        print(f'       -> Fixed')
        else:
            if args.verbose and not args.summary:
                print(f'[OK]   {filepath}')

    # Summary
    total = len(files)
    issue_count = len(issues)

    if args.summary or issue_count > 0:
        print()

    print(f'Checked {total} file(s): {issue_count} issue(s) found', end='')
    if args.fix:
        print(f', {fixed_count} fixed')
    else:
        print()

    if issue_count > 0 and not args.fix:
        print('Run with --fix to automatically correct issues.')
        sys.exit(1)

    sys.exit(0)


if __name__ == '__main__':
    main()
