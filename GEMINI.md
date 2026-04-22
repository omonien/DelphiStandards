# Delphi Standards - Project Context

This repository provides a battle-tested baseline for consistent Delphi code development, focusing on modern style guides, git templates, and build automation.

## Project Overview

*   **Purpose:** Establish shared standards for Delphi teams to improve code readability, maintainability, and repository cleanliness.
*   **Key Components:**
    *   **Style Guides:** Comprehensive guides in English (`Delphi Style Guide EN.md`) and German (`Delphi Style Guide DE.md`).
    *   **Git Templates:** Curated `.gitignore` and `.gitattributes` specifically tailored for Delphi projects to prevent IDE artifacts and binary bloat.
    *   **Build Automation:** A universal PowerShell script (`DelphiBuildDPROJ.ps1`) for CLI-based Delphi builds.
    *   **Release Tools:** Shell scripts and configurations (`release-tools/`) for generating PDF versions of the guides and packaging releases.

## Building and Running

### Build Automation (for Delphi Projects)
The `DelphiBuildDPROJ.ps1` script can be used to build any Delphi `.dproj` file via MSBuild on Windows. For WSL users, the `DelphiBuildDPROJ.sh` script provides a bridge to the Windows host.

#### Windows (PowerShell)
```powershell
# Basic usage (auto-detects Delphi version, builds Debug/Win32)
.\DelphiBuildDPROJ.ps1 -ProjectFile "MyProject.dproj"

# Build Release for Win64
.\DelphiBuildDPROJ.ps1 -ProjectFile "MyProject.dproj" -Config Release -Platform Win64
```

#### WSL (Bash)
```bash
# Basic usage (bridges to Windows host)
./DelphiBuildDPROJ.sh -ProjectFile "./MyProject.dproj"

# Build Release for Win64
./DelphiBuildDPROJ.sh -ProjectFile "./MyProject.dproj" -Config Release -Platform Win64
```

### Release Generation (for this Repository)
To generate release assets (PDFs and ZIP archives), use the provided release script. This requires `pandoc` and `xelatex`.

```bash
# Run from project root
./release-tools/create-release.sh
```

## Development Conventions

### Formatting
*   **Indentation:** 2 spaces.
*   **Line Length:** 120 characters maximum.
*   **Blocks:** `begin..end` always on separate lines.
*   **Language Features:** Prefer modern features like generics, anonymous methods, inline variables (10.3+), and multiline strings (12+).

### Naming Conventions
*   **Locals:** `L` prefix (e.g., `LCustomer`).
*   **Parameters:** `A` prefix (e.g., `const AValue: string`).
*   **Fields:** `F` prefix (e.g., `FName`).
*   **Types:** `T` prefix for classes/records, `I` prefix for interfaces.
*   **Constants:** `c` prefix for technical constants, `sc` for string constants.
*   **Unit Names:** Follow a namespace hierarchy, e.g., `Main.Form`, `Customer.Details.DM`.

### Best Practices
*   **Resource Management:** Always use `try..finally` blocks. Prefer `FreeAndNil(LObject)` over `.Free`.
*   **Language Sync:** Both German and English style guides must be kept in sync. Maintainers handle synchronization post-merge if only one language is updated in a PR.
*   **Git Hygiene:** Use the provided `Delphi GitIgnore.txt` and `Delphi GitAttributes.txt` in all Delphi repositories to ensure consistency.

## Key Files
*   `Delphi Style Guide EN.md`: The primary English style guide.
*   `Delphi Style Guide DE.md`: The primary German style guide.
*   `Delphi GitIgnore.txt`: Template for `.gitignore`.
*   `Delphi GitAttributes.txt`: Template for `.gitattributes`.
*   `DelphiBuildDPROJ.ps1`: Universal Delphi build script for Windows.
*   `DelphiBuildDPROJ.sh`: WSL bridge for the Delphi build script.
