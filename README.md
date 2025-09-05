<div align="left">
  <a href="https://www.embarcadero.com/products/delphi">
    <img src="https://commons.wikimedia.org/wiki/Special:FilePath/Delphi_Logo_12.svg" alt="Delphi Logo" width="120" />
  </a>
</div>


# Delphi Standards

[![Lang-EN](https://img.shields.io/badge/lang-EN-blue.svg)](README.md) [![Lang-DE](https://img.shields.io/badge/lang-DE-lightgrey.svg)](README.de.md)

> Contributions in German or English are welcome. Language synchronization is handled by the maintainer no later than post-merge.

## Overview

This repository provides a compact, battle-tested baseline for consistent Delphi code. It focuses on:

- A clear, modern Delphi Style Guide (German and English)
- A curated .gitignore tailored for Delphi

Both help teams write consistent code and avoid common friction.

## Why this matters

Teams frequently commit files that should not be versioned (IDE artifacts, build outputs, local settings). This leads to:

- unnecessary diffs and merge conflicts
- unstable builds due to local artifacts
- unwanted side effects across the team

The curated, Delphi-specific .gitignore prevents exactly that. The Style Guide, in parallel, provides a shared understanding of structure, naming, and modern language features.

## Contents

- Delphi Style Guide (DE): [Delphi Style Guide.md](Delphi%20Style%20Guide.md)
- Delphi Style Guide (EN): [Delphi Style Guide EN.md](Delphi%20Style%20Guide%20EN.md)
- Curated GitIgnore template: [Delphi GitIgnore.txt](Delphi%20GitIgnore.txt)

Note: The style guide is maintained in German and English. Please keep both documents in sync (maintainer will handle synchronization post-merge if needed).

## Quick Start

1) Agree on the style guide in your team
- Indentation: 2 spaces; line length: 120 chars (keep formatter/editor guideline in sync)
- Naming conventions (A-/L-/F‑prefixes, components, constants, enums with SCOPEDENUMS)
- Modern features: generics, anonymous methods, inline variables (10.3+), multiline strings (12+)

2) Enable .gitignore
- Copy "Delphi GitIgnore.txt" to the repo root and rename it to ".gitignore"
- Add project-specific rules as needed

## Contributing

- Issues and PRs are welcome
- Single-language PRs (DE or EN) are fine; maintainer will sync the other language after merge

## Ideas

- Provide configuration templates for code formatters to implement this style guide (e.g., 2-space indentation, 120-char line length, synchronized vertical guideline)...
- Shared project presets (.dproj) or IDE settings for teams

## License

MIT License – see notices in the files