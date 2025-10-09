# Release Tools

This directory contains tools and configuration for creating releases of the Delphi Standards project.

## Contents

- **`release-config.yaml`** - Configuration file defining release structure and content
- **`create-release.sh`** - Automated release creation script

## Quick Start

### Prerequisites

Install required tools:

```bash
# macOS
brew install pandoc
brew install --cask basictex

# Linux (Debian/Ubuntu)
sudo apt-get install pandoc texlive-xetex texlive-fonts-recommended
```

Verify installation:
```bash
pandoc --version
xelatex --version
zip --version
```

### Create a Release

1. **Update version** in `release-config.yaml`:
   ```yaml
   release:
     version: "2.2.0"
     date: "2025-10-10"
   ```

2. **Update style guides** with matching version and date:
   ```markdown
   > **Version:** 2.2
   > **Stand:** 2025-10-10
   ```

3. **Run the release script** from the project root:
   ```bash
   ./release-tools/create-release.sh
   ```

   Or from this directory:
   ```bash
   cd release-tools
   ./create-release.sh
   ```

4. **Review the release**:
   ```bash
   ls -lh release-assets/v2.2.0/
   unzip -l release-assets/DelphiStandards-v2.2.0.zip
   ```

5. **Create Git tag**:
   ```bash
   git tag -a v2.2.0 -m "Release v2.2.0 - Description"
   ```

6. **Push to remote** (optional):
   ```bash
   git push origin master
   git push origin v2.2.0
   ```

## What the Script Does

The `create-release.sh` script automatically:

1. Checks for required tools (pandoc, xelatex, zip)
2. Creates release directory structure
3. Converts Markdown style guides to PDF
4. Copies Git templates (.gitignore, .gitattributes)
5. Copies documentation files
6. Creates ZIP archive
7. Displays summary and next steps

## Release Structure

Each release contains:

```
release-assets/v{version}/
├── Delphi Style Guide DE.pdf      (Generated from Markdown)
├── Delphi Style Guide EN.pdf      (Generated from Markdown)
├── .gitignore                     (Ready-to-use Git ignore file)
├── .gitattributes                 (Ready-to-use Git attributes file)
├── LICENSE.md
├── README.md
└── README.de.md
```

Packaged as: `release-assets/DelphiStandards-v{version}.zip`

### Directory Layout

```
release-assets/
├── v2.1.0/
│   ├── Delphi Style Guide DE.pdf
│   ├── Delphi Style Guide EN.pdf
│   ├── .gitignore
│   ├── .gitattributes
│   ├── LICENSE.md
│   ├── README.md
│   └── README.de.md
├── DelphiStandards-v2.1.0.zip
└── README.md
```

## Configuration

### release-config.yaml

Key sections:

```yaml
release:
  version: "2.1.0"          # Update for each release
  date: "2025-10-09"        # Update for each release

sources:
  style_guides:             # Markdown files to convert to PDF
  git_templates:            # Git configuration templates
  documentation:            # Additional documentation

output:
  base_dir: "release-assets"
  archive:
    format: "zip"
    name: "DelphiStandards-v{version}.zip"

pdf_generation:
  enabled: true
  tool: "pandoc"
  pandoc:
    engine: "xelatex"       # LaTeX engine
    options: [...]          # PDF generation options
```

## Troubleshooting

### PDF Generation Fails

**Problem:** `pandoc: xelatex not found`

**Solution:** Install LaTeX (see Prerequisites)

---

**Problem:** LaTeX warnings about missing characters

**Solution:** These are usually harmless. The script filters them out automatically.

---

**Problem:** PDF has broken links

**Solution:** This is a known LaTeX issue with internal references. Links still work, warnings can be ignored.

### Archive Creation Fails

**Problem:** `zip: command not found`

**Solution:** Install zip utility (usually pre-installed)

### Script Permission Denied

**Problem:** `bash: ./create-release.sh: Permission denied`

**Solution:** Make script executable:
```bash
chmod +x create-release.sh
```

### Script Can't Find Files

**Problem:** Files not found when running script

**Solution:**
- Always run from project root or use `./release-tools/create-release.sh`
- The script automatically changes to project root

## Manual Release Process

If the automated script doesn't work, you can create a release manually:

### 1. Create Directory

```bash
mkdir -p release-assets/v2.1.0
```

### 2. Generate PDFs

```bash
pandoc "Delphi Style Guide DE.md" \
  -o "release-assets/v2.1.0/Delphi Style Guide DE.pdf" \
  --pdf-engine=xelatex \
  --toc --toc-depth=2 --number-sections \
  -V geometry:margin=2.5cm -V fontsize=11pt

pandoc "Delphi Style Guide EN.md" \
  -o "release-assets/v2.1.0/Delphi Style Guide EN.pdf" \
  --pdf-engine=xelatex \
  --toc --toc-depth=2 --number-sections \
  -V geometry:margin=2.5cm -V fontsize=11pt
```

### 3. Copy Files

```bash
cp "Delphi GitIgnore.txt" release-assets/v2.1.0/.gitignore
cp "Delphi GitAttributes.txt" release-assets/v2.1.0/.gitattributes
cp LICENSE.md README.md README.de.md release-assets/v2.1.0/
```

### 4. Create Archive

```bash
cd release-assets
zip -r DelphiStandards-v2.1.0.zip v2.1.0/
cd ..
```

## Best Practices

1. **Always test the release locally** before pushing to GitHub
2. **Verify PDF quality** - open and check formatting
3. **Check archive contents** - ensure all files are included
4. **Update version consistently** - in config, style guides, and git tag
5. **Keep both language versions in sync** - German and English

## Future Improvements

Potential enhancements for the release process:

- [ ] Automated version bumping
- [ ] Changelog generation from git commits
- [ ] GitHub Release creation via API
- [ ] PDF quality checks (broken links, formatting)
- [ ] Multi-format support (EPUB, HTML)
- [ ] Automated testing of release contents

## Notes

- The script works from any directory (automatically finds project root)
- Markdown files are converted to PDF and removed from release
- Git templates are renamed to `.gitignore` and `.gitattributes` (ready to use)
- Both German and English versions must be updated before release

---

**Last Updated:** 2025-10-09  
**Maintainer:** Olaf Monien

