#!/bin/bash
# Delphi Standards Release Script
# ================================
# This script creates a release based on release-config.yaml

set -e  # Exit on error

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
# Get the project root (parent of release-tools)
PROJECT_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

# Change to project root
cd "$PROJECT_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Functions
print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# Check if required tools are installed
check_requirements() {
    print_header "Checking Requirements"
    
    local missing_tools=()
    
    if ! command -v pandoc &> /dev/null; then
        missing_tools+=("pandoc")
    fi
    
    if ! command -v zip &> /dev/null; then
        missing_tools+=("zip")
    fi
    
    if [ ${#missing_tools[@]} -gt 0 ]; then
        print_error "Missing required tools: ${missing_tools[*]}"
        echo ""
        echo "Install missing tools:"
        for tool in "${missing_tools[@]}"; do
            case $tool in
                pandoc)
                    echo "  - pandoc: brew install pandoc (macOS) or apt-get install pandoc (Linux)"
                    echo "    Also install LaTeX: brew install --cask mactex-no-gui (macOS)"
                    ;;
                zip)
                    echo "  - zip: Should be pre-installed on most systems"
                    ;;
            esac
        done
        exit 1
    fi
    
    print_success "All required tools are installed"
}

# Read version from config
get_version() {
    # Simple YAML parsing (assumes version is on its own line)
    grep "^  version:" release-tools/release-config.yaml | sed 's/.*"\(.*\)".*/\1/'
}

# Main release process
main() {
    print_header "Delphi Standards Release Creator"
    
    # Check requirements
    check_requirements
    
    # Get version
    VERSION=$(get_version)
    if [ -z "$VERSION" ]; then
        print_error "Could not read version from release-tools/release-config.yaml"
        exit 1
    fi
    
    print_info "Creating release v${VERSION}"
    echo ""
    
    # Create directories
    print_header "Creating Directory Structure"
    RELEASE_DIR="release-assets/v${VERSION}"

    # Remove old release directory if it exists
    if [ -d "$RELEASE_DIR" ]; then
        rm -rf "$RELEASE_DIR"
        print_info "Removed old release directory"
    fi

    mkdir -p "$RELEASE_DIR"
    print_success "Created directory: $RELEASE_DIR"
    
    # Copy source files
    print_header "Copying Source Files"
    
    # Copy Markdown files
    cp "Delphi Style Guide DE.md" "$RELEASE_DIR/"
    print_success "Copied Delphi Style Guide DE.md"
    
    cp "Delphi Style Guide EN.md" "$RELEASE_DIR/"
    print_success "Copied Delphi Style Guide EN.md"
    
    # Copy Git templates (rename to final names)
    cp "Delphi GitIgnore.txt" "$RELEASE_DIR/.gitignore"
    print_success "Copied Delphi GitIgnore.txt -> .gitignore"

    cp "Delphi GitAttributes.txt" "$RELEASE_DIR/.gitattributes"
    print_success "Copied Delphi GitAttributes.txt -> .gitattributes"
    
    # Copy documentation
    cp "LICENSE.md" "$RELEASE_DIR/"
    print_success "Copied LICENSE.md"
    
    cp "README.md" "$RELEASE_DIR/"
    print_success "Copied README.md"
    
    cp "README.de.md" "$RELEASE_DIR/"
    print_success "Copied README.de.md"
    
    # Generate PDFs
    print_header "Generating PDF Files"
    
    print_info "Converting Delphi Style Guide DE.md to PDF..."
    pandoc "Delphi Style Guide DE.md" \
        -o "$RELEASE_DIR/Delphi Style Guide DE.pdf" \
        --pdf-engine=xelatex \
        --toc \
        --toc-depth=2 \
        --number-sections \
        -V geometry:margin=2.5cm \
        -V fontsize=11pt \
        -V documentclass=article \
        -V papersize=a4 \
        -V colorlinks=true \
        -V linkcolor=blue \
        -V urlcolor=blue \
        -V lang=de \
        -V title="Delphi Style Guide (Deutsch)" \
        -V author="Olaf Monien" \
        -V date="$(date +%Y-%m-%d)" \
        2>&1 | grep -v "Missing character" || true
    
    if [ -f "$RELEASE_DIR/Delphi Style Guide DE.pdf" ]; then
        print_success "Generated Delphi Style Guide DE.pdf"
    else
        print_error "Failed to generate German PDF"
        exit 1
    fi
    
    print_info "Converting Delphi Style Guide EN.md to PDF..."
    pandoc "Delphi Style Guide EN.md" \
        -o "$RELEASE_DIR/Delphi Style Guide EN.pdf" \
        --pdf-engine=xelatex \
        --toc \
        --toc-depth=2 \
        --number-sections \
        -V geometry:margin=2.5cm \
        -V fontsize=11pt \
        -V documentclass=article \
        -V papersize=a4 \
        -V colorlinks=true \
        -V linkcolor=blue \
        -V urlcolor=blue \
        -V lang=en \
        -V title="Delphi Style Guide (English)" \
        -V author="Olaf Monien" \
        -V date="$(date +%Y-%m-%d)" \
        2>&1 | grep -v "Missing character" || true
    
    if [ -f "$RELEASE_DIR/Delphi Style Guide EN.pdf" ]; then
        print_success "Generated Delphi Style Guide EN.pdf"
    else
        print_error "Failed to generate English PDF"
        exit 1
    fi
    
    # Remove Markdown files from release (we only want PDFs)
    print_info "Removing Markdown files from release directory..."
    rm "$RELEASE_DIR/Delphi Style Guide DE.md"
    rm "$RELEASE_DIR/Delphi Style Guide EN.md"
    print_success "Removed Markdown files (PDFs only in release)"
    
    # Create ZIP archive
    print_header "Creating ZIP Archive"
    
    ARCHIVE_NAME="DelphiStandards-v${VERSION}.zip"
    cd release-assets
    
    # Remove old archive if exists
    if [ -f "$ARCHIVE_NAME" ]; then
        rm "$ARCHIVE_NAME"
        print_info "Removed old archive"
    fi
    
    zip -r "$ARCHIVE_NAME" "v${VERSION}/" > /dev/null
    cd ..
    
    print_success "Created archive: release-assets/$ARCHIVE_NAME"
    
    # Show archive contents
    print_header "Archive Contents"
    unzip -l "release-assets/$ARCHIVE_NAME"
    
    # Show file sizes
    print_header "Release Summary"
    echo ""
    echo "Release Directory: $RELEASE_DIR"
    ls -lh "$RELEASE_DIR"
    echo ""
    echo "Archive: release-assets/$ARCHIVE_NAME"
    ls -lh "release-assets/$ARCHIVE_NAME"
    echo ""
    
    print_success "Release v${VERSION} created successfully!"
    echo ""
    print_info "Next steps:"
    echo "  1. Review the release files in $RELEASE_DIR"
    echo "  2. Create Git tag: git tag -a v${VERSION} -m 'Release v${VERSION}'"
    echo "  3. Push to remote: git push origin master && git push origin v${VERSION}"
    echo "  4. Create GitHub release and upload release-assets/$ARCHIVE_NAME"
}

# Run main function
main

