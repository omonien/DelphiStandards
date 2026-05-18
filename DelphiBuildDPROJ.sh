#!/bin/bash

# =============================================================================
# DelphiBuildDPROJ.sh - Universal Delphi Project Builder for WSL
# =============================================================================
# Copyright (c) 2025 Olaf Monien - The MIT License (MIT)
#   https://github.com/omonien/DelphiStandards
# =============================================================================
# Bridges WSL to the Windows Delphi environment via PowerShell.
# This script wraps DelphiBuildDPROJ.ps1 for use within WSL.
# =============================================================================

# Colors for output
CYAN='\033[0;36m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PS_SCRIPT_PATH="${SCRIPT_DIR}/DelphiBuildDPROJ.ps1"

function show_usage() {
    echo -e "${CYAN}Usage:${NC}"
    echo "  $0 -ProjectFile <path_to_dproj> [options]"
    echo ""
    echo -e "${CYAN}Options:${NC}"
    echo "  -Config <name>          Build configuration (default: Debug)"
    echo "  -Platform <name>        Target platform (default: Win32)"
    echo "  -DelphiVersion <ver>    Specific Delphi version (e.g., 22.0)"
    echo "  -VerboseOutput          Enable verbose MSBuild output"
    echo "  -LinuxMap               Enable Linux debug symbols and map file"
    echo "  -h, --help              Show this help message"
    echo ""
    echo -e "${CYAN}Example:${NC}"
    echo "  $0 -ProjectFile ./MyProject.dproj -Config Release -Platform Win64"
}

# 1. WSL Check
if ! grep -qEi "(Microsoft|WSL)" /proc/version &> /dev/null; then
    echo -e "${RED}Error: This script must be run from within WSL (Windows Subsystem for Linux).${NC}"
    exit 1
fi

# 2. Check for powershell.exe
if ! command -v powershell.exe &> /dev/null; then
    echo -e "${RED}Error: powershell.exe not found in PATH.${NC}"
    echo "Ensure Windows PowerShell is accessible from your WSL environment."
    exit 1
fi

# 3. Check for Delphi installation on the Windows host
# We perform a quick registry check for Embarcadero BDS keys
DELPHI_REG_CHECK=$(powershell.exe -NoProfile -Command "Get-Item -Path 'HKLM:\SOFTWARE\Embarcadero\BDS', 'HKLM:\SOFTWARE\WOW6432Node\Embarcadero\BDS' -ErrorAction SilentlyContinue | Measure-Object | Select-Object -ExpandProperty Count" 2>/dev/null | tr -d '\r\n')

if [[ -z "$DELPHI_REG_CHECK" || "$DELPHI_REG_CHECK" -eq "0" ]]; then
    echo -e "${RED}Error: Delphi installation not detected on the Windows host.${NC}"
    echo "This script requires Embarcadero Delphi to be installed on Windows."
    exit 1
fi

# 4. Check if the PowerShell script exists
if [[ ! -f "$PS_SCRIPT_PATH" ]]; then
    echo -e "${RED}Error: Companion script not found: ${PS_SCRIPT_PATH}${NC}"
    exit 1
fi

# 5. Parse Arguments
PROJECT_FILE=""
CONFIG=""
PLATFORM=""
DELPHI_VERSION=""
VERBOSE=false
LINUX_MAP=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -ProjectFile)
            PROJECT_FILE="$2"
            shift 2
            ;;
        -Config)
            CONFIG="$2"
            shift 2
            ;;
        -Platform)
            PLATFORM="$2"
            shift 2
            ;;
        -DelphiVersion)
            DELPHI_VERSION="$2"
            shift 2
            ;;
        -VerboseOutput)
            VERBOSE=true
            shift
            ;;
        -LinuxMap)
            LINUX_MAP=true
            shift
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown argument: $1${NC}"
            show_usage
            exit 1
            ;;
    esac
done

if [[ -z "$PROJECT_FILE" ]]; then
    echo -e "${RED}Error: -ProjectFile is mandatory.${NC}"
    show_usage
    exit 1
fi

# 6. Convert Paths
# Check if project file exists in WSL
if [[ ! -f "$PROJECT_FILE" ]]; then
    echo -e "${RED}Error: Project file not found at: ${PROJECT_FILE}${NC}"
    exit 1
fi

# Convert WSL path to Windows path for the PowerShell script
WIN_PROJECT_FILE=$(wslpath -w "$PROJECT_FILE")
WIN_PS_SCRIPT_PATH=$(wslpath -w "$PS_SCRIPT_PATH")

# 7. Construct and Run PowerShell command
# We use an array to handle arguments safely and avoid literal quote issues
# -InputFormat None prevents PowerShell from trying to read from stdin, which can cause hangs
PS_CMD_ARGS=("-NoProfile" "-NonInteractive" "-InputFormat" "None" "-ExecutionPolicy" "Bypass")

# Using -Command with & is more robust for UNC paths from WSL than -File
# We wrap the call in a try/catch in PowerShell to ensure we see errors if initialization fails
COMMAND="try { & '$WIN_PS_SCRIPT_PATH' -ProjectFile '$WIN_PROJECT_FILE'"

[[ -n "$CONFIG" ]] && COMMAND="$COMMAND -Config '$CONFIG'"
[[ -n "$PLATFORM" ]] && COMMAND="$COMMAND -Platform '$PLATFORM'"
[[ -n "$DELPHI_VERSION" ]] && COMMAND="$COMMAND -DelphiVersion '$DELPHI_VERSION'"
[[ "$VERBOSE" = true ]] && COMMAND="$COMMAND -VerboseOutput"
[[ "$LINUX_MAP" = true ]] && COMMAND="$COMMAND -LinuxMap"

COMMAND="$COMMAND } catch { Write-Error \$_.Exception.Message; exit 1 }"

echo -e "${CYAN}Bridging build to Windows host...${NC}"
# Note: we use -Command because it handles UNC paths better than -File in some PS versions
powershell.exe "${PS_CMD_ARGS[@]}" -Command "$COMMAND"
EXIT_CODE=$?

if [[ $EXIT_CODE -eq 0 ]]; then
    echo -e "${GREEN}WSL Bridge: Build finished successfully.${NC}"
else
    echo -e "${RED}WSL Bridge: Build failed with exit code $EXIT_CODE.${NC}"
fi

exit $EXIT_CODE
