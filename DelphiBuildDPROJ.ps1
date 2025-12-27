# Universal DPROJ Build Script
# Builds any Delphi project file (.dproj) with MSBuild
# Can be used in any Delphi project - no dependencies to specific projects
#
# USAGE:
#   .\Build-DPROJ.ps1 -ProjectFile "MyProject.dproj" [-Config "Debug|Release"] [-Platform "Win32|Win64"] [-DelphiVersion "XX.X"] [-VerboseOutput]
#
# PARAMETERS:
#   -ProjectFile     : Path to the .dproj file to build (mandatory)
#   -Config          : Build configuration (default: "Debug")
#   -Platform        : Target platform (default: "Win32")
#   -DelphiVersion   : Delphi version to use (default: auto-detect latest)
#   -VerboseOutput   : Enable verbose MSBuild output
#
# EXAMPLES:
#   .\Build-DPROJ.ps1 -ProjectFile "MyApp.dproj"
#   .\Build-DPROJ.ps1 -ProjectFile "MyApp.dproj" -Config Release -Platform Win64
#   .\Build-DPROJ.ps1 -ProjectFile "MyApp.dproj" -DelphiVersion "22.0" -VerboseOutput
#
# REQUIREMENTS:
#   - Embarcadero Delphi installed
#   - MSBuild available (Visual Studio Build Tools or full Visual Studio)
#
# AUTO-DETECTION:
#   The script automatically detects the latest installed Delphi version by reading:
#   Registry: HKEY_LOCAL_MACHINE\SOFTWARE\Embarcadero\BDS (and Wow6432Node for 32-bit on 64-bit)
#

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectFile,

    [string]$Config = "Debug",
    [string]$Platform = "Win32",
    [string]$DelphiVersion = "",
    [switch]$VerboseOutput
)

# Color output functions
function Write-Info($Message) { Write-Host $Message -ForegroundColor Cyan }
function Write-Success($Message) { Write-Host $Message -ForegroundColor Green }
function Write-Warning($Message) { Write-Host $Message -ForegroundColor Yellow }
function Write-Error($Message) { Write-Host $Message -ForegroundColor Red }
function Write-Detail($Message) { Write-Host $Message -ForegroundColor Gray }

function Get-LatestDelphiVersion {
    <#
    .SYNOPSIS
    Auto-detects the latest installed Delphi version from Windows Registry

    .DESCRIPTION
    Scans the Windows Registry for Embarcadero BDS installations and returns
    the highest version number found along with its installation path.

    .OUTPUTS
    PSCustomObject with Version and RootDir properties, or $null if none found
    #>

    # Registry paths to check (both 64-bit and 32-bit registry views)
    $RegistryPaths = @(
        "HKLM:\SOFTWARE\Embarcadero\BDS",           # 64-bit registry
        "HKLM:\SOFTWARE\WOW6432Node\Embarcadero\BDS" # 32-bit registry on 64-bit Windows
    )

    $FoundVersions = @()

    foreach ($RegPath in $RegistryPaths) {
        if (Test-Path $RegPath) {
            Write-Host "  Scanning registry path: $RegPath" -ForegroundColor Gray

            try {
                $BDSKeys = Get-ChildItem -Path $RegPath -ErrorAction SilentlyContinue

                foreach ($Key in $BDSKeys) {
                    $VersionName = $Key.PSChildName

                    # Check if this looks like a version number (e.g., "23.0", "22.0")
                    if ($VersionName -match '^\d+\.\d+$') {
                        try {
                            # Get the RootDir value to verify it's a valid installation
                            $RootDir = Get-ItemProperty -Path $Key.PSPath -Name "RootDir" -ErrorAction SilentlyContinue

                            if ($RootDir -and $RootDir.RootDir -and (Test-Path $RootDir.RootDir)) {
                                $FoundVersions += [PSCustomObject]@{
                                    Version = $VersionName
                                    RootDir = $RootDir.RootDir
                                    RegistryPath = $Key.PSPath
                                }
                                Write-Host "  Found Delphi $VersionName at: $($RootDir.RootDir)" -ForegroundColor Gray
                            }
                        }
                        catch {
                            # Silently ignore versions we can't read
                        }
                    }
                }
            }
            catch {
                # Silently ignore registry paths we can't access
            }
        }
    }

    if ($FoundVersions.Count -eq 0) {
        Write-Warning "No Delphi installations found in Windows Registry"
        Write-Warning "Checked registry paths:"
        foreach ($Path in $RegistryPaths) {
            Write-Warning "  $Path"
        }
        return $null
    }

    # Sort versions numerically and get the latest
    $SortedVersions = $FoundVersions | Sort-Object { [Version]$_.Version } -Descending
    $LatestVersion = $SortedVersions[0]

    Write-Host "  Auto-detected Delphi versions: $($SortedVersions.Version -join ', ')" -ForegroundColor Gray
    Write-Host "  Using latest version: $($LatestVersion.Version)" -ForegroundColor Gray

    return $LatestVersion
}

function Initialize-DelphiEnvironment {
    param([string]$DelphiVersion)

    Write-Info "Initializing Delphi environment..."

    # Auto-detect Delphi version if not specified
    if ([string]::IsNullOrEmpty($DelphiVersion)) {
        Write-Warning "No Delphi version specified, auto-detecting..."
        $DelphiInfo = Get-LatestDelphiVersion

        if ($null -eq $DelphiInfo -or [string]::IsNullOrEmpty($DelphiInfo.Version)) {
            Write-Error "Could not auto-detect Delphi version and none was specified"
            Write-Error "Please specify a Delphi version using the -DelphiVersion parameter"
            Write-Error "Or ensure Delphi is properly installed and registered in Windows Registry"
            exit 1
        }

        $DelphiVersion = $DelphiInfo.Version
        $DelphiPath = $DelphiInfo.RootDir
        Write-Info "Auto-detected and using Delphi version: $DelphiVersion"
        Write-Detail "Installation path: $DelphiPath"
    } else {
        Write-Detail "Using specified Delphi version: $DelphiVersion"
        # Use default path for manually specified version
        $DelphiPath = "C:\Program Files (x86)\Embarcadero\Studio\$DelphiVersion"
    }

    # Set Delphi paths
    $RSVars = Join-Path $DelphiPath "bin\rsvars.bat"

    # Check if rsvars.bat exists
    if (-not (Test-Path $RSVars)) {
        Write-Error "rsvars.bat not found at $RSVars"
        Write-Error "Please check Delphi installation or adjust DelphiVersion parameter"
        Write-Error "Current DelphiVersion: $DelphiVersion"
        exit 1
    }

    Write-Warning "Setting up Delphi environment..."

    # Execute rsvars.bat and capture environment variables
    $tempFile = [System.IO.Path]::GetTempFileName()
    cmd /c "`"$RSVars`" && set > `"$tempFile`""

    # Read environment variables from temp file
    Get-Content $tempFile | ForEach-Object {
        if ($_ -match "^(.*?)=(.*)$") {
            $name = $matches[1]
            $value = $matches[2]
            Set-Item -Path "env:$name" -Value $value
        }
    }
    Remove-Item $tempFile

    # Now find MSBuild (after rsvars.bat has updated PATH)
    $MSBuild = Get-Command msbuild.exe -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source
    if (-not $MSBuild) {
        Write-Error "msbuild.exe not found in PATH even after running rsvars.bat"
        Write-Error "Please ensure MSBuild is installed (Visual Studio or Build Tools)"
        Write-Error "Delphi's rsvars.bat should add MSBuild to PATH"
        exit 1
    }

    Write-Success "Delphi environment initialized"
    Write-Detail "MSBuild: $MSBuild"
    return $MSBuild
}

function Build-DPROJProject {
    param(
        [string]$ProjectFile,
        [string]$Config,
        [string]$Platform,
        [string]$MSBuild,
        [bool]$VerboseOutput
    )
    
    # Validate project file
    if (-not (Test-Path $ProjectFile)) {
        Write-Error "Project file not found: $ProjectFile"
        exit 1
    }
    
    # Get absolute path for better error messages
    $ProjectPath = Resolve-Path $ProjectFile
    $ProjectName = [System.IO.Path]::GetFileNameWithoutExtension($ProjectFile)
    
    Write-Warning "Building project: $ProjectName"
    Write-Detail "  File: $ProjectPath"
    Write-Detail "  Config: $Config"
    Write-Detail "  Platform: $Platform"
    Write-Host ""
    
    # Prepare MSBuild arguments
    $MSBuildArgs = @(
        $ProjectPath,
        "/t:Build",
        "/p:Config=$Config",
        "/p:Platform=$Platform",
        "/nologo",
        "/m"
    )
    
    if ($VerboseOutput) {
        $MSBuildArgs += "/v:normal"
    } else {
        $MSBuildArgs += "/v:minimal"
    }
    
    # Build the project
    Write-Warning "Starting build..."
    Write-Host ""

    # Capture output and display it
    $BuildOutput = & $MSBuild @MSBuildArgs 2>&1
    $BuildExitCode = $LASTEXITCODE

    # Display the output
    $BuildOutput | ForEach-Object { Write-Host $_ }

    Write-Host ""

    if ($BuildExitCode -eq 0) {
        Write-Success "Build completed successfully!"
        $true
    } else {
        Write-Error "Build failed with exit code: $BuildExitCode"
        $false
    }
}

function Show-BuildSummary {
    param(
        [string]$ProjectFile,
        [string]$Config,
        [string]$Platform,
        [bool]$Success
    )
    
    $ProjectName = [System.IO.Path]::GetFileNameWithoutExtension($ProjectFile)
    
    Write-Host ""
    Write-Host "=" * 60 -ForegroundColor DarkGray
    Write-Host "Build Summary" -ForegroundColor White
    Write-Host "=" * 60 -ForegroundColor DarkGray
    Write-Detail "Project: $ProjectName"
    Write-Detail "Config: $Config"
    Write-Detail "Platform: $Platform"
    Write-Host ""
    
    if ($Success) {
        Write-Success "✓ BUILD SUCCESSFUL"
    } else {
        Write-Error "✗ BUILD FAILED"
    }
    
    Write-Host "=" * 60 -ForegroundColor DarkGray
}

# Main execution
try {
    Write-Info "Universal DPROJ Build Script"
    Write-Host ""
    
    # Initialize Delphi environment
    $MSBuild = Initialize-DelphiEnvironment -DelphiVersion $DelphiVersion
    Write-Host ""
    
    # Build the project
    $BuildSuccess = Build-DPROJProject -ProjectFile $ProjectFile -Config $Config -Platform $Platform -MSBuild $MSBuild -VerboseOutput $VerboseOutput

    # Ensure we have a clean boolean value
    if ($BuildSuccess -eq $true) {
        $BuildResult = $true
    } else {
        $BuildResult = $false
    }

    # Show summary
    Show-BuildSummary -ProjectFile $ProjectFile -Config $Config -Platform $Platform -Success $BuildResult
    
    # Exit with appropriate code
    if (-not $BuildResult) {
        exit 1
    }
}
catch {
    Write-Error "Unexpected error: $($_.Exception.Message)"
    Write-Error "Stack trace: $($_.ScriptStackTrace)"
    exit 1
}
