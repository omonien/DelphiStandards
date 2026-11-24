# fetch_r8.ps1 - downloads R8 9.0.32 next to DX.R8.targets (r8\r8.jar) and checks its SHA-256.
# Do not commit r8.jar (55 MB); add "r8/" to .gitignore.
# Delphi's bundled R8 (cmdline-tools 16.0, R8 8.6.17) rejects --build-metadata-output, so it cannot be used.
$ErrorActionPreference = 'Stop'
$Version  = '9.0.32'
$Expected = 'ce5b937b03efea9ce63bd5b534faed130e00ffe8e0faad4098c1bb436916c17b'
$dir = Join-Path $PSScriptRoot 'r8'
New-Item -ItemType Directory -Force -Path $dir | Out-Null
$jar = Join-Path $dir 'r8.jar'
if (-not (Test-Path $jar)) {
  Invoke-WebRequest -Uri "https://storage.googleapis.com/r8-releases/raw/$Version/r8.jar" -OutFile $jar -UseBasicParsing
}
$hash = (Get-FileHash $jar -Algorithm SHA256).Hash.ToLowerInvariant()
if ($hash -ne $Expected) { Remove-Item $jar; throw "r8.jar SHA-256 mismatch: $hash (expected $Expected for R8 $Version)" }
$java = if ($env:JAVA_HOME) { Join-Path $env:JAVA_HOME 'bin\java.exe' } else { 'java' }
& $java -cp $jar com.android.tools.r8.R8 --version
"R8_JAR=$jar SHA256=$hash"
