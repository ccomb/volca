# =============================================================================
# volca installer (Windows / PowerShell)
#
# Usage:
#   iwr -useb https://raw.githubusercontent.com/ccomb/volca/main/install.ps1 | iex
#   & ([scriptblock]::Create((iwr -useb 'https://raw.githubusercontent.com/ccomb/volca/main/install.ps1').Content)) v0.7.0
#
# What it does:
#   1. Detects platform (windows-amd64).
#   2. Downloads volca-<version>-windows-amd64.zip from the GH Release.
#   3. Downloads volca-data-<data-version>.tar.gz (reference data bundle).
#   4. Verifies both against SHA256SUMS.
#   5. Extracts to (matches platformdirs.user_data_dir("volca", appauthor=False)):
#        $env:LOCALAPPDATA\volca\<version>\volca.exe
#        $env:LOCALAPPDATA\volca\data\<data-version>\{flows.csv,...}
#      and points $env:LOCALAPPDATA\volca\data\current at <data-version>.
#      Same root as install.sh and pyvolca.download().
#   6. Installs a thin shim at $env:LOCALAPPDATA\volca\bin\volca.cmd that
#      sets VOLCA_DATA_DIR and execs the real binary.
#
# Override the install root with $env:VOLCA_HOME = 'C:\full\path'.
# =============================================================================

[CmdletBinding()]
param(
    [string]$Version = ''
)

$ErrorActionPreference = 'Stop'

$Repo = 'ccomb/volca'
$Prefix = if ($env:VOLCA_HOME) { $env:VOLCA_HOME } else { Join-Path $env:LOCALAPPDATA 'volca' }
$BinDir = Join-Path $Prefix 'bin'
$Shim = Join-Path $BinDir 'volca.cmd'

function Say($msg) { Write-Host "==> $msg" }
function Die($msg) { Write-Error "install.ps1: $msg"; exit 1 }

# --- Platform detection ------------------------------------------------------

$arch = $env:PROCESSOR_ARCHITECTURE
if ($arch -eq 'AMD64' -or $arch -eq 'x64') {
    $Platform = 'windows-amd64'
} else {
    Die "unsupported Windows arch: $arch (only AMD64 is built)"
}

# --- Resolve release tag -----------------------------------------------------

if (-not $Version) {
    Say "Resolving latest release of $Repo"
    $rel = Invoke-RestMethod -UseBasicParsing -Uri "https://api.github.com/repos/$Repo/releases/latest"
    $Version = $rel.tag_name
}
if ($Version -notmatch '^v') { $Version = "v$Version" }
$PlainVersion = $Version.TrimStart('v')

Say "Installing volca $Version for $Platform"

# --- Download SHA256SUMS -----------------------------------------------------

$Work = New-Item -ItemType Directory -Path (Join-Path $env:TEMP "volca-install-$([guid]::NewGuid().Guid)")
try {
    $RelBase = "https://github.com/$Repo/releases/download/$Version"

    Say 'Downloading SHA256SUMS'
    $SumsPath = Join-Path $Work 'SHA256SUMS'
    Invoke-WebRequest -UseBasicParsing -Uri "$RelBase/SHA256SUMS" -OutFile $SumsPath

    $Sums = Get-Content $SumsPath
    $BinAsset = "volca-$PlainVersion-$Platform.zip"
    if (-not ($Sums -match "  $([regex]::Escape($BinAsset))$")) {
        Die "release $Version has no asset $BinAsset"
    }

    $DataAsset = ($Sums | ForEach-Object {
        if ($_ -match '^[0-9a-f]+\s+(volca-data-.*\.tar\.gz)$') { $matches[1] }
    }) | Select-Object -First 1
    if (-not $DataAsset) { Die "release $Version has no volca-data-* asset" }
    $DataVersion = $DataAsset -replace '^volca-data-(.*)\.tar\.gz$', '$1'

    # --- Download + verify ---------------------------------------------------

    Say "Downloading $BinAsset"
    $BinPath = Join-Path $Work $BinAsset
    Invoke-WebRequest -UseBasicParsing -Uri "$RelBase/$BinAsset" -OutFile $BinPath

    Say "Downloading $DataAsset"
    $DataPath = Join-Path $Work $DataAsset
    Invoke-WebRequest -UseBasicParsing -Uri "$RelBase/$DataAsset" -OutFile $DataPath

    function Test-Sha256($file, $expected) {
        $actual = (Get-FileHash -Algorithm SHA256 -Path $file).Hash.ToLower()
        if ($actual -ne $expected.ToLower()) {
            Die "SHA256 mismatch for $(Split-Path -Leaf $file): expected $expected, got $actual"
        }
    }
    foreach ($line in $Sums) {
        if ($line -match '^([0-9a-f]+)\s+(.+)$') {
            $hash = $matches[1]; $name = $matches[2]
            if ($name -eq $BinAsset)  { Test-Sha256 $BinPath  $hash }
            if ($name -eq $DataAsset) { Test-Sha256 $DataPath $hash }
        }
    }

    # --- Install -------------------------------------------------------------

    $VersionDir = Join-Path $Prefix $PlainVersion
    $DataDir = Join-Path (Join-Path $Prefix 'data') $DataVersion
    $CurrentDataLink = Join-Path (Join-Path $Prefix 'data') 'current'

    New-Item -ItemType Directory -Force -Path $VersionDir, $DataDir, $BinDir | Out-Null

    Say "Extracting binary to $VersionDir"
    Expand-Archive -Force -Path $BinPath -DestinationPath $VersionDir

    Say "Extracting data bundle to $DataDir"
    # PowerShell 5+ ships tar.exe (bsdtar). Use it so we don't depend on 7-Zip.
    & tar.exe -xzf $DataPath -C $DataDir
    if ($LASTEXITCODE -ne 0) { Die 'tar extraction failed' }

    # "current" pointer. Try junction first (no admin needed); fall back to a
    # plain copy if New-Item -ItemType Junction is unavailable.
    if (Test-Path $CurrentDataLink) { Remove-Item -Recurse -Force $CurrentDataLink }
    try {
        New-Item -ItemType Junction -Path $CurrentDataLink -Target $DataDir | Out-Null
    } catch {
        Say 'Junction unsupported, falling back to copy'
        Copy-Item -Recurse -Force $DataDir $CurrentDataLink
    }

    Say "Installing shim at $Shim"
    $VolcaExe = Join-Path $VersionDir 'volca.exe'
    @"
@echo off
rem Auto-generated by volca install.ps1 — do not edit. Re-run to regenerate.
set "VOLCA_DATA_DIR=$CurrentDataLink"
"$VolcaExe" %*
"@ | Set-Content -Path $Shim -Encoding ASCII

    # --- Post-install guidance ----------------------------------------------

    $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    if (-not ($userPath -split ';' -contains $BinDir)) {
        Write-Host ""
        Write-Host "NOTE: $BinDir is not on your user PATH. To add it (this terminal only):"
        Write-Host "    `$env:Path = '$BinDir;' + `$env:Path"
        Write-Host "Or persist for future sessions:"
        Write-Host "    [Environment]::SetEnvironmentVariable('Path', '$BinDir;' + [Environment]::GetEnvironmentVariable('Path', 'User'), 'User')"
    }

    Write-Host ""
    Write-Host "volca $Version installed."
    Write-Host "  binary:  $VolcaExe"
    Write-Host "  data:    $CurrentDataLink -> $DataVersion"
    Write-Host "  shim:    $Shim"
    Write-Host ""
    Write-Host "Try:  volca --version"
}
finally {
    Remove-Item -Recurse -Force $Work -ErrorAction SilentlyContinue
}
