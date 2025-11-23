$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

param(
    [switch] $Publish = $false,
    [ValidateSet('Debug','Release')]
    [string] $Configuration = 'Release',
    [ValidateSet('win-x64','win-arm64')]
    [string] $Runtime = 'win-x64',
    [switch] $SelfContained = $false
)

# Resolve repo root (script lives in scripts/)
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir
Set-Location $RepoRoot

Write-Host "🚀 Time_Warp build script starting..." -ForegroundColor Cyan
Write-Host "Configuration: $Configuration | Publish: $Publish | Runtime: $Runtime | SelfContained: $SelfContained"

# Basic checks
if (-not (Get-Command dotnet -ErrorAction SilentlyContinue)) {
    throw "dotnet CLI not found. Install .NET SDK 9.0+ and ensure it's on PATH."
}

$dotnetVersion = (& dotnet --version).Trim()
Write-Host "Using dotnet SDK: $dotnetVersion"

$slnPath = Join-Path $RepoRoot 'Time_Warp.sln'
$wpfCsproj = Join-Path $RepoRoot 'platforms\windows\TimeWarp.Wpf\TimeWarp.Wpf.csproj'

if (-not (Test-Path $slnPath)) { throw "Solution not found: $slnPath" }
if (-not (Test-Path $wpfCsproj)) { throw "WPF project not found: $wpfCsproj" }

# Restore
Write-Host "🔧 Restoring solution..." -ForegroundColor Yellow
dotnet restore $slnPath --nologo

if (-not $Publish) {
    # Build
    Write-Host "🏗️  Building solution ($Configuration)..." -ForegroundColor Yellow
    dotnet build $slnPath -c $Configuration --nologo

    Write-Host "✅ Build complete." -ForegroundColor Green
    $outDir = Join-Path $RepoRoot "platforms\windows\TimeWarp.Wpf\bin\$Configuration"
    if (Test-Path $outDir) {
        Write-Host "Output directory: $outDir" -ForegroundColor Green
    }
}
else {
    # Publish WPF app
    Write-Host "📦 Publishing WPF app ($Configuration | $Runtime | SelfContained=$SelfContained)..." -ForegroundColor Yellow
    $sc = if ($SelfContained) { 'true' } else { 'false' }
    dotnet publish $wpfCsproj -c $Configuration -r $Runtime --nologo `
        -p:PublishSingleFile=true `
        -p:IncludeNativeLibrariesForSelfExtract=true `
        -p:SelfContained=$sc

    $tfm = 'net8.0-windows'
    $publishDir = Join-Path $RepoRoot "platforms\windows\TimeWarp.Wpf\bin\$Configuration\$tfm\$Runtime\publish"
    if (Test-Path $publishDir) {
        Write-Host "✅ Publish complete." -ForegroundColor Green
        Write-Host "Publish directory: $publishDir" -ForegroundColor Green
        Get-ChildItem $publishDir | Select-Object Name, Length, LastWriteTime | Sort-Object Length -Descending | Select-Object -First 10 | Format-Table
    }
}

Write-Host "🎉 Done." -ForegroundColor Cyan
