# Installer test: builds, installs silently, verifies files/registry/shortcuts, then uninstalls.

$dist = Join-Path $PSScriptRoot "..\dist"
Write-Host "Looking for installer in: $dist"
$installer = Get-ChildItem -Path $dist -Filter '*.exe' | Select-Object -First 1
if (-not $installer) {
    Write-Error "Installer not found in $dist"
    exit 2
}
$installerPath = $installer.FullName
Write-Host "Found installer: $installerPath"

# Run silent install (/S is standard NSIS silent switch)
Write-Host "Installing silently..."
$proc = Start-Process -FilePath $installerPath -ArgumentList '/S' -PassThru -Wait
Write-Host "Installer exit code: $($proc.ExitCode)"

# Wait briefly for install to settle
Start-Sleep -Seconds 3

# Check installed location
$possible = @(
    "$env:ProgramFiles\TimeWarp\TimeWarpIDE.exe",
    "$env:ProgramFiles\TimeWarpIDE\TimeWarpIDE.exe",
    "$env:ProgramFiles\Time Warp Studio\TimeWarpIDE.exe",
    "$env:ProgramFiles\TimeWarpStudio\TimeWarpIDE.exe"
)
$found = $false
foreach ($p in $possible) {
    if (Test-Path $p) { Write-Host "Installed exe found at: $p"; $found = $true; $installedExe = $p; break }
}
if (-not $found) {
    Write-Error "Installed EXE not found in expected locations. Checking registry..."
    # Check Uninstall registry key
    $regKey = 'HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Time Warp IDE'
    if (-not (Test-Path $regKey)) {
        Write-Error "Uninstall registry key not found: $regKey"
        exit 3
    } else {
        Write-Host "Found registry uninstall key: $regKey"
        # try to read InstallLocation or UninstallString
        $uninstallString = (Get-ItemProperty $regKey -ErrorAction SilentlyContinue).UninstallString
        Write-Host "UninstallString: $uninstallString"
    }
}

# Start the installed exe to make sure it launches
if ($installedExe) {
    Write-Host "Launching installed exe for smoke test..."
    $p = Start-Process -FilePath $installedExe -PassThru
    Start-Sleep -Seconds 3
    if (!$p.HasExited) { Write-Host "Process started (PID: $($p.Id)), terminating"; Stop-Process -Id $p.Id -Force }
} else {
    Write-Host "No installed EXE path captured - skipping launch test"
}

# Check Start Menu shortcuts
$startMenuPaths = @(
    "$env:ProgramData\Microsoft\Windows\Start Menu\Programs\Time Warp IDE",
    "$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Time Warp IDE",
    "$env:ProgramData\Microsoft\Windows\Start Menu\Programs\Time Warp Studio",
    "$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Time Warp Studio"
)
$shortcutFound = $false
foreach ($s in $startMenuPaths) {
    if (Test-Path $s) { Write-Host "Found start menu folder: $s"; $shortcutFound = $true }
}
if (-not $shortcutFound) { Write-Warning "Start menu shortcuts not found in common locations" }

# Now run uninstaller if we can find the uninstall executable
$uninstallExe = $null
if ($installedExe) {
    $installDir = Split-Path -Parent $installedExe
    $possibleUn = Join-Path $installDir 'Uninstall.exe'
    if (Test-Path $possibleUn) { $uninstallExe = $possibleUn }
}

if (-not $uninstallExe) {
    # try registry UninstallString
    $regKey = 'HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Time Warp IDE'
    if (Test-Path $regKey) {
        $uStr = (Get-ItemProperty $regKey -ErrorAction SilentlyContinue).UninstallString
        if ($uStr) { $uninstallExe = $uStr -replace '"','' }
    }
}

if ($uninstallExe -and Test-Path $uninstallExe) {
    Write-Host "Running uninstaller: $uninstallExe (silent)
"
    Start-Process -FilePath $uninstallExe -ArgumentList '/S' -Wait -PassThru
    Start-Sleep -Seconds 2
    Write-Host "Uninstall attempted"
} else {
    Write-Warning "Uninstaller not found, manual cleanup may be required"
}

Write-Host "Installer test completed"
exit 0
