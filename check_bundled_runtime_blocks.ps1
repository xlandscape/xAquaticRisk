param(
    [string]$ProjectRoot = $PSScriptRoot
)

$ErrorActionPreference = "Stop"

$ProjectRoot = [string]$ProjectRoot
$ProjectRoot = $ProjectRoot.Trim()
$ProjectRoot = $ProjectRoot.Trim('"')
if ([string]::IsNullOrWhiteSpace($ProjectRoot)) {
    $ProjectRoot = $PSScriptRoot
}
try {
    $ProjectRoot = (Resolve-Path -LiteralPath $ProjectRoot).Path
}
catch {
    Write-Host "ERROR: Invalid project root path: $ProjectRoot" -ForegroundColor Red
    exit 4
}

function Test-BlockedFiles {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )
    if (-not (Test-Path -LiteralPath $Path)) {
        return @("__MISSING__::$Path")
    }
    $blocked = New-Object System.Collections.Generic.List[string]
    Get-ChildItem -LiteralPath $Path -Recurse -File | ForEach-Object {
        $filePath = $_.FullName
        try {
            Get-Item -LiteralPath $filePath -Stream Zone.Identifier -ErrorAction Stop | Out-Null
            $blocked.Add($filePath) | Out-Null
        }
        catch {
        }
    }
    return $blocked
}

$runtimePaths = @(
    (Join-Path $ProjectRoot "model\core\bin\python-3.9.7-amd64"),
    (Join-Path $ProjectRoot "controlpanel\python"),
    (Join-Path $ProjectRoot "analysis\python")
)

$allBlocked = New-Object System.Collections.Generic.List[string]
$missing = New-Object System.Collections.Generic.List[string]

foreach ($runtimePath in $runtimePaths) {
    $result = Test-BlockedFiles -Path $runtimePath
    foreach ($entry in $result) {
        if ($entry.StartsWith("__MISSING__::")) {
            $missing.Add($entry.Substring(12)) | Out-Null
        }
        else {
            $allBlocked.Add($entry) | Out-Null
        }
    }
}

if ($missing.Count -gt 0) {
    Write-Host "ERROR: Missing bundled runtime folders:" -ForegroundColor Red
    $missing | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
    Write-Host ""
    Write-Host "Run maintainer setup utilities to rebuild bundled runtimes:" -ForegroundColor Yellow
    Write-Host "  .\setup_all_runtimes.bat"
    exit 2
}

if ($allBlocked.Count -gt 0) {
    Write-Host "ERROR: Blocked runtime binaries detected (Zone.Identifier found)." -ForegroundColor Red
    Write-Host "The model may fail with WinError 4551 until these files are unblocked." -ForegroundColor Red
    Write-Host ""
    Write-Host "First blocked files:" -ForegroundColor Yellow
    $allBlocked | Select-Object -First 20 | ForEach-Object { Write-Host "  $_" }
    if ($allBlocked.Count -gt 20) {
        Write-Host "  ... and $($allBlocked.Count - 20) more"
    }
    Write-Host ""
    Write-Host "Fix command (PowerShell):" -ForegroundColor Yellow
    Write-Host "  Get-ChildItem -Path '$ProjectRoot\\model\\core\\bin\\python-3.9.7-amd64' -Recurse -File | Unblock-File"
    Write-Host "  Get-ChildItem -Path '$ProjectRoot\\controlpanel\\python' -Recurse -File | Unblock-File"
    Write-Host "  Get-ChildItem -Path '$ProjectRoot\\analysis\\python' -Recurse -File | Unblock-File"
    exit 3
}

Write-Host "OK: Bundled runtimes are present and no blocked binaries were detected."
exit 0
