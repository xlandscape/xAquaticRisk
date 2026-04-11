param(
    [string]$TargetRef = "HEAD",
    [switch]$StrictRuntime = $true
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Add-Result {
    param(
        [string]$Name,
        [ValidateSet("PASS", "WARN", "FAIL")]
        [string]$Status,
        [string]$Details,
        [string]$Remediation = ""
    )
    $script:Results += [pscustomobject]@{
        Name = $Name
        Status = $Status
        Details = $Details
        Remediation = $Remediation
    }
}

function Test-PathAny {
    param([string[]]$Candidates)
    foreach ($candidate in $Candidates) {
        if (Test-Path -LiteralPath $candidate) {
            return $candidate
        }
    }
    return $null
}

$Results = @()
$SubmoduleRows = @()

try {
    $repoRoot = (git rev-parse --show-toplevel).Trim()
    if (-not $repoRoot) {
        throw "Not a git repository."
    }
    Set-Location -LiteralPath $repoRoot
} catch {
    Write-Error "Cannot determine repository root: $($_.Exception.Message)"
    exit 2
}

$headRef = (git rev-parse --abbrev-ref HEAD).Trim()
Add-Result -Name "git.repository" -Status "PASS" -Details "Repository root: $repoRoot (HEAD: $headRef)"

$requiredFiles = @(
    "__start__.bat",
    "controlpanel.bat",
    "setup_controlpanel_python.bat",
    "setup_analysis_python.bat",
    ".gitmodules"
)

$optionalFiles = @(
    "dashboard.bat"
)

$missingFiles = @()
foreach ($rel in $requiredFiles) {
    if (-not (Test-Path -LiteralPath (Join-Path $repoRoot $rel))) {
        $missingFiles += $rel
    }
}
if ($missingFiles.Count -gt 0) {
    Add-Result -Name "repo.required-files" -Status "FAIL" -Details ("Missing files: " + ($missingFiles -join ", ")) -Remediation "Restore missing launcher/setup files in repository."
} else {
    Add-Result -Name "repo.required-files" -Status "PASS" -Details "All required launcher/setup files are present."
}

$missingOptionalFiles = @()
foreach ($rel in $optionalFiles) {
    if (-not (Test-Path -LiteralPath (Join-Path $repoRoot $rel))) {
        $missingOptionalFiles += $rel
    }
}
if ($missingOptionalFiles.Count -gt 0) {
    Add-Result -Name "repo.optional-files" -Status "WARN" -Details ("Missing optional launchers: " + ($missingOptionalFiles -join ", ")) -Remediation "If these tools are expected in this release, restore them."
} else {
    Add-Result -Name "repo.optional-files" -Status "PASS" -Details "Optional launcher files are present."
}

$startBatPath = Join-Path $repoRoot "__start__.bat"
if (Test-Path -LiteralPath $startBatPath) {
    $startBat = Get-Content -LiteralPath $startBatPath -Raw
    $usesLocalRuntime = $startBat -match "python-.*-amd64\\python\.exe" -and $startBat -match "%~dp0"
    $usesBarePython = $startBat -match "(^|\s)python(\.exe)?(\s|$)"

    if ($usesLocalRuntime -and -not $usesBarePython) {
        Add-Result -Name "launcher.__start__.bat" -Status "PASS" -Details "Starter uses repo-local runtime path."
    } else {
        Add-Result -Name "launcher.__start__.bat" -Status "FAIL" -Details "Starter may depend on system Python or non-portable path." -Remediation "Use a %~dp0-rooted path to model/core/bin/python-*-amd64/python.exe."
    }
}

$runtimeCandidates = [ordered]@{
    "runtime.model-core" = @(
        (Join-Path $repoRoot "model/core/bin/python-3.9.7-amd64/python.exe"),
        (Join-Path $repoRoot "model/core/bin/python.exe")
    )
    "runtime.controlpanel" = @((Join-Path $repoRoot "controlpanel/python/python.exe"))
    "runtime.analysis" = @((Join-Path $repoRoot "analysis/python/python.exe"))
}

foreach ($key in $runtimeCandidates.Keys) {
    $found = Test-PathAny -Candidates $runtimeCandidates[$key]
    if ($found) {
        Add-Result -Name $key -Status "PASS" -Details "Found: $found"
    } else {
        if ($StrictRuntime) {
            Add-Result -Name $key -Status "FAIL" -Details "Missing runtime executable." -Remediation "Run corresponding setup_*_python.bat and include runtime folder for distribution."
        } else {
            Add-Result -Name $key -Status "WARN" -Details "Runtime executable missing." -Remediation "Run corresponding setup_*_python.bat before packaging/copying."
        }
    }
}

$gitmodulesPath = Join-Path $repoRoot ".gitmodules"
if (-not (Test-Path -LiteralPath $gitmodulesPath)) {
    Add-Result -Name "submodules.gitmodules" -Status "FAIL" -Details ".gitmodules not found." -Remediation "Restore .gitmodules and ensure submodules are tracked."
} else {
    Add-Result -Name "submodules.gitmodules" -Status "PASS" -Details ".gitmodules present."
}

$declaredSubmodules = @{}
$currentPath = ""
Get-Content -LiteralPath $gitmodulesPath | ForEach-Object {
    $line = $_.Trim()
    if ($line -match '^\[submodule\s+"(.+)"\]$') {
        $currentPath = ""
    } elseif ($line -match '^path\s*=\s*(.+)$') {
        $currentPath = $matches[1].Trim()
        $declaredSubmodules[$currentPath] = [pscustomobject]@{
            Path = $currentPath
        }
    }
}

if ($declaredSubmodules.Count -eq 0) {
    Add-Result -Name "submodules.declared" -Status "FAIL" -Details "No submodules parsed from .gitmodules." -Remediation "Fix .gitmodules syntax and path entries."
} else {
    Add-Result -Name "submodules.declared" -Status "PASS" -Details "Declared submodules: $($declaredSubmodules.Count)"
}

$pinnedByPath = @{}
$lsTreeLines = @(git ls-tree -r $TargetRef)
foreach ($line in $lsTreeLines) {
    if ($line -match '^160000\s+commit\s+([0-9a-f]{40})\t(.+)$') {
        $pinnedByPath[$matches[2]] = $matches[1]
    }
}

if ($pinnedByPath.Count -eq 0) {
    Add-Result -Name "submodules.pinned" -Status "FAIL" -Details "No pinned submodule commits found in ref '$TargetRef'." -Remediation "Verify TargetRef and that submodules are committed in that ref."
} else {
    Add-Result -Name "submodules.pinned" -Status "PASS" -Details "Pinned submodule commits in '$TargetRef': $($pinnedByPath.Count)"
}

$statusByPath = @{}
$statusLines = @(git submodule status --recursive)
foreach ($line in $statusLines) {
    if ($line -match '^(.)([0-9a-f]{40})\s+([^\s]+)(?:\s+\((.+)\))?$') {
        $prefix = $matches[1]
        $sha = $matches[2]
        $path = $matches[3]
        $extra = $matches[4]
        $statusByPath[$path] = [pscustomobject]@{
            Prefix = $prefix
            Sha = $sha
            Extra = $extra
        }
    }
}

foreach ($subPath in $declaredSubmodules.Keys | Sort-Object) {
    $pinned = $pinnedByPath[$subPath]
    $state = "ok"
    $checkedOut = ""
    $status = "PASS"
    $details = ""
    $remediation = ""

    if (-not $pinned) {
        $status = "FAIL"
        $state = "missing-pinned"
        $details = "Submodule declared but not pinned in ref '$TargetRef': $subPath"
        $remediation = "Commit submodule pointer in target ref before release."
    } elseif (-not $statusByPath.ContainsKey($subPath)) {
        $status = "FAIL"
        $state = "not-initialized"
        $details = "Submodule not initialized: $subPath"
        $remediation = "Run: git submodule update --init --recursive"
    } else {
        $entry = $statusByPath[$subPath]
        $checkedOut = $entry.Sha
        if ($entry.Prefix -eq "-") {
            $status = "FAIL"
            $state = "not-initialized"
            $details = "Submodule not initialized: $subPath"
            $remediation = "Run: git submodule update --init --recursive"
        } elseif ($entry.Prefix -eq "+") {
            $status = "FAIL"
            $state = "diverged"
            $details = "Submodule checkout differs from pinned commit: $subPath"
            $remediation = "Run: git submodule update --init --recursive or intentionally repin and commit."
        } elseif ($entry.Prefix -eq "U") {
            $status = "FAIL"
            $state = "conflict"
            $details = "Submodule merge conflict: $subPath"
            $remediation = "Resolve submodule conflict and commit resolved pointer."
        } elseif ($checkedOut -ne $pinned) {
            $status = "FAIL"
            $state = "mismatch"
            $details = "Submodule SHA mismatch for $subPath (pinned=$pinned checked-out=$checkedOut)"
            $remediation = "Reset/update submodule to pinned commit for target ref."
        } else {
            $details = "Submodule matches pinned commit."
        }
    }

    Add-Result -Name ("submodule." + $subPath) -Status $status -Details $details -Remediation $remediation
    $SubmoduleRows += [pscustomobject]@{
        Path = $subPath
        Pinned = $pinned
        CheckedOut = $checkedOut
        State = $state
        Result = $status
    }
}

$passCount = @($Results | Where-Object { $_.Status -eq "PASS" }).Count
$warnCount = @($Results | Where-Object { $_.Status -eq "WARN" }).Count
$failCount = @($Results | Where-Object { $_.Status -eq "FAIL" }).Count

Write-Host ""
Write-Host "xcopy-readiness summary" -ForegroundColor Cyan
Write-Host "TargetRef: $TargetRef"
Write-Host "PASS: $passCount  WARN: $warnCount  FAIL: $failCount"
Write-Host ""

$Results | Sort-Object Status, Name | Format-Table -AutoSize

if ($SubmoduleRows.Count -gt 0) {
    Write-Host ""
    Write-Host "Submodule pointer matrix" -ForegroundColor Cyan
    $SubmoduleRows | Sort-Object Path | Format-Table -AutoSize
}

if ($failCount -gt 0) {
    Write-Host ""
    Write-Host "Remediation" -ForegroundColor Yellow
    $Results |
        Where-Object { $_.Status -eq "FAIL" -and $_.Remediation } |
        Select-Object Name, Remediation |
        Format-Table -AutoSize
    exit 1
}

exit 0
