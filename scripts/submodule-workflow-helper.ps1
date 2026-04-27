<#
.SYNOPSIS
    Manages submodule workflow operations for xAquaticRisk development.

.DESCRIPTION
    Helper script to simplify submodule operations when developing xAquaticRisk with component dependencies.
    Provides functions to create feature branches, validate branch status, and reset submodules for release.

    Key functions:
    - NewFeature: Create and check out a feature branch in a submodule
    - Validate: Verify all submodules are on development branches (not master)
    - GetStatus: Display current status of all submodules
    - ResetToRelease: Pin all submodules back to master

.PARAMETER Action
    The action to perform: NewFeature, Validate, GetStatus, ResetToRelease

.PARAMETER SubmodulePath
    (For NewFeature only) The relative path to the submodule (e.g., "model/variant/CmfContinuous")

.PARAMETER FeatureName
    (For NewFeature only) The name for the new feature branch (e.g., "parameter-updates-2.87")

.EXAMPLE
    .\submodule-workflow-helper.ps1 -Action NewFeature -SubmodulePath "model/variant/CmfContinuous" -FeatureName "parameter-updates"
    Creates and checks out a new feature branch in CmfContinuous submodule

.EXAMPLE
    .\submodule-workflow-helper.ps1 -Action Validate
    Validates all submodules are on development branches

.EXAMPLE
    .\submodule-workflow-helper.ps1 -Action GetStatus
    Displays status table of all submodules

.EXAMPLE
    .\submodule-workflow-helper.ps1 -Action ResetToRelease
    Pins all submodules to master for release preparation

.LINK
    See CONTRIBUTING.md for submodule development guidelines
#>

param(
    [Parameter(Mandatory=$true)]
    [ValidateSet('NewFeature', 'Validate', 'GetStatus', 'ResetToRelease')]
    [string]$Action,

    [Parameter(Mandatory=$false)]
    [string]$SubmodulePath,

    [Parameter(Mandatory=$false)]
    [string]$FeatureName
)

# Configuration
$RepoRoot = Split-Path -Parent $PSScriptRoot
$GitmodulesPath = Join-Path $RepoRoot ".gitmodules"

# Helper functions
function Read-Gitmodules {
    <#
    .SYNOPSIS
        Parse .gitmodules file and return array of submodule objects
    #>
    $submodules = @()
    $currentSubmodule = $null

    Get-Content $GitmodulesPath | ForEach-Object {
        $line = $_.Trim()

        if ($line -match '^\[submodule "(.+)"\]$') {
            if ($currentSubmodule) {
                $submodules += $currentSubmodule
            }
            $currentSubmodule = @{ Name = $matches[1] }
        }
        elseif ($line -match '^\s*path\s*=\s*(.+)$') {
            $currentSubmodule.Path = $matches[1]
        }
        elseif ($line -match '^\s*url\s*=\s*(.+)$') {
            $currentSubmodule.Url = $matches[1]
        }
        elseif ($line -match '^\s*branch\s*=\s*(.+)$') {
            $currentSubmodule.Branch = $matches[1]
        }
    }

    if ($currentSubmodule) {
        $submodules += $currentSubmodule
    }

    return $submodules
}

function Get-SubmoduleBranch {
    <#
    .SYNOPSIS
        Get the current branch of a submodule by querying the repository
    #>
    param([string]$SubmodulePath)

    $submoduleDir = Join-Path $RepoRoot $SubmodulePath
    if (-not (Test-Path $submoduleDir)) {
        return "NOT CLONED"
    }

    try {
        $branch = & git -C $submoduleDir rev-parse --abbrev-ref HEAD 2>$null
        if ($LASTEXITCODE -ne 0) {
            return "DETACHED"
        }
        return $branch
    }
    catch {
        return "ERROR"
    }
}

function Get-SubmoduleCommit {
    <#
    .SYNOPSIS
        Get the current commit SHA of a submodule
    #>
    param([string]$SubmodulePath)

    $submoduleDir = Join-Path $RepoRoot $SubmodulePath
    if (-not (Test-Path $submoduleDir)) {
        return "N/A"
    }

    try {
        $commit = & git -C $submoduleDir rev-parse --short HEAD 2>$null
        return $commit
    }
    catch {
        return "ERROR"
    }
}

function Invoke-NewFeatureBranch {
    <#
    .SYNOPSIS
        Create and check out a feature branch in a submodule, update .gitmodules
    #>
    param(
        [string]$SubmodulePath,
        [string]$FeatureName
    )

    # Validate parameters
    if ([string]::IsNullOrWhiteSpace($SubmodulePath) -or [string]::IsNullOrWhiteSpace($FeatureName)) {
        Write-Host "ERROR: -SubmodulePath and -FeatureName are required for NewFeature action" -ForegroundColor Red
        exit 1
    }

    $submoduleDir = Join-Path $RepoRoot $SubmodulePath
    $fullBranchName = "feature/xAquaticRisk-$FeatureName"

    # Verify submodule exists
    if (-not (Test-Path $submoduleDir)) {
        Write-Host "ERROR: Submodule path not found: $SubmodulePath" -ForegroundColor Red
        exit 1
    }

    Write-Host "Creating feature branch in $SubmodulePath..." -ForegroundColor Cyan

    # Create and check out feature branch in submodule
    try {
        & git -C $submoduleDir checkout -b $fullBranchName 2>&1 | ForEach-Object {
            if ($_ -match "already exists" -or $_ -match "Switched to a new branch") {
                Write-Host "  $_"
            }
        }

        if ($LASTEXITCODE -ne 0) {
            Write-Host "ERROR: Failed to create feature branch" -ForegroundColor Red
            exit 1
        }
    }
    catch {
        Write-Host "ERROR: Git command failed: $_" -ForegroundColor Red
        exit 1
    }

    Write-Host "  ✓ Feature branch '$fullBranchName' created and checked out" -ForegroundColor Green

    # Update .gitmodules to point to the new branch
    Write-Host "Updating .gitmodules to point to feature branch..." -ForegroundColor Cyan
    $gitmodulesContent = Get-Content $GitmodulesPath -Raw

    # Find and update the submodule config
    $pattern = "(\[submodule `"[^`"]*`"\](?:\s|\r\n)*path\s*=\s*$([regex]::Escape($SubmodulePath))(?:\s|\r\n)*url\s*=\s*[^\r\n]+)(?:(\s|\r\n)*branch\s*=\s*[^\r\n]+)?"
    $replacement = "`$1`n    branch = $fullBranchName"

    if ($gitmodulesContent -match $pattern) {
        $gitmodulesContent = $gitmodulesContent -replace $pattern, $replacement
        Set-Content $GitmodulesPath $gitmodulesContent -Encoding UTF8 -NoNewline
        Write-Host "  ✓ .gitmodules updated" -ForegroundColor Green
    }
    else {
        # If no branch entry exists, add one
        $pattern = "(\[submodule `"[^`"]*`"\](?:\s|\r\n)*path\s*=\s*$([regex]::Escape($SubmodulePath))(?:\s|\r\n)*url\s*=\s*[^\r\n]+)"
        $replacement = "`$1`n    branch = $fullBranchName"
        $gitmodulesContent = $gitmodulesContent -replace $pattern, $replacement
        Set-Content $GitmodulesPath $gitmodulesContent -Encoding UTF8 -NoNewline
        Write-Host "  ✓ .gitmodules updated (branch entry added)" -ForegroundColor Green
    }

    Write-Host ""
    Write-Host "SUCCESS: Feature branch workflow initialized" -ForegroundColor Green
    Write-Host "  Submodule: $SubmodulePath" -ForegroundColor Green
    Write-Host "  Branch: $fullBranchName" -ForegroundColor Green
    Write-Host ""
    Write-Host "Next steps:" -ForegroundColor Yellow
    Write-Host "  1. Stage .gitmodules changes: git add .gitmodules"
    Write-Host "  2. Commit: git commit -m 'Start development on $FeatureName'"
    Write-Host "  3. Make changes in the submodule"
    Write-Host "  4. Validate branches: .\scripts\submodule-workflow-helper.ps1 -Action Validate"
}

function Invoke-ValidateSubmodules {
    <#
    .SYNOPSIS
        Validate that all submodules are on development branches (not master)
    #>
    $submodules = Read-Gitmodules
    $violations = @()
    $compliant = @()

    Write-Host "Validating submodule branches..." -ForegroundColor Cyan
    Write-Host ""

    foreach ($submodule in $submodules) {
        $branch = Get-SubmoduleBranch $submodule.Path
        $commit = Get-SubmoduleCommit $submodule.Path
        $isMaster = $branch -eq "master" -or $branch -eq "main"

        if ($isMaster) {
            $violations += @{
                Path = $submodule.Path
                Branch = $branch
                Commit = $commit
                Issue = "Pointing to master branch"
            }
            Write-Host "  ✗ $($submodule.Path)" -ForegroundColor Red
            Write-Host "    Branch: $branch (⚠ should be on feature/dev branch)" -ForegroundColor Red
        }
        else {
            $compliant += @{
                Path = $submodule.Path
                Branch = $branch
            }
            Write-Host "  ✓ $($submodule.Path)" -ForegroundColor Green
            Write-Host "    Branch: $branch" -ForegroundColor Green
        }
    }

    Write-Host ""
    if ($violations.Count -gt 0) {
        Write-Host "VALIDATION FAILED: $($violations.Count) submodule(s) on master branch" -ForegroundColor Red
        Write-Host ""
        Write-Host "Action required:" -ForegroundColor Yellow
        foreach ($v in $violations) {
            Write-Host "  - $($v.Path): Create feature branch or use helper script" -ForegroundColor Yellow
        }
        exit 1
    }
    else {
        Write-Host "VALIDATION PASSED: All submodules on development branches" -ForegroundColor Green
        exit 0
    }
}

function Invoke-GetStatus {
    <#
    .SYNOPSIS
        Display status table of all submodules
    #>
    $submodules = Read-Gitmodules
    $statusTable = @()

    foreach ($submodule in $submodules) {
        $branch = Get-SubmoduleBranch $submodule.Path
        $commit = Get-SubmoduleCommit $submodule.Path
        $isMaster = $branch -eq "master" -or $branch -eq "main"
        $status = if ($isMaster) { "⚠ MASTER" } else { "✓ DEV" }

        $statusTable += [PSCustomObject]@{
            Path = $submodule.Path
            Branch = $branch
            Commit = $commit
            Status = $status
        }
    }

    Write-Host ""
    Write-Host "xAquaticRisk Submodule Status:" -ForegroundColor Cyan
    Write-Host ""
    $statusTable | Format-Table -AutoSize -Property Path, Branch, Commit, Status
    Write-Host ""
}

function Invoke-ResetToRelease {
    <#
    .SYNOPSIS
        Pin all submodules to master (for release preparation)
    #>
    Write-Host "Preparing for release: Resetting submodules to master..." -ForegroundColor Cyan
    Write-Host ""

    $submodules = Read-Gitmodules
    $updated = 0

    foreach ($submodule in $submodules) {
        $submoduleDir = Join-Path $RepoRoot $submodule.Path
        if (-not (Test-Path $submoduleDir)) {
            continue
        }

        $branch = Get-SubmoduleBranch $submodule.Path
        if ($branch -ne "master" -and $branch -ne "main") {
            Write-Host "  Checking out master in $($submodule.Path)..." -ForegroundColor Cyan
            try {
                & git -C $submoduleDir checkout master 2>&1 | Out-Null
                & git -C $submoduleDir pull origin master 2>&1 | Out-Null
                Write-Host "    ✓ Switched to master" -ForegroundColor Green
                $updated++
            }
            catch {
                Write-Host "    ✗ Failed: $_" -ForegroundColor Red
            }
        }
    }

    Write-Host ""
    Write-Host "Release preparation complete: $updated submodule(s) updated" -ForegroundColor Green
    Write-Host ""
    Write-Host "Next steps:" -ForegroundColor Yellow
    Write-Host "  1. Review .gitmodules to confirm all submodules point to master"
    Write-Host "  2. Stage and commit: git add .gitmodules && git commit -m 'Release preparation: pin submodules to master'"
    Write-Host "  3. Create release PR"
    Write-Host "  4. Verify CI validation passes"
}

# Main execution
try {
    switch ($Action) {
        'NewFeature' {
            Invoke-NewFeatureBranch -SubmodulePath $SubmodulePath -FeatureName $FeatureName
        }
        'Validate' {
            Invoke-ValidateSubmodules
        }
        'GetStatus' {
            Invoke-GetStatus
        }
        'ResetToRelease' {
            Invoke-ResetToRelease
        }
    }
}
catch {
    Write-Host "ERROR: An unexpected error occurred: $_" -ForegroundColor Red
    exit 1
}
