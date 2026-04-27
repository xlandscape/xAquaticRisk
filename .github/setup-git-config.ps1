<#
.SYNOPSIS
    Optional setup script to configure local git safeguards for xAquaticRisk development.

.DESCRIPTION
    This script configures local pre-push hooks in each submodule to warn developers before
    pushing to master/main branches. This is an optional safety measure; the primary protection
    is the CI validation workflow.

    Safeguards added:
    - Pre-push hook warning if attempting to push to master/main in submodules
    - Git config reminder to use feature branches

.PARAMETER Install
    If $true, installs the safeguards. If $false, uninstalls them.

.EXAMPLE
    .\setup-git-config.ps1 -Install $true
    Installs git safeguards in all submodules

.EXAMPLE
    .\setup-git-config.ps1 -Install $false
    Removes git safeguards from all submodules

.LINK
    See CONTRIBUTING.md for submodule development guidelines
#>

param(
    [Parameter(Mandatory=$true)]
    [bool]$Install
)

# Configuration
$RepoRoot = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$GitmodulesPath = Join-Path $RepoRoot ".gitmodules"

# Pre-push hook script content
$PrePushHookScript = @'
#!/bin/bash
# Pre-push hook: warn before pushing to master/main

protected_branch='master|main'
current_branch=$(git rev-parse --abbrev-ref HEAD)

if [[ $current_branch =~ ^($protected_branch)$ ]]; then
    echo "⚠️  WARNING: You are about to push to '$current_branch' branch"
    echo ""
    echo "For xAquaticRisk development, please use feature branches:"
    echo "  - Ask your deployment team to create a feature branch, OR"
    echo "  - Use: git checkout -b feature/xAquaticRisk-your-feature-name"
    echo ""
    read -p "Are you sure you want to push to $current_branch? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Push cancelled"
        exit 1
    fi
fi

exit 0
'@

function Get-Submodules {
    <#
    .SYNOPSIS
        Parse .gitmodules and return list of submodule paths
    #>
    $submodules = @()
    $currentSubmodule = $null

    Get-Content $GitmodulesPath | ForEach-Object {
        $line = $_.Trim()
        if ($line -match '^\[submodule') {
            $currentSubmodule = @{}
        }
        elseif ($line -match '^\s*path\s*=\s*(.+)$') {
            $currentSubmodule.Path = $matches[1]
            $submodules += $currentSubmodule
        }
    }

    return $submodules
}

function Install-Safeguards {
    <#
    .SYNOPSIS
        Install git hooks in all submodules
    #>
    Write-Host "Installing git safeguards in submodules..." -ForegroundColor Cyan
    Write-Host ""

    $submodules = Get-Submodules
    $installed = 0

    foreach ($submodule in $submodules) {
        $submoduleDir = Join-Path $RepoRoot $submodule.Path
        $hookDir = Join-Path $submoduleDir ".git\hooks"

        if (-not (Test-Path $submoduleDir)) {
            Write-Host "  ⊘ $($submodule.Path): not cloned (skipped)" -ForegroundColor Yellow
            continue
        }

        if (-not (Test-Path $hookDir)) {
            Write-Host "  ⊘ $($submodule.Path): no .git/hooks directory (skipped)" -ForegroundColor Yellow
            continue
        }

        # Windows pre-push hook as batch file
        $hookPath = Join-Path $hookDir "pre-push"
        $hookBatPath = Join-Path $hookDir "pre-push.bat"

        # Create bash script (for Windows Git Bash)
        Set-Content $hookPath $PrePushHookScript -Encoding ASCII -NoNewline
        Add-Content $hookPath "`n" -Encoding ASCII

        # Also create batch script for PowerShell/CMD users
        $prePushBat = @"
@echo off
REM Pre-push hook warning for Windows cmd/PowerShell

for /f %%i in ('git rev-parse --abbrev-ref HEAD') do set current_branch=%%i

if "%current_branch%"=="master" (
    echo.
    echo ^!^!^! WARNING: You are about to push to master branch ^!^!^!
    echo.
    echo For xAquaticRisk development, please use feature branches:
    echo   Use: git checkout -b feature/xAquaticRisk-your-feature-name
    echo.
    set /p response="Type 'yes' to continue, or press Enter to cancel: "
    if not "%response%"=="yes" (
        echo Push cancelled
        exit /b 1
    )
)

exit /b 0
"@

        Set-Content $hookBatPath $prePushBat -Encoding ASCII

        # Make bash script executable (if in Git Bash context)
        Write-Host "  ✓ $($submodule.Path): safeguard installed" -ForegroundColor Green
        $installed++
    }

    Write-Host ""
    Write-Host "Installed safeguards in $installed submodule(s)" -ForegroundColor Green
    Write-Host ""
    Write-Host "Safeguards will warn before pushing to master/main branches." -ForegroundColor Green
    Write-Host "To bypass: Answer 'yes' when prompted (not recommended)." -ForegroundColor Yellow
}

function Uninstall-Safeguards {
    <#
    .SYNOPSIS
        Uninstall git hooks from all submodules
    #>
    Write-Host "Removing git safeguards from submodules..." -ForegroundColor Cyan
    Write-Host ""

    $submodules = Get-Submodules
    $removed = 0

    foreach ($submodule in $submodules) {
        $submoduleDir = Join-Path $RepoRoot $submodule.Path
        $hookDir = Join-Path $submoduleDir ".git\hooks"

        if (-not (Test-Path $submoduleDir)) {
            continue
        }

        $hookPath = Join-Path $hookDir "pre-push"
        $hookBatPath = Join-Path $hookDir "pre-push.bat"

        if (Test-Path $hookPath) {
            Remove-Item $hookPath -Force -ErrorAction SilentlyContinue
            Write-Host "  ✓ $($submodule.Path): hook removed" -ForegroundColor Green
            $removed++
        }

        if (Test-Path $hookBatPath) {
            Remove-Item $hookBatPath -Force -ErrorAction SilentlyContinue
        }
    }

    Write-Host ""
    Write-Host "Removed safeguards from $removed submodule(s)" -ForegroundColor Green
}

# Main execution
try {
    if ($Install) {
        Install-Safeguards
    }
    else {
        Uninstall-Safeguards
    }
}
catch {
    Write-Host "ERROR: $_" -ForegroundColor Red
    exit 1
}
