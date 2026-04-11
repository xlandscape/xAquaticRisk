param(
    [string]$PrepRoot = "C:\TrainingPrep",
    [string]$Branch = "feature/usability",
    [string]$RepoUrl = "https://github.com/xlandscape/xAquaticRisk.git",
    [switch]$SkipZip,
    [switch]$LaunchSandbox,
    [string]$SandboxProfile = "C:\TrainingPrep\04-profiles\xAquaticRisk-sandbox.wsb"
)

$ErrorActionPreference = "Stop"

function Write-Step([string]$message) {
    Write-Host "[xAquaticRisk training prep] $message"
}

$cleanClone = Join-Path $PrepRoot "01-clean-clone\xAquaticRisk"
$runInput = Join-Path $PrepRoot "02-run-input\xAquaticRisk"
$packageDir = Join-Path $PrepRoot "03-packages"
$logDir = Join-Path $PrepRoot "05-logs"

Write-Step "Ensuring staging folder layout exists"
New-Item -ItemType Directory -Force -Path (Join-Path $PrepRoot "01-clean-clone") | Out-Null
New-Item -ItemType Directory -Force -Path (Join-Path $PrepRoot "02-run-input") | Out-Null
New-Item -ItemType Directory -Force -Path $packageDir | Out-Null
New-Item -ItemType Directory -Force -Path $logDir | Out-Null

if (!(Test-Path (Join-Path $cleanClone ".git"))) {
    Write-Step "Cloning clean repository from $RepoUrl ($Branch)"
    git clone --recursive --branch $Branch $RepoUrl $cleanClone
}
else {
    Write-Step "Refreshing existing clean clone to origin/$Branch"
    git -C $cleanClone fetch --all --tags
    git -C $cleanClone checkout $Branch
    git -C $cleanClone reset --hard ("origin/" + $Branch)
    git -C $cleanClone submodule sync --recursive
    git -C $cleanClone submodule update --init --recursive
}

Write-Step "Resetting run-input folder"
if (Test-Path $runInput) {
    Remove-Item $runInput -Recurse -Force
}
New-Item -ItemType Directory -Force -Path $runInput | Out-Null

Write-Step "Mirroring clean clone into run-input"
$null = robocopy $cleanClone $runInput /MIR /XD .git .venv run analysis_output /XF *.log /R:1 /W:1
if ($LASTEXITCODE -ge 8) {
    throw "robocopy failed with exit code $LASTEXITCODE"
}

$zipPath = $null
if (!$SkipZip) {
    $stamp = Get-Date -Format "yyyyMMdd-HHmm"
    $zipPath = Join-Path $packageDir ("xAquaticRisk-" + $stamp + ".zip")
    Write-Step "Creating zip artifact: $zipPath"
    if (Test-Path $zipPath) {
        Remove-Item $zipPath -Force
    }
    Compress-Archive -Path (Join-Path $runInput "*") -DestinationPath $zipPath -Force
}

if ($LaunchSandbox) {
    if (!(Test-Path $SandboxProfile)) {
        throw "Sandbox profile not found: $SandboxProfile"
    }
    Write-Step "Launching Windows Sandbox profile: $SandboxProfile"
    Start-Process $SandboxProfile
}

$summary = [ordered]@{
    prep_root = $PrepRoot
    repo_url = $RepoUrl
    branch = $Branch
    clean_clone = $cleanClone
    run_input = $runInput
    zip_created = [bool](!$SkipZip)
    zip_path = $zipPath
    sandbox_launched = [bool]$LaunchSandbox
    timestamp = (Get-Date).ToString("s")
}

$logPath = Join-Path $logDir ("reset-" + (Get-Date -Format "yyyyMMdd-HHmmss") + ".json")
$summary | ConvertTo-Json -Depth 4 | Set-Content -Path $logPath -Encoding UTF8

Write-Step "Reset complete"
$summary | ConvertTo-Json -Depth 4
Write-Step "Log written to $logPath"
