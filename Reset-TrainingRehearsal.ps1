param(
    [string]$PrepRoot = "C:\TrainingPrep",
    [string]$Branch = "feature/usability",
    [string]$RepoUrl = "https://github.com/xlandscape/xAquaticRisk.git",
    [switch]$SkipZip,
    [switch]$LaunchSandbox,
    [string]$SandboxProfile = "C:\TrainingPrep\04-profiles\xAquaticRisk-sandbox.wsb"
)

$scriptPath = Join-Path $PSScriptRoot "scripts\training\Reset-TrainingRehearsal.ps1"
if (!(Test-Path $scriptPath)) {
    throw "Training reset script not found: $scriptPath"
}

& $scriptPath -PrepRoot $PrepRoot -Branch $Branch -RepoUrl $RepoUrl -SkipZip:$SkipZip -LaunchSandbox:$LaunchSandbox -SandboxProfile $SandboxProfile
