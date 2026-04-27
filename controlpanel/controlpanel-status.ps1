#Requires -Version 5.1
<#
.SYNOPSIS
Check and manage controlpanel server lock status

.DESCRIPTION
Query the controlpanel server status via HTTP endpoint, or manage the lock file directly.
Works with a running server (via HTTP) or a stopped server (via lock file).

.EXAMPLES
  .\controlpanel-status.ps1                    # Show status
  .\controlpanel-status.ps1 -Clean             # Clean stale locks
  .\controlpanel-status.ps1 -Kill 8090         # Kill server on port 8090
  .\controlpanel-status.ps1 -All               # Status + Clean
  .\controlpanel-status.ps1 -Port 8090         # Check specific port
#>
param(
    [switch]$Status,
    [int]$Kill=0,
    [switch]$Clean,
    [switch]$All,
    [int]$Port=8090
)

$lockFile = Join-Path (Split-Path $MyInvocation.MyCommand.Path) "server.instance.lock"

function Get-StatusViaHTTP($port) {
    try {
        $response = @(Invoke-WebRequest -Uri "http://localhost:$port/api/controlpanel/status" -UseBasicParsing -ErrorAction Stop)
        return $response[0].Content | ConvertFrom-Json
    } catch {
        return $null
    }
}

function Get-StatusFromLock {
    if (-not (Test-Path $lockFile)) { return $null }
    try {
        $lock = Get-Content $lockFile -Raw | ConvertFrom-Json
        $procId = [int]$lock.pid
        $alive = $false
        try { 
            $nil = Get-Process -Id $procId -ErrorAction Stop
            $alive = $true 
        } catch { }
        return @{
            status = if($alive) { "running" } else { "zombie" }
            pid = $procId
            port = $lock.port
            started_at = $lock.started_at
            uptime_seconds = [int]((Get-Date) - [datetime]::UnixEpoch.AddSeconds($lock.started_at)).TotalSeconds
            alive = $alive
            lock_file = $lockFile
        }
    } catch {
        return $null
    }
}

function Show-Status {
    # Try HTTP first (if server is running)
    $info = Get-StatusViaHTTP($Port)
    if ($null -eq $info) {
        # Fall back to lock file
        $info = Get-StatusFromLock
    }
    
    if ($null -eq $info) {
        Write-Host "No server running" -ForegroundColor Green
        return
    }
    
    Write-Host ""
    $statusText = if($info.alive -eq $true) { "RUNNING" } else { "ZOMBIE" }
    $statusColor = if($info.alive -eq $true) { "Green" } else { "Yellow" }
    Write-Host "Status: $statusText" -ForegroundColor $statusColor
    Write-Host "PID: $($info.pid) | Port: $($info.port)"
    
    if ($info.uptime_seconds -ne $null) {
        $uptime = [timespan]::FromSeconds([int]$info.uptime_seconds)
        Write-Host "Uptime: $($uptime.Hours)h $($uptime.Minutes)m $($uptime.Seconds)s"
    }
    
    if (-not ($info.alive -eq $true)) {
        Write-Host "WARNING: Stale lock" -ForegroundColor Yellow
    }
    Write-Host ""
}

function Remove-StaleLocks {
    $info = Get-StatusFromLock
    if ($null -eq $info) {
        Write-Host "No lock file found" -ForegroundColor Green
        return
    }
    
    if (-not $info.alive) {
        try {
            Remove-Item $lockFile -Force
            Write-Host "Removed stale lock for PID $($info.pid)" -ForegroundColor Green
        } catch {
            Write-Host "Failed to remove lock: $_" -ForegroundColor Red
        }
    } else {
        Write-Host "Server running on port $($info.port) (PID $($info.pid))" -ForegroundColor Cyan
        Write-Host "Use -Kill $($info.port) to stop it" -ForegroundColor Cyan
    }
}

function Stop-ControlPanelServer($port) {
    $info = Get-StatusFromLock
    if ($null -eq $info) {
        Write-Host "No lock file found" -ForegroundColor Yellow
        return
    }
    if ($info.port -ne $port) {
        Write-Host "Lock shows port $($info.port), not $port" -ForegroundColor Yellow
        return
    }
    if (-not $info.alive) {
        Write-Host "Process already dead, removing lock" -ForegroundColor Yellow
        Get-Item $lockFile -Force -ErrorAction SilentlyContinue | Remove-Item -Force
        return
    }
    
    try {
        Stop-Process -Id $info.pid -Force
        Start-Sleep -Milliseconds 500
        if (Test-Path $lockFile) {
            Remove-Item $lockFile -Force -ErrorAction SilentlyContinue
        }
        Write-Host "Stopped PID $($info.pid)" -ForegroundColor Green
    } catch {
        Write-Host "Failed to stop: $_" -ForegroundColor Red
    }
}

# Main logic
if ($All) { $Status = $true; $Clean = $true }
if ($Status -or ($Kill -eq 0 -and -not $Clean)) { Show-Status }
if ($Clean -or $All) { Remove-StaleLocks }
if ($Kill -gt 0) { Stop-ControlPanelServer $Kill }

