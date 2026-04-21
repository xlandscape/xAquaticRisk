# Controlpanel Server Status & Management

Quick commands to check and manage controlpanel server instances.

## HTTP Status Endpoint (Recommended)

Available at: `GET http://localhost:PORT/api/controlpanel/status` where PORT is 8090 (default), 8091, etc.

**Advantages:** Works reliably, no dependencies, returns JSON.

## Self-Contained Runtime Health

Available at: `GET http://localhost:PORT/api/runtime/self-contained-status`

Use this endpoint to verify that the copied working tree still contains all
bundled runtimes required for full controlpanel functionality.

Response highlights:

- `status`: `ready` or `incomplete`
- `warnings`: list of deployment/runtime issues
- `controlpanel`: bundled controlpanel runtime checks
- `analysis`: bundled analysis runtime checks and required package checks
- `analysis_runtime_config`: active analysis mode and remote service configuration
- `model_runtime_present`: whether model-core runtime is present for `__start__.bat`

## Analysis Runtime Mode (Separation Scaffold)

Available at: `GET http://localhost:PORT/api/analysis/runtime-config`

Response highlights:

- `mode`: `local` or `remote`
- `service_url`: configured remote analysis service URL (if any)
- `service_configured`: whether remote service URL is set
- `supported_modes`: currently supported values for mode

Environment variables:

- `XAQ_ANALYSIS_MODE`: `local` (default) or `remote`
- `XAQ_ANALYSIS_SERVICE_URL`: base URL for future remote analysis service

Notes:

- Current branch behavior is backward compatible in default `local` mode.
- `remote` mode is a scaffold for the service split and currently returns a clear not-implemented response for analysis start.

### Quick checks via PowerShell

```powershell
# Check status on default port 8090
$response = Invoke-WebRequest -Uri "http://localhost:8090/api/controlpanel/status" -UseBasicParsing
$info = $response.Content | ConvertFrom-Json
"Server: $($info.status) | PID: $($info.pid) | Uptime: $($info.uptime_seconds)s"

# Check on specific port
Invoke-WebRequest -Uri "http://localhost:8091/api/controlpanel/status" -UseBasicParsing | 
  Select-Object -Expand Content | ConvertFrom-Json | Format-Table
```

### Response Format

When server is running:

```json
{
  "status": "running",
  "pid": 35864,
  "port": 8091,
  "started_at": 1775894309,
  "uptime_seconds": 180,
  "alive": true,
  "lock_file": "c:\\...\\server.instance.lock"
}
```

When no server: 404 error (no response available)

When stale lock exists (zombie):

```json
{
  "status": "zombie",
  "pid": 9999,
  "port": 8090,
  "uptime_seconds": -99999,
  "alive": false,
  "lock_file": "c:\\...\\server.instance.lock"
}
```

## PowerShell Helper Script

Located at: `controlpanel/controlpanel-status.ps1`

Works when server is running OR via lock file fallback.

### Usage

```powershell
# Show current server status
.\controlpanel-status.ps1 -Port 8091

# Check specific port
.\controlpanel-status.ps1 -Port 8090

# Show status and clean stale locks
.\controlpanel-status.ps1 -Port 8091 -All

# Remove stale lock files only
.\controlpanel-status.ps1 -Clean

# Stop server on specific port
.\controlpanel-status.ps1 -Kill 8091
```

### Example Output

```text
Status: RUNNING
PID: 35864 | Port: 8091
Uptime: 0h 0m 28s
```

## Troubleshooting

### Check if server is running

```powershell
# Method 1: HTTP endpoint
Invoke-WebRequest http://localhost:8090/api/controlpanel/status -UseBasicParsing | ForEach-Object {
  $_.Content | ConvertFrom-Json | Select status, pid, uptime_seconds
}

# Method 2: PowerShell script
.\controlpanel-status.ps1 -Port 8090
```

### Port already in use but no process found

```powershell
# Clean stale lock if server crashed
Remove-Item c:\LocalWork\xAquaticRisk\controlpanel\server.instance.lock -Force

# Or use the helper
.\controlpanel-status.ps1 -Clean
```

### Multiple instances accidentally running

```powershell
# Check which port each instance is on
Invoke-WebRequest http://localhost:8090/api/controlpanel/status -UseBasicParsing | 
  Select-Object -Expand Content | ConvertFrom-Json | Select pid, port, alive

# Kill by port
.\controlpanel-status.ps1 -Kill 8090
.\controlpanel-status.ps1 -Kill 8091
```

### Windows Task Manager Integration

1. Open Task Manager
2. Find `python.exe` running `server.py` (look in Details tab, Command Line column)
3. Right-click â†’ End Process

Then clean up:

```powershell
.\controlpanel-status.ps1 -Clean  # Remove stale lock file
```
