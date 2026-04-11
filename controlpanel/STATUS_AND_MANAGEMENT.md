# Controlpanel Server Status & Management

Quick commands to check and manage controlpanel server instances.

## HTTP Status Endpoint (Recommended)

Available at: `GET http://localhost:PORT/api/controlpanel/status` where PORT is 8090 (default), 8091, etc.

**Advantages:** Works reliably, no dependencies, returns JSON.

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

```
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
3. Right-click → End Process

Then clean up:
```powershell
.\controlpanel-status.ps1 -Clean  # Remove stale lock file
```

