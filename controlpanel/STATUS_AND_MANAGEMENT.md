# Controlpanel Server Status & Management

Quick commands to check and manage controlpanel server instances.

## PowerShell Helper Script

Located at: `controlpanel/controlpanel-status.ps1`

### Usage

```powershell
# Show current server status
.\controlpanel-status.ps1

# Show status and clean stale locks
.\controlpanel-status.ps1 -All

# Remove stale lock files (dead processes)
.\controlpanel-status.ps1 -Clean

# Kill server on specific port
.\controlpanel-status.ps1 -Kill 8090
```

### Example Output

```
Lock file: C:\LocalWork\xAquaticRisk\controlpanel\server.instance.lock
Status: ✓ RUNNING
PID: 12345
Port: 8090
Started: 2026-04-11 14:22:33 (2m 45s ago)
Python: C:\LocalWork\xAquaticRisk\.venv\Scripts\python.exe
```

## HTTP Status Endpoint

Available at: `GET http://localhost:8090/api/controlpanel/status`

### Response Formats

**Server running:**
```json
{
  "status": "running",
  "pid": 12345,
  "port": 8090,
  "started_at": 1712846553,
  "uptime_seconds": 165,
  "alive": true,
  "lock_file": "C:\\...\\server.instance.lock"
}
```

**No server running:**
```json
{
  "status": "not_running",
  "pid": null,
  "port": null,
  "started_at": null,
  "uptime_seconds": null,
  "alive": false,
  "lock_file": "C:\\...\\server.instance.lock"
}
```

**Stale process (zombie lock):**
```json
{
  "status": "zombie",
  "pid": 9999,
  "port": 8090,
  "started_at": 1712845000,
  "uptime_seconds": -87,
  "alive": false,
  "lock_file": "C:\\...\\server.instance.lock"
}
```

### Quick Check via curl

```powershell
curl http://localhost:8090/api/controlpanel/status | ConvertFrom-Json | Format-Table

# Or with error handling:
$response = curl -s http://localhost:8090/api/controlpanel/status | ConvertFrom-Json
"Server: $($response.status) (PID: $($response.pid), Port: $($response.port))"
```

## Troubleshooting

### Port already in use but no process found

```powershell
# Clean stale locks:
.\controlpanel-status.ps1 -Clean

# Then restart server
```

### Multiple instances accidentally running

```powershell
# Check all instances:
.\controlpanel-status.ps1

# Kill by port:
.\controlpanel-status.ps1 -Kill 8090
```

### Windows Task Manager Integration

Find controlpanel process using `server.py` in Task Manager → right-click → End Process, then clean lock:

```powershell
.\controlpanel-status.ps1 -Clean
```
