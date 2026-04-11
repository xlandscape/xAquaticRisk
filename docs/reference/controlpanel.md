# Control Panel

The **Control Panel** is an integrated web application that merges the parameterisation editor and the run-monitoring dashboard into a single browser tab. It replaces the need to run two separate tools (`webui.bat` and `dashboard.bat`) and adds a seamless *Configure → Run → Monitor* workflow.

!!! tip "Quick start"
    Double-click **`controlpanel.bat`** at the model root. Your browser opens at **<http://localhost:8090>**.

---

## Starting the Control Panel

### One-click launch

Double-click **`controlpanel.bat`** in the model root directory.

### Command-line launch

```powershell
# Using the bundled Python runtime
model\core\bin\python-3.9.7-amd64\python.exe  controlpanel\server.py

# Custom port
model\core\bin\python-3.9.7-amd64\python.exe  controlpanel\server.py  --port 9000

# Monitor a different run folder
model\core\bin\python-3.9.7-amd64\python.exe  controlpanel\server.py  --run-dir  C:\other\run
```

| Option | Description | Default |
|--------|-------------|---------|
| `--port PORT` | HTTP port | `8090` |
| `--run-dir DIR` | Path to the `run/` folder to monitor | `run/` (relative to model root) |

---

## Layout

The Control Panel has two tabs accessible from the top bar:

### Configure tab

A form-based parameterisation editor that lets you set all xAquaticRisk parameters without editing XML:

- **xrun File Operations** — browse a directory for existing `.xrun` files, load them into the form, or save the current configuration with a custom filename.
- **Experiment Configuration** — `ExperimentID`, number of Monte Carlo runs, parallel processes, landscape scenario, and simulation period.
- **PPP Use** — application rate and time windows.
- **Mitigation** — in-crop buffer and drift-reduction technology.
- **Exposure** — Rautmann class or custom deposition file.
- **Environmental Fate** — toggle StepsRiverNetwork / CascadeToxswa and all substance physico-chemical properties.
- **Effect Modelling** — toggle LGuts, warm-up / recovery periods, and GUTS-SD / GUTS-IT parameters for up to three species.
- **Additional Parameters** — any template parameters not covered by the sections above are shown here dynamically.

Each section can be collapsed / expanded by clicking its header. Advanced sections (*Environmental Fate*, *Effect Modelling*) start collapsed.

**Actions:**

| Button | Description |
|--------|-------------|
| **Reset to Template** | Reloads all values from `template.xrun` |
| **Save** | Writes the current parameter values back to the active `.xrun` file (or to `<ExperimentID>.xrun` in the model root) |
| **Run Simulation** | Creates the `.xrun` file, launches `__start__.bat` in a new console window, and automatically switches to the **Monitor** tab |

### Monitor tab

A real-time dashboard for tracking simulation runs. It provides the same monitoring workflow that used to be available through the standalone dashboard launcher:

- **Sidebar** — lists all runs in `run/`, sorted by most recent, with colour-coded status dots.
- **Overview cards** — status, elapsed time, MC count, error/warning counts.
- **Abort button** — appears below the overview cards when a simulation was launched from the current server session and is still running. Clicking it forcefully terminates the entire process tree.
- **Monte Carlo progress bars** — per-MC progress with the currently executing component shown below each bar.
- **Component pipeline** — checklist of all initialised components for the first MC run.
- **Log viewer** — tabbed log output (experiment + per-MC logs) with severity filter, auto-scroll toggle, and 3-second refresh.
- **Parameters table** — parameter values read from the run's `user.xml`.

!!! warning "Abort limitation"
    Only simulations launched from the **current** Control Panel session can be aborted. If you restart the server, PID tracking is lost and the abort button will not appear for previously started runs.

---

## Analysis runtime

The Control Panel starts `analysis/run_basic_analysis.py` with the dedicated embedded runtime at `analysis\python\python.exe`.

To keep analysis xcopy-deployable, that runtime is stored completely inside `analysis\python\`. Populate it once by running `setup_analysis_python.bat` before you package or copy the model folder to another machine.

If the folder is missing or incomplete, the Control Panel now stops before launching the job and returns a setup error instead of a Python traceback.

---

## Typical workflow

1. Open the Control Panel (`controlpanel.bat`).
2. On the **Configure** tab, set parameters or load an existing `.xrun` file.
3. Click **Run Simulation**.
4. The panel auto-switches to the **Monitor** tab and selects the new run.
5. Watch progress bars, component pipeline, and logs update every 3 seconds.
6. If needed, click **Abort Simulation** to stop a running simulation.
7. When the run finishes, inspect results in `run/<ExperimentID>/reporting`.

---

## Server Management and Monitoring

### HTTP Status Endpoint

The Control Panel exposes a status endpoint that reports the server's current state, useful for monitoring or automation:

**Endpoint:** `GET http://localhost:PORT/api/controlpanel/status` (PORT defaults to 8090)

**Response when server is running:**
```json
{
  "status": "running",
  "pid": 12345,
  "port": 8090,
  "started_at": 1712846553,
  "uptime_seconds": 300,
  "alive": true,
  "lock_file": "c:\\...\\server.instance.lock"
}
```

**Response when no server is running:** 404 error

**Quick check from PowerShell:**
```powershell
# Check if server is running
$response = Invoke-WebRequest -Uri "http://localhost:8090/api/controlpanel/status" -UseBasicParsing -ErrorAction SilentlyContinue
if ($response) {
    $info = $response.Content | ConvertFrom-Json
    Write-Host "Server is $($info.status) | PID: $($info.pid) | Uptime: $($info.uptime_seconds)s"
}
```

### PowerShell Helper Script

A convenience script at `controlpanel/controlpanel-status.ps1` can check server status and manage instances:

**Check status on a specific port:**
```powershell
.\controlpanel\controlpanel-status.ps1 -Port 8091
```

**Output:**
```
Status: RUNNING
PID: 35864 | Port: 8091
Uptime: 0h 1m 28s
```

**Clean up stale lock files** (e.g., after a crash):
```powershell
.\controlpanel\controlpanel-status.ps1 -Clean
```

**Stop a server on a specific port:**
```powershell
.\controlpanel\controlpanel-status.ps1 -Kill 8090
```

**Combine status check and cleanup:**
```powershell
.\controlpanel\controlpanel-status.ps1 -Port 8090 -All
```

### Troubleshooting

**Port already in use but no process found:**

If the Control Panel fails to start due to a port conflict, the process may have crashed and left a lock file:

```powershell
# Remove the stale lock file
Remove-Item controlpanel\server.instance.lock -Force

# Or use the helper script
.\controlpanel\controlpanel-status.ps1 -Clean
```

**Multiple instances accidentally running:**

Multiple Control Panel processes can block each other. The single-instance lock guard prevents this, but if you start them on different ports:

```powershell
# Check all instances
Invoke-WebRequest http://localhost:8090/api/controlpanel/status -UseBasicParsing | 
  Select-Object -Expand Content | ConvertFrom-Json | Select pid, port, status

# Kill a specific instance
.\controlpanel\controlpanel-status.ps1 -Kill 8090
```

**Stale zombie process in Task Manager:**

If you kill the process via Task Manager instead of the `Abort` button:

```powershell
# Clean up the orphaned lock file
.\controlpanel\controlpanel-status.ps1 -Clean
```

---

## Architecture

The Control Panel is a single Python HTTP server (`controlpanel/server.py`) that exposes both the parameterisation API and the monitoring API:

**Parameterisation endpoints:**

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/api/template` | Parsed `template.xrun` with comment metadata |
| `GET` | `/api/scenarios` | Available landscape scenarios |
| `POST` | `/api/run` | Create `.xrun` and launch simulation |
| `POST` | `/api/save` | Save current parameterisation |
| `POST` | `/api/save-as` | Save with custom filename |
| `POST` | `/api/open-xrun` | Load an existing `.xrun` file |
| `POST` | `/api/xrun-files` | List `.xrun` files in a directory |

**Monitoring endpoints:**

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/api/runs` | List all simulation runs (includes `abortable` flag) |
| `GET` | `/api/runs/<id>` | Detailed run information (includes `abortable` flag) |
| `GET` | `/api/runs/<id>/log/<name>` | Tail parsed log entries |
| `POST` | `/api/runs/<id>/abort` | Abort a running simulation (kills entire process tree) |

All data is served as JSON. The front-end is a single `index.html` file with no external dependencies.

---

## Standalone tools

The standalone WebUI and Dashboard remain available for users who prefer separate tools:

| Tool | Launcher | Port | Description |
|------|----------|------|-------------|
| WebUI | `webui.bat` | 8080 | Parameterisation editor only |
| Dashboard | `dashboard.bat` | 8050 | Run monitor only |
| **Control Panel** | `controlpanel.bat` | 8090 | Both, integrated |
