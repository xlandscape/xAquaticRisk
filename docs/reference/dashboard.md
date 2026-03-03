# Run Dashboard

The **Run Dashboard** is a web-based monitoring tool that lets you follow the progress and logging output of xAquaticRisk simulations in real time. It automatically discovers all runs in the `run/` folder, displays component-level progress bars, and provides a filterable log viewer — all in your browser.

<img src="../img/dashboard-overview.png" alt="Run Dashboard overview" width="900"/>

!!! info "No model changes required"
    The dashboard reads log files produced by the standard xAquaticRisk logging system. It does not modify the model, inject any observers, or interfere with running simulations.

---

## Starting the Dashboard

### One-click launch

Double-click **`dashboard.bat`** at the model root. The dashboard opens at **<http://localhost:8050>**.

### Command-line launch

```powershell
# Using the project's virtual environment
.venv\Scripts\python.exe  dashboard\server.py

# Custom port
.venv\Scripts\python.exe  dashboard\server.py  --port 9000

# Monitor a different run folder
.venv\Scripts\python.exe  dashboard\server.py  --run-dir  C:\other\run
```

| Option | Description | Default |
|--------|-------------|---------|
| `--port PORT` | HTTP port for the dashboard server | `8050` |
| `--run-dir DIR` | Path to the `run/` folder to monitor | `run/` (relative to the model root) |

!!! tip
    You can start the dashboard **before, during, or after** a simulation. It picks up new runs automatically via a 3-second polling interval.

---

## Dashboard Layout

The interface is split into three areas:

### Sidebar — Run List

Lists all simulation run folders found in `run/`, sorted by most recent first. Each entry shows:

- **Status indicator** — a colour-coded dot:

    | Colour | Meaning |
    |--------|---------|
    | Blue (pulsing) | Running |
    | Green | Finished successfully |
    | Yellow | Finished with warnings |
    | Red | Finished with errors |
    | Grey (pulsing) | Initialising |

- **Run ID** — the `ExperimentID` (with timestamp suffix appended by xAquaticRisk)
- **MC progress** — e.g., `MC: 2/3` means 2 of 3 Monte Carlo runs have completed
- **Elapsed time** — total wall-clock time once the run finishes

Click a run to open its detail view in the main panel.

### Main Panel — Overview

When a run is selected, the main panel shows:

#### Status Cards

A row of summary cards at the top:

| Card | Description |
|------|-------------|
| **Status** | Overall run status (Running, Finished, Warnings, Errors) |
| **Elapsed** | Total elapsed time |
| **MC Runs** | Number of Monte Carlo runs in this experiment |
| **Errors** | Count of ERROR-level messages in `experiment.log` |
| **Warnings** | Count of WARN-level messages in `experiment.log` |

#### Monte Carlo Progress

For each MC run, a progress bar shows:

- **Percentage complete** — based on the number of components finished vs. initialised
- **Currently executing component** — displayed below the bar while running (e.g., `▶ CascadeToxswa`)
- **Colour** — blue (running), green (done), yellow (warnings), red (errors)

#### Component Pipeline

A grid of all components discovered during initialisation for the first MC run. Each component is marked:

| Icon | State |
|------|-------|
| &#10003; (green) | Completed |
| &#9654; (blue) | Currently running |
| &#9679; (grey) | Pending |

!!! note
    The component list is **dynamically built** from each run's own log. It adapts automatically to runs with different configurations (e.g., runs without CascadeToxswa, or without GUTS effects).

### Log Viewer

A tabbed panel at the bottom of the detail view:

- **experiment** tab — the experiment-level log (`experiment.log`)
- **mc\_\<ID\>** tabs — one tab per Monte Carlo run log

#### Controls

| Control | Description |
|---------|-------------|
| **Auto-scroll** | When checked, the log view scrolls to the latest entries on each refresh |
| **Filter** | Drop-down to show only a specific severity: All, Errors, Warnings, Notes, OK, Info |

Log entries are colour-coded by severity:

| Severity | Colour | Meaning |
|----------|--------|---------|
| `ERROR` | Red | Something failed — check results carefully |
| `WARN` | Yellow | Non-critical issue — results may still be valid |
| `NOTE` | Blue | Informational note (e.g., missing documentation metadata) |
| `OK` | Green | Operation succeeded |
| `INFO` | White/grey | Progress and timing information |

### Parameters Table

Below the log viewer, a table displays all user parameters from `user.xml` (a snapshot of the `.xrun` file used for the run), grouped by section. This is useful for quickly verifying which settings were active without opening the run folder.

---

## Typical Workflow

1. **Start the dashboard** — run `dashboard.bat` (or launch from the command line).
2. **Start a simulation** — drag an `.xrun` file onto `__start__.bat` (or use the [WebUI](../getstarted/getstarted.md#running-xaquaticrisk)).
3. **Open the browser** — navigate to <http://localhost:8050> (opens automatically with `dashboard.bat`).
4. **Select the new run** — it appears at the top of the sidebar within a few seconds.
5. **Monitor progress** — watch the MC progress bars advance and the log stream in real time.
6. **Review results** — once the run finishes, check the error/warning counts and inspect specific log entries using the severity filter.

!!! tip "Multiple simultaneous runs"
    If you start several simulations at once (e.g., with different parameterisations), all of them appear in the sidebar and can be monitored independently.

---

## Architecture

The dashboard consists of two files in the `dashboard/` folder:

| File | Purpose |
|------|---------|
| `server.py` | Python HTTP server (stdlib only — no external dependencies) |
| `index.html` | Single-page front-end (HTML + CSS + vanilla JavaScript) |

The server exposes three JSON API endpoints:

| Endpoint | Returns |
|----------|---------|
| `GET /api/runs` | List of all runs with status, MC counts, elapsed time |
| `GET /api/runs/<id>` | Full detail for one run: severity counts, MC progress, component pipeline, parameters |
| `GET /api/runs/<id>/log/<name>?tail=N` | Last *N* parsed entries from a specific log file |

The front-end polls `/api/runs` and the selected run's detail endpoint every 3 seconds. No WebSocket or server-push is used — this keeps the implementation simple and dependency-free.

### How log parsing works

The server reads log files directly from the `run/<ExperimentID>/log/` folder:

- **`experiment.log`** — created at experiment start; records version info, MC orchestration, and the final summary.
- **`mc_<MC_NAME>.log`** — one per Monte Carlo run; records component initialisation, execution, data-store operations, and per-component elapsed times.

Log lines follow the format `LEVEL MESSAGE` where `LEVEL` is one of `ERROR`, `WARN`, `NOTE`, `OK`, `INFO` (left-padded to 6 characters). Detail/continuation lines are indented with spaces. The server parses these into structured `{sev, msg}` objects for the front-end.

Component progress is derived by scanning for:

- `Initializing component <name>` — builds the ordered pipeline for the run
- `Running component <name>` — marks the currently active component
- `Component <name> finished` — marks completion

This approach is fully **dynamic** — no hardcoded component list is needed.

---

## Requirements

- **Python** ≥ 3.6 (uses f-strings, `http.server`, `json`, `xml.etree`, `glob` — all stdlib)
- A modern web browser (Chrome, Edge, Firefox)
- No external Python packages required

The dashboard works with the project's virtual environment (`.venv/`) or with the bundled Python at `model\core\bin\python-3.9.7-amd64\python.exe`:

```powershell
model\core\bin\python-3.9.7-amd64\python.exe  dashboard\server.py
```

---

## Relation to Other Tools

| Tool | Purpose | How it relates |
|------|---------|----------------|
| **Run Dashboard** (`dashboard.bat`) | Monitor running and completed simulations | Reads log files produced by xAR |
| **WebUI** (`webui.bat`) | Create and launch parameterisation files | Generates `.xrun` files; the dashboard then monitors the resulting runs |
| **Console output** | Real-time text log in the terminal | Shows the same messages as the dashboard, but without filtering, progress bars, or history |
| **Jupyter Notebooks** (`notebook.bat`) | Post-run analysis and visualisation | Use after a run completes — the dashboard helps you know when that is |

For details on parameterisation, see the [Parameterisation Reference](parameterisation.md). For running simulations, see [Getting Started — Running xAquaticRisk](../getstarted/getstarted.md#running-xaquaticrisk).
