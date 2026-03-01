"""
xAquaticRisk Run Dashboard — lightweight web server.

Monitors the ``run/`` folder for active and completed simulations, tails
experiment and MC log files, and exposes a JSON API consumed by the
single-page dashboard front-end (``index.html``).

Usage:
    python dashboard/server.py                       # default port 8050
    python dashboard/server.py --port 9000           # custom port
    python dashboard/server.py --run-dir C:/other    # custom run folder
"""

import argparse
import datetime
import glob
import json
import os
import re
import time
import xml.etree.ElementTree as ET
from http.server import HTTPServer, SimpleHTTPRequestHandler
from pathlib import Path

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
DEFAULT_RUN_DIR = os.path.join(BASE_DIR, "run")
PORT = 8050

# ---------------------------------------------------------------------------
# Log parsing helpers
# ---------------------------------------------------------------------------

_SEV_RE = re.compile(r"^(ERROR|WARN |NOTE |OK   |INFO )\s(.*)$")

COMPONENT_STAGES = [
    "LandscapeScenario", "Weather", "WaterTemperature", "Hydrology",
    "PPM", "SprayDrift", "DepositionToReach",
    "StepsRiverNetwork", "ExportStepsRiverNetworkReaches", "ExportStepsRiverNetworkPecs",
    "CascadeToxswa", "ExportCascadeToxswaReaches", "ExportCascadeToxswaPecs",
    # SD effects species 1-3 (StepsRiverNetwork)
    "IndEffect_StepsRiverNetwork_SD_Species1",
    "IndEffect_LP50_StepsRiverNetwork_SD_Species1",
    "ExportStepsRiverNetworkLP50Species1SD",
    "IndEffect_StepsRiverNetwork_SD_Species2",
    "IndEffect_LP50_StepsRiverNetwork_SD_Species2",
    "ExportStepsRiverNetworkLP50Species2SD",
    "IndEffect_StepsRiverNetwork_SD_Species3",
    "IndEffect_LP50_StepsRiverNetwork_SD_Species3",
    "ExportStepsRiverNetworkLP50Species3SD",
    # IT effects species 1-3 (StepsRiverNetwork)
    "IndEffect_StepsRiverNetwork_IT_Species1",
    "IndEffect_LP50_StepsRiverNetwork_IT_Species1",
    "ExportStepsRiverNetworkLP50Species1IT",
    "IndEffect_StepsRiverNetwork_IT_Species2",
    "IndEffect_LP50_StepsRiverNetwork_IT_Species2",
    "ExportStepsRiverNetworkLP50Species2IT",
    "IndEffect_StepsRiverNetwork_IT_Species3",
    "IndEffect_LP50_StepsRiverNetwork_IT_Species3",
    "ExportStepsRiverNetworkLP50Species3IT",
    # SD effects species 1-3 (CascadeToxswa)
    "IndEffect_CascadeToxswa_SD_Species1",
    "IndEffect_LP50_CascadeToxswa_SD_Species1",
    "ExportCascadeToxswaLP50Species1SD",
    "IndEffect_CascadeToxswa_SD_Species2",
    "IndEffect_LP50_CascadeToxswa_SD_Species2",
    "ExportCascadeToxswaLP50Species2SD",
    "IndEffect_CascadeToxswa_SD_Species3",
    "IndEffect_LP50_CascadeToxswa_SD_Species3",
    "ExportCascadeToxswaLP50Species3SD",
    # IT effects species 1-3 (CascadeToxswa)
    "IndEffect_CascadeToxswa_IT_Species1",
    "IndEffect_LP50_CascadeToxswa_IT_Species1",
    "ExportCascadeToxswaLP50Species1IT",
    "IndEffect_CascadeToxswa_IT_Species2",
    "IndEffect_LP50_CascadeToxswa_IT_Species2",
    "ExportCascadeToxswaLP50Species2IT",
    "IndEffect_CascadeToxswa_IT_Species3",
    "IndEffect_LP50_CascadeToxswa_IT_Species3",
    "ExportCascadeToxswaLP50Species3IT",
    # Post-processing
    "PopulationModelAsellusAquaticus",
    "AnalysisObserverComponent",
    "ReportingObserverComponent",
    "DeleteFolder",
]


def _parse_log_lines(path: str, tail: int = 0):
    """Return parsed log entries from *path*.

    Each entry is ``{"sev": str, "msg": str}``.
    If *tail* > 0, only the last *tail* raw lines are considered.
    """
    entries = []
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as fh:
            lines = fh.readlines()
    except (OSError, PermissionError):
        return entries

    if tail > 0:
        lines = lines[-tail:]

    current = None
    for raw in lines:
        raw = raw.rstrip("\n\r")
        m = _SEV_RE.match(raw)
        if m:
            if current is not None:
                entries.append(current)
            current = {"sev": m.group(1).strip(), "msg": m.group(2)}
        elif current is not None:
            # continuation / detail line
            current["msg"] += "\n" + raw.strip()
    if current is not None:
        entries.append(current)
    return entries


def _extract_mc_progress(entries):
    """Derive component progress list from parsed MC log entries.

    Returns (initialized, done, current_component) where *initialized* is
    the ordered list of all components that were initialised, *done* is the
    subset that finished, and *current_component* is the one currently
    executing (or None).
    """
    initialized = []
    components_done = []
    current_component = None
    for e in entries:
        msg = e["msg"]
        if msg.startswith("Initializing component "):
            name = msg[len("Initializing component "):]
            if name not in initialized:
                initialized.append(name)
        elif msg.startswith("Running component "):
            current_component = msg[len("Running component "):]
        elif msg.startswith("Component ") and msg.endswith(" finished"):
            name = msg[len("Component "):-len(" finished")]
            components_done.append(name)
            current_component = None
    return initialized, components_done, current_component


def _severity_counts(entries):
    """Count severity levels."""
    counts = {"ERROR": 0, "WARN": 0, "NOTE": 0, "OK": 0, "INFO": 0}
    for e in entries:
        sev = e["sev"]
        if sev in counts:
            counts[sev] += 1
    return counts


# ---------------------------------------------------------------------------
# Run discovery
# ---------------------------------------------------------------------------

def _parse_info_xml(run_dir):
    """Extract metadata from info.xml."""
    info_path = os.path.join(run_dir, "info.xml")
    meta = {}
    if os.path.isfile(info_path):
        try:
            tree = ET.parse(info_path)
            root = tree.getroot()
            for child in root:
                tag = re.sub(r"\{.*\}", "", child.tag)
                meta[tag] = child.text or ""
        except ET.ParseError:
            pass
    return meta


def _parse_user_xml(run_dir):
    """Extract user parameters from user.xml."""
    user_path = os.path.join(run_dir, "user.xml")
    params = {}
    if os.path.isfile(user_path):
        try:
            tree = ET.parse(user_path)
            root = tree.getroot()
            for section in root:
                sec_tag = re.sub(r"\{.*\}", "", section.tag)
                for param in section:
                    p_tag = re.sub(r"\{.*\}", "", param.tag)
                    params[f"{sec_tag}/{p_tag}"] = param.text or ""
        except ET.ParseError:
            pass
    return params


def discover_runs(run_root):
    """Return list of run summaries, most recent first."""
    runs = []
    if not os.path.isdir(run_root):
        return runs

    for entry in os.listdir(run_root):
        run_path = os.path.join(run_root, entry)
        if not os.path.isdir(run_path):
            continue
        log_dir = os.path.join(run_path, "log")
        exp_log = os.path.join(log_dir, "experiment.log")

        # Determine status from experiment log
        status = "unknown"
        elapsed = ""
        mc_total = 0
        mc_mode = ""
        if os.path.isfile(exp_log):
            exp_entries = _parse_log_lines(exp_log)
            for e in exp_entries:
                if "Experiment started" in e["msg"]:
                    status = "running"
                if "Experiment finished" in e["msg"]:
                    status = "finished"
                m = re.search(r"(Serial|Parallel) mode.*?(\d+) MC", e["msg"])
                if m:
                    mc_mode = m.group(1)
                    mc_total = int(m.group(2))
                if e["msg"].startswith("Elapsed time:"):
                    elapsed = e["msg"][len("Elapsed time:"):].strip()
            # Check for errors/warnings summary
            for e in exp_entries:
                if "completed with errors" in e["msg"]:
                    status = "error"
                elif "completed with warnings" in e["msg"] and status != "error":
                    status = "warning"
        else:
            status = "initializing"

        # Count MC log files
        mc_logs = glob.glob(os.path.join(log_dir, "mc_*.log"))
        mc_finished = 0
        for ml in mc_logs:
            entries = _parse_log_lines(ml, tail=20)
            for e in entries:
                if "MC run finished" in e["msg"]:
                    mc_finished += 1
                    break

        # Get folder modification time
        try:
            mtime = os.path.getmtime(run_path)
            mtime_str = datetime.datetime.fromtimestamp(mtime).strftime("%Y-%m-%d %H:%M:%S")
        except OSError:
            mtime_str = ""

        runs.append({
            "id": entry,
            "status": status,
            "elapsed": elapsed,
            "mc_total": mc_total,
            "mc_running": len(mc_logs),
            "mc_finished": mc_finished,
            "mc_mode": mc_mode,
            "modified": mtime_str,
        })

    runs.sort(key=lambda r: r["modified"], reverse=True)
    return runs


def run_detail(run_root, run_id):
    """Return detailed information for a single run."""
    run_path = os.path.join(run_root, run_id)
    if not os.path.isdir(run_path):
        return None

    log_dir = os.path.join(run_path, "log")
    exp_log = os.path.join(log_dir, "experiment.log")

    # Experiment log
    exp_entries = _parse_log_lines(exp_log) if os.path.isfile(exp_log) else []
    exp_sev = _severity_counts(exp_entries)

    # User parameters
    params = _parse_user_xml(run_path)

    # MC runs
    mc_logs = sorted(glob.glob(os.path.join(log_dir, "mc_*.log")))
    mc_runs = []
    for ml in mc_logs:
        mc_name = os.path.splitext(os.path.basename(ml))[0].replace("mc_", "")
        entries = _parse_log_lines(ml)
        sev = _severity_counts(entries)
        initialized, done, current = _extract_mc_progress(entries)

        # Determine MC status
        mc_status = "running"
        mc_elapsed = ""
        for e in reversed(entries):
            if "MC run finished" in e["msg"]:
                mc_status = "finished"
            if "MC run completed with errors" in e["msg"]:
                mc_status = "error"
            if "MC run completed with warnings" in e["msg"] and mc_status not in ("error",):
                mc_status = "warning"
            if e["msg"].startswith("Elapsed time:") and mc_status in ("finished", "error", "warning"):
                mc_elapsed = e["msg"][len("Elapsed time:"):].strip()
                break

        # Progress fraction based on dynamically discovered components
        total_components = len(initialized) if initialized else 1
        progress = len(done) / total_components

        mc_runs.append({
            "name": mc_name,
            "status": mc_status,
            "elapsed": mc_elapsed,
            "initialized": initialized,
            "components_done": done,
            "current_component": current,
            "progress": round(progress, 4),
            "severity_counts": sev,
        })

    # Experiment status
    status = "unknown"
    elapsed = ""
    for e in exp_entries:
        if "Experiment started" in e["msg"]:
            status = "running"
        if "Experiment finished" in e["msg"]:
            status = "finished"
        if "completed with errors" in e["msg"]:
            status = "error"
        elif "completed with warnings" in e["msg"] and status != "error":
            status = "warning"
        if e["msg"].startswith("Elapsed time:"):
            elapsed = e["msg"][len("Elapsed time:"):].strip()

    return {
        "id": run_id,
        "status": status,
        "elapsed": elapsed,
        "severity_counts": exp_sev,
        "parameters": params,
        "mc_runs": mc_runs,
    }


def tail_log(run_root, run_id, log_name, tail_lines=200):
    """Return the last *tail_lines* of a specific log, parsed."""
    run_path = os.path.join(run_root, run_id)
    # Sanitise log_name to prevent directory traversal
    log_name = os.path.basename(log_name)
    log_path = os.path.join(run_path, "log", log_name)
    if not os.path.isfile(log_path):
        return {"error": f"Log not found: {log_name}"}
    entries = _parse_log_lines(log_path, tail=tail_lines * 2)  # read more raw lines
    # Return the last tail_lines parsed entries
    return {"entries": entries[-tail_lines:], "total_lines": _count_lines(log_path)}


def _count_lines(path):
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as fh:
            return sum(1 for _ in fh)
    except OSError:
        return 0


# ---------------------------------------------------------------------------
# HTTP handler
# ---------------------------------------------------------------------------

class DashboardHandler(SimpleHTTPRequestHandler):
    """Serves the dashboard SPA and JSON API."""

    run_root = DEFAULT_RUN_DIR

    def __init__(self, *args, **kwargs):
        self._webdir = os.path.dirname(os.path.abspath(__file__))
        super().__init__(*args, directory=self._webdir, **kwargs)

    # --- Routing ---

    def do_GET(self):
        path = self.path.split("?")[0]

        if path in ("/", "/index.html"):
            self._serve_file("index.html", "text/html")
        elif path == "/api/runs":
            self._json_response(discover_runs(self.run_root))
        elif path.startswith("/api/runs/") and "/log/" in path:
            # /api/runs/<run_id>/log/<log_name>?tail=200
            parts = path.split("/")
            run_id = parts[3]
            log_name = parts[5] if len(parts) > 5 else "experiment.log"
            qs = self._query_params()
            tail = int(qs.get("tail", 200))
            self._json_response(tail_log(self.run_root, run_id, log_name, tail))
        elif path.startswith("/api/runs/"):
            run_id = path.split("/")[3]
            self._json_response(run_detail(self.run_root, run_id))
        else:
            super().do_GET()

    # --- Helpers ---

    def _serve_file(self, filename, content_type):
        fpath = os.path.join(self._webdir, filename)
        try:
            with open(fpath, "rb") as fh:
                data = fh.read()
            self.send_response(200)
            self.send_header("Content-Type", content_type)
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
        except FileNotFoundError:
            self.send_error(404)

    def _json_response(self, obj):
        payload = json.dumps(obj, ensure_ascii=False).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(payload)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(payload)

    def _query_params(self):
        qs = {}
        if "?" in self.path:
            for pair in self.path.split("?", 1)[1].split("&"):
                if "=" in pair:
                    k, v = pair.split("=", 1)
                    qs[k] = v
        return qs

    def log_message(self, fmt, *args):
        """Suppress default request logging."""
        pass


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="xAquaticRisk Run Dashboard")
    parser.add_argument("--port", type=int, default=PORT, help="HTTP port (default: %(default)s)")
    parser.add_argument("--run-dir", default=DEFAULT_RUN_DIR, help="Path to the run/ folder")
    args = parser.parse_args()

    DashboardHandler.run_root = os.path.abspath(args.run_dir)

    server = HTTPServer(("0.0.0.0", args.port), DashboardHandler)
    print(f"xAquaticRisk Dashboard — http://localhost:{args.port}")
    print(f"Monitoring: {DashboardHandler.run_root}")
    print("Press Ctrl+C to stop.\n")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down.")
        server.server_close()


if __name__ == "__main__":
    main()
