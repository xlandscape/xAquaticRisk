"""
xAquaticRisk Control Panel — unified web server.

Merges the parameterisation editor (WebUI) and run monitoring dashboard
into a single application served from one HTTP port.

Endpoints
~~~~~~~~~
    Static
        GET  /                          → index.html
    Parameterisation
        GET  /api/template              → parsed template.xrun with metadata
        GET  /api/scenarios             → available landscape scenarios
        POST /api/run                   → create xrun & launch __start__.bat
        POST /api/save                  → save current parameterisation
        POST /api/save-as               → save parameterisation to custom file
        POST /api/open-xrun             → load an existing xrun file
        POST /api/xrun-files            → list xrun files in a directory
    Monitoring
        GET  /api/runs                  → list of all simulation runs
        GET  /api/runs/<id>             → detailed run info (includes abortable flag)
        GET  /api/runs/<id>/log/<name>  → tail parsed log entries
        POST /api/runs/<id>/abort       → abort a running simulation

Usage::

    python controlpanel/server.py                       # default port 8090
    python controlpanel/server.py --port 9000            # custom port
    python controlpanel/server.py --run-dir C:/other     # custom run folder
"""

import argparse
import datetime
import glob
import json
import os
import re
import shutil
import subprocess
import sys
import threading
import time
import xml.etree.ElementTree as ET
from http.server import HTTPServer, SimpleHTTPRequestHandler
from pathlib import Path
from urllib.parse import urlparse, parse_qs, unquote_plus

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
DEFAULT_RUN_DIR = os.path.join(BASE_DIR, "run")
TEMPLATE_PATH = os.path.join(BASE_DIR, "template.xrun")
OUTPUT_DIR = BASE_DIR
START_BAT = os.path.join(BASE_DIR, "__start__.bat")
ANALYSIS_SCRIPT = os.path.join(BASE_DIR, "analysis", "run_basic_analysis.py")
ANALYSIS_OUTPUT_ROOT = os.path.join(BASE_DIR, "analysis_output")
_venv_py = os.path.join(BASE_DIR, "analysis", ".venv", "Scripts", "python.exe")
ANALYSIS_PYTHON = _venv_py if os.path.isfile(_venv_py) else sys.executable
PORT = 8090

# Track running simulation processes: {experiment_id: subprocess.Popen}
_running_processes = {}
_proc_lock = threading.Lock()

# Track analysis jobs: {job_id: {"proc", "output_dir", "started_at", "log_path"}}
_analysis_jobs = {}
_analysis_lock = threading.Lock()


# ═══════════════════════════════════════════════════════════════════
# Parameterisation helpers  (from webui/server.py)
# ═══════════════════════════════════════════════════════════════════

def get_available_xrun_files(path: str = None) -> list:
    """List .xrun files in *path*."""
    if not path:
        path = BASE_DIR
    path = os.path.abspath(path)
    if not os.path.isdir(path):
        return []
    try:
        return sorted(
            [{"name": f, "path": os.path.join(path, f)}
             for f in os.listdir(path) if f.endswith(".xrun")],
            key=lambda x: x["name"],
        )
    except PermissionError:
        return []


def parse_xrun_template(template_path: str) -> dict:
    """Parse *template_path* and return parameters with comment metadata."""
    parameters = {}
    try:
        with open(template_path, "r", encoding="utf-8") as fh:
            content = fh.read()
        tree = ET.parse(template_path)
        root = tree.getroot()

        def extract(element, prefix=""):
            for child in element:
                tag = child.tag.replace("{urn:xAquaticRisk}", "")
                key = f"{prefix}{tag}" if prefix else tag
                if len(child) > 0:
                    extract(child, f"{key}/")
                else:
                    value = child.text.strip() if child.text else ""
                    description = values_hint = remark = ""
                    pat = (
                        rf"<!--\s*Parameter\s*:\s*{re.escape(tag)}\s*"
                        rf"Description\s*:\s*(.*?)\s*Values\s*:\s*(.*?)\s*"
                        rf"(?:Remark\s*:\s*(.*?))?\s*-->"
                    )
                    m = re.search(pat, content, re.DOTALL | re.IGNORECASE)
                    if m:
                        description = m.group(1).strip()
                        values_hint = m.group(2).strip()
                        remark = (m.group(3) or "").strip()
                    parameters[key] = {
                        "value": value,
                        "tag": tag,
                        "description": description,
                        "values_hint": values_hint,
                        "remark": remark,
                    }
        extract(root)
    except Exception as exc:
        print(f"Error parsing template: {exc}")
    return parameters


def create_xrun_file(parameters: dict, output_path: str, template_path: str) -> str:
    """Write a .xrun file with *parameters* applied to the template tree."""
    tree = ET.parse(template_path)
    root = tree.getroot()

    def update(element, prefix=""):
        for child in element:
            tag = child.tag.replace("{urn:xAquaticRisk}", "")
            key = f"{prefix}{tag}" if prefix else tag
            if len(child) > 0:
                update(child, f"{key}/")
            elif key in parameters:
                child.text = parameters[key]
    update(root)
    tree.write(output_path, encoding="utf-8", xml_declaration=True)
    return output_path


def get_scenarios() -> list:
    """Return list of scenario directories."""
    scenario_dir = os.path.join(BASE_DIR, "scenario")
    if not os.path.isdir(scenario_dir):
        return []
    return [
        {"name": d, "path": f"scenario/{d}"}
        for d in sorted(os.listdir(scenario_dir))
        if os.path.isdir(os.path.join(scenario_dir, d))
    ]


def get_scenario_extent(scenario_path: str) -> dict:
    """Read TemporalExtent from scenario.xproject and return from/to dates."""
    full_path = os.path.join(BASE_DIR, scenario_path, "scenario.xproject")
    if not os.path.isfile(full_path):
        return {"error": f"scenario.xproject not found in {scenario_path}"}
    try:
        tree = ET.parse(full_path)
        root = tree.getroot()
        # Strip any namespace from tags
        def _text(tag):
            for el in root.iter():
                if re.sub(r"\{.*?\}", "", el.tag) == tag:
                    return (el.text or "").strip()
            return ""
        return {"from_date": _text("FromDate"), "to_date": _text("ToDate")}
    except Exception as exc:
        return {"error": str(exc)}


# ═══════════════════════════════════════════════════════════════════
# Dashboard / monitoring helpers  (from dashboard/server.py)
# ═══════════════════════════════════════════════════════════════════

_SEV_RE = re.compile(r"^(ERROR|WARN |NOTE |OK   |INFO )\s(.*)$")


def _parse_log_lines(path: str, tail: int = 0):
    """Return parsed log entries ``[{sev, msg}, ...]``."""
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
            current["msg"] += "\n" + raw.strip()
    if current is not None:
        entries.append(current)
    return entries


def _extract_mc_progress(entries):
    """Return (initialized, done, current_component)."""
    initialized, done, current = [], [], None
    for e in entries:
        msg = e["msg"]
        if msg.startswith("Initializing component "):
            name = msg[len("Initializing component "):]
            if name not in initialized:
                initialized.append(name)
        elif msg.startswith("Running component "):
            current = msg[len("Running component "):]
        elif msg.startswith("Component ") and msg.endswith(" finished"):
            name = msg[len("Component "):-len(" finished")]
            done.append(name)
            current = None
    return initialized, done, current


def _severity_counts(entries):
    counts = {"ERROR": 0, "WARN": 0, "NOTE": 0, "OK": 0, "INFO": 0}
    for e in entries:
        sev = e["sev"]
        if sev in counts:
            counts[sev] += 1
    return counts


def _parse_user_xml(run_dir):
    user_path = os.path.join(run_dir, "user.xml")
    params = {}
    if not os.path.isfile(user_path):
        return params
    try:
        tree = ET.parse(user_path)
        for section in tree.getroot():
            sec = re.sub(r"\{.*\}", "", section.tag)
            for p in section:
                params[f"{sec}/{re.sub(r'{.*}', '', p.tag)}"] = p.text or ""
    except ET.ParseError:
        pass
    return params


def _count_lines(path):
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as fh:
            return sum(1 for _ in fh)
    except OSError:
        return 0


# ═══════════════════════════════════════════════════════════════════
# Analysis helpers
# ═══════════════════════════════════════════════════════════════════

def list_runs_with_mcs(run_root):
    """Return [{ experiment, mcs: [{id, has_store}] }], sorted newest first."""
    result = []
    if not os.path.isdir(run_root):
        return result
    for entry in sorted(os.listdir(run_root), reverse=True):
        run_path = os.path.join(run_root, entry)
        if not os.path.isdir(run_path):
            continue
        mcs_path = os.path.join(run_path, "mcs")
        mcs = []
        if os.path.isdir(mcs_path):
            for mc in sorted(os.listdir(mcs_path)):
                mc_path = os.path.join(mcs_path, mc)
                if os.path.isdir(mc_path):
                    has_store = os.path.isfile(
                        os.path.join(mc_path, "store", "arr.dat"))
                    mcs.append({"id": mc, "has_store": has_store})
        if mcs:
            result.append({"experiment": entry, "mcs": mcs})
    return result


def analysis_job_status(job_id):
    """Return status dict for an analysis job."""
    with _analysis_lock:
        job = _analysis_jobs.get(job_id)
    if job is None:
        return {"error": "Job not found"}
    proc = job["proc"]
    running   = proc.poll() is None
    exit_code = proc.poll()
    log_lines = []
    if os.path.isfile(job["log_path"]):
        try:
            with open(job["log_path"], "r", encoding="utf-8", errors="replace") as fh:
                log_lines = [l.rstrip() for l in fh.readlines()[-300:]]
        except OSError:
            pass
    return {
        "job_id":      job_id,
        "running":     running,
        "exit_code":   exit_code,
        "output_dir":  job["output_dir"],
        "started_at":  job["started_at"],
        "log_lines":   log_lines,
    }


def analysis_job_outputs(job_id):
    """List output files produced by an analysis job."""
    with _analysis_lock:
        job = _analysis_jobs.get(job_id)
    if job is None:
        return {"error": "Job not found"}
    output_dir = job["output_dir"]
    files = []
    if os.path.isdir(output_dir):
        for fn in sorted(os.listdir(output_dir)):
            fp = os.path.join(output_dir, fn)
            if not os.path.isfile(fp) or fn == "analysis.log":
                continue
            ext = os.path.splitext(fn)[1].lower()
            if ext == ".json":          # internal data files – not shown as chips
                continue
            ftype = ("image"  if ext in (".png", ".jpg", ".svg") else
                     "excel"  if ext == ".xlsx"                    else
                     "other")
            files.append({"name": fn, "size": os.path.getsize(fp), "type": ftype})
    return {"files": files, "output_dir": output_dir}


def discover_runs(run_root):
    """Return list of run summaries, most-recent first."""
    # Clean up finished/dead processes from tracking dict
    with _proc_lock:
        for rid in list(_running_processes):
            p = _running_processes[rid]
            if p.poll() is not None:          # process has exited
                del _running_processes[rid]

    runs = []
    if not os.path.isdir(run_root):
        return runs
    for entry in os.listdir(run_root):
        run_path = os.path.join(run_root, entry)
        if not os.path.isdir(run_path):
            continue
        log_dir = os.path.join(run_path, "log")
        exp_log = os.path.join(log_dir, "experiment.log")

        status, elapsed, mc_total, mc_mode = "unknown", "", 0, ""
        if os.path.isfile(exp_log):
            exp_entries = _parse_log_lines(exp_log)
            for e in exp_entries:
                if "Experiment started" in e["msg"]:
                    status = "running"
                if "Experiment finished" in e["msg"]:
                    status = "finished"
                m = re.search(r"(Serial|Parallel) mode.*?(\d+) MC", e["msg"])
                if m:
                    mc_mode, mc_total = m.group(1), int(m.group(2))
                if e["msg"].startswith("Elapsed time:"):
                    elapsed = e["msg"][len("Elapsed time:"):].strip()
            for e in exp_entries:
                if "completed with errors" in e["msg"]:
                    status = "error"
                elif "completed with warnings" in e["msg"] and status != "error":
                    status = "warning"
        else:
            status = "initializing"

        mc_logs = glob.glob(os.path.join(log_dir, "mc_*.log"))
        mc_finished = 0
        for ml in mc_logs:
            for e in _parse_log_lines(ml, tail=20):
                if "MC run finished" in e["msg"]:
                    mc_finished += 1
                    break

        try:
            mtime = datetime.datetime.fromtimestamp(
                os.path.getmtime(run_path)
            ).strftime("%Y-%m-%d %H:%M:%S")
        except OSError:
            mtime = ""

        runs.append({
            "id": entry,
            "status": status,
            "elapsed": elapsed,
            "mc_total": mc_total,
            "mc_running": len(mc_logs),
            "mc_finished": mc_finished,
            "mc_mode": mc_mode,
            "modified": mtime,
            "abortable": entry in _running_processes,
        })
    runs.sort(key=lambda r: r["modified"], reverse=True)
    return runs


def run_detail(run_root, run_id):
    """Detailed info for a single run."""
    run_path = os.path.join(run_root, run_id)
    if not os.path.isdir(run_path):
        return None
    log_dir = os.path.join(run_path, "log")
    exp_log = os.path.join(log_dir, "experiment.log")
    exp_entries = _parse_log_lines(exp_log) if os.path.isfile(exp_log) else []
    exp_sev = _severity_counts(exp_entries)
    params = _parse_user_xml(run_path)

    mc_logs = sorted(glob.glob(os.path.join(log_dir, "mc_*.log")))
    mc_runs = []
    for ml in mc_logs:
        mc_name = os.path.splitext(os.path.basename(ml))[0].replace("mc_", "")
        entries = _parse_log_lines(ml)
        sev = _severity_counts(entries)
        initialized, done, current = _extract_mc_progress(entries)
        mc_status, mc_elapsed = "running", ""
        for e in reversed(entries):
            if "MC run finished" in e["msg"]:
                mc_status = "finished"
            if "MC run completed with errors" in e["msg"]:
                mc_status = "error"
            if "MC run completed with warnings" in e["msg"] and mc_status != "error":
                mc_status = "warning"
            if e["msg"].startswith("Elapsed time:") and mc_status in ("finished", "error", "warning"):
                mc_elapsed = e["msg"][len("Elapsed time:"):].strip()
                break
        total = len(initialized) if initialized else 1
        mc_runs.append({
            "name": mc_name,
            "status": mc_status,
            "elapsed": mc_elapsed,
            "initialized": initialized,
            "components_done": done,
            "current_component": current,
            "progress": round(len(done) / total, 4),
            "severity_counts": sev,
        })

    status, elapsed = "unknown", ""
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
        "abortable": run_id in _running_processes,
    }


def tail_log(run_root, run_id, log_name, tail_lines=200):
    """Return the last *tail_lines* parsed entries of a log file."""
    log_name = os.path.basename(log_name)
    log_path = os.path.join(run_root, run_id, "log", log_name)
    if not os.path.isfile(log_path):
        return {"error": f"Log not found: {log_name}"}
    entries = _parse_log_lines(log_path, tail=tail_lines * 2)
    return {"entries": entries[-tail_lines:], "total_lines": _count_lines(log_path)}


# ═══════════════════════════════════════════════════════════════════
# HTTP handler
# ═══════════════════════════════════════════════════════════════════

class ControlPanelHandler(SimpleHTTPRequestHandler):
    """Serves the integrated control-panel SPA and all JSON APIs."""

    run_root = DEFAULT_RUN_DIR

    def __init__(self, *args, **kwargs):
        self._webdir = os.path.dirname(os.path.abspath(__file__))
        super().__init__(*args, directory=self._webdir, **kwargs)

    # ── helpers ────────────────────────────────────────────────────

    def _json_response(self, obj, status=200):
        payload = json.dumps(obj, ensure_ascii=False).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(payload)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(payload)

    def _read_json_body(self):
        length = int(self.headers.get("Content-Length", 0))
        if length == 0:
            return None
        return json.loads(self.rfile.read(length).decode())

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

    def _query_params(self):
        qs = {}
        if "?" in self.path:
            for pair in self.path.split("?", 1)[1].split("&"):
                if "=" in pair:
                    k, v = pair.split("=", 1)
                    qs[k] = v
        return qs

    def log_message(self, fmt, *args):
        pass  # suppress per-request logging

    # ── GET routes ────────────────────────────────────────────────

    def do_GET(self):
        path = self.path.split("?")[0]

        if path in ("/", "/index.html"):
            self._serve_file("index.html", "text/html")

        # -- parameterisation --
        elif path == "/api/template":
            self._json_response(parse_xrun_template(TEMPLATE_PATH))
        elif path == "/api/scenarios":
            self._json_response(get_scenarios())
        elif path == "/api/scenario-extent":
            scenario_path = self._query_params().get("path", "")
            self._json_response(get_scenario_extent(scenario_path))

        # -- monitoring --
        elif path == "/api/runs":
            self._json_response(discover_runs(self.run_root))
        elif path == "/api/analysis/default-run-dir":
            self._json_response({"run_dir": self.run_root})
        elif path.startswith("/api/runs/") and "/log/" in path:
            parts = path.split("/")
            run_id = parts[3]
            log_name = parts[5] if len(parts) > 5 else "experiment.log"
            tail = int(self._query_params().get("tail", 200))
            self._json_response(tail_log(self.run_root, run_id, log_name, tail))
        elif path.startswith("/api/runs/"):
            run_id = path.split("/")[3]
            detail = run_detail(self.run_root, run_id)
            self._json_response(detail if detail else {"error": "not found"})

        # -- analysis --
        elif path == "/api/analysis/runs" or self.path.startswith("/api/analysis/runs?"):
            qs = parse_qs(urlparse(self.path).query)
            run_root_raw = qs.get("run_root", [""])[0].strip()
            run_root = os.path.abspath(run_root_raw) if run_root_raw else self.run_root
            self._json_response(list_runs_with_mcs(run_root))
        elif path.startswith("/api/analysis/status/"):
            job_id = path.rstrip("/").split("/")[4]
            self._json_response(analysis_job_status(job_id))
        elif path.startswith("/api/analysis/outputs/"):
            job_id = path.rstrip("/").split("/")[4]
            self._json_response(analysis_job_outputs(job_id))
        elif path.startswith("/api/analysis/table/"):
            job_id = path.rstrip("/").split("/")[4]
            with _analysis_lock:
                job = _analysis_jobs.get(job_id)
            if job is None:
                return self._json_response({"error": "Job not found"}, 404)
            tbl = os.path.join(job["output_dir"], "pecsw_table.json")
            if not os.path.isfile(tbl):
                return self._json_response({"error": "Table not yet available"}, 404)
            with open(tbl, encoding="utf-8") as fh:
                self._json_response(json.load(fh))
        elif path.startswith("/api/analysis/file/"):
            # /api/analysis/file/<job_id>/<filename>
            p = path.split("/", 5)
            job_id   = p[4] if len(p) > 4 else ""
            filename = p[5] if len(p) > 5 else ""
            self._serve_analysis_file(job_id, filename)

        else:
            super().do_GET()

    # ── POST routes ───────────────────────────────────────────────

    def do_POST(self):
        path = self.path
        try:
            if path == "/api/xrun-files":
                self._handle_xrun_files()
            elif path == "/api/run":
                self._handle_run()
            elif path == "/api/save":
                self._handle_save()
            elif path == "/api/save-as":
                self._handle_save_as()
            elif path == "/api/open-xrun":
                self._handle_open_xrun()
            elif path.startswith("/api/runs/") and path.endswith("/abort"):
                self._handle_abort(path)
            elif path.startswith("/api/runs/") and path.endswith("/delete"):
                self._handle_delete(path)
            elif path == "/api/analysis/start":
                self._handle_analysis_start()
            else:
                self._json_response({"status": "error", "message": f"Unknown endpoint: {path}"}, 404)
        except Exception as exc:
            self._json_response({"status": "error", "message": str(exc)}, 500)

    # ── POST handlers ─────────────────────────────────────────────

    def _handle_xrun_files(self):
        data = self._read_json_body()
        if not data or not data.get("path", "").strip():
            return self._json_response(
                {"status": "error", "message": "Path not provided"}, 400
            )
        files = get_available_xrun_files(data["path"].strip())
        self._json_response({"status": "success", "files": files, "count": len(files)})

    def _handle_run(self):
        params = self._read_json_body()
        sim_id = params.get(
            "Control/ExperimentID", params.get("ExperimentID", "Simulation")
        )
        output_filename = f"{sim_id}.xrun"
        output_path = os.path.join(OUTPUT_DIR, output_filename)
        create_xrun_file(params, output_path, TEMPLATE_PATH)

        def _launch():
            try:
                proc = subprocess.Popen(
                    [START_BAT, output_path],
                    cwd=BASE_DIR,
                    creationflags=subprocess.CREATE_NEW_CONSOLE,
                )
                with _proc_lock:
                    _running_processes[sim_id] = proc
            except Exception as e:
                print(f"Error starting simulation: {e}")

        threading.Thread(target=_launch, daemon=True).start()
        self._json_response({
            "status": "success",
            "message": f"Model started with configuration: {output_filename}",
            "experiment_id": sim_id,
            "xrun_path": output_path,
        })

    def _handle_save(self):
        data = self._read_json_body()
        params = data.get("parameters", data)
        if "path" in data and "filename" in data:
            fn = data["filename"]
            if not fn.endswith(".xrun"):
                fn += ".xrun"
            output = os.path.join(os.path.abspath(data["path"]), fn)
        else:
            sid = params.get("Control/ExperimentID", "Simulation")
            output = os.path.join(OUTPUT_DIR, f"{sid}.xrun")
        create_xrun_file(params, output, TEMPLATE_PATH)
        self._json_response({
            "status": "success",
            "message": f"Configuration saved to: {os.path.basename(output)}",
            "xrun_path": output,
        })

    def _handle_save_as(self):
        data = self._read_json_body()
        fn = data.get("filename", "configuration")
        if not fn.endswith(".xrun"):
            fn += ".xrun"
        save_dir = os.path.abspath(data.get("path", OUTPUT_DIR))
        os.makedirs(save_dir, exist_ok=True)
        output = os.path.join(save_dir, fn)
        create_xrun_file(data.get("parameters", {}), output, TEMPLATE_PATH)
        self._json_response({
            "status": "success",
            "message": f"Configuration saved as: {fn}",
            "filename": fn,
            "xrun_path": output,
        })

    def _handle_abort(self, path):
        """Kill a running simulation's entire process tree."""
        # path = /api/runs/<id>/abort
        parts = path.strip("/").split("/")
        run_id = parts[2] if len(parts) >= 4 else None
        if not run_id:
            return self._json_response(
                {"status": "error", "message": "Missing run ID"}, 400
            )

        proc = None
        with _proc_lock:
            proc = _running_processes.get(run_id)

        if proc is None:
            return self._json_response(
                {"status": "error",
                 "message": f"No tracked process for '{run_id}'. "
                            "Only simulations launched from this server session can be aborted."},
                404,
            )

        pid = proc.pid
        try:
            # Windows: kill the entire process tree (bat → python → children)
            subprocess.run(
                ["taskkill", "/F", "/T", "/PID", str(pid)],
                capture_output=True,
                timeout=10,
            )
            with _proc_lock:
                _running_processes.pop(run_id, None)
            print(f"Aborted simulation '{run_id}' (PID {pid})")
            return self._json_response({
                "status": "success",
                "message": f"Simulation '{run_id}' aborted (PID {pid})",
            })
        except Exception as exc:
            return self._json_response(
                {"status": "error", "message": f"Failed to kill process: {exc}"},
                500,
            )

    def _handle_delete(self, path):
        """Permanently delete a simulation run folder (aborting it first if still running)."""
        # path = /api/runs/<id>/delete
        parts = path.strip("/").split("/")
        run_id = parts[2] if len(parts) >= 4 else None
        if not run_id:
            return self._json_response(
                {"status": "error", "message": "Missing run ID"}, 400
            )

        # Security: reject any run_id that contains path separators or traversal sequences
        if any(c in run_id for c in (os.sep, '/', '\\', '..')):
            return self._json_response(
                {"status": "error", "message": "Invalid run ID"}, 400
            )

        run_path = os.path.abspath(os.path.join(self.run_root, run_id))
        run_root_abs = os.path.abspath(self.run_root)
        if not run_path.startswith(run_root_abs + os.sep):
            return self._json_response(
                {"status": "error", "message": "Invalid run ID"}, 400
            )

        if not os.path.isdir(run_path):
            return self._json_response(
                {"status": "error", "message": f"Run '{run_id}' not found"}, 404
            )

        # If the simulation is still tracked as running, abort it first
        with _proc_lock:
            proc = _running_processes.get(run_id)
            if proc is not None and proc.poll() is None:
                try:
                    subprocess.run(
                        ["taskkill", "/F", "/T", "/PID", str(proc.pid)],
                        capture_output=True,
                        timeout=10,
                    )
                except Exception:
                    pass
            _running_processes.pop(run_id, None)

        try:
            shutil.rmtree(run_path)
            print(f"Deleted run folder: {run_path}")
            return self._json_response({
                "status": "success",
                "message": f"Run '{run_id}' deleted.",
            })
        except Exception as exc:
            return self._json_response(
                {"status": "error", "message": f"Failed to delete run: {exc}"},
                500,
            )

    def _handle_analysis_start(self):
        """Launch run_basic_analysis.py as a subprocess for a selected MC run."""
        data = self._read_json_body() or {}
        experiment = (data.get("experiment") or "").strip()
        mc_run     = (data.get("mc_run")     or "").strip()
        if not experiment or not mc_run:
            return self._json_response(
                {"status": "error", "message": "experiment and mc_run are required"}, 400)
        for val, name in [(experiment, "experiment"), (mc_run, "mc_run")]:
            if any(c in val for c in (os.sep, "/", "\\", "..")):
                return self._json_response(
                    {"status": "error", "message": f"Invalid {name}"}, 400)
        run_root_raw = (data.get("run_root") or "").strip()
        run_root = os.path.abspath(run_root_raw) if run_root_raw else self.run_root
        mc_path = os.path.abspath(
            os.path.join(run_root, experiment, "mcs", mc_run))
        if not os.path.isdir(mc_path):
            return self._json_response(
                {"status": "error",
                 "message": f"MC run folder not found: {mc_path}"}, 404)
        scenario_rel  = (data.get("scenario_path") or "").strip()
        scenario_path = (os.path.abspath(os.path.join(BASE_DIR, scenario_rel))
                         if scenario_rel else BASE_DIR)
        scenario_name = (data.get("scenario_name") or "").strip()
        ts        = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
        subfolder = f"{experiment}_{mc_run}__{ts}"
        job_id    = subfolder
        out_raw   = (data.get("output_dir") or "").strip()
        base_dir  = os.path.abspath(out_raw) if out_raw else ANALYSIS_OUTPUT_ROOT
        out_dir   = os.path.join(base_dir, subfolder)
        os.makedirs(out_dir, exist_ok=True)
        log_path = os.path.join(out_dir, "analysis.log")
        cmd = [
            ANALYSIS_PYTHON, ANALYSIS_SCRIPT,
            "--mc-path",       mc_path,
            "--scenario-path", scenario_path,
            "--scenario-name", scenario_name,
            "--output-dir",    out_dir,
            "--run-pec",       str(data.get("run_pec",       True)).lower(),
            "--run-guts",      str(data.get("run_guts",      True)).lower(),
            "--exposed-only",  str(data.get("exposed_only",  False)).lower(),
        ]
        for flag, key in [
            ("--reach-ids-single", "reach_ids_single"),
            ("--reach-ids-group",  "reach_ids_group"),
            ("--plotzoom-from",    "plotzoom_from"),
            ("--plotzoom-to",      "plotzoom_to"),
        ]:
            val = (data.get(key) or "").strip()
            if val:
                cmd += [flag, val]
        try:
            with open(log_path, "w", encoding="utf-8") as lf:
                proc = subprocess.Popen(cmd, stdout=lf, stderr=subprocess.STDOUT,
                                        cwd=BASE_DIR)
            with _analysis_lock:
                _analysis_jobs[job_id] = {
                    "proc":       proc,
                    "output_dir": out_dir,
                    "log_path":   log_path,
                    "started_at": datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
                }
            print(f"Analysis started: job={job_id} pid={proc.pid}")
            return self._json_response({
                "status":  "success",
                "job_id":  job_id,
                "message": f"Analysis started (job: {job_id})",
            })
        except Exception as exc:
            return self._json_response(
                {"status": "error",
                 "message": f"Failed to start analysis: {exc}"}, 500)

    def _serve_analysis_file(self, job_id, filename):
        """Serve an output file (image / Excel) from an analysis job."""
        with _analysis_lock:
            job = _analysis_jobs.get(job_id)
        if job is None:
            return self._json_response({"error": "Job not found"}, 404)
        filename  = os.path.basename(filename)
        file_path = os.path.join(job["output_dir"], filename)
        if not os.path.isfile(file_path):
            return self.send_error(404)
        ext = os.path.splitext(filename)[1].lower()
        ct_map = {
            ".png":  "image/png",
            ".jpg":  "image/jpeg",
            ".svg":  "image/svg+xml",
            ".xlsx": ("application/vnd.openxmlformats-officedocument"
                      ".spreadsheetml.sheet"),
        }
        ct = ct_map.get(ext, "application/octet-stream")
        try:
            with open(file_path, "rb") as fh:
                raw = fh.read()
            self.send_response(200)
            self.send_header("Content-Type", ct)
            self.send_header("Content-Length", str(len(raw)))
            if ext not in (".png", ".jpg", ".svg"):
                self.send_header("Content-Disposition",
                                 f'attachment; filename="{filename}"')
            self.end_headers()
            self.wfile.write(raw)
        except (OSError, ConnectionError):
            pass

    def _handle_open_xrun(self):
        data = self._read_json_body()
        xrun_dir = os.path.abspath(data.get("path", BASE_DIR))
        xrun_path = os.path.join(xrun_dir, data.get("filename", ""))
        xrun_path = os.path.abspath(xrun_path)
        if not os.path.isfile(xrun_path) or not xrun_path.endswith(".xrun"):
            return self._json_response(
                {"status": "error", "message": "File not found"}, 404
            )
        tree = ET.parse(xrun_path)
        params = {}

        def extract(element, prefix=""):
            for child in element:
                tag = child.tag.replace("{urn:xAquaticRisk}", "")
                key = f"{prefix}{tag}" if prefix else tag
                if len(child) > 0:
                    extract(child, f"{key}/")
                else:
                    params[key] = (child.text or "").strip()
        extract(tree.getroot())
        self._json_response({
            "status": "success",
            "parameters": params,
            "filename": data.get("filename"),
        })


# ═══════════════════════════════════════════════════════════════════
# Main
# ═══════════════════════════════════════════════════════════════════

def main():
    ap = argparse.ArgumentParser(description="xAquaticRisk Control Panel")
    ap.add_argument("--port", type=int, default=PORT)
    ap.add_argument("--run-dir", default=DEFAULT_RUN_DIR)
    args = ap.parse_args()

    ControlPanelHandler.run_root = os.path.abspath(args.run_dir)

    server = HTTPServer(("0.0.0.0", args.port), ControlPanelHandler)
    print("=" * 60)
    print("  xAquaticRisk Control Panel")
    print("=" * 60)
    print(f"  URL         : http://localhost:{args.port}")
    print(f"  Template    : {TEMPLATE_PATH}")
    print(f"  Run folder  : {ControlPanelHandler.run_root}")
    print(f"  Scenarios   : {os.path.join(BASE_DIR, 'scenario')}")
    print("=" * 60)
    print("  Press Ctrl+C to stop.\n")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down.")
        server.server_close()


if __name__ == "__main__":
    main()
