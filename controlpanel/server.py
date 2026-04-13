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
import atexit
import csv
import datetime
import glob
import multiprocessing
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import threading
import time
import uuid
import xml.etree.ElementTree as ET
from collections import OrderedDict
from http.server import HTTPServer, SimpleHTTPRequestHandler
from pathlib import Path
from typing import Optional
from urllib.parse import urlparse, parse_qs, unquote_plus

try:
    import h5py  # type: ignore
except Exception:  # pragma: no cover - optional runtime dependency
    h5py = None

try:
    import geopandas as gpd  # type: ignore
except Exception:  # pragma: no cover - optional runtime dependency
    gpd = None

try:
    import pandas as pd  # type: ignore
except Exception:  # pragma: no cover - optional runtime dependency
    pd = None

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
CPANEL_DIR = os.path.abspath(os.path.dirname(__file__))
DEFAULT_RUN_DIR = os.path.join(BASE_DIR, "run")
OUTPUT_DIR = os.path.join(BASE_DIR, "parameterisation")
SCENARIO_DIR = os.path.join(BASE_DIR, "scenario")
TEMPLATE_PATH = os.path.join(OUTPUT_DIR, "template.xrun")
START_BAT = os.path.join(BASE_DIR, "__start__.bat")
ANALYSIS_SCRIPT = os.path.join(BASE_DIR, "analysis", "run_basic_analysis.py")
ANALYSIS_OUTPUT_ROOT = os.path.join(BASE_DIR, "analysis_output")
ANALYSIS_RUNTIME_DIR = os.path.join(BASE_DIR, "analysis", "python")
_embedded_analysis_py = os.path.join(ANALYSIS_RUNTIME_DIR, "python.exe")
ANALYSIS_REQUIRED_MODULES = [
    "h5py",
    "numpy",
    "pandas",
    "matplotlib",
    "seaborn",
    "openpyxl",
    "geopandas",
    "pyogrio",
]
PORT = 8090
PARAM_FILE_EXTENSIONS = (".xrun", ".yaml", ".yml")
HYDRO_TIME_FORMAT = "%Y-%m-%dT%H:%M"
HYDRO_ARRAY_DATASETS = ("flow", "depth", "volume", "area")

# Load Step 2 pandas backend for CSV trimming (optional)
step2_backend = None
try:
    import step2_pandas_backend as step2_backend  # type: ignore
except Exception:
    pass  # Step 2 backend is optional

# Track running simulation processes: {experiment_id: subprocess.Popen}
_running_processes = {}
_running_started_at = {}
_proc_lock = threading.Lock()

# Track analysis jobs: {job_id: {"proc", "output_dir", "started_at", "log_path"}}
_analysis_jobs = {}
_analysis_lock = threading.Lock()

# Track scenario subset jobs: {job_id: {running, completed, progress_pct, message, ...}}
_subset_jobs = {}
_subset_lock = threading.Lock()

_map_geometry_cache = OrderedDict()
_map_timeseries_cache = OrderedDict()
_map_cache_lock = threading.Lock()
_MAP_GEOMETRY_CACHE_LIMIT = 8
_MAP_TIMESERIES_CACHE_LIMIT = 32
_MAP_HOURLY_MAX_POINTS = 500000
_INSTANCE_LOCK_PATH = os.path.join(CPANEL_DIR, "server.instance.lock")
_INSTANCE_LOCK_HELD = False


class SubsetJobCancelled(Exception):
    """Raised when a running subset creation job is cancelled by the user."""


def _pid_is_running(pid: int) -> bool:
    if pid <= 0:
        return False
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    except OSError:
        return False
    return True


def _read_instance_lock(path: str) -> dict:
    try:
        with open(path, "r", encoding="utf-8") as handle:
            payload = json.load(handle)
            if isinstance(payload, dict):
                return payload
    except Exception:
        return {}
    return {}


def _release_single_instance_lock():
    global _INSTANCE_LOCK_HELD
    if not _INSTANCE_LOCK_HELD:
        return
    try:
        payload = _read_instance_lock(_INSTANCE_LOCK_PATH)
        # Only remove lock file if we still own it.
        if int(payload.get("pid", -1)) == os.getpid():
            os.remove(_INSTANCE_LOCK_PATH)
    except FileNotFoundError:
        pass
    except Exception:
        pass
    _INSTANCE_LOCK_HELD = False


def _acquire_single_instance_lock(port: int):
    global _INSTANCE_LOCK_HELD
    payload = {
        "pid": os.getpid(),
        "port": int(port),
        "started_at": int(time.time()),
        "python": sys.executable,
        "script": os.path.abspath(__file__),
    }

    for _ in range(2):
        try:
            fd = os.open(_INSTANCE_LOCK_PATH, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
            with os.fdopen(fd, "w", encoding="utf-8") as handle:
                json.dump(payload, handle)
            _INSTANCE_LOCK_HELD = True
            atexit.register(_release_single_instance_lock)
            return
        except FileExistsError:
            existing = _read_instance_lock(_INSTANCE_LOCK_PATH)
            existing_pid = int(existing.get("pid", -1))
            existing_port = existing.get("port", "?")
            if existing_pid > 0 and _pid_is_running(existing_pid):
                raise RuntimeError(
                    "Another controlpanel server is already running "
                    f"(pid={existing_pid}, port={existing_port}). "
                    "Stop that process first or use a different port."
                )
            # Stale lock file from a dead process; remove and retry once.
            try:
                os.remove(_INSTANCE_LOCK_PATH)
            except FileNotFoundError:
                pass

    raise RuntimeError("Could not acquire single-instance lock for controlpanel server")


def _get_server_status_info() -> dict:
    """Return current server instance info for status endpoint."""
    info = {
        "status": "unknown",
        "pid": None,
        "port": None,
        "started_at": None,
        "uptime_seconds": None,
        "alive": False,
        "lock_file": _INSTANCE_LOCK_PATH,
    }
    
    if not os.path.exists(_INSTANCE_LOCK_PATH):
        info["status"] = "not_running"
        return info
    
    try:
        payload = _read_instance_lock(_INSTANCE_LOCK_PATH)
        pid = int(payload.get("pid", -1))
        started_at = int(payload.get("started_at", -1))
        
        info.update({
            "pid": pid,
            "port": payload.get("port"),
            "started_at": started_at,
        })
        
        if pid > 0 and started_at > 0:
            info["alive"] = _pid_is_running(pid)
            info["uptime_seconds"] = int(time.time()) - started_at
            info["status"] = "running" if info["alive"] else "zombie"
        else:
            info["status"] = "invalid_lock"
    except Exception as exc:
        info["status"] = "error"
        info["error"] = str(exc)
    
    return info


def _cache_get(cache: OrderedDict, key):
    with _map_cache_lock:
        value = cache.get(key)
        if value is not None:
            cache.move_to_end(key)
        return value


def _cache_set(cache: OrderedDict, key, value, limit: int):
    with _map_cache_lock:
        cache[key] = value
        cache.move_to_end(key)
        while len(cache) > limit:
            cache.popitem(last=False)


def _union_geometries(geometries):
    if hasattr(geometries, "union_all"):
        return geometries.union_all()
    return geometries.unary_union


def _embedded_json_call(script: str, payload: dict):
    analysis_python = _pick_analysis_python()
    if not analysis_python:
        raise RuntimeError(
            "Map explorer dependencies are unavailable in control panel runtime and embedded analysis runtime is missing"
        )
    cmd = [analysis_python, "-c", script, json.dumps(payload)]
    proc = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        cwd=BASE_DIR,
        env=_analysis_subprocess_env(),
        check=False,
    )
    if proc.returncode != 0:
        detail = (proc.stderr or proc.stdout or "unknown error").strip()
        raise RuntimeError(f"Embedded runtime call failed: {detail}")
    try:
        return json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        raise RuntimeError(f"Embedded runtime returned invalid JSON: {exc}") from exc


def _analysis_subprocess_env() -> dict:
    """Return the environment used for analysis subprocesses."""
    env = os.environ.copy()
    # Isolate analysis subprocesses from parent Python runtime path settings.
    env.pop("PYTHONPATH", None)
    env.pop("PYTHONHOME", None)
    env["PYTHONNOUSERSITE"] = "1"
    return env


def _python_supports_modules(python_exe: str, modules: list[str]) -> bool:
    if not python_exe or not os.path.isfile(python_exe):
        return False
    probe = [
        python_exe,
        "-c",
        (
            "import importlib.util, sys; "
            f"mods={tuple(modules)!r}; "
            "missing=[m for m in mods if importlib.util.find_spec(m) is None]; "
            "sys.exit(1 if missing else 0)"
        ),
    ]
    try:
        result = subprocess.run(
            probe,
            capture_output=True,
            text=True,
            check=False,
            cwd=BASE_DIR,
            env=_analysis_subprocess_env(),
        )
    except OSError:
        return False
    return result.returncode == 0


def _pick_analysis_python() -> Optional[str]:
    """Resolve the preferred Python executable for analysis jobs."""
    candidates = [_embedded_analysis_py]
    if os.environ.get("XAQ_ALLOW_DEV_PYTHON", "").strip().lower() in {"1", "true", "yes"}:
        candidates.extend(
            [
                os.path.join(BASE_DIR, ".venv", "Scripts", "python.exe"),
                sys.executable,
            ]
        )
    for candidate in candidates:
        if _python_supports_modules(candidate, ["h5py", "pandas", "geopandas"]):
            return candidate
    return None


def _analysis_runtime_error() -> Optional[str]:
    """Return a user-facing analysis runtime error, if any."""
    if not os.path.isfile(ANALYSIS_SCRIPT):
        return f"Analysis script not found: {ANALYSIS_SCRIPT}"
    analysis_python = _pick_analysis_python()
    if not analysis_python:
        return (
            "Analysis runtime is missing. Expected embedded Python at "
            f"{_embedded_analysis_py}. Run setup_analysis_python.bat before distributing or copying the model folder."
        )

    probe = [
        analysis_python,
        "-c",
        (
            "import importlib.util; "
            f"mods={tuple(ANALYSIS_REQUIRED_MODULES)!r}; "
            "missing=[m for m in mods if importlib.util.find_spec(m) is None]; "
            "print(','.join(missing))"
        ),
    ]
    try:
        result = subprocess.run(
            probe,
            capture_output=True,
            text=True,
            check=False,
            cwd=BASE_DIR,
            env=_analysis_subprocess_env(),
        )
    except OSError as exc:
        return f"Failed to verify analysis runtime: {exc}"

    missing = (result.stdout or "").strip()
    if result.returncode != 0:
        detail = (result.stderr or missing or "unknown error").strip()
        return f"Failed to verify analysis runtime: {detail}"
    if missing:
        return (
            "Analysis runtime is incomplete: missing Python packages "
            f"[{missing}]. Run setup_analysis_python.bat and include analysis/python in the copied folder."
        )
    return None


# ═══════════════════════════════════════════════════════════════════
# Analysis portable-runtime check
# ═══════════════════════════════════════════════════════════════════

def check_analysis_portable():
    """Check if analysis/python/python.exe exists and all required packages are importable."""
    analysis_py = os.path.join(BASE_DIR, "analysis", "python", "python.exe")
    result = {
        "ready": False,
        "missing_python": not os.path.isfile(analysis_py),
        "missing_packages": [],
        "details": ""
    }
    if result["missing_python"]:
        result["details"] = "analysis/python/python.exe not found. Run setup_analysis_python.bat and bundle the folder."
        return result
    try:
        code = (
            "import importlib.util; mods = %r; "
            "missing = [m for m in mods if not importlib.util.find_spec(m)]; "
            "print(','.join(missing))"
        ) % (ANALYSIS_REQUIRED_MODULES,)
        proc = subprocess.run(
            [analysis_py, "-c", code],
            capture_output=True,
            text=True,
            timeout=30,
            cwd=BASE_DIR,
            env=_analysis_subprocess_env(),
        )
        missing = proc.stdout.strip().split(",") if proc.returncode == 0 else ANALYSIS_REQUIRED_MODULES
        missing = [m for m in missing if m]
        result["missing_packages"] = missing
        result["ready"] = (len(missing) == 0)
        result["details"] = ("All required packages present." if result["ready"]
                             else f"Missing packages: {', '.join(missing)}")
    except Exception as e:
        result["details"] = f"Error checking analysis/python: {e}"
    return result


def get_analysis_python():
    """Return bundled analysis Python interpreter path if available."""
    return _pick_analysis_python()


# ═══════════════════════════════════════════════════════════════════
# Parameterisation helpers  (from webui/server.py)
# ═══════════════════════════════════════════════════════════════════

def get_available_parameter_files(path: str = None) -> list:
    """List .xrun/.yaml files in *path*."""
    if not path:
        path = BASE_DIR
    path = os.path.abspath(path)
    if not os.path.isdir(path):
        return []
    try:
        files = []
        for f in os.listdir(path):
            if os.path.splitext(f)[1].lower() not in PARAM_FILE_EXTENSIONS:
                continue
            full = os.path.join(path, f)
            try:
                mtime = os.path.getmtime(full)
                modified = datetime.datetime.fromtimestamp(mtime).strftime("%Y-%m-%d %H:%M:%S")
            except OSError:
                modified = ""
            files.append({"name": f, "path": full, "modified": modified})
        return sorted(
            files,
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


def _normalize_run_parameters(parameters: dict, output_path: str) -> dict:
    """Return a copy of runtime parameters with file references adjusted for the xrun location."""
    normalized = dict(parameters)
    scenario_key = "Scenario/LandscapeScenario"
    scenario_value = (normalized.get(scenario_key) or "").strip()
    if scenario_value and not os.path.isabs(scenario_value):
        scenario_abs = os.path.normpath(os.path.join(BASE_DIR, scenario_value))
        scenario_rel = os.path.relpath(scenario_abs, os.path.dirname(output_path))
        normalized[scenario_key] = scenario_rel.replace("\\", "/")
    return normalized


def parse_xrun_file(xrun_path: str) -> dict:
    """Parse an xrun file into flat Control/Param keys."""
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
    return params


def _strip_inline_yaml_comment(value: str) -> str:
    in_quote = None
    escaped = False
    for i, ch in enumerate(value):
        if escaped:
            escaped = False
            continue
        if ch == "\\":
            escaped = True
            continue
        if ch in ('"', "'"):
            if in_quote is None:
                in_quote = ch
            elif in_quote == ch:
                in_quote = None
            continue
        if ch == "#" and in_quote is None:
            if i == 0 or value[i - 1].isspace():
                return value[:i].rstrip()
    return value.rstrip()


def _yaml_unquote(value: str) -> str:
    value = value.strip()
    if len(value) >= 2 and value[0] == value[-1] and value[0] in ('"', "'"):
        body = value[1:-1]
        if value[0] == '"':
            return body.replace('\\"', '"').replace('\\\\', '\\')
        return body.replace("''", "'")
    if value.lower() in ("null", "~"):
        return ""
    return value


def parse_yaml_file(yaml_path: str) -> dict:
    """Parse a simple nested YAML parameter file into flat Section/Param keys."""
    params = {}
    current_section = None

    with open(yaml_path, "r", encoding="utf-8") as fh:
        for raw in fh:
            line = raw.rstrip("\r\n")
            stripped = line.strip()
            if not stripped or stripped.startswith("#"):
                continue

            # Section header: SectionName:
            if not line.startswith(" ") and stripped.endswith(":") and stripped.count(":") == 1:
                current_section = stripped[:-1].strip()
                continue

            # Key/value inside section: two-space indentation
            if current_section and line.startswith("  "):
                kv = line.strip()
                if ":" not in kv:
                    continue
                key, value = kv.split(":", 1)
                key = key.strip()
                value = _yaml_unquote(_strip_inline_yaml_comment(value))
                if key:
                    params[f"{current_section}/{key}"] = value

    return params


def parse_parameter_file(file_path: str) -> dict:
    ext = os.path.splitext(file_path)[1].lower()
    if ext == ".xrun":
        return parse_xrun_file(file_path)
    if ext in (".yaml", ".yml"):
        return parse_yaml_file(file_path)
    raise ValueError(f"Unsupported file extension: {ext}")


def _yaml_render_scalar(value: str) -> str:
    text = "" if value is None else str(value)
    if text == "":
        return ""
    if re.fullmatch(r"(?i:true|false)", text):
        return text.lower()
    if re.fullmatch(r"[-+]?\d+(?:\.\d+)?", text):
        return text
    escaped = text.replace("\\", "\\\\").replace('"', '\\"')
    return f'"{escaped}"'


def write_yaml_file(parameters: dict, output_path: str) -> str:
    """Write parameters to a YAML file using section/key structure."""
    sections = OrderedDict()
    for full_key, raw_value in parameters.items():
        if "/" not in full_key:
            continue
        section, key = full_key.split("/", 1)
        if not section or not key:
            continue
        if section not in sections:
            sections[section] = OrderedDict()
        sections[section][key] = "" if raw_value is None else str(raw_value)

    with open(output_path, "w", encoding="utf-8", newline="\n") as fh:
        for section, entries in sections.items():
            fh.write(f"{section}:\n")
            for key, value in entries.items():
                fh.write(f"  {key}: {_yaml_render_scalar(value)}\n")
            fh.write("\n")
    return output_path


def write_parameter_file(parameters: dict, output_path: str) -> str:
    ext = os.path.splitext(output_path)[1].lower()
    if ext == ".xrun":
        return create_xrun_file(parameters, output_path, TEMPLATE_PATH)
    if ext in (".yaml", ".yml"):
        return write_yaml_file(parameters, output_path)
    raise ValueError(f"Unsupported file extension: {ext}")


def normalize_parameter_filename(filename: str, default_ext: str = ".xrun") -> str:
    base = (filename or "").strip()
    if not base:
        raise ValueError("Filename is required")
    ext = os.path.splitext(base)[1].lower()
    if ext:
        if ext not in PARAM_FILE_EXTENSIONS:
            raise ValueError("Unsupported file extension. Use .xrun, .yaml or .yml")
        return base
    return base + default_ext


def get_scenarios() -> list:
    """Return list of scenario directories."""
    if not os.path.isdir(SCENARIO_DIR):
        return []
    return [
        {"name": d, "path": f"scenario/{d}"}
        for d in sorted(os.listdir(SCENARIO_DIR))
        if os.path.isdir(os.path.join(SCENARIO_DIR, d))
    ]


def _strip_xml_ns(tag: str) -> str:
    return re.sub(r"\{.*?\}", "", tag)


def _find_xml_text(root, tag: str) -> str:
    for element in root.iter():
        if _strip_xml_ns(element.tag) == tag:
            return (element.text or "").strip()
    return ""


def _normalize_scenario_rel_path(scenario_path: str) -> str:
    rel = (scenario_path or "").strip().replace("\\", "/")
    rel = rel.lstrip("/")
    if rel.startswith("./"):
        rel = rel[2:]
    if not rel:
        raise ValueError("Scenario path is required")
    if not rel.startswith("scenario/"):
        rel = f"scenario/{rel}"
    return rel.rstrip("/")


def _resolve_scenario_path(scenario_path: str) -> tuple[str, str]:
    rel = _normalize_scenario_rel_path(scenario_path)
    abs_path = os.path.abspath(os.path.join(BASE_DIR, rel))
    if not (abs_path == SCENARIO_DIR or abs_path.startswith(SCENARIO_DIR + os.sep)):
        raise ValueError("Invalid scenario path")
    return rel, abs_path


def _validate_subset_scenario_name(name: str) -> str:
    clean = (name or "").strip()
    if not clean:
        raise ValueError("Target scenario name is required")
    if clean in (".", "..") or "/" in clean or "\\" in clean:
        raise ValueError("Target scenario name must be a single folder name")
    if not re.fullmatch(r"[A-Za-z0-9._-]+", clean):
        raise ValueError("Target scenario name may only contain letters, numbers, dots, hyphens, and underscores")
    return clean


def _scenario_project_display_name(folder_name: str) -> str:
    # scenario.xproject Name only allows letters, numbers and spaces by schema
    text = re.sub(r"[^A-Za-z0-9 ]+", " ", str(folder_name or "").strip())
    text = re.sub(r"\s+", " ", text).strip()
    if not text:
        return "Scenario"
    if len(text) == 1:
        return f"{text} 0"
    return text


def _scenario_project_path(abs_path: str) -> str:
    return os.path.join(abs_path, "scenario.xproject")


def _scenario_readme_path(abs_path: str) -> str:
    return os.path.join(abs_path, "readme.txt")


def _scenario_hydro_path(abs_path: str) -> str:
    return os.path.join(abs_path, "hydro", "hydro_reaches.h5")


def _scenario_timeseries_dir(abs_path: str) -> str:
    return os.path.join(abs_path, "hydro", "TimeSeries")


def _parse_scenario_project_extent(project_path: str) -> dict:
    if not os.path.isfile(project_path):
        return {}
    tree = ET.parse(project_path)
    root = tree.getroot()
    return {
        "from_date": _find_xml_text(root, "FromDate") or None,
        "to_date": _find_xml_text(root, "ToDate") or None,
        "name": _find_xml_text(root, "Name") or None,
    }


def _parse_readme_extent(readme_path: str) -> dict:
    if not os.path.isfile(readme_path):
        return {}
    try:
        with open(readme_path, "r", encoding="utf-8", errors="replace") as handle:
            first_line = handle.readline().strip()
    except OSError:
        return {}
    match = re.search(
        r"(\d{4}-\d{2}-\d{2}T\d{2}:\d{2})\s+to\s+(\d{4}-\d{2}-\d{2}T\d{2}:\d{2})",
        first_line,
    )
    if not match:
        return {}
    return {"from_datetime": match.group(1), "to_datetime": match.group(2), "summary": first_line}


def _parse_hydro_datetime(value):
    if not value:
        return None
    try:
        return datetime.datetime.strptime(value, HYDRO_TIME_FORMAT)
    except ValueError:
        return None


def _format_hydro_datetime(value: datetime.datetime) -> str:
    return value.strftime(HYDRO_TIME_FORMAT)


def _hdf_text_value(dataset) -> Optional[str]:
    if dataset is None:
        return None
    try:
        raw = dataset[0]
    except Exception:
        return None
    if raw is None:
        return None
    if isinstance(raw, bytes):
        return raw.decode("ascii", errors="replace")
    return str(raw)


def _embedded_hydro_inspect(hydro_path: str) -> dict:
    script = r'''
import json
import h5py
import sys

def text_value(dataset):
    if dataset is None:
        return None
    try:
        raw = dataset[0]
    except Exception:
        return None
    if raw is None:
        return None
    if isinstance(raw, bytes):
        return raw.decode("ascii", errors="replace")
    return str(raw)

path = json.loads(sys.argv[1])["hydro_path"]
with h5py.File(path, "r") as handle:
    datasets = {}
    for key in handle.keys():
        obj = handle[key]
        datasets[key] = {
            "shape": list(getattr(obj, "shape", ())),
            "dtype": str(getattr(obj, "dtype", "")),
        }
    result = {
        "datasets": datasets,
        "keys": list(handle.keys()),
        "time_from": text_value(handle.get("time_from")),
        "time_to": text_value(handle.get("time_to")),
        "reach_count": int(handle["reaches"].shape[0]) if "reaches" in handle else 0,
    }
print(json.dumps(result))
'''
    return _embedded_json_call(script, {"hydro_path": hydro_path})


def _read_hydro_metadata(hydro_path: str) -> dict:
    if not os.path.isfile(hydro_path):
        return {}
    if h5py is None:
        return _embedded_hydro_inspect(hydro_path)
    with h5py.File(hydro_path, "r") as handle:
        datasets = {}
        for key in handle.keys():
            obj = handle[key]
            datasets[key] = {
                "shape": list(getattr(obj, "shape", ())),
                "dtype": str(getattr(obj, "dtype", "")),
            }
        return {
            "datasets": datasets,
            "keys": list(handle.keys()),
            "time_from": _hdf_text_value(handle.get("time_from")),
            "time_to": _hdf_text_value(handle.get("time_to")),
            "reach_count": int(handle["reaches"].shape[0]) if "reaches" in handle else 0,
        }


def _compute_effective_hydro_extent(time_from, time_to):
    start_dt = _parse_hydro_datetime(time_from)
    end_dt = _parse_hydro_datetime(time_to)
    if start_dt is None or end_dt is None:
        return {}
    min_from = start_dt.date()
    if start_dt.time() > datetime.time(1, 0):
        min_from += datetime.timedelta(days=1)
    max_to = (end_dt - datetime.timedelta(days=1)).date()
    return {
        "from_date": min_from.isoformat(),
        "to_date": max_to.isoformat(),
        "valid": min_from <= max_to,
    }


def inspect_scenario(scenario_path: str) -> dict:
    try:
        scenario_rel, scenario_abs = _resolve_scenario_path(scenario_path)
    except Exception as exc:
        return {"error": str(exc)}
    try:
        if not os.path.isdir(scenario_abs):
            return {"error": f"Scenario folder not found: {scenario_rel}"}

        project_path = _scenario_project_path(scenario_abs)
        readme_path = _scenario_readme_path(scenario_abs)
        hydro_path = _scenario_hydro_path(scenario_abs)
        inflow_dir = _scenario_timeseries_dir(scenario_abs)
        warnings = []

        if not os.path.isfile(hydro_path):
            warnings.append(f"Hydrology HDF5 file not found: {os.path.relpath(hydro_path, scenario_abs)}")
        if not os.path.isfile(project_path):
            warnings.append(f"Scenario project file not found: {os.path.relpath(project_path, scenario_abs)}")
        if not os.path.isfile(readme_path):
            warnings.append(f"Scenario readme file not found: {os.path.relpath(readme_path, scenario_abs)}")

        project_extent = _parse_scenario_project_extent(project_path) if os.path.isfile(project_path) else {}
        readme_extent = _parse_readme_extent(readme_path) if os.path.isfile(readme_path) else {}
        hydro_meta = _read_hydro_metadata(hydro_path) if os.path.isfile(hydro_path) else {}
        effective_extent = _compute_effective_hydro_extent(
            hydro_meta.get("time_from"),
            hydro_meta.get("time_to"),
        ) if hydro_meta else {}

        if hydro_meta and not effective_extent.get("valid", True):
            warnings.append("Hydrology HDF5 does not contain a full valid day window for simulation dates.")
        if project_extent.get("from_date") and effective_extent.get("from_date") and project_extent.get("from_date") != effective_extent.get("from_date"):
            warnings.append(
                f"scenario.xproject start date {project_extent['from_date']} differs from hydrology-derived start date {effective_extent['from_date']}."
            )
        if project_extent.get("to_date") and effective_extent.get("to_date") and project_extent.get("to_date") != effective_extent.get("to_date"):
            warnings.append(
                f"scenario.xproject end date {project_extent['to_date']} differs from hydrology-derived end date {effective_extent['to_date']}."
            )
        if readme_extent.get("from_datetime") and hydro_meta.get("time_from") and readme_extent.get("from_datetime") != hydro_meta.get("time_from"):
            warnings.append(
                f"readme.txt start datetime {readme_extent['from_datetime']} differs from hydrology HDF5 start datetime {hydro_meta['time_from']}."
            )
        if readme_extent.get("to_datetime") and hydro_meta.get("time_to") and readme_extent.get("to_datetime") != hydro_meta.get("time_to"):
            warnings.append(
                f"readme.txt end datetime {readme_extent['to_datetime']} differs from hydrology HDF5 end datetime {hydro_meta['time_to']}."
            )

        inflow_count = 0
        if os.path.isdir(inflow_dir):
            inflow_count = len([name for name in os.listdir(inflow_dir) if name.lower().endswith(".csv")])

        primary_extent = effective_extent or project_extent
        return {
            "scenario_path": scenario_rel,
            "scenario_name": os.path.basename(scenario_abs),
            "project_extent": {
                "from_date": project_extent.get("from_date"),
                "to_date": project_extent.get("to_date"),
            },
            "readme_extent": readme_extent,
            "hdf_extent": {
                "from_datetime": hydro_meta.get("time_from"),
                "to_datetime": hydro_meta.get("time_to"),
                "reach_count": hydro_meta.get("reach_count", 0),
                "datasets": hydro_meta.get("datasets", {}),
            },
            "effective_extent": {
                "from_date": primary_extent.get("from_date"),
                "to_date": primary_extent.get("to_date"),
                "valid": primary_extent.get("valid", True),
            },
            "from_date": primary_extent.get("from_date"),
            "to_date": primary_extent.get("to_date"),
            "warnings": warnings,
            "files": {
                "project": os.path.isfile(project_path),
                "hydro_hdf": os.path.isfile(hydro_path),
                "timeseries_dir": os.path.isdir(inflow_dir),
                "timeseries_csv_count": inflow_count,
                "readme": os.path.isfile(readme_path),
            },
        }
    except Exception as exc:
        return {"error": f"Scenario inspection failed: {exc}"}


def get_scenario_extent(scenario_path: str) -> dict:
    """Return the best available scenario extent, preferring hydrology-derived bounds."""
    inspected = inspect_scenario(scenario_path)
    if inspected.get("error"):
        return inspected
    return {
        "from_date": inspected.get("from_date"),
        "to_date": inspected.get("to_date"),
        "project_extent": inspected.get("project_extent", {}),
        "hdf_extent": inspected.get("hdf_extent", {}),
        "effective_extent": inspected.get("effective_extent", {}),
        "warnings": inspected.get("warnings", []),
    }


def _parse_iso_date(value: str, field_name: str):
    try:
        return datetime.date.fromisoformat((value or "").strip())
    except ValueError as exc:
        raise ValueError(f"{field_name} must use format YYYY-MM-DD") from exc


def _validate_subset_window(inspected: dict, start_date: datetime.date, end_date: datetime.date):
    if end_date < start_date:
        raise ValueError("Subset end date must be greater than or equal to subset start date")
    effective = inspected.get("effective_extent", {})
    allowed_from = effective.get("from_date")
    allowed_to = effective.get("to_date")
    if not allowed_from or not allowed_to:
        raise ValueError("Scenario does not expose a valid hydrology time window")
    min_date = datetime.date.fromisoformat(allowed_from)
    max_date = datetime.date.fromisoformat(allowed_to)
    if start_date < min_date or end_date > max_date:
        raise ValueError(
            f"Selected subset window {start_date.isoformat()} to {end_date.isoformat()} is outside the valid range {allowed_from} to {allowed_to}"
        )


def _copy_scenario_template(source_abs: str, target_abs: str, progress_cb=None, pct_from: int = 0, pct_to: int = 100, cancel_cb=None):
    manifest = []
    for root_dir, dirs, files in os.walk(source_abs):
        if cancel_cb and cancel_cb():
            raise SubsetJobCancelled("Subset creation was cancelled during template scan")
        rel_dir = os.path.relpath(root_dir, source_abs).replace("\\", "/")
        if rel_dir == ".":
            rel_dir = ""
        if rel_dir == "hydro":
            dirs[:] = [d for d in dirs if d != "TimeSeries"]
            files = [f for f in files if f != "hydro_reaches.h5"]
        for fname in files:
            src_file = os.path.join(root_dir, fname)
            rel_file = os.path.relpath(src_file, source_abs)
            try:
                size = os.path.getsize(src_file)
            except OSError:
                size = 0
            manifest.append((src_file, rel_file, size))

    os.makedirs(target_abs, exist_ok=True)
    if not manifest:
        if progress_cb:
            progress_cb(pct_to, "Scenario template copy complete")
        return

    total_bytes = sum(max(item[2], 1) for item in manifest)
    copied_bytes = 0
    total_files = len(manifest)
    for idx, (src_file, rel_file, size) in enumerate(manifest, start=1):
        if cancel_cb and cancel_cb():
            raise SubsetJobCancelled("Subset creation was cancelled during scenario copy")
        dst_file = os.path.join(target_abs, rel_file)
        os.makedirs(os.path.dirname(dst_file), exist_ok=True)
        shutil.copy2(src_file, dst_file)
        copied_bytes += max(size, 1)
        if progress_cb and (idx == 1 or idx == total_files or idx % 10 == 0):
            frac = copied_bytes / total_bytes if total_bytes else 1.0
            pct = int(pct_from + (pct_to - pct_from) * frac)
            progress_cb(pct, f"Copying scenario files ({idx}/{total_files})")


def _slice_hydrology_hdf_embedded(source_hdf: str, target_hdf: str, start_iso: str, end_iso: str, selected_reach_ids=None):
    script = r'''
import datetime
import json
import os
import h5py
import sys

FMT = "%Y-%m-%dT%H:%M"
payload = json.loads(sys.argv[1])
source_hdf = payload["source_hdf"]
target_hdf = payload["target_hdf"]
start_dt = datetime.datetime.strptime(payload["start_iso"], FMT)
end_dt = datetime.datetime.strptime(payload["end_iso"], FMT)
selected = set(str(v) for v in payload.get("selected_reach_ids", []) if str(v).strip())

os.makedirs(os.path.dirname(target_hdf), exist_ok=True)
with h5py.File(source_hdf, "r") as src, h5py.File(target_hdf, "w") as dst:
    src_start = datetime.datetime.strptime(src["time_from"][0].decode("ascii"), FMT)
    src_end = datetime.datetime.strptime(src["time_to"][0].decode("ascii"), FMT)
    if start_dt < src_start or end_dt > src_end:
        raise ValueError(f"Subset range {start_dt} to {end_dt} is outside source hydrology bounds {src_start} to {src_end}")
    start_idx = int((start_dt - src_start).total_seconds() // 3600)
    end_idx = int((end_dt - src_start).total_seconds() // 3600)
    reach_vals = src["reaches"][:]
    if selected:
        selected_idx = [i for i, v in enumerate(reach_vals) if str(int(v)) in selected]
        if not selected_idx:
            raise ValueError("None of the selected reaches exist in scenario hydrology")
        reaches_out = reach_vals[selected_idx]
    else:
        selected_idx = None
        reaches_out = reach_vals
    for name in ("flow", "depth", "volume", "area"):
        ds = src[name]
        if selected_idx is None:
            sliced = ds[start_idx:end_idx + 1, :]
        else:
            sliced = ds[start_idx:end_idx + 1, selected_idx]
        dst.create_dataset(name, data=sliced, dtype=ds.dtype)
    dst.create_dataset("reaches", data=reaches_out, dtype=src["reaches"].dtype)
    dst.create_dataset("time_from", data=[payload["start_iso"].encode("ascii")])
    dst.create_dataset("time_to", data=[payload["end_iso"].encode("ascii")])

print(json.dumps({"status": "success", "time_steps": end_idx - start_idx + 1}))
'''
    return _embedded_json_call(
        script,
        {
            "source_hdf": source_hdf,
            "target_hdf": target_hdf,
            "start_iso": start_iso,
            "end_iso": end_iso,
            "selected_reach_ids": _coerce_list_of_reach_ids(selected_reach_ids or []),
        },
    )


def _slice_hydrology_hdf(source_hdf: str, target_hdf: str, start_dt: datetime.datetime, end_dt: datetime.datetime, progress_cb=None, pct_from: int = 0, pct_to: int = 100, cancel_cb=None, selected_reach_ids=None):
    start_iso = _format_hydro_datetime(start_dt)
    end_iso = _format_hydro_datetime(end_dt)
    if h5py is None:
        if progress_cb:
            progress_cb(pct_from, "Slicing hydrology HDF5 (embedded runtime)")
        if cancel_cb and cancel_cb():
            raise SubsetJobCancelled("Subset creation was cancelled before HDF5 slicing")
        result = _slice_hydrology_hdf_embedded(source_hdf, target_hdf, start_iso, end_iso, selected_reach_ids=selected_reach_ids)
        if progress_cb:
            progress_cb(pct_to, "Hydrology HDF5 slicing complete")
        return result

    os.makedirs(os.path.dirname(target_hdf), exist_ok=True)
    with h5py.File(source_hdf, "r") as src, h5py.File(target_hdf, "w") as dst:
        if cancel_cb and cancel_cb():
            raise SubsetJobCancelled("Subset creation was cancelled before HDF5 slicing")
        src_start = _parse_hydro_datetime(src["time_from"][0].decode("ascii"))
        src_end = _parse_hydro_datetime(src["time_to"][0].decode("ascii"))
        if src_start is None or src_end is None:
            raise ValueError("Source hydrology file has invalid time_from/time_to metadata")
        if start_dt < src_start or end_dt > src_end:
            raise ValueError(
                f"Subset range {start_iso} to {end_iso} is outside source hydrology bounds {_format_hydro_datetime(src_start)} to {_format_hydro_datetime(src_end)}"
            )
        start_idx = int((start_dt - src_start).total_seconds() // 3600)
        end_idx = int((end_dt - src_start).total_seconds() // 3600)
        slice_rows = end_idx - start_idx + 1
        src_reaches = src["reaches"][:]
        selected_indices = None
        if selected_reach_ids:
            selected_set = set(_coerce_list_of_reach_ids(selected_reach_ids))
            src_reach_ids = [_normalize_reach_id(v) for v in src_reaches]
            selected_indices = [idx for idx, rid in enumerate(src_reach_ids) if rid in selected_set]
            if not selected_indices:
                raise ValueError("None of the selected reaches exist in scenario hydrology")
            selected_reaches_raw = src_reaches[selected_indices]
        else:
            selected_reaches_raw = src_reaches
        total_units = max(slice_rows * len(HYDRO_ARRAY_DATASETS), 1)
        done_units = 0
        for name in HYDRO_ARRAY_DATASETS:
            ds = src[name]
            out_cols = len(selected_reaches_raw)
            out = dst.create_dataset(name, shape=(slice_rows, out_cols), dtype=ds.dtype)
            chunk_rows = min(2048, max(64, slice_rows // 25 or 64))
            for row0 in range(0, slice_rows, chunk_rows):
                if cancel_cb and cancel_cb():
                    raise SubsetJobCancelled("Subset creation was cancelled during HDF5 slicing")
                row1 = min(slice_rows, row0 + chunk_rows)
                if selected_indices is None:
                    out[row0:row1, :] = ds[start_idx + row0:start_idx + row1, :]
                else:
                    out[row0:row1, :] = ds[start_idx + row0:start_idx + row1, selected_indices]
                done_units += (row1 - row0)
                if progress_cb:
                    frac = done_units / total_units
                    pct = int(pct_from + (pct_to - pct_from) * frac)
                    progress_cb(pct, f"Slicing hydrology dataset {name} ({row1}/{slice_rows} rows)")
        dst.create_dataset("reaches", data=selected_reaches_raw, dtype=src["reaches"].dtype)
        dst.create_dataset("time_from", data=[start_iso.encode("ascii")])
        dst.create_dataset("time_to", data=[end_iso.encode("ascii")])
    if progress_cb:
        progress_cb(pct_to, "Hydrology HDF5 slicing complete")
    return {"status": "success", "time_steps": end_idx - start_idx + 1}


def _slice_timeseries_csv(source_csv: str, target_csv: str, start_dt: datetime.datetime, end_dt: datetime.datetime, cancel_cb=None, selected_reach_ids=None):
    # Try pandas backend (step 2) first if available
    if step2_backend and hasattr(step2_backend, "_slice_timeseries_csv_pandas") and step2_backend.pd:
        try:
            kept = step2_backend._slice_timeseries_csv_pandas(source_csv, target_csv, start_dt, end_dt, cancel_cb, selected_reach_ids)
            if kept is not None:
                return kept
        except Exception:
            pass  # Fallback to CSV backend
    
    # CSV backend (original - stable fallback)
    os.makedirs(os.path.dirname(target_csv), exist_ok=True)
    kept_rows = 0
    processed_rows = 0
    selected_set = set(_coerce_list_of_reach_ids(selected_reach_ids)) if selected_reach_ids else None
    start_key = _format_hydro_datetime(start_dt)
    end_key = _format_hydro_datetime(end_dt)
    ts_cache = {}
    reach_cache = {}

    def _is_plausible_hydro_timestamp(value: str) -> bool:
        return (
            len(value) == 16
            and value[4] == "-"
            and value[7] == "-"
            and value[10] == "T"
            and value[13] == ":"
            and value[0:4].isdigit()
            and value[5:7].isdigit()
            and value[8:10].isdigit()
            and value[11:13].isdigit()
            and value[14:16].isdigit()
        )

    with open(source_csv, "r", encoding="utf-8", newline="", buffering=1024 * 1024) as src_handle, open(target_csv, "w", encoding="utf-8", newline="", buffering=1024 * 1024) as dst_handle:
        reader = csv.reader(src_handle)
        writer = csv.writer(dst_handle)
        header = next(reader, None)
        if header:
            writer.writerow(header)
        for row in reader:
            processed_rows += 1
            if cancel_cb and processed_rows % 5000 == 0 and cancel_cb():
                raise SubsetJobCancelled("Subset creation was cancelled during inflow CSV trimming")
            if len(row) < 3:
                continue
            row_ts = row[1].strip()
            ts_ok = ts_cache.get(row_ts)
            if ts_ok is None:
                # Fast lexical pre-filter, then strict parse once per unique timestamp.
                if not _is_plausible_hydro_timestamp(row_ts) or row_ts < start_key or row_ts > end_key:
                    ts_ok = False
                else:
                    try:
                        datetime.datetime.strptime(row_ts, HYDRO_TIME_FORMAT)
                        ts_ok = True
                    except ValueError:
                        ts_ok = False
                ts_cache[row_ts] = ts_ok
            if not ts_ok:
                continue

            raw_reach = row[0]
            row_reach = reach_cache.get(raw_reach)
            if raw_reach not in reach_cache:
                row_reach = _normalize_reach_id(raw_reach)
                reach_cache[raw_reach] = row_reach
            reach_ok = (selected_set is None) or (row_reach in selected_set)
            if reach_ok:
                writer.writerow(row)
                kept_rows += 1
    return kept_rows


def _update_scenario_project_for_subset(project_path: str, target_name: str, start_date: datetime.date, end_date: datetime.date):
    if not os.path.isfile(project_path):
        return
    ET.register_namespace("", "urn:xLandscapeModelScenarioInfo")
    ET.register_namespace("xsi", "http://www.w3.org/2001/XMLSchema-instance")
    tree = ET.parse(project_path)
    root = tree.getroot()
    display_name = _scenario_project_display_name(target_name)
    for element in root.iter():
        tag = _strip_xml_ns(element.tag)
        if tag == "Name":
            element.text = display_name
        elif tag == "FromDate":
            element.text = start_date.isoformat()
        elif tag == "ToDate":
            element.text = end_date.isoformat()
    tree.write(project_path, encoding="utf-8", xml_declaration=True)


def _rewrite_subset_readme(readme_path, source_rel, start_date, end_date, subset_hdf_from, subset_hdf_to, source_hdf_from, source_hdf_to):
    existing_lines = []
    if os.path.isfile(readme_path):
        try:
            with open(readme_path, "r", encoding="utf-8", errors="replace") as handle:
                existing_lines = handle.read().splitlines()
        except OSError:
            existing_lines = []
    if existing_lines:
        existing_lines[0] = f"Timeseries: {subset_hdf_from} to {subset_hdf_to}"
    else:
        existing_lines = [f"Timeseries: {subset_hdf_from} to {subset_hdf_to}"]

    note_lines = [
        "",
        "Subset scenario note:",
        f"Source scenario: {source_rel}",
        f"Subset simulation period: {start_date.isoformat()} to {end_date.isoformat()}",
    ]
    if source_hdf_from and source_hdf_to:
        note_lines.append(f"Source hydrology HDF5 coverage: {source_hdf_from} to {source_hdf_to}")
    note_lines.append("Derived hydro/doc statistics were copied from the source scenario and may not reflect the subset period.")
    with open(readme_path, "w", encoding="utf-8", newline="\n") as handle:
        handle.write("\n".join(existing_lines + note_lines) + "\n")


def _expand_reach_selection_downstream(scenario_abs: str, selected_reach_ids: list) -> tuple:
    """
    Given a set of selected reach IDs, returns a topologically-closed superset
    by walking each reach's 'downstream' link until reaching 'Outlet' or a
    reach already in the set.  The scenario shapefile's 'key' and 'downstream'
    columns drive the traversal.

    Returns (expanded_ids, added_ids) where both are lists of normalised
    reach-ID strings.  If the shapefile is unavailable the original list is
    returned unchanged.
    """
    shp_path = _find_reach_shapefile(scenario_abs)
    if not shp_path or not os.path.isfile(shp_path):
        return list(selected_reach_ids), []

    # Build a key -> downstream map from the shapefile.
    # Works whether geopandas is available locally or not.
    def _build_topology(shp_path):
        """Returns dict {norm_key: norm_downstream_or_None}."""
        if gpd is not None:
            gdf = gpd.read_file(shp_path)
            topo = {}
            for _, row in gdf.iterrows():
                k = _normalize_reach_id(row.get("key"))
                d = _normalize_reach_id(row.get("downstream"))
                if k is not None:
                    # "Outlet" stays as None (no further downstream)
                    topo[k] = d if (d is not None and d.strip().upper() != "OUTLET") else None
            return topo

        # Fallback: embedded subprocess with analysis Python
        script = r'''
import json, sys
import geopandas as gpd
import re

def norm(v):
    if v is None:
        return None
    s = str(v).strip()
    if not s:
        return None
    try:
        return str(int(float(s)))
    except (ValueError, OverflowError):
        return s

payload = json.loads(sys.argv[1])
gdf = gpd.read_file(payload["shp_path"])
topo = {}
for _, row in gdf.iterrows():
    k = norm(row.get("key"))
    d = norm(row.get("downstream"))
    if k is not None:
        topo[k] = d if (d is not None and d.upper() != "OUTLET") else None
print(json.dumps({"topo": topo}))
'''
        result = _embedded_json_call(script, {"shp_path": shp_path})
        return result.get("topo", {})

    try:
        topo = _build_topology(shp_path)
    except Exception:
        return list(selected_reach_ids), []

    # Walk downstream from every selected reach and collect all reached nodes
    all_reach_keys = set(topo.keys())
    selected_set = set(selected_reach_ids)
    added = set()

    for reach_id in list(selected_set):
        current = topo.get(reach_id)
        while current is not None and current not in selected_set and current not in added:
            if current not in all_reach_keys:
                break  # dangling reference – stop walking
            added.add(current)
            current = topo.get(current)

    expanded = list(selected_set | added)
    return expanded, list(added)


def _slice_lulc_shapefile_for_subset(scenario_abs: str, selected_reach_ids: list, max_distance: int = 100):
    """Filter LULC shapefile to features within specified distance of selected reaches."""
    if not selected_reach_ids or max_distance < 0:
        return
    
    # Find both shapefiles
    reach_shp_path = _find_reach_shapefile(scenario_abs)
    if not reach_shp_path or not os.path.isfile(reach_shp_path):
        return
    
    lulc_dir = os.path.join(scenario_abs, "geo")
    lulc_path = os.path.join(lulc_dir, "LULC.shp")
    if not os.path.isfile(lulc_path):
        return
    
    selected = set(_coerce_list_of_reach_ids(selected_reach_ids))
    
    if gpd is not None:
        # Read reach geometries
        reach_gdf = gpd.read_file(reach_shp_path)
        if reach_gdf.empty:
            return
        
        reach_col = _select_reach_id_column(reach_gdf, selected)
        if reach_col is None:
            return
        
        reach_gdf["__reach_id__"] = reach_gdf[reach_col].map(_normalize_reach_id)
        selected_reaches_gdf = reach_gdf[reach_gdf["__reach_id__"].isin(selected)].copy()
        if selected_reaches_gdf.empty:
            return
        
        # Create buffered geometry union of selected reaches
        buffered_geom = _union_geometries(selected_reaches_gdf.geometry).buffer(max_distance)
        
        # Read LULC features
        lulc_gdf = gpd.read_file(lulc_path)
        if lulc_gdf.empty:
            return
        
        # Filter LULC features that intersect or are within distance of buffered reach geometry
        lulc_filtered = lulc_gdf[lulc_gdf.geometry.intersects(buffered_geom)].copy()
        if lulc_filtered.empty:
            # If no features found, keep empty to signal filtering occurred
            pass
        
        # Write filtered LULC shapefile
        base, _ = os.path.splitext(lulc_path)
        for ext in (".shp", ".shx", ".dbf", ".prj", ".cpg", ".qix", ".fix"):
            try:
                os.remove(base + ext)
            except FileNotFoundError:
                pass
        
        lulc_filtered.to_file(lulc_path)
        return
    
    # Fallback to embedded script when geopandas not available
    script = r'''
import json
import os
import sys
import geopandas as gpd

payload = json.loads(sys.argv[1])
reach_shp_path = payload["reach_shp_path"]
lulc_path = payload["lulc_path"]
selected = set(payload["selected"])
max_distance = payload["max_distance"]

reach_gdf = gpd.read_file(reach_shp_path)
if reach_gdf.empty:
    raise RuntimeError("Reach shapefile is empty")

non_geom_reach = [c for c in reach_gdf.columns if c != "geometry"]
preferred = ["key", "reach_id", "reachid", "segment_id", "reach", "name", "id"]
best_col = non_geom_reach[0] if non_geom_reach else None
best_score = -1.0

for col in non_geom_reach:
    vals = [str(v).strip() if v is not None else None for v in reach_gdf[col].head(100) if v is not None]
    if not vals:
        continue
    unique_vals = set(vals)
    matched_unique = len(set(v for v in unique_vals if v in selected))
    if matched_unique == 0:
        continue
    unique_ratio = len(unique_vals) / max(len(vals), 1)
    score = float(matched_unique) + 0.5 * unique_ratio
    lower_col = col.lower()
    if lower_col in preferred:
        score += 5.0
    if score > best_score:
        best_score = score
        best_col = col

if best_col is None:
    raise RuntimeError("Could not identify reach ID column")

reach_gdf["__reach_id__"] = reach_gdf[best_col].astype(str).str.strip()
selected_reaches_gdf = reach_gdf[reach_gdf["__reach_id__"].isin(selected)].copy()
if selected_reaches_gdf.empty:
    raise RuntimeError("No selected reaches found")

buffered_geom = _union_geometries(selected_reaches_gdf.geometry).buffer(max_distance)

lulc_gdf = gpd.read_file(lulc_path)
if lulc_gdf.empty:
    raise RuntimeError("LULC shapefile is empty")

lulc_filtered = lulc_gdf[lulc_gdf.geometry.intersects(buffered_geom)].copy()

base, _ = os.path.splitext(lulc_path)
for ext in (".shp", ".shx", ".dbf", ".prj", ".cpg", ".qix", ".fix"):
    path = base + ext
    if os.path.exists(path):
        os.remove(path)

lulc_filtered.to_file(lulc_path)
print(json.dumps({"status": "ok", "lulc_feature_count": int(len(lulc_filtered))}))
'''
    _embedded_json_call(
        script,
        {
            "reach_shp_path": reach_shp_path,
            "lulc_path": lulc_path,
            "selected": list(selected),
            "max_distance": max_distance,
        }
    )


def _slice_reach_shapefile_for_subset(scenario_abs: str, selected_reach_ids: list):
    """Filter copied reach shapefile so geometry-derived inputs match sliced hydrology reaches."""
    if not selected_reach_ids:
        return
    shp_path = _find_reach_shapefile(scenario_abs)
    if not shp_path or not os.path.isfile(shp_path):
        return

    selected = set(_coerce_list_of_reach_ids(selected_reach_ids))

    if gpd is not None:
        gdf = gpd.read_file(shp_path)
        if gdf.empty:
            raise ValueError("Reach shapefile has no features")
        reach_col = _select_reach_id_column(gdf, selected)
        if reach_col is None:
            raise ValueError("Could not determine reach identifier column in reach shapefile")
        gdf["__reach_id__"] = gdf[reach_col].map(_normalize_reach_id)
        filtered = gdf[gdf["__reach_id__"].isin(selected)].copy()
        if filtered.empty:
            raise ValueError("No selected reaches were found in reach shapefile")
        filtered = filtered.drop(columns=["__reach_id__"])
        base, _ = os.path.splitext(shp_path)
        for ext in (".shp", ".shx", ".dbf", ".prj", ".cpg", ".qix", ".fix"):
            try:
                os.remove(base + ext)
            except FileNotFoundError:
                pass
        filtered.to_file(shp_path)
        return

    script = r'''
import json
import os
import re
import sys
import geopandas as gpd

def norm(value):
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if re.fullmatch(r"[-+]?\d+(?:\.0+)?", text):
        return str(int(float(text)))
    return text

payload = json.loads(sys.argv[1])
shp_path = payload["shp_path"]
selected = set(payload["selected"])
gdf = gpd.read_file(shp_path)
if gdf.empty:
    raise RuntimeError("Reach shapefile has no features")
non_geom = [c for c in gdf.columns if c != "geometry"]
if not non_geom:
    raise RuntimeError("Reach shapefile has no attribute columns")
preferred = ["key", "reach_id", "reachid", "segment_id", "reach", "name", "id"]
best_col = non_geom[0]
best_score = -1.0
for col in non_geom:
    vals = [norm(v) for v in gdf[col].dropna().head(2000)]
    if not vals:
        continue
    unique_vals = set(v for v in vals if v is not None)
    matched_unique = len(set(v for v in unique_vals if v in selected))
    if matched_unique == 0:
        continue
    unique_ratio = len(unique_vals) / max(len(vals), 1)
    score = float(matched_unique) + 0.5 * unique_ratio
    lower_col = col.lower()
    if lower_col in preferred:
        score += 5.0
    elif any(p in lower_col for p in preferred):
        score += 2.0
    if score > best_score:
        best_score = score
        best_col = col

gdf["__reach_id__"] = gdf[best_col].map(norm)
filtered = gdf[gdf["__reach_id__"].isin(selected)].copy()
if filtered.empty:
    raise RuntimeError("No selected reaches were found in reach shapefile")
filtered = filtered.drop(columns=["__reach_id__"])
base, _ = os.path.splitext(shp_path)
for ext in (".shp", ".shx", ".dbf", ".prj", ".cpg", ".qix", ".fix"):
    path = base + ext
    if os.path.exists(path):
        os.remove(path)
filtered.to_file(shp_path)
print(json.dumps({"status": "ok", "feature_count": int(len(filtered))}))
'''
    _embedded_json_call(script, {"shp_path": shp_path, "selected": list(selected)})


def _validate_subset_geometry_contract(scenario_abs: str) -> dict:
    """Validate reach shapefile and hydrology reach-id consistency for a subset scenario."""
    warnings = []
    hdf_path = _scenario_hydro_path(scenario_abs)
    shp_path = _find_reach_shapefile(scenario_abs)

    if not os.path.isfile(hdf_path):
        warnings.append(f"Subset hydrology HDF5 not found: {hdf_path}")
        return {
            "ok": False,
            "warnings": warnings,
            "hydro_path": hdf_path,
            "reach_shapefile": shp_path,
        }

    if not shp_path or not os.path.isfile(shp_path):
        warnings.append("Reach shapefile is missing in subset scenario; geometry-dependent analysis plots may be skipped.")
        return {
            "ok": False,
            "warnings": warnings,
            "hydro_path": hdf_path,
            "reach_shapefile": shp_path,
        }

    script = r'''
import json
import re
import sys
import geopandas as gpd
import h5py

def norm(value):
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if re.fullmatch(r"[-+]?\d+(?:\.0+)?", text):
        return str(int(float(text)))
    return text

payload = json.loads(sys.argv[1])
hdf_path = payload["hdf_path"]
shp_path = payload["shp_path"]

with h5py.File(hdf_path, "r") as hf:
    if "reaches" in hf:
        hydro_ids = [norm(v.decode("utf-8", errors="replace") if isinstance(v, bytes) else v) for v in hf["reaches"][:]]
    else:
        hydro_ids = []
hydro_ids = [v for v in hydro_ids if v is not None]
hydro_set = set(hydro_ids)

gdf = gpd.read_file(shp_path)
if gdf.empty:
    print(json.dumps({
        "ok": False,
        "warnings": ["Reach shapefile has no features"],
        "reach_id_column": None,
        "hydro_reach_count": len(hydro_set),
        "shape_feature_count": 0,
        "shape_reach_id_count": 0,
        "overlap_count": 0,
        "missing_in_shapefile": hydro_ids[:20],
        "missing_in_hydro": [],
    }))
    raise SystemExit(0)

non_geom = [c for c in gdf.columns if c != "geometry"]
preferred = ["key", "reach_id", "reachid", "segment_id", "reach", "name", "id"]
lower_to_col = {c.lower(): c for c in non_geom}

def sample(col_name):
    vals = gdf[col_name].dropna().head(2000)
    return [norm(v) for v in vals]

best_col = None
best_score = -1.0

for low_name in preferred:
    col = lower_to_col.get(low_name)
    if not col:
        continue
    vals = sample(col)
    if not vals:
        continue
    matched = len({v for v in vals if v in hydro_set})
    if matched > best_score:
        best_score = matched
        best_col = col

if best_col is None:
    for col in non_geom:
        vals = sample(col)
        if not vals:
            continue
        unique_vals = {v for v in vals if v is not None}
        matched = len({v for v in unique_vals if v in hydro_set})
        if matched == 0:
            continue
        ratio = len(unique_vals) / max(len(vals), 1)
        score = float(matched) + 0.5 * ratio
        if score > best_score:
            best_score = score
            best_col = col

if best_col is None and non_geom:
    best_col = non_geom[0]

shape_ids = []
if best_col is not None:
    shape_ids = [norm(v) for v in gdf[best_col].tolist()]
shape_ids = [v for v in shape_ids if v is not None]
shape_set = set(shape_ids)

missing_in_shape = sorted(hydro_set - shape_set)
missing_in_hydro = sorted(shape_set - hydro_set)
overlap_count = len(hydro_set & shape_set)

warnings = []
if best_col is None:
    warnings.append("Could not determine reach identifier column in reach shapefile")
if missing_in_shape:
    warnings.append(f"{len(missing_in_shape)} hydrology reaches are missing in reach shapefile")
if missing_in_hydro:
    warnings.append(f"{len(missing_in_hydro)} shapefile reaches are missing in hydrology HDF5")
if overlap_count == 0:
    warnings.append("No overlap between hydrology reaches and reach shapefile IDs")

print(json.dumps({
    "ok": len(warnings) == 0,
    "warnings": warnings,
    "reach_id_column": best_col,
    "hydro_reach_count": len(hydro_set),
    "shape_feature_count": int(len(gdf)),
    "shape_reach_id_count": len(shape_set),
    "overlap_count": overlap_count,
    "missing_in_shapefile": missing_in_shape[:20],
    "missing_in_hydro": missing_in_hydro[:20],
}))
'''

    try:
        result = _embedded_json_call(script, {"hdf_path": hdf_path, "shp_path": shp_path})
    except Exception as exc:
        warnings.append(f"Geometry contract validation failed: {exc}")
        return {
            "ok": False,
            "warnings": warnings,
            "hydro_path": hdf_path,
            "reach_shapefile": shp_path,
        }

    result.setdefault("warnings", [])
    result["hydro_path"] = hdf_path
    result["reach_shapefile"] = shp_path
    return result


def create_scenario_subset(source_scenario: str, target_name: str, subset_start: str, subset_end: str, progress_cb=None, cancel_cb=None, selected_reaches=None, max_lulc_distance: int = 100, max_number_cores: Optional[int] = None) -> dict:
    def _emit(pct: int, message: str):
        if cancel_cb and cancel_cb():
            raise SubsetJobCancelled("Subset creation was cancelled")
        if progress_cb:
            progress_cb(max(0, min(100, int(pct))), message)

    _emit(2, "Inspecting source scenario")
    inspected = inspect_scenario(source_scenario)
    if inspected.get("error"):
        raise ValueError(inspected["error"])

    _emit(6, "Validating subset window")
    clean_name = _validate_subset_scenario_name(target_name)
    user_selected_reaches = _coerce_list_of_reach_ids(selected_reaches or [])
    start_date = _parse_iso_date(subset_start, "subset_start")
    end_date = _parse_iso_date(subset_end, "subset_end")
    _validate_subset_window(inspected, start_date, end_date)

    source_rel, source_abs = _resolve_scenario_path(source_scenario)
    target_abs = os.path.join(SCENARIO_DIR, clean_name)
    target_rel = f"scenario/{clean_name}"
    if os.path.exists(target_abs):
        raise ValueError(f"Target scenario already exists: {target_rel}")

    # Expand reach selection to ensure topological completeness: every selected
    # reach's downstream chain must also be included so the simulation model can
    # resolve the 'downstream' pointer for each reach in its reach list.
    topology_added = []
    if user_selected_reaches:
        _emit(7, "Expanding reach selection for network topology")
        selected_reaches, topology_added = _expand_reach_selection_downstream(source_abs, user_selected_reaches)
    else:
        selected_reaches = user_selected_reaches

    subset_hdf_start = datetime.datetime.combine(start_date, datetime.time(1, 0))
    subset_hdf_end = datetime.datetime.combine(end_date + datetime.timedelta(days=1), datetime.time(0, 0))
    csv_start = datetime.datetime.combine(start_date, datetime.time(0, 0))
    csv_end = subset_hdf_end

    source_hdf = _scenario_hydro_path(source_abs)
    source_timeseries_dir = _scenario_timeseries_dir(source_abs)

    try:
        _emit(8, "Copying scenario template")
        _copy_scenario_template(source_abs, target_abs, progress_cb=_emit, pct_from=8, pct_to=30, cancel_cb=cancel_cb)

        if selected_reaches:
            _emit(30, "Slicing reach geometry")
            _slice_reach_shapefile_for_subset(target_abs, selected_reaches)
            _emit(30.2, "Slicing LULC geometries")
            _slice_lulc_shapefile_for_subset(target_abs, selected_reaches, max_lulc_distance)

        target_hdf = _scenario_hydro_path(target_abs)
        _emit(31, "Slicing hydrology HDF5")
        _slice_hydrology_hdf(
            source_hdf,
            target_hdf,
            subset_hdf_start,
            subset_hdf_end,
            progress_cb=_emit,
            pct_from=31,
            pct_to=80,
            cancel_cb=cancel_cb,
            selected_reach_ids=selected_reaches,
        )

        sliced_csvs = 0
        if os.path.isdir(source_timeseries_dir):
            target_timeseries_dir = _scenario_timeseries_dir(target_abs)
            os.makedirs(target_timeseries_dir, exist_ok=True)
            csv_files = [entry for entry in sorted(os.listdir(source_timeseries_dir)) if entry.lower().endswith(".csv")]
            csv_total = len(csv_files)
            csv_paths = [os.path.join(source_timeseries_dir, entry) for entry in csv_files]
            csv_total_bytes = 0
            for csv_path in csv_paths:
                try:
                    csv_total_bytes += os.path.getsize(csv_path)
                except OSError:
                    pass

            has_parallel_backend = bool(
                step2_backend
                and hasattr(step2_backend, "process_csv_files_parallel")
                and step2_backend.pd
            )
            requested_workers = max_number_cores or max(1, multiprocessing.cpu_count() - 1)
            requested_workers = max(1, int(requested_workers))
            parallel_workers = max(1, min(requested_workers, csv_total))

            # Auto mode keeps conservative thresholds to avoid regressions on
            # tiny workloads, but an explicit user core cap forces parallel
            # mode whenever there are at least 2 CSV files to distribute.
            auto_parallel_eligible = csv_total >= 4 and csv_total_bytes >= (32 * 1024 * 1024)
            force_parallel = bool(max_number_cores and max_number_cores > 1)
            use_parallel = has_parallel_backend and parallel_workers >= 2 and (force_parallel or auto_parallel_eligible)

            if not use_parallel:
                if not has_parallel_backend:
                    _emit(80.2, "Parallel CSV backend unavailable; using sequential inflow trimming")
                elif csv_total < 2:
                    _emit(80.2, "Only one inflow CSV file found; multicore CSV parallelization is not possible")
                elif parallel_workers < 2:
                    _emit(80.2, "Parallel worker count resolved to 1; using sequential inflow trimming")
                elif not force_parallel:
                    _emit(
                        80.2,
                        (
                            "Parallel CSV threshold not met in auto mode "
                            f"(files={csv_total}, size={csv_total_bytes // (1024 * 1024)} MB); using sequential trimming"
                        ),
                    )
            
            if use_parallel:
                _emit(
                    80.5,
                    (
                        f"Processing {csv_total} inflow CSV files in parallel "
                        f"({csv_total_bytes // (1024 * 1024)} MB total, workers={parallel_workers}, requested={requested_workers})"
                    ),
                )
                def parallel_progress(pct, msg):
                    _emit(max(80, min(95, pct)), msg)
                
                try:
                    par_result = step2_backend.process_csv_files_parallel(
                        csv_paths,
                        csv_start,
                        csv_end,
                        target_timeseries_dir,
                        selected_reach_ids=selected_reaches,
                        num_workers=parallel_workers,
                        progress_cb=parallel_progress,
                        cancel_cb=cancel_cb,
                    )
                    sliced_csvs = par_result.get("processed", 0)
                    failed_parallel_files = par_result.get("failed_files", [])
                    if par_result.get("error"):
                        if "cancel" in str(par_result.get("error", "")).lower():
                            raise SubsetJobCancelled("Subset creation was cancelled and worker processes were terminated")
                        _emit(81, f"Parallel processing fell back to sequential: {par_result['error']}")
                        use_parallel = False
                        failed_parallel_files = csv_paths
                    elif failed_parallel_files:
                        _emit(
                            81,
                            f"Parallel processing completed with {len(failed_parallel_files)} failed file(s), retrying sequentially",
                        )
                        for source_csv in failed_parallel_files:
                            _slice_timeseries_csv(
                                source_csv,
                                os.path.join(target_timeseries_dir, os.path.basename(source_csv)),
                                csv_start,
                                csv_end,
                                cancel_cb=cancel_cb,
                                selected_reach_ids=selected_reaches,
                            )
                except Exception as e:
                    if "cancel" in str(e).lower():
                        raise SubsetJobCancelled("Subset creation was cancelled and worker processes were terminated")
                    _emit(81, f"Parallel processing error, using sequential: {str(e)}")
                    use_parallel = False
            
            # Fallback to sequential processing
            if not use_parallel:
                for idx, entry in enumerate(csv_files, start=1):
                    _slice_timeseries_csv(
                        os.path.join(source_timeseries_dir, entry),
                        os.path.join(target_timeseries_dir, entry),
                        csv_start,
                        csv_end,
                        cancel_cb=cancel_cb,
                        selected_reach_ids=selected_reaches,
                    )
                    sliced_csvs += 1
                    if csv_total:
                        pct = int(80 + 15 * (idx / csv_total))
                        _emit(pct, f"Trimming inflow time series ({idx}/{csv_total})")

        _emit(96, "Updating scenario metadata")
        _update_scenario_project_for_subset(_scenario_project_path(target_abs), clean_name, start_date, end_date)
        _rewrite_subset_readme(
            _scenario_readme_path(target_abs),
            source_rel,
            start_date,
            end_date,
            _format_hydro_datetime(subset_hdf_start),
            _format_hydro_datetime(subset_hdf_end),
            inspected.get("hdf_extent", {}).get("from_datetime"),
            inspected.get("hdf_extent", {}).get("to_datetime"),
        )
        geometry_validation = _validate_subset_geometry_contract(target_abs)
        _emit(100, "Subset scenario created")
    except Exception:
        if os.path.isdir(target_abs):
            shutil.rmtree(target_abs, ignore_errors=True)
        raise

    warnings = [
        "hydro/doc statistics were copied from the source scenario and were not recalculated for the subset period."
    ]
    if topology_added:
        warnings.append(
            f"Reach selection was automatically expanded from {len(user_selected_reaches)} to {len(selected_reaches)} reaches to preserve network topology."
        )
    warnings.extend(geometry_validation.get("warnings", []))

    return {
        "scenario_path": target_rel,
        "scenario_name": clean_name,
        "simulation_start": start_date.isoformat(),
        "simulation_end": end_date.isoformat(),
        "hdf_time_from": _format_hydro_datetime(subset_hdf_start),
        "hdf_time_to": _format_hydro_datetime(subset_hdf_end),
        "timeseries_csv_count": sliced_csvs,
        "selected_reach_count": len(selected_reaches),
        "user_selected_reach_count": len(user_selected_reaches),
        "topology_added_reach_count": len(topology_added),
        "selected_reaches": selected_reaches,
        "warnings": warnings,
        "geometry_validation": geometry_validation,
    }


def _subset_job_update(job_id: str, **updates):
    with _subset_lock:
        job = _subset_jobs.get(job_id)
        if not job:
            return
        job.update(updates)


def start_subset_job(source_scenario: str, target_name: str, subset_start: str, subset_end: str, selected_reaches=None, max_lulc_distance: int = 100, max_number_cores: Optional[int] = None) -> str:
    job_id = f"subset-{uuid.uuid4().hex[:12]}"
    cancel_event = threading.Event()
    with _subset_lock:
        _subset_jobs[job_id] = {
            "job_id": job_id,
            "running": True,
            "completed": False,
            "cancel_requested": False,
            "cancelled": False,
            "progress_pct": 0,
            "message": "Queued",
            "error": None,
            "result": None,
            "started_at": time.time(),
            "finished_at": None,
            "_cancel_event": cancel_event,
        }

    def _progress(pct, message):
        _subset_job_update(job_id, progress_pct=int(pct), message=str(message or "Working"))

    def _worker():
        try:
            payload = create_scenario_subset(
                source_scenario,
                target_name,
                subset_start,
                subset_end,
                progress_cb=_progress,
                cancel_cb=cancel_event.is_set,
                selected_reaches=selected_reaches,
                max_lulc_distance=max_lulc_distance,
                max_number_cores=max_number_cores,
            )
            _subset_job_update(
                job_id,
                running=False,
                completed=True,
                progress_pct=100,
                message="Subset scenario created",
                result=payload,
                finished_at=time.time(),
            )
        except SubsetJobCancelled as exc:
            _subset_job_update(
                job_id,
                running=False,
                completed=True,
                cancelled=True,
                message=str(exc),
                error=None,
                finished_at=time.time(),
            )
        except Exception as exc:
            _subset_job_update(
                job_id,
                running=False,
                completed=True,
                message="Subset creation failed",
                error=str(exc),
                finished_at=time.time(),
            )

    threading.Thread(target=_worker, daemon=True).start()
    return job_id


def subset_job_status(job_id: str) -> dict:
    with _subset_lock:
        job = _subset_jobs.get(job_id)
        if not job:
            return {"error": "Job not found"}
        return {k: v for k, v in job.items() if not k.startswith("_")}


def cancel_subset_job(job_id: str) -> dict:
    with _subset_lock:
        job = _subset_jobs.get(job_id)
        if not job:
            return {"error": "Job not found"}
        if not job.get("running"):
            return {
                "job_id": job_id,
                "running": False,
                "completed": True,
                "cancel_requested": bool(job.get("cancel_requested")),
                "cancelled": bool(job.get("cancelled")),
                "message": "Job is not running",
            }
        job["cancel_requested"] = True
        cancel_event = job.get("_cancel_event")
        if cancel_event is not None:
            cancel_event.set()
        job["message"] = "Cancellation requested"
    return {
        "job_id": job_id,
        "running": True,
        "cancel_requested": True,
        "message": "Cancellation requested",
    }


# ═══════════════════════════════════════════════════════════════════
# Dashboard / monitoring helpers  (from dashboard/server.py)
# ═══════════════════════════════════════════════════════════════════

_SEV_RE = re.compile(r"^(ERROR|WARN |NOTE |OK   |INFO )\s(.*)$")

# Downgrade selected non-fatal errors to WARN for monitor readability.
_ERROR_TO_WARN_RULES = [
    {
        "id": "gdal_minmax_no_valid_pixels",
        "match": re.compile(r"^Failed to compute min/max, no valid pixels found in sampling\. \(GDAL error 1\)$"),
        "description": "GDAL min/max sampling failure with no valid pixels.",
    },
]

# Reclassify all WARN entries to NOTE for cleaner monitoring logs.
_WARN_TO_NOTE_RULES = [
    {
        "id": "all_warn_to_note",
        "match": re.compile(r"^.*$"),
        "description": "Global policy: every WARN log entry is displayed as NOTE.",
    },
]


def _warning_downgrade_rule(msg: str):
    for rule in _WARN_TO_NOTE_RULES:
        if rule["match"].match(msg):
            return rule
    return None


def _error_downgrade_rule(msg: str):
    for rule in _ERROR_TO_WARN_RULES:
        if rule["match"].match(msg):
            return rule
    return None


def get_warning_downgrade_rules():
    return [
        {
            "id": r["id"],
            "pattern": r["match"].pattern,
            "description": r["description"],
        }
        for r in _WARN_TO_NOTE_RULES
    ]


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
            sev = m.group(1).strip()
            msg = m.group(2)
            entry = {"sev": sev, "msg": msg}
            if sev == "ERROR":
                rule = _error_downgrade_rule(msg)
                if rule is not None:
                    entry["sev"] = "WARN"
                    entry["reclassified_from"] = "ERROR"
                    entry["reclassification_rule"] = rule["id"]
            if sev == "WARN":
                entry["sev"] = "NOTE"
                entry["reclassified_from"] = "WARN"
                entry["reclassification_rule"] = "all_warn_to_note"
            current = entry
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
            name = msg[len("Initializing component "):].strip()
            if name not in initialized:
                initialized.append(name)
        elif msg.startswith("Running component "):
            current = msg[len("Running component "):].strip()
        elif msg.startswith("Component ") and msg.endswith(" finished"):
            name = msg[len("Component "):-len(" finished")].strip()
            done.append(name)
            # Only clear active component when that same component finished.
            if current == name:
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


def _format_elapsed_from_seconds(total_seconds: float) -> str:
    total = max(0, int(total_seconds))
    h, rem = divmod(total, 3600)
    m, s = divmod(rem, 60)
    return f"{h}:{m:02d}:{s:02d}"


def _normalize_reach_id(value):
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if re.fullmatch(r"[-+]?\d+(?:\.0+)?", text):
        return str(int(float(text)))
    return text


def _coerce_list_of_reach_ids(values):
    result = []
    seen = set()
    for raw in values or []:
        rid = _normalize_reach_id(raw)
        if rid is None or rid in seen:
            continue
        seen.add(rid)
        result.append(rid)
    return result


def _coerce_max_number_cores(raw_value):
    """Parse optional user core cap; return None for auto mode."""
    if raw_value in (None, "", 0, "0"):
        return None
    try:
        max_cores = int(raw_value)
    except (TypeError, ValueError):
        raise ValueError("max_number_cores must be an integer")
    if max_cores < 1:
        raise ValueError("max_number_cores must be >= 1")
    return min(max_cores, multiprocessing.cpu_count())


def _decode_text(raw):
    if isinstance(raw, bytes):
        return raw.decode("utf-8", errors="replace")
    return str(raw)


def _resolve_mc_store_paths(run_root, experiment, mc_run):
    for val, name in ((experiment, "experiment"), (mc_run, "mc_run")):
        if any(c in val for c in (os.sep, "/", "\\", "..")):
            raise ValueError(f"Invalid {name}")

    run_root_abs = os.path.abspath(run_root)
    run_path = os.path.abspath(os.path.join(run_root_abs, experiment))
    if not run_path.startswith(run_root_abs + os.sep):
        raise ValueError("Invalid experiment path")
    if not os.path.isdir(run_path):
        raise FileNotFoundError(f"Run folder not found: {run_path}")

    mc_path = os.path.abspath(os.path.join(run_path, "mcs", mc_run))
    if not mc_path.startswith(run_path + os.sep):
        raise ValueError("Invalid MC path")
    if not os.path.isdir(mc_path):
        raise FileNotFoundError(f"MC run not found: {mc_path}")

    arr_path = os.path.join(mc_path, "store", "arr.dat")
    if not os.path.isfile(arr_path):
        raise FileNotFoundError(f"Store file not found: {arr_path}")

    params = _parse_user_xml(run_path)
    scenario_rel = (
        params.get("Scenario/LandscapeScenario")
        or params.get("Scenario/Project")
        or ""
    ).strip()
    if not scenario_rel:
        raise FileNotFoundError("Scenario path is missing in run metadata")

    scenario_path = os.path.abspath(os.path.join(BASE_DIR, scenario_rel))
    if not scenario_path.startswith(BASE_DIR + os.sep):
        raise ValueError("Invalid scenario path")
    if not os.path.isdir(scenario_path):
        raise FileNotFoundError(f"Scenario folder not found: {scenario_path}")

    return {
        "run_path": run_path,
        "mc_path": mc_path,
        "arr_path": arr_path,
        "scenario_rel": scenario_rel,
        "scenario_path": scenario_path,
    }


def _find_reach_shapefile(scenario_path):
    preferred_names = ["Reachlist_shp.shp", "ReachList_shp.shp"]
    for name in preferred_names:
        preferred = os.path.join(scenario_path, "geo", name)
        if os.path.isfile(preferred):
            return preferred
    geo_dir = os.path.join(scenario_path, "geo")
    if os.path.isdir(geo_dir):
        shp_files = sorted(glob.glob(os.path.join(geo_dir, "*.shp")))
        if shp_files:
            return shp_files[0]
    shp_files = sorted(glob.glob(os.path.join(scenario_path, "**", "*.shp"), recursive=True))
    return shp_files[0] if shp_files else None


def _find_lulc_shapefile(scenario_path):
    preferred_names = ["LULC.shp", "lulc.shp"]
    for name in preferred_names:
        preferred = os.path.join(scenario_path, "geo", name)
        if os.path.isfile(preferred):
            return preferred
    return None


def _normalize_map_gdf(gdf):
    source_crs = str(gdf.crs) if gdf.crs else ""
    if gdf.crs is not None:
        try:
            if not gdf.crs.is_geographic:
                gdf = gdf.to_crs(epsg=4326)
        except Exception:
            pass
    gdf = gdf.dropna(subset=["geometry"])
    return gdf, source_crs


def _build_lulc_geojson(shp_path):
    if not shp_path:
        return {
            "geojson": {"type": "FeatureCollection", "features": []},
            "meta": {
                "feature_count": 0,
                "shapefile": None,
                "source_crs": "",
                "map_crs": "EPSG:4326",
            },
        }

    if gpd is None:
        raise RuntimeError("geopandas is not available in this Python runtime")

    gdf = gpd.read_file(shp_path)
    if gdf.empty:
        return {
            "geojson": {"type": "FeatureCollection", "features": []},
            "meta": {
                "feature_count": 0,
                "shapefile": shp_path,
                "source_crs": str(gdf.crs) if gdf.crs else "",
                "map_crs": "EPSG:4326",
            },
        }

    gdf, source_crs = _normalize_map_gdf(gdf)
    geojson = json.loads(gdf[["geometry"]].to_json())
    for feature in geojson.get("features", []):
        feature["properties"] = {}
    return {
        "geojson": geojson,
        "meta": {
            "feature_count": len(geojson.get("features", [])),
            "shapefile": shp_path,
            "source_crs": source_crs,
            "map_crs": "EPSG:4326",
        },
    }


def _load_reach_ids_from_scenario_hydro(hydro_path):
    if not os.path.isfile(hydro_path):
        raise FileNotFoundError(f"Hydrology file not found: {hydro_path}")
    if h5py is None:
        script = r'''
import json
import h5py
import sys

payload = json.loads(sys.argv[1])
with h5py.File(payload["hydro_path"], "r") as hf:
    if "reaches" not in hf:
        raise RuntimeError("Dataset 'reaches' not found in hydrology file")
    reaches = [str(int(v)) for v in hf["reaches"][:]]
print(json.dumps({"reach_ids": reaches}))
'''
        return _coerce_list_of_reach_ids(_embedded_json_call(script, {"hydro_path": hydro_path}).get("reach_ids", []))

    with h5py.File(hydro_path, "r") as hf:
        if "reaches" not in hf:
            raise KeyError("Dataset 'reaches' not found in hydrology file")
        reaches = [str(int(v)) for v in hf["reaches"][:]]
    return _coerce_list_of_reach_ids(reaches)


def build_scenario_geometry(scenario_path):
    scenario_rel, scenario_abs = _resolve_scenario_path(scenario_path)
    if not os.path.isdir(scenario_abs):
        raise FileNotFoundError(f"Scenario folder not found: {scenario_rel}")

    shp_path = _find_reach_shapefile(scenario_abs)
    if not shp_path:
        raise FileNotFoundError("No shapefile found for selected scenario")

    reach_ids = _load_reach_ids_from_scenario_hydro(_scenario_hydro_path(scenario_abs))
    reach_set = set(reach_ids)

    if gpd is None:
        script = r'''
import glob
import json
import os
import re
import sys
import geopandas as gpd

def norm(value):
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if re.fullmatch(r"[-+]?\d+(?:\.0+)?", text):
        return str(int(float(text)))
    return text

payload = json.loads(sys.argv[1])
shp_path = payload["shp_path"]
reach_ids = set(payload["reach_ids"])
gdf = gpd.read_file(shp_path)
if gdf.empty:
    raise RuntimeError("Shapefile has no features")
source_crs = str(gdf.crs) if gdf.crs else ""
if gdf.crs is not None:
    try:
        if not gdf.crs.is_geographic:
            gdf = gdf.to_crs(epsg=4326)
    except Exception:
        pass
non_geom = [c for c in gdf.columns if c != "geometry"]
if not non_geom:
    raise RuntimeError("No attribute columns available in shapefile")
preferred = ["key", "reach_id", "reachid", "segment_id", "reach", "name", "id"]
best_col = non_geom[0]
best_score = -1.0
for col in non_geom:
    vals = [norm(v) for v in gdf[col].dropna().head(2000)]
    if not vals:
        continue
    unique_vals = set(v for v in vals if v is not None)
    matched_unique = len(set(v for v in unique_vals if v in reach_ids))
    if matched_unique == 0:
        continue
    unique_ratio = len(unique_vals) / max(len(vals), 1)
    score = float(matched_unique) + 0.5 * unique_ratio
    col_l = col.lower()
    if col_l in preferred:
        score += 5.0
    elif any(p in col_l for p in preferred):
        score += 2.0
    if score > best_score:
        best_score = score
        best_col = col
gdf = gdf.dropna(subset=["geometry"])
gdf["__reach_id__"] = gdf[best_col].map(norm)
gdf = gdf.dropna(subset=["__reach_id__"])
gdf = gdf[gdf["__reach_id__"].isin(reach_ids)]
if gdf.empty:
    raise RuntimeError("No stream features could be mapped to selected scenario reaches")
minx, miny, maxx, maxy = gdf.total_bounds
geojson = json.loads(gdf[["__reach_id__", "geometry"]].to_json())
for f in geojson.get("features", []):
    rid = f.get("properties", {}).get("__reach_id__")
    f["properties"] = {"reach_id": rid}
print(json.dumps({
    "geojson": geojson,
    "meta": {
        "reach_id_field": best_col,
        "feature_count": len(geojson.get("features", [])),
        "bounds": [float(minx), float(miny), float(maxx), float(maxy)],
        "shapefile": shp_path,
        "source_crs": source_crs,
        "map_crs": "EPSG:4326",
    }
}))
'''
        embedded = _embedded_json_call(script, {"shp_path": shp_path, "reach_ids": list(reach_set)})
        return {
            "scenario_path": scenario_rel,
            "geojson": embedded["geojson"],
            "meta": embedded["meta"],
            "reach_count": len(reach_ids),
        }

    gdf = gpd.read_file(shp_path)
    if gdf.empty:
        raise ValueError("Shapefile has no features")
    gdf, source_crs = _normalize_map_gdf(gdf)
    reach_col = _select_reach_id_column(gdf, reach_set)
    if reach_col is None:
        raise ValueError("Could not determine reach identifier column")
    gdf["__reach_id__"] = gdf[reach_col].map(_normalize_reach_id)
    gdf = gdf.dropna(subset=["__reach_id__"])
    gdf = gdf[gdf["__reach_id__"].isin(reach_set)]
    if gdf.empty:
        raise ValueError("No stream features could be mapped to selected scenario reaches")
    minx, miny, maxx, maxy = gdf.total_bounds
    geom_json = json.loads(gdf[["__reach_id__", "geometry"]].to_json())
    for feature in geom_json.get("features", []):
        rid = feature.get("properties", {}).get("__reach_id__")
        feature["properties"] = {"reach_id": rid}
    return {
        "scenario_path": scenario_rel,
        "geojson": geom_json,
        "meta": {
            "reach_id_field": reach_col,
            "feature_count": len(geom_json.get("features", [])),
            "bounds": [float(minx), float(miny), float(maxx), float(maxy)],
            "shapefile": shp_path,
            "source_crs": source_crs,
            "map_crs": "EPSG:4326",
        },
        "reach_count": len(reach_ids),
    }


def _load_reach_ids_from_hdf(arr_path):
    if h5py is None:
        raise RuntimeError("h5py is not available in this Python runtime")
    with h5py.File(arr_path, "r") as hf:
        if "CascadeToxswa/ConLiqWatTgtAvg" not in hf:
            raise KeyError("Dataset CascadeToxswa/ConLiqWatTgtAvg not found")
        ds = hf["CascadeToxswa/ConLiqWatTgtAvg"]
        names_ref = ds.attrs.get("dim1_element_names")
        if names_ref is None:
            raise KeyError("dim1_element_names attribute missing")
        try:
            raw_ids = hf[names_ref][:]
        except Exception:
            names_path = _decode_text(names_ref)
            if names_path not in hf:
                raise KeyError(f"Reach names dataset not found: {names_path}")
            raw_ids = hf[names_path][:]
    return _coerce_list_of_reach_ids(_decode_text(v) for v in raw_ids)


def _select_reach_id_column(gdf, reach_ids_hint):
    non_geom = [c for c in gdf.columns if c != "geometry"]
    if not non_geom:
        return None

    # Prefer canonical reach identifier columns first.
    exact_priority = ["key", "reach_id", "reachid", "segment_id", "reach", "name", "id"]
    lower_to_col = {c.lower(): c for c in non_geom}

    hints = set(_coerce_list_of_reach_ids(reach_ids_hint or []))

    def _normalized_sample(col_name: str):
        vals = gdf[col_name].dropna().head(2000)
        if vals.empty:
            return []
        return [_normalize_reach_id(v) for v in vals]

    if hints:
        # Try exact-priority columns first if they overlap with hinted reach IDs.
        priority_best = None
        priority_matches = -1
        for low_name in exact_priority:
            col = lower_to_col.get(low_name)
            if not col:
                continue
            norm_vals = _normalized_sample(col)
            if not norm_vals:
                continue
            matched_unique = len({v for v in norm_vals if v in hints})
            if matched_unique > priority_matches:
                priority_matches = matched_unique
                priority_best = col
        if priority_best and priority_matches > 0:
            return priority_best

        # Fallback: score by unique-id overlap and penalize low-cardinality columns.
        best_col = None
        best_score = -1.0
        preferred_tokens = ["reach_id", "reachid", "key", "segment_id", "reach", "name", "id"]
        for col in non_geom:
            norm_vals = _normalized_sample(col)
            if not norm_vals:
                continue
            unique_vals = {v for v in norm_vals if v is not None}
            matched_unique = len({v for v in unique_vals if v in hints})
            if matched_unique == 0:
                continue
            unique_ratio = (len(unique_vals) / max(len(norm_vals), 1))
            score = float(matched_unique) + 0.5 * unique_ratio
            col_name = col.lower()
            if col_name in preferred_tokens:
                score += 5.0
            elif any(tok in col_name for tok in preferred_tokens):
                score += 2.0
            if score > best_score:
                best_score = score
                best_col = col
        if best_col:
            return best_col

    # Without hints, pick best-known column name if available.
    for low_name in exact_priority:
        col = lower_to_col.get(low_name)
        if col:
            return col

    return non_geom[0]


def _build_map_geometry_embedded(arr_path, scenario_path):
    script = r'''
import glob
import json
import os
import re
import sys
import geopandas as gpd
import h5py

def norm(value):
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if re.fullmatch(r"[-+]?\d+(?:\.0+)?", text):
        return str(int(float(text)))
    return text

def find_shp(scenario_path):
    for name in ("Reachlist_shp.shp", "ReachList_shp.shp"):
        preferred = os.path.join(scenario_path, "geo", name)
        if os.path.isfile(preferred):
            return preferred
    geo_dir = os.path.join(scenario_path, "geo")
    if os.path.isdir(geo_dir):
        files = sorted(glob.glob(os.path.join(geo_dir, "*.shp")))
        if files:
            return files[0]
    files = sorted(glob.glob(os.path.join(scenario_path, "**", "*.shp"), recursive=True))
    return files[0] if files else None

def find_lulc_shp(scenario_path):
    for name in ("LULC.shp", "lulc.shp"):
        preferred = os.path.join(scenario_path, "geo", name)
        if os.path.isfile(preferred):
            return preferred
    return None

payload = json.loads(sys.argv[1])
arr_path = payload["arr_path"]
scenario_path = payload["scenario_path"]
shp_path = find_shp(scenario_path)
lulc_path = find_lulc_shp(scenario_path)
if not shp_path:
    raise RuntimeError("No shapefile found for selected scenario")

with h5py.File(arr_path, "r") as hf:
    ds = hf["CascadeToxswa/ConLiqWatTgtAvg"]
    names_ref = ds.attrs["dim1_element_names"]
    try:
        raw_ids = hf[names_ref][:]
    except Exception:
        names_path = names_ref.decode("utf-8", errors="replace") if isinstance(names_ref, bytes) else str(names_ref)
        raw_ids = hf[names_path][:]
    reach_ids = {norm(v.decode("utf-8", errors="replace") if isinstance(v, bytes) else v) for v in raw_ids}

gdf = gpd.read_file(shp_path)
if gdf.empty:
    raise RuntimeError("Shapefile has no features")
source_crs = str(gdf.crs) if gdf.crs else ""
if gdf.crs is not None:
    try:
        if not gdf.crs.is_geographic:
            gdf = gdf.to_crs(epsg=4326)
    except Exception:
        pass

non_geom = [c for c in gdf.columns if c != "geometry"]
if not non_geom:
    raise RuntimeError("No attribute columns available in shapefile")

preferred = ["key", "reach_id", "reachid", "segment_id", "reach", "name", "id"]
best_col = non_geom[0]
best_score = -1.0
for col in non_geom:
    vals = [norm(v) for v in gdf[col].dropna().head(2000)]
    if not vals:
        continue
    unique_vals = set(v for v in vals if v is not None)
    matched_unique = len(set(v for v in unique_vals if v in reach_ids))
    if matched_unique == 0:
        continue
    unique_ratio = len(unique_vals) / max(len(vals), 1)
    score = float(matched_unique) + 0.5 * unique_ratio
    col_l = col.lower()
    if col_l in preferred:
        score += 5.0
    elif any(p in col_l for p in preferred):
        score += 2.0
    if score > best_score:
        best_score = score
        best_col = col

gdf = gdf.dropna(subset=["geometry"]) 
gdf["__reach_id__"] = gdf[best_col].map(norm)
gdf = gdf.dropna(subset=["__reach_id__"])
gdf = gdf[gdf["__reach_id__"].isin(reach_ids)]
if gdf.empty:
    raise RuntimeError("No stream features could be mapped to PECsw reach IDs")

minx, miny, maxx, maxy = gdf.total_bounds
geojson = json.loads(gdf[["__reach_id__", "geometry"]].to_json())
for f in geojson.get("features", []):
    rid = f.get("properties", {}).get("__reach_id__")
    f["properties"] = {"reach_id": rid}

lulc_geojson = {"type": "FeatureCollection", "features": []}
lulc_meta = {
    "feature_count": 0,
    "shapefile": lulc_path,
    "source_crs": "",
    "map_crs": "EPSG:4326",
}
if lulc_path:
    lulc_gdf = gpd.read_file(lulc_path)
    if not lulc_gdf.empty:
        lulc_source_crs = str(lulc_gdf.crs) if lulc_gdf.crs else ""
        if lulc_gdf.crs is not None:
            try:
                if not lulc_gdf.crs.is_geographic:
                    lulc_gdf = lulc_gdf.to_crs(epsg=4326)
            except Exception:
                pass
        lulc_gdf = lulc_gdf.dropna(subset=["geometry"])
        lulc_geojson = json.loads(lulc_gdf[["geometry"]].to_json())
        for f in lulc_geojson.get("features", []):
            f["properties"] = {}
        lulc_meta = {
            "feature_count": len(lulc_geojson.get("features", [])),
            "shapefile": lulc_path,
            "source_crs": lulc_source_crs,
            "map_crs": "EPSG:4326",
        }

print(json.dumps({
    "geojson": geojson,
    "lulc_geojson": lulc_geojson,
    "meta": {
        "reach_id_field": best_col,
        "feature_count": len(geojson.get("features", [])),
        "bounds": [float(minx), float(miny), float(maxx), float(maxy)],
        "shapefile": shp_path,
        "source_crs": source_crs,
        "map_crs": "EPSG:4326",
    },
    "lulc_meta": lulc_meta,
}))
'''
    return _embedded_json_call(script, {"arr_path": arr_path, "scenario_path": scenario_path})


def _build_map_timeseries_embedded(arr_path, reach_ids, time_from=None, time_to=None, resolution="auto"):
    script = r'''
import json
import re
import sys
import h5py
import pandas as pd

def norm(value):
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if re.fullmatch(r"[-+]?\d+(?:\.0+)?", text):
        return str(int(float(text)))
    return text

payload = json.loads(sys.argv[1])
arr_path = payload["arr_path"]
requested = [norm(v) for v in payload.get("reach_ids", []) if norm(v)]
requested = list(dict.fromkeys(requested))
time_from = payload.get("time_from")
time_to = payload.get("time_to")
resolution = (payload.get("resolution") or "auto").lower()

with h5py.File(arr_path, "r") as hf:
    ds = hf["CascadeToxswa/ConLiqWatTgtAvg"]
    names_ref = ds.attrs["dim1_element_names"]
    try:
        raw_ids = hf[names_ref][:]
    except Exception:
        names_path = names_ref.decode("utf-8", errors="replace") if isinstance(names_ref, bytes) else str(names_ref)
        raw_ids = hf[names_path][:]
    all_ids = [norm(v.decode("utf-8", errors="replace") if isinstance(v, bytes) else v) for v in raw_ids]
    idx = {rid: i for i, rid in enumerate(all_ids) if rid}

    present = [rid for rid in requested if rid in idx]
    missing = [rid for rid in requested if rid not in idx]
    if not present:
        raise RuntimeError("None of the requested reach IDs exist in PECsw data")

    start_raw = ds.attrs.get("dim0_offset")
    start_text = start_raw.decode("utf-8", errors="replace") if isinstance(start_raw, bytes) else str(start_raw)
    start_dt = pd.to_datetime(start_text, errors="coerce")
    if pd.isna(start_dt):
        start_dt = pd.Timestamp("1970-01-01T00:00:00")
    full_index = pd.date_range(start=start_dt, periods=ds.shape[0], freq="h")

    from_dt = pd.to_datetime(time_from, errors="coerce") if time_from else full_index[0]
    to_dt = pd.to_datetime(time_to, errors="coerce") if time_to else full_index[-1]
    if pd.isna(from_dt):
        from_dt = full_index[0]
    if pd.isna(to_dt):
        to_dt = full_index[-1]
    if to_dt < from_dt:
        raise RuntimeError("time_to must be greater than or equal to time_from")

    i0 = max(0, int(full_index.searchsorted(from_dt, side="left")))
    i1 = min(len(full_index)-1, int(full_index.searchsorted(to_dt, side="right") - 1))
    if i1 < i0:
        raise RuntimeError("Selected time window does not overlap with available data")

    part_index = full_index[i0:i1+1]
    if resolution not in ("auto", "daily", "hourly"):
        resolution = "auto"
    if resolution == "auto":
        resolution = "hourly" if len(part_index) <= 24 * 14 else "daily"
    
    # Auto-downgrade hourly to daily if payload would be too large (500k points)
    if resolution == "hourly" and len(part_index) * len(present) > 500000:
        resolution = "daily"

    # h5py requires indices in increasing order; create mapping to restore original order
    cols = [idx[rid] for rid in present]
    sorted_cols_enum = sorted(enumerate(cols), key=lambda x: x[1])
    sorted_cols = [x[1] for x in sorted_cols_enum]
    sort_order = [x[0] for x in sorted_cols_enum]
    
    arr = ds[i0:i1+1, sorted_cols] * 1_000_000.0
    # Reorder columns back to match original present order
    arr = arr[:, [sort_order.index(i) for i in range(len(sort_order))]]

frame = pd.DataFrame(arr, index=part_index, columns=present)
if resolution == "daily":
    frame = frame.resample("D").max()

times = [t.isoformat() for t in frame.index.to_pydatetime()]
series = [{"reach_id": rid, "values": frame[rid].astype(float).round(6).tolist()} for rid in present]

print(json.dumps({
    "times": times,
    "series": series,
    "meta": {
        "units": "ng/L",
        "resolution_used": resolution,
        "requested_resolution": (payload.get("resolution") or "auto").lower(),
        "time_from": times[0] if times else None,
        "time_to": times[-1] if times else None,
        "requested_reach_count": len(requested),
        "returned_reach_count": len(present),
        "missing_reaches": missing,
        "available_time_start": full_index[0].isoformat(),
        "available_time_end": full_index[-1].isoformat(),
    },
}))
'''
    return _embedded_json_call(
        script,
        {
            "arr_path": arr_path,
            "reach_ids": reach_ids,
            "time_from": time_from,
            "time_to": time_to,
            "resolution": resolution,
        },
    )


def _build_map_geometry(arr_path, scenario_path):
    if gpd is None or h5py is None:
        return _build_map_geometry_embedded(arr_path, scenario_path)

    shp_path = _find_reach_shapefile(scenario_path)
    lulc_path = _find_lulc_shapefile(scenario_path)
    if not shp_path:
        raise FileNotFoundError("No shapefile found for selected scenario")

    cache_key = hashlib.sha1(f"{arr_path}|{shp_path}|{lulc_path or ''}".encode("utf-8")).hexdigest()
    cached = _cache_get(_map_geometry_cache, cache_key)
    if cached is not None:
        return cached

    reach_ids = _load_reach_ids_from_hdf(arr_path)
    gdf = gpd.read_file(shp_path)
    if gdf.empty:
        raise ValueError("Shapefile has no features")
    gdf, source_crs = _normalize_map_gdf(gdf)
    reach_col = _select_reach_id_column(gdf, set(reach_ids))
    if reach_col is None:
        raise ValueError("Could not determine reach identifier column")

    gdf["__reach_id__"] = gdf[reach_col].map(_normalize_reach_id)
    gdf = gdf.dropna(subset=["__reach_id__"])
    gdf = gdf[gdf["__reach_id__"].isin(set(reach_ids))]
    if gdf.empty:
        raise ValueError("No stream features could be mapped to PECsw reach IDs")

    minx, miny, maxx, maxy = gdf.total_bounds
    geom_json = json.loads(gdf[["__reach_id__", "geometry"]].to_json())
    for feature in geom_json.get("features", []):
        rid = feature.get("properties", {}).get("__reach_id__")
        feature["properties"] = {"reach_id": rid}

    lulc_payload = _build_lulc_geojson(lulc_path)

    payload = {
        "geojson": geom_json,
        "lulc_geojson": lulc_payload["geojson"],
        "meta": {
            "reach_id_field": reach_col,
            "feature_count": len(geom_json.get("features", [])),
            "bounds": [float(minx), float(miny), float(maxx), float(maxy)],
            "shapefile": shp_path,
            "source_crs": source_crs,
            "map_crs": "EPSG:4326",
        },
        "lulc_meta": lulc_payload["meta"],
    }
    _cache_set(_map_geometry_cache, cache_key, payload, _MAP_GEOMETRY_CACHE_LIMIT)
    return payload


def _resolve_resolution(requested, dt_index):
    requested = (requested or "auto").lower()
    if requested in ("hour", "hr"):
        requested = "hourly"
    if requested not in ("auto", "daily", "hourly"):
        requested = "auto"
    if requested == "auto":
        if len(dt_index) <= 24 * 14:
            return "hourly"
        return "daily"
    return requested


def _build_map_timeseries(arr_path, reach_ids, time_from=None, time_to=None, resolution="auto"):
    if h5py is None or pd is None:
        return _build_map_timeseries_embedded(
            arr_path,
            reach_ids,
            time_from=time_from,
            time_to=time_to,
            resolution=resolution,
        )
    if not reach_ids:
        raise ValueError("At least one reach_id is required")

    norm_reach_ids = _coerce_list_of_reach_ids(reach_ids)
    cache_key = hashlib.sha1(json.dumps({
        "arr": arr_path,
        "ids": norm_reach_ids,
        "from": time_from or "",
        "to": time_to or "",
        "res": resolution or "auto",
    }, sort_keys=True).encode("utf-8")).hexdigest()
    cached = _cache_get(_map_timeseries_cache, cache_key)
    if cached is not None:
        return cached

    with h5py.File(arr_path, "r") as hf:
        if "CascadeToxswa/ConLiqWatTgtAvg" not in hf:
            raise KeyError("Dataset CascadeToxswa/ConLiqWatTgtAvg not found")
        ds = hf["CascadeToxswa/ConLiqWatTgtAvg"]
        names_ref = ds.attrs.get("dim1_element_names")
        try:
            raw_ids = hf[names_ref][:]
        except Exception:
            names_path = _decode_text(names_ref)
            raw_ids = hf[names_path][:]
        all_reach_ids = _coerce_list_of_reach_ids(_decode_text(v) for v in raw_ids)
        idx_by_reach = {rid: idx for idx, rid in enumerate(all_reach_ids)}

        requested_present = [rid for rid in norm_reach_ids if rid in idx_by_reach]
        missing = [rid for rid in norm_reach_ids if rid not in idx_by_reach]
        if not requested_present:
            raise ValueError("None of the requested reach IDs exist in PECsw data")

        start_raw = ds.attrs.get("dim0_offset")
        start_dt = pd.to_datetime(_decode_text(start_raw), errors="coerce")
        if pd.isna(start_dt):
            start_dt = pd.Timestamp("1970-01-01T00:00:00")

        full_index = pd.date_range(start=start_dt, periods=ds.shape[0], freq="h")
        from_dt = pd.to_datetime(time_from, errors="coerce") if time_from else full_index[0]
        to_dt = pd.to_datetime(time_to, errors="coerce") if time_to else full_index[-1]
        if pd.isna(from_dt):
            from_dt = full_index[0]
        if pd.isna(to_dt):
            to_dt = full_index[-1]
        if to_dt < from_dt:
            raise ValueError("time_to must be greater than or equal to time_from")

        i0 = max(0, int(full_index.searchsorted(from_dt, side="left")))
        i1 = min(len(full_index) - 1, int(full_index.searchsorted(to_dt, side="right") - 1))
        if i1 < i0:
            raise ValueError("Selected time window does not overlap with available data")

        sel_index = full_index[i0:i1 + 1]
        resolution_used = _resolve_resolution(resolution, sel_index)

        # Auto-downgrade hourly to daily if payload would be too large
        if resolution_used == "hourly" and len(sel_index) * len(requested_present) > _MAP_HOURLY_MAX_POINTS:
            resolution_used = "daily"

        # h5py requires indices in increasing order; create mapping to restore original order
        col_indices = [idx_by_reach[rid] for rid in requested_present]
        sorted_indices = sorted(enumerate(col_indices), key=lambda x: x[1])
        sorted_col_indices = [x[1] for x in sorted_indices]
        sort_order = [x[0] for x in sorted_indices]
        
        arr = ds[i0:i1 + 1, sorted_col_indices] * 1_000_000.0
        # Reorder columns back to match requested_present order
        arr = arr[:, [sort_order.index(i) for i in range(len(sort_order))]]

    frame = pd.DataFrame(arr, index=sel_index, columns=requested_present)
    if resolution_used == "daily":
        frame = frame.resample("D").max()

    times = [t.isoformat() for t in frame.index.to_pydatetime()]
    series = [{"reach_id": rid, "values": frame[rid].astype(float).round(6).tolist()} for rid in requested_present]
    payload = {
        "times": times,
        "series": series,
        "meta": {
            "units": "ng/L",
            "resolution_used": resolution_used,
            "requested_resolution": (resolution or "auto").lower(),
            "time_from": times[0] if times else None,
            "time_to": times[-1] if times else None,
            "requested_reach_count": len(norm_reach_ids),
            "returned_reach_count": len(requested_present),
            "missing_reaches": missing,
            "available_time_start": full_index[0].isoformat(),
            "available_time_end": full_index[-1].isoformat(),
        },
    }
    _cache_set(_map_timeseries_cache, cache_key, payload, _MAP_TIMESERIES_CACHE_LIMIT)
    return payload


def list_map_explorer_runs(run_root):
    """Return runs that have scenario geometry and arr.dat store for map explorer."""
    candidates = list_runs_with_mcs(run_root)
    items = []
    for entry in candidates:
        experiment = entry["experiment"]
        run_path = os.path.join(run_root, experiment)
        params = _parse_user_xml(run_path)
        scenario_rel = (entry.get("landscape_scenario") or "").strip()
        scenario_path = os.path.abspath(os.path.join(BASE_DIR, scenario_rel)) if scenario_rel else ""
        shapefile = _find_reach_shapefile(scenario_path) if scenario_path and os.path.isdir(scenario_path) else None
        valid_mcs = [mc for mc in entry.get("mcs", []) if mc.get("has_store")]
        if not valid_mcs:
            continue
        items.append({
            "experiment": experiment,
            "scenario_path": scenario_rel,
            "scenario_name": os.path.basename(scenario_path) if scenario_path else "unknown",
            "geometry_available": bool(shapefile),
            "simulation_start": (params.get("Scenario/SimulationStart") or "").strip(),
            "simulation_end": (params.get("Scenario/SimulationEnd") or "").strip(),
            "mcs": valid_mcs,
        })
    return items


# ═══════════════════════════════════════════════════════════════════
# Analysis helpers
# ═══════════════════════════════════════════════════════════════════

ANALYSIS_EXPOSURE_MODEL_DATASETS = {
    "CascadeToxswa": {
        "pecsw": "CascadeToxswa/ConLiqWatTgtAvg",
        "pecsed": "CascadeToxswa/CntSedTgt1",
    },
    "StepsRiverNetwork": {
        "pecsw": "StepsRiverNetwork/PEC_SW",
        "pecsed": "StepsRiverNetwork/PEC_SED",
    },
}


def get_analysis_exposure_models(run_root: str, experiment: str, mc_run: str) -> dict:
    """Inspect arr.dat and report which exposure models have usable exposure datasets."""
    if not experiment or not mc_run:
        return {"available_models": [], "details": {}, "message": "experiment and mc_run are required"}
    for val, name in ((experiment, "experiment"), (mc_run, "mc_run")):
        if any(c in val for c in (os.sep, "/", "\\", "..")):
            raise ValueError(f"Invalid {name}")

    arr_path = os.path.abspath(os.path.join(run_root, experiment, "mcs", mc_run, "store", "arr.dat"))
    if not os.path.isfile(arr_path):
        return {
            "available_models": [],
            "details": {},
            "arr_path": arr_path,
            "message": f"arr.dat not found: {arr_path}",
        }

    script = r'''
import json
import sys
import h5py

payload = json.loads(sys.argv[1])
arr_path = payload["arr_path"]
datasets = payload["datasets"]
details = {}
available = []

with h5py.File(arr_path, "r") as f:
    for model, keys in datasets.items():
        pecsw_key = keys["pecsw"]
        pecsed_key = keys["pecsed"]
        has_pecsw = pecsw_key in f
        has_pecsed = pecsed_key in f
        details[model] = {
            "pecsw": has_pecsw,
            "pecsed": has_pecsed,
        }
        if has_pecsw and has_pecsed:
            available.append(model)

print(json.dumps({
    "available_models": available,
    "details": details,
    "arr_path": arr_path,
}))
'''
    return _embedded_json_call(
        script,
        {"arr_path": arr_path, "datasets": ANALYSIS_EXPOSURE_MODEL_DATASETS},
    )

def list_runs_with_mcs(run_root):
    """Return analysis runs with MC and scenario metadata, sorted newest first."""
    result = []
    if not os.path.isdir(run_root):
        return result
    for entry in sorted(os.listdir(run_root), reverse=True):
        run_path = os.path.join(run_root, entry)
        if not os.path.isdir(run_path):
            continue
        params = _parse_user_xml(run_path)
        landscape_scenario = (
            params.get("Scenario/LandscapeScenario")
            or params.get("Scenario/Project")
            or ""
        ).strip()
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
            result.append({
                "experiment": entry,
                "mcs": mcs,
                "landscape_scenario": landscape_scenario,
            })
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
                _running_started_at.pop(rid, None)

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

        if not elapsed and status in ("running", "initializing"):
            with _proc_lock:
                started = _running_started_at.get(entry)
            if started is not None:
                runs[-1]["elapsed"] = _format_elapsed_from_seconds(time.time() - started)

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
        has_finished = any("MC run finished" in e["msg"] for e in entries)
        has_error_completion = any("MC run completed with errors" in e["msg"] for e in entries)
        has_warning_completion = any("MC run completed with warnings" in e["msg"] for e in entries)

        if has_error_completion:
            mc_status = "error"
        elif has_warning_completion:
            mc_status = "warning"
        elif has_finished:
            mc_status = "finished"
        else:
            mc_status = "running"

        mc_elapsed = ""
        if mc_status in ("finished", "error", "warning"):
            for e in reversed(entries):
                if e["msg"].startswith("Elapsed time:"):
                    mc_elapsed = e["msg"][len("Elapsed time:"):].strip()
                    break

        total = len(initialized) if initialized else 1
        progress = round(len(done) / total, 4)
        if mc_status in ("finished", "warning", "error"):
            progress = 1.0
        mc_runs.append({
            "name": mc_name,
            "status": mc_status,
            "elapsed": mc_elapsed,
            "initialized": initialized,
            "components_done": done,
            "current_component": current,
            "progress": progress,
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

    if not elapsed and status in ("running", "initializing"):
        with _proc_lock:
            started = _running_started_at.get(run_id)
        if started is not None:
            elapsed = _format_elapsed_from_seconds(time.time() - started)

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
            # Always serve fresh SPA assets; stale browser cache caused UI regressions to linger.
            self.send_header("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
            self.send_header("Pragma", "no-cache")
            self.send_header("Expires", "0")
            self.end_headers()
            self.wfile.write(data)
        except FileNotFoundError:
            self.send_error(404)

    def _query_params(self):
        parsed = parse_qs(urlparse(self.path).query, keep_blank_values=True)
        return {key: values[-1] if values else "" for key, values in parsed.items()}

    def log_message(self, fmt, *args):
        pass  # suppress per-request logging

    # ── GET routes ────────────────────────────────────────────────

    def do_GET(self):
        raw_path = self.path.split("?")[0]
        path = unquote_plus(raw_path)
        if path != "/":
            path = path.rstrip("/")

        # Keep this endpoint robust against encoded/trailing-slash variations.
        if path.rstrip("/") == "/api/analysis-portable-check":
            return self._json_response(check_analysis_portable())

        # Server status endpoint (no auth required, useful for monitoring and debugging)
        if path == "/api/controlpanel/status":
            return self._json_response(_get_server_status_info())


        if path in ("/", "/index.html"):
            self._serve_file("index.html", "text/html")

        # -- parameterisation --
        elif path == "/api/template":
            self._json_response(parse_xrun_template(TEMPLATE_PATH))
        elif path == "/api/scenarios":
            self._json_response(get_scenarios())
        elif path == "/api/scenario-extent":
            scenario_path = self._query_params().get("path", "")
            try:
                payload = get_scenario_extent(scenario_path)
                status = 400 if payload.get("error") else 200
                self._json_response(payload, status)
            except Exception as exc:
                self._json_response({"error": f"Scenario extent failed: {exc}"}, 500)
        elif path == "/api/scenario-inspect":
            scenario_path = self._query_params().get("path", "")
            try:
                payload = inspect_scenario(scenario_path)
                status = 400 if payload.get("error") else 200
                self._json_response(payload, status)
            except Exception as exc:
                self._json_response({"error": f"Scenario inspect failed: {exc}"}, 500)
        elif path == "/api/scenario-geometry":
            scenario_path = self._query_params().get("path", "")
            try:
                self._json_response({"status": "success", **build_scenario_geometry(scenario_path)})
            except Exception as exc:
                self._json_response({"status": "error", "message": str(exc)}, 400)
        elif path.startswith("/api/scenario-subset/status/"):
            job_id = path.rstrip("/").split("/")[4]
            payload = subset_job_status(job_id)
            status = 404 if payload.get("error") else 200
            self._json_response(payload, status)
        elif path == "/api/log-warning-downgrades":
            self._json_response({"rules": get_warning_downgrade_rules()})

        # -- monitoring --
        elif path == "/api/runs":
            self._json_response(discover_runs(self.run_root))
        elif path == "/api/map-explorer/runs" or self.path.startswith("/api/map-explorer/runs?"):
            qs = parse_qs(urlparse(self.path).query)
            run_root_raw = qs.get("run_root", [""])[0].strip()
            run_root = os.path.abspath(run_root_raw) if run_root_raw else self.run_root
            self._json_response({"runs": list_map_explorer_runs(run_root)})
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
        elif path == "/api/analysis/exposure-models" or self.path.startswith("/api/analysis/exposure-models?"):
            qs = parse_qs(urlparse(self.path).query)
            experiment = qs.get("experiment", [""])[0].strip()
            mc_run = qs.get("mc_run", [""])[0].strip()
            run_root_raw = qs.get("run_root", [""])[0].strip()
            run_root = os.path.abspath(run_root_raw) if run_root_raw else self.run_root
            try:
                self._json_response(get_analysis_exposure_models(run_root, experiment, mc_run))
            except ValueError as exc:
                self._json_response({"message": str(exc)}, 400)
            except RuntimeError as exc:
                self._json_response({"message": str(exc)}, 500)
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
        raw_path = self.path.split("?")[0]
        path = unquote_plus(raw_path)
        if path != "/":
            path = path.rstrip("/")
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
            elif path == "/api/scenario-subset/start":
                self._handle_scenario_subset_start()
            elif path.startswith("/api/scenario-subset/cancel/"):
                self._handle_scenario_subset_cancel(path)
            elif path == "/api/scenario-subset":
                self._handle_scenario_subset()
            elif path == "/api/map-explorer/geometry":
                self._handle_map_geometry()
            elif path == "/api/map-explorer/timeseries":
                self._handle_map_timeseries()
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

    def _handle_map_geometry(self):
        data = self._read_json_body() or {}
        experiment = (data.get("experiment") or "").strip()
        mc_run = (data.get("mc_run") or "").strip()
        run_root_raw = (data.get("run_root") or "").strip()
        run_root = os.path.abspath(run_root_raw) if run_root_raw else self.run_root
        if not experiment or not mc_run:
            return self._json_response(
                {"status": "error", "message": "experiment and mc_run are required"},
                400,
            )
        try:
            paths = _resolve_mc_store_paths(run_root, experiment, mc_run)
            geom_payload = _build_map_geometry(paths["arr_path"], paths["scenario_path"])
            self._json_response({
                "status": "success",
                "scenario_path": paths["scenario_rel"],
                "geojson": geom_payload["geojson"],
                "lulc_geojson": geom_payload.get("lulc_geojson", {"type": "FeatureCollection", "features": []}),
                "meta": geom_payload["meta"],
                "lulc_meta": geom_payload.get("lulc_meta", {}),
            })
        except Exception as exc:
            self._json_response({"status": "error", "message": str(exc)}, 400)

    def _handle_scenario_subset(self):
        data = self._read_json_body() or {}
        source_scenario = (data.get("source_scenario") or "").strip()
        target_name = (data.get("target_name") or "").strip()
        subset_start = (data.get("subset_start") or "").strip()
        subset_end = (data.get("subset_end") or "").strip()
        selected_reaches = data.get("selected_reaches") or []
        max_lulc_distance = int(data.get("max_lulc_distance") or 100)
        max_number_cores = _coerce_max_number_cores(data.get("max_number_cores"))
        if not source_scenario or not target_name or not subset_start or not subset_end:
            return self._json_response(
                {"status": "error", "message": "source_scenario, target_name, subset_start, and subset_end are required"},
                400,
            )
        try:
            payload = create_scenario_subset(
                source_scenario,
                target_name,
                subset_start,
                subset_end,
                selected_reaches=selected_reaches,
                max_lulc_distance=max_lulc_distance,
                max_number_cores=max_number_cores,
            )
            self._json_response({
                "status": "success",
                "message": f"Created subset scenario {payload['scenario_path']}",
                **payload,
            })
        except Exception as exc:
            self._json_response({"status": "error", "message": str(exc)}, 400)

    def _handle_scenario_subset_start(self):
        data = self._read_json_body() or {}
        source_scenario = (data.get("source_scenario") or "").strip()
        target_name = (data.get("target_name") or "").strip()
        subset_start = (data.get("subset_start") or "").strip()
        subset_end = (data.get("subset_end") or "").strip()
        selected_reaches = data.get("selected_reaches") or []
        max_lulc_distance = int(data.get("max_lulc_distance") or 100)
        max_number_cores = _coerce_max_number_cores(data.get("max_number_cores"))
        if not source_scenario or not target_name or not subset_start or not subset_end:
            return self._json_response(
                {"status": "error", "message": "source_scenario, target_name, subset_start, and subset_end are required"},
                400,
            )
        try:
            job_id = start_subset_job(
                source_scenario,
                target_name,
                subset_start,
                subset_end,
                selected_reaches=selected_reaches,
                max_lulc_distance=max_lulc_distance,
                max_number_cores=max_number_cores,
            )
            self._json_response({
                "status": "success",
                "job_id": job_id,
                "message": "Subset creation started",
            })
        except Exception as exc:
            self._json_response({"status": "error", "message": str(exc)}, 400)

    def _handle_scenario_subset_cancel(self, path: str):
        job_id = path.rstrip("/").split("/")[4] if path else ""
        if not job_id:
            return self._json_response({"status": "error", "message": "job_id is required"}, 400)
        payload = cancel_subset_job(job_id)
        if payload.get("error"):
            return self._json_response({"status": "error", "message": payload["error"]}, 404)
        self._json_response({"status": "success", **payload})

    def _handle_map_timeseries(self):
        data = self._read_json_body() or {}
        experiment = (data.get("experiment") or "").strip()
        mc_run = (data.get("mc_run") or "").strip()
        run_root_raw = (data.get("run_root") or "").strip()
        run_root = os.path.abspath(run_root_raw) if run_root_raw else self.run_root
        if not experiment or not mc_run:
            return self._json_response(
                {"status": "error", "message": "experiment and mc_run are required"},
                400,
            )
        try:
            reach_ids = _coerce_list_of_reach_ids(data.get("reach_ids") or [])
            paths = _resolve_mc_store_paths(run_root, experiment, mc_run)
            payload = _build_map_timeseries(
                paths["arr_path"],
                reach_ids,
                time_from=(data.get("time_from") or "").strip() or None,
                time_to=(data.get("time_to") or "").strip() or None,
                resolution=(data.get("resolution") or "auto").strip() or "auto",
            )
            self._json_response({"status": "success", **payload})
        except Exception as exc:
            self._json_response({"status": "error", "message": str(exc)}, 400)

    def _handle_xrun_files(self):
        data = self._read_json_body()
        if not data or not data.get("path", "").strip():
            return self._json_response(
                {"status": "error", "message": "Path not provided"}, 400
            )
        files = get_available_parameter_files(data["path"].strip())
        self._json_response({"status": "success", "files": files, "count": len(files)})

    def _handle_run(self):
        params = self._read_json_body()
        base_sim_id = params.get(
            "Control/ExperimentID", params.get("ExperimentID", "Simulation")
        )
        base_sim_id = re.sub(r"_(?:\d{6}\d{6}|\d{6}-\d{6}|\d{8}-\d{6}|\d{14})$", "", str(base_sim_id).strip())
        ts = datetime.datetime.now().strftime("%d%m%Y-%H%M%S")
        sim_id = f"{base_sim_id}_{ts}"

        # Use timestamped experiment ID for execution folder names under run/.
        params["Control/ExperimentID"] = sim_id
        params["ExperimentID"] = sim_id

        output_filename = f"{sim_id}.xrun"
        output_path = os.path.join(OUTPUT_DIR, output_filename)
        run_params = _normalize_run_parameters(params, output_path)
        create_xrun_file(run_params, output_path, TEMPLATE_PATH)

        def _launch():
            try:
                proc = subprocess.Popen(
                    [START_BAT, output_path],
                    cwd=BASE_DIR,
                    creationflags=subprocess.CREATE_NEW_CONSOLE,
                )
                with _proc_lock:
                    _running_processes[sim_id] = proc
                    _running_started_at[sim_id] = time.time()
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
            fn = normalize_parameter_filename(data["filename"])
            save_dir = os.path.abspath(data["path"])
            stem = Path(fn).stem
        else:
            sid = params.get("Control/ExperimentID", "Simulation")
            save_dir = OUTPUT_DIR
            stem = sid

        os.makedirs(save_dir, exist_ok=True)
        xrun_output = os.path.join(save_dir, f"{stem}.xrun")
        yaml_output = os.path.join(save_dir, f"{stem}.yaml")

        write_parameter_file(params, xrun_output)
        write_parameter_file(params, yaml_output)
        self._json_response({
            "status": "success",
            "message": (
                f"Configuration saved to: {os.path.basename(xrun_output)} and "
                f"{os.path.basename(yaml_output)}"
            ),
            "xrun_path": xrun_output,
            "yaml_path": yaml_output,
        })

    def _handle_save_as(self):
        data = self._read_json_body()
        fn = normalize_parameter_filename(data.get("filename", "configuration"))
        save_dir = os.path.abspath(data.get("path", OUTPUT_DIR))
        os.makedirs(save_dir, exist_ok=True)
        output = os.path.join(save_dir, fn)
        write_parameter_file(data.get("parameters", {}), output)
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
                _running_started_at.pop(run_id, None)
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
            _running_started_at.pop(run_id, None)

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

        runtime_error = _analysis_runtime_error()
        if runtime_error:
            return self._json_response(
                {"status": "error", "message": runtime_error}, 500)
        analysis_python = _pick_analysis_python()
        if not analysis_python:
            return self._json_response(
                {"status": "error", "message": "Analysis runtime disappeared during startup. Please retry."},
                500,
            )
        scenario_rel  = (data.get("scenario_path") or "").strip()
        scenario_path = (os.path.abspath(os.path.join(BASE_DIR, scenario_rel))
                         if scenario_rel else BASE_DIR)
        scenario_name = (data.get("scenario_name") or "").strip()
        exposure_model = (data.get("exposure_model") or "CascadeToxswa").strip()
        _valid_exposure_models = {"CascadeToxswa", "StepsRiverNetwork"}
        if exposure_model not in _valid_exposure_models:
            return self._json_response(
                {"status": "error",
                 "message": f"Invalid exposure_model '{exposure_model}'. "
                            f"Must be one of: {', '.join(sorted(_valid_exposure_models))}"},
                400,
            )
        ts        = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
        subfolder = f"{experiment}_{mc_run}__{ts}"
        job_id    = subfolder
        out_raw   = (data.get("output_dir") or "").strip()
        base_dir  = os.path.abspath(out_raw) if out_raw else ANALYSIS_OUTPUT_ROOT
        out_dir   = os.path.join(base_dir, subfolder)
        os.makedirs(out_dir, exist_ok=True)
        log_path = os.path.join(out_dir, "analysis.log")
        cmd = [
            analysis_python, ANALYSIS_SCRIPT,
            "--mc-path",       mc_path,
            "--scenario-path", scenario_path,
            "--scenario-name", scenario_name,
            "--output-dir",    out_dir,
            "--run-pec",       str(data.get("run_pec",       True)).lower(),
            "--run-guts",      str(data.get("run_guts",      True)).lower(),
            "--exposed-only",  str(data.get("exposed_only",  False)).lower(),
            "--exposure-model", exposure_model,
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
                                        cwd=BASE_DIR,
                                        env=_analysis_subprocess_env())
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
        if not os.path.isfile(xrun_path):
            return self._json_response(
                {"status": "error", "message": "File not found"}, 404
            )
        ext = os.path.splitext(xrun_path)[1].lower()
        if ext not in PARAM_FILE_EXTENSIONS:
            return self._json_response(
                {"status": "error", "message": "Unsupported file extension"}, 400
            )
        params = parse_parameter_file(xrun_path)
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
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    try:
        _acquire_single_instance_lock(args.port)
    except RuntimeError as exc:
        print(f"[controlpanel] {exc}")
        sys.exit(1)

    try:
        server = HTTPServer(("0.0.0.0", args.port), ControlPanelHandler)
        print("=" * 60)
        print("  xAquaticRisk Control Panel")
        print("=" * 60)
        print(f"  URL              : http://localhost:{args.port}")
        print(f"  Template         : {TEMPLATE_PATH}")
        print(f"  Parameterisation : {OUTPUT_DIR}")
        print(f"  Run folder       : {ControlPanelHandler.run_root}")
        print(f"  Scenarios        : {os.path.join(BASE_DIR, 'scenario')}")
        print("=" * 60)
        print("  Press Ctrl+C to stop.\n")
        try:
            server.serve_forever()
        except KeyboardInterrupt:
            print("\nShutting down.")
        finally:
            server.server_close()
    finally:
        _release_single_instance_lock()


if __name__ == "__main__":
    main()
