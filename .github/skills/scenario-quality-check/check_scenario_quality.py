#!/usr/bin/env python3
"""
Deep quality checker for generated landscape scenarios.

Compares a candidate scenario against a reference scenario and validates:
- scenario structure
- hydro/hydro_reaches.h5 consistency and data quality
- consistency between HDF5 reaches, reach shapefile, and TimeSeries CSV files
"""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import json
import math
import os
import re
import sys
from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence, Set, Tuple

import h5py

try:
    import geopandas as gpd
except Exception:  # pragma: no cover
    gpd = None

HYDRO_DATASETS = ("flow", "depth", "volume", "area")
REQUIRED_HDF_DATASETS = HYDRO_DATASETS + ("reaches", "time_from", "time_to")
TIME_FORMAT = "%Y-%m-%dT%H:%M"


@dataclass
class Finding:
    severity: str
    code: str
    message: str
    hint: str = ""


def norm_reach_id(value) -> Optional[str]:
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if re.fullmatch(r"[-+]?\d+(?:\.0+)?", text):
        return str(int(float(text)))
    return text


def parse_hydro_time(raw) -> Optional[dt.datetime]:
    if raw is None:
        return None
    if isinstance(raw, bytes):
        text = raw.decode("ascii", errors="replace")
    else:
        text = str(raw)
    text = text.strip()
    if not text:
        return None
    try:
        return dt.datetime.strptime(text, TIME_FORMAT)
    except ValueError:
        return None


def find_reach_shapefile(scenario_path: str) -> Optional[str]:
    for name in ("Reachlist_shp.shp", "ReachList_shp.shp"):
        path = os.path.join(scenario_path, "geo", name)
        if os.path.isfile(path):
            return path
    geo_dir = os.path.join(scenario_path, "geo")
    if os.path.isdir(geo_dir):
        shp_files = sorted(
            os.path.join(geo_dir, f)
            for f in os.listdir(geo_dir)
            if f.lower().endswith(".shp")
        )
        if shp_files:
            return shp_files[0]
    return None


def resolve_scenario(base_dir: str, scenario_arg: str) -> str:
    candidate = os.path.abspath(scenario_arg)
    if os.path.isdir(candidate):
        return candidate
    candidate = os.path.abspath(os.path.join(base_dir, scenario_arg))
    if os.path.isdir(candidate):
        return candidate
    candidate = os.path.abspath(os.path.join(base_dir, "scenario", scenario_arg))
    if os.path.isdir(candidate):
        return candidate
    raise FileNotFoundError(f"Scenario not found: {scenario_arg}")


def inspect_hdf5(hdf_path: str) -> Dict:
    findings: List[Finding] = []
    summary: Dict = {"path": hdf_path}
    if not os.path.isfile(hdf_path):
        findings.append(Finding("critical", "hdf_missing", f"Hydrology file not found: {hdf_path}"))
        return {"summary": summary, "findings": findings, "reach_ids": [], "interval": (None, None)}

    with h5py.File(hdf_path, "r") as hf:
        missing = [d for d in REQUIRED_HDF_DATASETS if d not in hf]
        if missing:
            findings.append(
                Finding(
                    "critical",
                    "hdf_datasets_missing",
                    f"Missing required HDF5 datasets: {', '.join(missing)}",
                    "Ensure subset generation writes all mandatory hydro datasets.",
                )
            )

        for name in HYDRO_DATASETS:
            if name not in hf:
                continue
            if hf[name].ndim != 2:
                findings.append(
                    Finding("critical", "hdf_dataset_rank", f"Dataset '{name}' is not 2D (ndim={hf[name].ndim}).")
                )

        shapes = {}
        for name in HYDRO_DATASETS:
            if name in hf and hf[name].ndim == 2:
                shapes[name] = tuple(hf[name].shape)
        summary["dataset_shapes"] = shapes
        unique_shapes = {v for v in shapes.values()}
        if len(unique_shapes) > 1:
            findings.append(
                Finding(
                    "critical",
                    "hdf_shape_mismatch",
                    f"Hydro dataset shapes differ: {shapes}",
                    "All hydro arrays must have the same [time, reach] shape.",
                )
            )

        reaches: List[str] = []
        if "reaches" in hf:
            reaches = [norm_reach_id(v) for v in hf["reaches"][:]]
            reaches = [r for r in reaches if r is not None]
            summary["reach_count"] = len(reaches)
            unique_reaches = len(set(reaches))
            summary["unique_reach_count"] = unique_reaches
            if unique_reaches != len(reaches):
                findings.append(
                    Finding(
                        "high",
                        "hdf_reach_duplicates",
                        "Duplicate reach IDs in HDF5 'reaches' dataset.",
                        "Reach IDs should be unique and aligned with spatial dimension.",
                    )
                )

        if shapes and reaches:
            first_shape = next(iter(shapes.values()))
            if first_shape[1] != len(reaches):
                findings.append(
                    Finding(
                        "critical",
                        "hdf_reach_dim_mismatch",
                        f"Hydro column count ({first_shape[1]}) does not match reaches length ({len(reaches)}).",
                    )
                )

        time_from = parse_hydro_time(hf["time_from"][0] if "time_from" in hf and len(hf["time_from"]) else None)
        time_to = parse_hydro_time(hf["time_to"][0] if "time_to" in hf and len(hf["time_to"]) else None)
        summary["time_from"] = time_from.isoformat(timespec="minutes") if time_from else None
        summary["time_to"] = time_to.isoformat(timespec="minutes") if time_to else None

        if time_from is None or time_to is None:
            findings.append(Finding("critical", "hdf_time_invalid", "Invalid or missing time_from/time_to metadata."))
        elif time_to < time_from:
            findings.append(Finding("critical", "hdf_time_order", "time_to is earlier than time_from."))
        elif shapes:
            rows = next(iter(shapes.values()))[0]
            expected_rows = int((time_to - time_from).total_seconds() // 3600) + 1
            summary["time_rows"] = rows
            summary["expected_rows_from_metadata"] = expected_rows
            if rows != expected_rows:
                findings.append(
                    Finding(
                        "high",
                        "hdf_time_rows_mismatch",
                        f"Hydro row count ({rows}) does not match metadata-derived hourly rows ({expected_rows}).",
                    )
                )

        # Chunked finite-value scan for each hydro dataset.
        finite_stats = {}
        for name in HYDRO_DATASETS:
            if name not in hf or hf[name].ndim != 2:
                continue
            ds = hf[name]
            rows, cols = ds.shape
            finite = 0
            total = rows * cols
            dmin = math.inf
            dmax = -math.inf
            chunk = min(4096, rows) if rows else 1
            for r0 in range(0, rows, chunk):
                block = ds[r0 : r0 + chunk, :]
                finite_mask = (block == block) & (block != math.inf) & (block != -math.inf)
                finite += int(finite_mask.sum())
                if finite_mask.any():
                    vals = block[finite_mask]
                    dmin = min(dmin, float(vals.min()))
                    dmax = max(dmax, float(vals.max()))
            ratio = (finite / total) if total else 1.0
            finite_stats[name] = {
                "finite_ratio": ratio,
                "min": None if dmin is math.inf else dmin,
                "max": None if dmax is -math.inf else dmax,
            }
            if ratio < 1.0:
                sev = "high" if ratio < 0.95 else "medium"
                findings.append(
                    Finding(
                        sev,
                        "hdf_non_finite",
                        f"Dataset '{name}' contains non-finite values (finite ratio={ratio:.6f}).",
                        "Check source hydrology generation and slicing indices.",
                    )
                )
        summary["finite_stats"] = finite_stats

    return {
        "summary": summary,
        "findings": findings,
        "reach_ids": reaches,
        "interval": (summary.get("time_from"), summary.get("time_to")),
    }


def select_reach_column(gdf, hint_ids: Set[str]) -> Optional[str]:
    non_geom = [c for c in gdf.columns if c != "geometry"]
    if not non_geom:
        return None
    preferred = ["reach_id", "reachid", "id", "key", "reach", "name", "segment_id"]
    best = non_geom[0]
    best_score = -1
    for col in non_geom:
        vals = gdf[col].dropna().head(1000)
        score = sum(1 for v in vals if norm_reach_id(v) in hint_ids)
        low = col.lower()
        if low in preferred:
            score += 10
        elif any(p in low for p in preferred):
            score += 4
        if score > best_score:
            best_score = score
            best = col
    return best


def inspect_reach_shapefile(shp_path: Optional[str], hydro_reaches: Sequence[str]) -> Dict:
    findings: List[Finding] = []
    summary: Dict = {"path": shp_path}
    if not shp_path:
        findings.append(Finding("critical", "reach_shp_missing", "Reach shapefile not found in scenario/geo."))
        return {"summary": summary, "findings": findings, "reach_ids": []}
    if gpd is None:
        findings.append(Finding("medium", "geopandas_missing", "geopandas not available; shapefile checks skipped."))
        return {"summary": summary, "findings": findings, "reach_ids": []}

    gdf = gpd.read_file(shp_path)
    if gdf.empty:
        findings.append(Finding("critical", "reach_shp_empty", "Reach shapefile has no features."))
        return {"summary": summary, "findings": findings, "reach_ids": []}

    hint = set(hydro_reaches)
    col = select_reach_column(gdf, hint)
    if col is None:
        findings.append(Finding("critical", "reach_col_missing", "No usable reach-id column in reach shapefile."))
        return {"summary": summary, "findings": findings, "reach_ids": []}

    shp_ids = [norm_reach_id(v) for v in gdf[col].tolist()]
    shp_ids = [x for x in shp_ids if x is not None]
    summary["reach_id_column"] = col
    summary["feature_count"] = int(len(gdf))
    summary["reach_id_count"] = len(shp_ids)
    summary["unique_reach_id_count"] = len(set(shp_ids))

    hset = set(hydro_reaches)
    sset = set(shp_ids)
    missing_in_shape = sorted(hset - sset)
    missing_in_hydro = sorted(sset - hset)
    summary["missing_in_shape_count"] = len(missing_in_shape)
    summary["missing_in_hydro_count"] = len(missing_in_hydro)

    if missing_in_shape:
        findings.append(
            Finding(
                "critical",
                "reach_missing_in_shape",
                f"{len(missing_in_shape)} hydro reaches are missing in reach shapefile.",
                "Filter/synchronize reach shapefile against hydro_reaches.h5 reaches.",
            )
        )
    if missing_in_hydro:
        findings.append(
            Finding(
                "high",
                "reach_missing_in_hydro",
                f"{len(missing_in_hydro)} shapefile reaches are missing in hydro_reaches.h5.",
                "Likely geometry/hydrology mismatch from scenario slicing.",
            )
        )

    return {"summary": summary, "findings": findings, "reach_ids": shp_ids}


def inspect_timeseries(
    ts_dir: str,
    hydro_reaches: Sequence[str],
    t_from: Optional[str],
    t_to: Optional[str],
    max_rows: int,
) -> Dict:
    findings: List[Finding] = []
    summary: Dict = {"path": ts_dir}
    if not os.path.isdir(ts_dir):
        findings.append(Finding("medium", "timeseries_dir_missing", f"Missing TimeSeries directory: {ts_dir}"))
        return {"summary": summary, "findings": findings}

    files = sorted(f for f in os.listdir(ts_dir) if f.lower().endswith(".csv"))
    summary["csv_file_count"] = len(files)
    if not files:
        findings.append(Finding("medium", "timeseries_empty", "No CSV files in hydro/TimeSeries."))
        return {"summary": summary, "findings": findings}

    t0 = dt.datetime.fromisoformat(t_from) if t_from else None
    t1 = dt.datetime.fromisoformat(t_to) if t_to else None
    hset = set(hydro_reaches)

    parse_errors = 0
    reach_outside_hydro = 0
    rows_total = 0
    out_of_range = 0
    files_skipped_schema = 0

    def _find_col(headers: List[str], candidates: Sequence[str]) -> Optional[int]:
        for i, h in enumerate(headers):
            hl = h.lower().strip()
            if any(c in hl for c in candidates):
                return i
        return None

    truncated = False
    for name in files:
        path = os.path.join(ts_dir, name)
        with open(path, "r", encoding="utf-8", newline="") as handle:
            sample = handle.read(4096)
            handle.seek(0)
            try:
                dialect = csv.Sniffer().sniff(sample, delimiters=",;\t")
            except Exception:
                dialect = csv.excel
            reader = csv.reader(handle, dialect=dialect)
            header = next(reader, None)
            if not header:
                files_skipped_schema += 1
                continue
            reach_idx = _find_col(header, ("reach", "key", "segment"))
            time_idx = _find_col(header, ("time", "date"))
            if reach_idx is None or time_idx is None:
                files_skipped_schema += 1
                continue
            for row in reader:
                if rows_total >= max_rows:
                    truncated = True
                    break
                max_idx = max(reach_idx, time_idx)
                if len(row) <= max_idx:
                    continue
                rows_total += 1
                rid = norm_reach_id(row[reach_idx])
                if rid is not None and rid not in hset:
                    reach_outside_hydro += 1
                try:
                    when = dt.datetime.strptime(row[time_idx].strip(), TIME_FORMAT)
                    if t0 and when < t0:
                        out_of_range += 1
                    if t1 and when > t1:
                        out_of_range += 1
                except Exception:
                    parse_errors += 1
        if truncated:
            break

    summary["rows_total"] = rows_total
    summary["rows_scanned_limit"] = max_rows
    summary["rows_scan_truncated"] = truncated
    summary["files_skipped_schema"] = files_skipped_schema
    summary["datetime_parse_errors"] = parse_errors
    summary["rows_with_reach_outside_hydro"] = reach_outside_hydro
    summary["rows_outside_hydro_interval"] = out_of_range

    if truncated:
        findings.append(
            Finding(
                "low",
                "timeseries_scan_truncated",
                f"TimeSeries scan stopped at {max_rows} rows for performance.",
                "Increase --timeseries-max-rows for exhaustive checks.",
            )
        )

    if parse_errors:
        findings.append(Finding("high", "timeseries_datetime_parse", f"{parse_errors} CSV rows have invalid datetime format."))
    if rows_total == 0 and files:
        findings.append(
            Finding(
                "medium",
                "timeseries_no_data_rows",
                "No parseable data rows were detected in TimeSeries CSV files.",
                "Check CSV delimiter/encoding and whether files contain data beyond headers.",
            )
        )
    if files_skipped_schema:
        findings.append(
            Finding(
                "low",
                "timeseries_schema_skipped",
                f"Skipped {files_skipped_schema} CSV files without detectable reach/time columns.",
                "If these files are relevant, extend header detection rules in the checker.",
            )
        )
    if reach_outside_hydro:
        findings.append(
            Finding(
                "high",
                "timeseries_reach_mismatch",
                f"{reach_outside_hydro} CSV rows reference reaches not in hydro_reaches.h5.",
            )
        )
    if out_of_range:
        findings.append(
            Finding(
                "medium",
                "timeseries_time_range",
                f"{out_of_range} CSV rows are outside HDF5 time_from/time_to interval.",
            )
        )

    return {"summary": summary, "findings": findings}


def compare_candidate_reference(candidate: Dict, reference: Dict) -> List[Finding]:
    findings: List[Finding] = []

    csum = candidate["hdf"]["summary"]
    rsum = reference["hdf"]["summary"]

    csets = set((csum.get("dataset_shapes") or {}).keys())
    rsets = set((rsum.get("dataset_shapes") or {}).keys())
    if csets != rsets:
        findings.append(
            Finding(
                "high",
                "compare_dataset_set",
                f"Candidate hydro dataset set differs from reference ({sorted(csets)} vs {sorted(rsets)}).",
            )
        )

    creaches = set(candidate["hdf"].get("reach_ids", []))
    rreaches = set(reference["hdf"].get("reach_ids", []))
    overlap = len(creaches & rreaches)
    if creaches:
        overlap_ratio = overlap / len(creaches)
    else:
        overlap_ratio = 0.0

    if len(creaches) > len(rreaches):
        findings.append(
            Finding(
                "medium",
                "compare_reach_count",
                "Candidate has more reaches than reference.",
                "This may be valid, but double-check scenario/reference pairing.",
            )
        )

    if overlap_ratio < 0.5:
        findings.append(
            Finding(
                "medium",
                "compare_reach_overlap",
                f"Low reach overlap with reference ({overlap_ratio:.2%}).",
                "Ensure the chosen reference scenario is comparable (same catchment family).",
            )
        )

    c_from = csum.get("time_from")
    c_to = csum.get("time_to")
    r_from = rsum.get("time_from")
    r_to = rsum.get("time_to")
    if c_from and c_to and r_from and r_to:
        c0 = dt.datetime.fromisoformat(c_from)
        c1 = dt.datetime.fromisoformat(c_to)
        r0 = dt.datetime.fromisoformat(r_from)
        r1 = dt.datetime.fromisoformat(r_to)
        if c0 < r0 or c1 > r1:
            findings.append(
                Finding(
                    "medium",
                    "compare_time_coverage",
                    "Candidate hydro time interval extends outside reference interval.",
                )
            )

    return findings


def severity_rank(sev: str) -> int:
    return {"critical": 0, "high": 1, "medium": 2, "low": 3}.get(sev, 4)


def scope_findings(scope: str, findings: Sequence[Finding]) -> List[Finding]:
    scoped = []
    for f in findings:
        scoped.append(
            Finding(
                severity=f.severity,
                code=f"{scope}_{f.code}",
                message=f"[{scope}] {f.message}",
                hint=f.hint,
            )
        )
    return scoped


def scan_scenario(scenario_path: str, timeseries_max_rows: int) -> Dict:
    findings: List[Finding] = []
    structure = {
        "scenario_path": scenario_path,
        "exists": os.path.isdir(scenario_path),
    }
    if not structure["exists"]:
        findings.append(Finding("critical", "scenario_missing", f"Scenario folder not found: {scenario_path}"))
        return {"structure": structure, "findings": findings, "hdf": {"summary": {}, "findings": []}}

    hydro_path = os.path.join(scenario_path, "hydro", "hydro_reaches.h5")
    ts_dir = os.path.join(scenario_path, "hydro", "TimeSeries")
    reach_shp = find_reach_shapefile(scenario_path)

    structure.update(
        {
            "hydro_h5": hydro_path,
            "timeseries_dir": ts_dir,
            "reach_shapefile": reach_shp,
        }
    )

    hdf = inspect_hdf5(hydro_path)
    findings.extend(hdf["findings"])

    shp = inspect_reach_shapefile(reach_shp, hdf.get("reach_ids", []))
    findings.extend(shp["findings"])

    ts = inspect_timeseries(
        ts_dir,
        hdf.get("reach_ids", []),
        hdf["summary"].get("time_from"),
        hdf["summary"].get("time_to"),
        max_rows=timeseries_max_rows,
    )
    findings.extend(ts["findings"])

    return {
        "structure": structure,
        "hdf": hdf,
        "shapefile": shp,
        "timeseries": ts,
        "findings": findings,
    }


def print_report(candidate_name: str, reference_name: str, result: Dict):
    cand = result["candidate"]
    ref = result["reference"]
    findings: List[Finding] = result["findings"]

    print("=" * 72)
    print("Scenario Quality Check")
    print("=" * 72)
    print(f"Candidate: {candidate_name}")
    print(f"Reference: {reference_name}")
    print("")

    print("Candidate HDF summary:")
    csum = cand["hdf"]["summary"]
    print(f"- path: {csum.get('path')}")
    print(f"- time_from: {csum.get('time_from')}")
    print(f"- time_to: {csum.get('time_to')}")
    print(f"- reach_count: {csum.get('reach_count')}")
    print(f"- dataset_shapes: {csum.get('dataset_shapes')}")

    ssum = cand["shapefile"]["summary"]
    print("Candidate reach shapefile summary:")
    print(f"- path: {ssum.get('path')}")
    print(f"- feature_count: {ssum.get('feature_count')}")
    print(f"- reach_id_column: {ssum.get('reach_id_column')}")
    print(f"- missing_in_shape_count: {ssum.get('missing_in_shape_count')}")
    print(f"- missing_in_hydro_count: {ssum.get('missing_in_hydro_count')}")

    print("")
    print("Findings:")
    if not findings:
        print("- PASS: no findings")
        return

    findings = sorted(findings, key=lambda x: (severity_rank(x.severity), x.code))
    for f in findings:
        line = f"- [{f.severity.upper()}] {f.code}: {f.message}"
        print(line)
        if f.hint:
            print(f"  hint: {f.hint}")


def to_dict_findings(findings: Sequence[Finding]) -> List[Dict[str, str]]:
    return [{"severity": f.severity, "code": f.code, "message": f.message, "hint": f.hint} for f in findings]


def make_json_safe(value):
    if isinstance(value, Finding):
        return {"severity": value.severity, "code": value.code, "message": value.message, "hint": value.hint}
    if isinstance(value, dict):
        return {k: make_json_safe(v) for k, v in value.items()}
    if isinstance(value, list):
        return [make_json_safe(v) for v in value]
    if isinstance(value, tuple):
        return [make_json_safe(v) for v in value]
    return value


def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(description="Deep scenario quality checker against a reference scenario")
    ap.add_argument("--candidate", required=True, help="Candidate scenario name or path")
    ap.add_argument("--reference", required=True, help="Reference scenario name or path")
    ap.add_argument("--base-dir", default=os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")), help="Repository base directory")
    ap.add_argument("--json-out", default="", help="Optional path to write JSON report")
    ap.add_argument("--timeseries-max-rows", type=int, default=200000, help="Maximum total TimeSeries CSV rows to scan per scenario")
    args = ap.parse_args(argv)

    base_dir = os.path.abspath(args.base_dir)
    candidate_path = resolve_scenario(base_dir, args.candidate)
    reference_path = resolve_scenario(base_dir, args.reference)

    candidate = scan_scenario(candidate_path, timeseries_max_rows=max(1, int(args.timeseries_max_rows)))
    reference = scan_scenario(reference_path, timeseries_max_rows=max(1, int(args.timeseries_max_rows)))

    findings = []
    findings.extend(scope_findings("candidate", candidate.get("findings", [])))
    findings.extend(scope_findings("reference", reference.get("findings", [])))
    findings.extend(scope_findings("compare", compare_candidate_reference(candidate, reference)))

    result = {
        "candidate": candidate,
        "reference": reference,
        "findings": findings,
        "status": "PASS",
    }

    if any(f.severity in ("critical", "high") for f in findings):
        result["status"] = "FAIL"
    elif findings:
        result["status"] = "WARN"

    print_report(candidate_path, reference_path, result)
    print("")
    print(f"Status: {result['status']}")

    if args.json_out:
        out_path = os.path.abspath(args.json_out)
        os.makedirs(os.path.dirname(out_path), exist_ok=True)
        serializable = {
            "status": result["status"],
            "candidate": make_json_safe(result["candidate"]),
            "reference": make_json_safe(result["reference"]),
            "findings": to_dict_findings(findings),
        }
        with open(out_path, "w", encoding="utf-8") as handle:
            json.dump(serializable, handle, indent=2)
        print(f"JSON report written: {out_path}")

    return 1 if result["status"] == "FAIL" else 0


if __name__ == "__main__":
    sys.exit(main())
