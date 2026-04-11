"""Step 2 inflow CSV trimming backend.

Provides chunked pandas filtering and multi-process execution for scenario
subset creation while keeping behavior compatible with the legacy CSV path.
"""

import datetime
import multiprocessing
import os
import threading
import time
from multiprocessing import Pool
from typing import Any, Dict, Optional

try:
    import pandas as pd
except ImportError:
    pd = None


def _normalize_reach_id(value):
    """Normalize reach ID to canonical form."""
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    # Try to coerce numeric strings to canonical int form
    try:
        return str(int(float(text)))
    except (ValueError, OverflowError):
        return text


def _format_hydro_datetime(dt: datetime.datetime) -> str:
    """Format datetime as YYYY-MM-DDTHH:MM."""
    return dt.strftime("%Y-%m-%dT%H:%M")


def _is_plausible_hydro_timestamp(value: str) -> bool:
    """Quick format check for hydro timestamps."""
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


def _timestamp_mask(series, start_key: str, end_key: str):
    """Return a strict-validity timestamp mask for a pandas string series."""
    ts = series.astype("string").str.strip()
    plausible = ts.str.len() == 16
    plausible &= ts.str[4] == "-"
    plausible &= ts.str[7] == "-"
    plausible &= ts.str[10] == "T"
    plausible &= ts.str[13] == ":"
    plausible &= ts.str[0:4].str.isdigit()
    plausible &= ts.str[5:7].str.isdigit()
    plausible &= ts.str[8:10].str.isdigit()
    plausible &= ts.str[11:13].str.isdigit()
    plausible &= ts.str[14:16].str.isdigit()
    in_range = plausible & (ts >= start_key) & (ts <= end_key)
    if not bool(in_range.any()):
        return in_range
    parsed = pd.to_datetime(ts[in_range], format="%Y-%m-%dT%H:%M", errors="coerce")
    strict_ok = parsed.notna()
    out = in_range.copy()
    out.loc[in_range] = strict_ok.values
    return out


def _slice_timeseries_csv_pandas(
    source_csv: str,
    target_csv: str,
    start_dt: datetime.datetime,
    end_dt: datetime.datetime,
    cancel_cb=None,
    selected_reach_ids=None,
    chunksize: int = 200000,
) -> Optional[int]:
    """
    Fast CSV trimming using pandas chunked reader with vectorized filtering.
    
    Returns:
        Number of rows kept, or None if fallback to CSV backend is needed
    """
    if pd is None:
        return None
    
    os.makedirs(os.path.dirname(target_csv), exist_ok=True)
    kept_rows = 0
    start_key = _format_hydro_datetime(start_dt)
    end_key = _format_hydro_datetime(end_dt)
    selected_set = set(_normalize_reach_id(v) for v in (selected_reach_ids or [])) if selected_reach_ids else None
    
    try:
        first_chunk = True
        with open(target_csv, "w", encoding="utf-8", newline="") as out_f:
            for chunk in (
                pd.read_csv(
                    source_csv,
                    dtype=str,
                    chunksize=chunksize,
                    encoding="utf-8",
                    on_bad_lines="skip",
                )
            ):
                if cancel_cb and cancel_cb():
                    raise RuntimeError("Cancelled")
                
                if chunk.empty or len(chunk.columns) < 3:
                    continue

                # Mirror legacy behavior for malformed rows: require at least 3 columns.
                wellformed = chunk.iloc[:, 2].notna()
                ts_mask = _timestamp_mask(chunk.iloc[:, 1], start_key, end_key)
                chunk_filtered = chunk[wellformed & ts_mask]
                if chunk_filtered.empty:
                    continue

                # Vectorized reach filtering if selected
                if selected_set is not None:
                    reach_col = chunk_filtered.iloc[:, 0].astype("string").str.strip()
                    reach_norm = reach_col.apply(_normalize_reach_id)
                    chunk_filtered = chunk_filtered[reach_norm.isin(selected_set)].copy()

                if chunk_filtered.empty:
                    continue

                # Write chunk
                chunk_filtered.to_csv(out_f, header=first_chunk, index=False, encoding="utf-8")
                kept_rows += len(chunk_filtered)
                first_chunk = False

        return kept_rows
    except Exception:
        # On any error, fallback to CSV backend
        return None


def _slice_timeseries_csv_worker(args: Dict[str, Any]) -> tuple:
    """
    Worker function for parallel CSV processing.
    
    Args from dict:
        - source_csv: path to source file
        - target_csv: path to target file
        - start_dt: start datetime
        - end_dt: end datetime
        - selected_reach_ids: list of reach IDs or None
        - method: "pandas" or "csv"
    
    Returns:
        (source_file, kept_rows, elapsed_ms, error_msg)
    """
    start_time = time.time()
    try:
        source_csv = args["source_csv"]
        target_csv = args["target_csv"]
        start_dt = args["start_dt"]
        end_dt = args["end_dt"]
        selected_reach_ids = args.get("selected_reach_ids")
        method = args.get("method", "pandas")

        def check_cancel():
            return False

        if method == "pandas":
            kept = _slice_timeseries_csv_pandas(source_csv, target_csv, start_dt, end_dt, check_cancel, selected_reach_ids)
            if kept is not None:
                elapsed_ms = int((time.time() - start_time) * 1000)
                return (source_csv, kept, elapsed_ms, None, "pandas")

        return (source_csv, 0, 0, f"Method '{method}' not available", method)
    except Exception as e:
        elapsed_ms = int((time.time() - start_time) * 1000)
        return (args.get("source_csv", "?"), 0, elapsed_ms, str(e), "error")


def process_csv_files_parallel(
    csv_files: list,
    start_dt: datetime.datetime,
    end_dt: datetime.datetime,
    target_dir: str,
    selected_reach_ids=None,
    num_workers: Optional[int] = None,
    progress_cb=None,
    cancel_cb=None,
) -> Dict[str, Any]:
    """
    Process multiple CSV files in parallel using multiprocessing.
    
    Args:
        csv_files: list of source CSV file paths
        start_dt: start datetime
        end_dt: end datetime
        target_dir: target directory for output files
        selected_reach_ids: list of reach IDs or None
        num_workers: number of processes (default: cpu_count - 1)
        progress_cb: progress callback(pct, message)
        cancel_cb: cancellation callback
    
    Returns:
        {
            "processed": count,
            "kept_total": total rows kept,
            "elapsed_ms": total time,
            "results": {filename: (kept_rows, elapsed_ms, error)},
            "backend": "pandas",
        }
    """
    if not csv_files:
        return {
            "processed": 0,
            "kept_total": 0,
            "elapsed_ms": 0,
            "results": {},
            "failed_files": [],
            "backend": "parallel",
        }
    
    if pd is None:
        return {"processed": 0, "kept_total": 0, "elapsed_ms": 0, "results": {}, "error": "pandas not available"}
    
    num_workers = num_workers or max(1, multiprocessing.cpu_count() - 1)
    num_workers = max(1, min(num_workers, len(csv_files)))
    kept_total = 0
    start_time = time.time()

    if cancel_cb and cancel_cb():
        return {
            "processed": 0,
            "kept_total": 0,
            "elapsed_ms": int((time.time() - start_time) * 1000),
            "results": {},
            "failed_files": list(csv_files),
            "error": "Cancelled",
        }
    
    try:
        os.makedirs(target_dir, exist_ok=True)
        worker_args = []
        for csv_file in csv_files:
            target_csv = os.path.join(target_dir, os.path.basename(csv_file))
            worker_args.append(
                {
                    "source_csv": csv_file,
                    "target_csv": target_csv,
                    "start_dt": start_dt,
                    "end_dt": end_dt,
                    "selected_reach_ids": selected_reach_ids,
                    "method": "pandas",
                }
            )

        results_dict = {}
        failed_files = []
        processed = 0

        with Pool(processes=num_workers, maxtasksperchild=8) as pool:
            cancel_stop = threading.Event()
            cancelled_flag = {"value": False}

            def _cancel_watcher():
                while not cancel_stop.wait(0.2):
                    if cancel_cb and cancel_cb():
                        cancelled_flag["value"] = True
                        try:
                            pool.terminate()
                        except Exception:
                            pass
                        return

            watcher = threading.Thread(target=_cancel_watcher, daemon=True)
            watcher.start()
            try:
                for source_file, kept, elapsed, error, backend in pool.imap_unordered(_slice_timeseries_csv_worker, worker_args, chunksize=1):
                    processed += 1
                    filename = os.path.basename(source_file)
                    if error:
                        failed_files.append(source_file)
                        results_dict[filename] = (0, elapsed, error)
                    else:
                        results_dict[filename] = (kept, elapsed, None)
                        kept_total += kept

                    if progress_cb:
                        pct = int(80 + 15 * (processed / len(csv_files)))
                        progress_cb(pct, f"Processing inflow CSV ({processed}/{len(csv_files)}): {filename} [{backend}]")

                    if cancel_cb and cancel_cb():
                        cancelled_flag["value"] = True
                        pool.terminate()
                        raise RuntimeError("Cancelled")
            finally:
                cancel_stop.set()

            if cancelled_flag["value"]:
                raise RuntimeError("Cancelled")

        elapsed_ms = int((time.time() - start_time) * 1000)
        return {
            "processed": processed,
            "kept_total": kept_total,
            "elapsed_ms": elapsed_ms,
            "results": results_dict,
            "failed_files": failed_files,
            "backend": "parallel",
        }
    except Exception as e:
        err_text = str(e)
        if cancel_cb and cancel_cb() and "cancel" not in err_text.lower():
            err_text = "Cancelled"
        return {
            "processed": 0,
            "kept_total": 0,
            "elapsed_ms": int((time.time() - start_time) * 1000),
            "results": {},
            "failed_files": list(csv_files),
            "error": err_text,
        }
