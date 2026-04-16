from __future__ import annotations

import re
from pathlib import Path
from typing import Optional


DEFAULT_PEC_TEMPORAL_PERCENTILES = [0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1.0]
DEFAULT_PEC_SPATIAL_PERCENTILES = [0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1.0]
DEFAULT_GUTS_TEMPORAL_PERCENTILES = [0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1.0]
DEFAULT_GUTS_SPATIAL_PERCENTILES = [0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1.0]
DEFAULT_BOXPLOT_TEMPORAL_PERCENTILES = [0.9, 0.99, 1.0]
DEFAULT_LP50_THRESHOLD_RANGE = (0.01, 1000)


SCENARIO_DEFAULTS = {
    "Rummen": {
        "reach_list_single": [1388],
        "reach_list_group": [1610, 1738, 1505],
        "plotzoom_from": "1995-05-01",
        "plotzoom_to": "1995-06-01",
    },
    "Oudebeek": {
        "reach_list_single": [42],
        "reach_list_group": [],
        "plotzoom_from": "",
        "plotzoom_to": "",
    },
    "Oudebeek-Beek7": {
        "reach_list_single": [42],
        "reach_list_group": [],
        "plotzoom_from": "",
        "plotzoom_to": "",
    },
    "Muenster": {
        "reach_list_single": [154],
        "reach_list_group": [772, 575, 149],
        "plotzoom_from": "2000-05-01",
        "plotzoom_to": "2000-06-01",
    },
    "Wetter_2": {
        "reach_list_single": [154],
        "reach_list_group": [131, 130, 437],
        "plotzoom_from": "1991-05-01",
        "plotzoom_to": "1991-05-10",
    },
    "Muschenheim": {
        "reach_list_single": [315],
        "reach_list_group": [151, 149, 735],
        "plotzoom_from": "",
        "plotzoom_to": "",
    },
    "Bruchenbruecken": {
        "reach_list_single": [660],
        "reach_list_group": [652, 653, 4443],
        "plotzoom_from": "1991-05-01",
        "plotzoom_to": "1991-06-01",
    },
    "Funne": {
        "reach_list_single": [12],
        "reach_list_group": [99, 97, 167],
        "plotzoom_from": "2010-05-01",
        "plotzoom_to": "2010-06-01",
    },
    "GKB": {
        "reach_list_single": [166],
        "reach_list_group": [12, 144, 165],
        "plotzoom_from": "2010-04-01",
        "plotzoom_to": "2010-07-01",
    },
}


EXPOSURE_MODEL_CONFIG = {
    "CascadeToxswa": {
        "pecsw_key": "CascadeToxswa/ConLiqWatTgtAvg",
        "pecsed_key": "CascadeToxswa/CntSedTgt1",
        "pecsw_scale": 1_000_000,
        "pecsed_scale": 1_000,
        "effect_prefix": "CascadeToxswa",
    },
    "StepsRiverNetwork": {
        "pecsw_key": "StepsRiverNetwork/PEC_SW",
        "pecsed_key": "StepsRiverNetwork/PEC_SED",
        "pecsw_scale": 1_000,
        "pecsed_scale": 1_000,
        "effect_prefix": "StepsRiverNetwork",
    },
}
VALID_EXPOSURE_MODELS = tuple(EXPOSURE_MODEL_CONFIG.keys())


def row_percentiles(row, percentiles):
    import numpy as np
    import pandas as pd

    return pd.Series(
        np.percentile(row.dropna(), [p * 100 for p in percentiles]),
        index=[f"Px{int(p * 100)}" for p in percentiles],
    )


def normalize_reach_id(value):
    if value is None:
        return None
    if isinstance(value, (bytes, bytearray)):
        text = value.decode("utf-8", errors="replace").strip()
    else:
        text = str(value).strip()
    if not text:
        return None
    if re.fullmatch(r"[-+]?\d+(?:\.0+)?", text):
        return str(int(float(text)))
    return text


def coerce_reach_ids(values):
    result = []
    seen = set()
    for raw in values or []:
        rid = normalize_reach_id(raw)
        if rid is None or rid in seen:
            continue
        seen.add(rid)
        result.append(rid)
    return result


def normalize_reach_ids(values):
    return [normalize_reach_id(v) for v in values]


def find_reach_shapefile(scenario_path: Path) -> Optional[Path]:
    preferred_names = ["Reachlist_shp.shp", "ReachList_shp.shp"]
    for name in preferred_names:
        shp = scenario_path / "geo" / name
        if shp.exists():
            return shp

    geo_dir = scenario_path / "geo"
    if geo_dir.exists():
        shp_files = sorted(geo_dir.glob("*.shp"))
        if shp_files:
            return shp_files[0]

    shp_files = sorted(scenario_path.glob("**/*.shp"))
    return shp_files[0] if shp_files else None


def select_reach_id_column(gdf, reach_ids_hint):
    non_geom = [c for c in gdf.columns if c != "geometry"]
    if not non_geom:
        return None

    exact_priority = ["key", "reach_id", "reachid", "segment_id", "reach", "name", "id"]
    lower_to_col = {c.lower(): c for c in non_geom}
    hints = set(coerce_reach_ids(reach_ids_hint or []))

    def _normalized_sample(col_name):
        vals = gdf[col_name].dropna().head(2000)
        if vals.empty:
            return []
        return [normalize_reach_id(v) for v in vals]

    if hints:
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
            unique_ratio = len(unique_vals) / max(len(norm_vals), 1)
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

    for low_name in exact_priority:
        col = lower_to_col.get(low_name)
        if col:
            return col

    return non_geom[0]


def read_source_scenario_name(scenario_path: Path) -> Optional[str]:
    readme = scenario_path / "readme.txt"
    if not readme.exists():
        return None
    try:
        for line in readme.read_text(encoding="utf-8", errors="replace").splitlines():
            stripped = line.strip()
            if stripped.lower().startswith("source scenario:"):
                after_colon = stripped.split(":", 1)[1].strip()
                folder = after_colon.rstrip("/").split("/")[-1]
                return folder
    except Exception:
        return None
    return None


def resolve_scenario_defaults(scenario_name: str, scenario_path: Path) -> dict:
    defaults = SCENARIO_DEFAULTS.get(scenario_name)
    if defaults:
        return defaults
    parent_folder = read_source_scenario_name(scenario_path)
    if not parent_folder:
        return {}
    lower_folder = parent_folder.lower()
    for key, entry in SCENARIO_DEFAULTS.items():
        if key.lower() == lower_folder:
            return entry
    for key, entry in SCENARIO_DEFAULTS.items():
        if key.lower() in lower_folder or lower_folder.startswith(key.lower()):
            return entry
    return {}


def parse_reach_id_csv(raw: str, default=None):
    values = [part.strip() for part in (raw or "").split(",") if part.strip()]
    if not values:
        return list(default or [])
    return [int(v) for v in values]


def extract_run_identifiers(mc_path: Path):
    parts = list(mc_path.parts)
    mc_run = mc_path.name
    exp_id = "Experiment"
    try:
        mcs_idx = parts.index("mcs")
        mc_run = parts[mcs_idx + 1]
        exp_id = parts[mcs_idx - 1]
    except (ValueError, IndexError):
        pass
    return exp_id, mc_run