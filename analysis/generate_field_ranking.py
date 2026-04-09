"""
generate_field_ranking.py
=========================
Generate a field-level spray-drift contribution ranking table from an
xAquaticRisk simulation run and its related scenario.

Usage
-----
    python generate_field_ranking.py \
        --run_path   <path/to/run_folder> \
        --scenario_path <path/to/scenario_folder> \
        [--output_table_path <path/to/output.csv>] \
        [--top_n 20]

The script writes a CSV conforming to the data contract at
docs/reference/field-drift-ranking-data-contract.md and prints the
top-N ranked arable fields to stdout.

Attribution method
------------------
SprayDrift/Exposure in the HDF5 store records daily deposition (g/ha)
to every spatial feature (arable fields + water-body polygons).  Only
water-body features (LULCTypeID 422) receive non-zero values; arable
fields are the *sources*, not the targets.

To attribute reach exposure back to source fields the script performs a
spatial proximity join:

1. Buffer each water-body polygon by 1 m (to capture shared edges).
2. Find all arable fields (LULCTypeID 222) whose geometry intersects
   the buffered water polygon.
3. Compute the shared-boundary length between each (field, reach) pair
   as a proportionality weight.
4. For each arable field: total_contribution =
       sum over adjacent reaches of
           (reach_total_exposure × shared_boundary_fraction_for_this_field)

A field that has never been applied (absent from PPM/AppliedFields) is
assigned total_contribution = 0 and flagged with applied=False.

Contribution unit: g/ha  (inherited from SprayDrift/Exposure.attrs['unit'])
"""

import argparse
import datetime
import os
import sys
import warnings

import geopandas as gpd
import h5py
import numpy as np
import pandas as pd
from shapely.ops import unary_union

warnings.filterwarnings("ignore", category=FutureWarning)


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
ARABLE_TYPE_ID = 222
WATER_TYPE_ID = 422
LULC_FEATURE_ID_COL = "ALVID"
LULC_TYPE_COL = "LULCTypeID"
LULC_REACH_KEY_COL = "key"
LULC_SUBDIR = "geo"
LULC_FILENAME = "LULC.shp"
PKG_INFO_FILENAME = "package.xinfo"
HDF5_GLOB = "mcs/*/store/arr.dat"
EXPOSURE_DATASET = "SprayDrift/Exposure"
FEATURE_IDS_DATASET = "LandscapeScenario/FeatureIds"
FEATURE_TYPE_IDS_DATASET = "LandscapeScenario/FeatureTypeIds"
PPM_APPLIED_FIELDS = "PPM/AppliedFields"
SIM_START_DATASET = "SprayDrift/SimulationStart"
SIM_END_DATASET = "SprayDrift/SimulationEnd"
DATASET_VERSION = "1.0.0"
SPATIAL_BUFFER_M = 1.0   # metres – captures shared edges without large overlap


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def find_hdf5(run_path: str) -> str:
    """Return path to the first arr.dat found under run_path/mcs/*/store/."""
    import glob
    pattern = os.path.join(run_path, HDF5_GLOB)
    matches = glob.glob(pattern)
    if not matches:
        raise FileNotFoundError(
            f"No HDF5 store (arr.dat) found under {run_path}/mcs/*/store/.\n"
            "Verify that the simulation completed and that run_path is correct."
        )
    return matches[0]


def scenario_id_from_path(scenario_path: str) -> str:
    return os.path.basename(os.path.normpath(scenario_path))


def run_id_from_path(run_path: str) -> str:
    return os.path.basename(os.path.normpath(run_path))


def ordinal_to_isodate(ordinal: int) -> datetime.date:
    return datetime.date.fromordinal(int(ordinal))


# ---------------------------------------------------------------------------
# Core generation
# ---------------------------------------------------------------------------

def generate(run_path: str,
             scenario_path: str,
             output_table_path: str | None = None,
             top_n: int = 20) -> pd.DataFrame:
    """
    Generate the field-ranking table.  Returns the full table (all arable
    fields with valid geometry and computed contribution).
    """
    run_path = os.path.normpath(run_path)
    scenario_path = os.path.normpath(scenario_path)

    # -----------------------------------------------------------------------
    # Step 1 – Validate paths
    # -----------------------------------------------------------------------
    for label, p in [("run_path", run_path), ("scenario_path", scenario_path)]:
        if not os.path.isdir(p):
            raise ValueError(
                f"status: invalid_input\n"
                f"failed_check: {label} not found\n"
                f"path: {p}\n"
                f"remediation: Provide a readable directory path."
            )

    lulc_path = os.path.join(scenario_path, LULC_SUBDIR, LULC_FILENAME)
    if not os.path.isfile(lulc_path):
        raise ValueError(
            f"status: invalid_input\n"
            f"failed_check: LULC shapefile missing\n"
            f"path: {lulc_path}\n"
            f"remediation: Ensure scenario/geo/LULC.shp exists."
        )

    hdf5_path = find_hdf5(run_path)

    if output_table_path is None:
        output_table_path = os.path.join(run_path, "field_ranking.csv")

    # -----------------------------------------------------------------------
    # Step 2 – Load LULC geodata
    # -----------------------------------------------------------------------
    print("Loading LULC shapefile …", flush=True)
    gdf = gpd.read_file(lulc_path)
    # Preserve the original row index – it is the column index in HDF5 Exposure
    gdf = gdf.reset_index(drop=True)
    gdf.index.name = "feature_col_idx"

    arable = gdf[gdf[LULC_TYPE_COL] == ARABLE_TYPE_ID].copy()
    water = gdf[gdf[LULC_TYPE_COL] == WATER_TYPE_ID].copy()

    print(f"  Arable fields  : {len(arable):,}")
    print(f"  Water features : {len(water):,}")

    if arable.empty:
        raise ValueError(
            "status: generation_failed\n"
            f"failed_step: LULC filter for LULCTypeID=={ARABLE_TYPE_ID}\n"
            "remediation: Verify that arable fields (type 222) exist in LULC.shp."
        )

    # -----------------------------------------------------------------------
    # Step 3 – Load HDF5 Exposure data
    # -----------------------------------------------------------------------
    print(f"Loading SprayDrift/Exposure from {hdf5_path} …", flush=True)
    with h5py.File(hdf5_path, "r") as f:
        exposure = f[EXPOSURE_DATASET][:]          # shape (days, n_features)
        unit = f[EXPOSURE_DATASET].attrs.get("unit", "g/ha")
        if isinstance(unit, bytes):
            unit = unit.decode()
        sim_start_raw = f[SIM_START_DATASET][()]
        sim_end_raw = f[SIM_END_DATASET][()]

        # Applied fields (PPM) – used to flag fields never applied
        ppm_applied = set(f[PPM_APPLIED_FIELDS][:].tolist())

    if isinstance(sim_start_raw, bytes):
        sim_start_raw = sim_start_raw.decode()
    if isinstance(sim_end_raw, bytes):
        sim_end_raw = sim_end_raw.decode()
    time_start = sim_start_raw
    time_end = sim_end_raw

    print(f"  Exposure shape : {exposure.shape}  (days × features)")
    print(f"  Simulation     : {time_start} → {time_end}")
    print(f"  Unit           : {unit}")

    # -----------------------------------------------------------------------
    # Step 4 – Total exposure per water feature column
    # -----------------------------------------------------------------------
    water_col_indices = water.index.values          # original row positions
    total_water_exposure = exposure[:, water_col_indices].sum(axis=0)  # (n_water,)
    water = water.copy()
    water["total_reach_exposure"] = total_water_exposure

    # Keep only reaches that actually received some exposure
    active_water = water[water["total_reach_exposure"] > 0].copy()
    print(f"  Reaches with non-zero exposure: {len(active_water):,} / {len(water):,}")

    if active_water.empty:
        raise ValueError(
            "status: generation_failed\n"
            "failed_step: No non-zero exposure values found in SprayDrift/Exposure for water features\n"
            "remediation: Verify that the simulation ran to completion and that "
            "PPM applications were configured."
        )

    # -----------------------------------------------------------------------
    # Step 5 – Spatial proximity join (arable ↔ water)
    # -----------------------------------------------------------------------
    print("Computing spatial adjacency (arable fields ↔ stream reaches) …", flush=True)
    # Build working GeoDataFrames with explicit, unambiguous column names
    arable_work = arable[["geometry", LULC_FEATURE_ID_COL]].copy()
    arable_work["arable_row"] = arable_work.index  # original LULC row position
    arable_work = arable_work.reset_index(drop=True)

    water_work = active_water[["geometry", LULC_REACH_KEY_COL, "total_reach_exposure"]].copy()
    water_work["water_row"] = water_work.index  # original LULC row position
    water_work_buffered = water_work.copy()
    water_work_buffered["geometry"] = water_work_buffered.geometry.buffer(SPATIAL_BUFFER_M)
    water_work_buffered = water_work_buffered.reset_index(drop=True)

    # Spatial join: find all arable fields that intersect buffered water polygons
    joined = gpd.sjoin(
        arable_work,
        water_work_buffered,
        how="inner",
        predicate="intersects",
    )
    # Resulting columns include: arable_row, ALVID, index_right, key, total_reach_exposure, water_row

    if joined.empty:
        raise ValueError(
            "status: generation_failed\n"
            "failed_step: Spatial adjacency join returned no matches\n"
            "remediation: Check that the LULC shapefile CRS matches the run data "
            f"and that water features exist within the landscape extent."
        )

    print(f"  (arable, reach) adjacency pairs: {len(joined):,}")

    # Rename to avoid any further ambiguity
    joined = joined.rename(columns={
        LULC_FEATURE_ID_COL: "alvid",
        LULC_REACH_KEY_COL: "reach_key",
    })

    # -----------------------------------------------------------------------
    # Step 6 – Compute shared boundary length as attribution weight
    # -----------------------------------------------------------------------
    print("Computing shared boundary lengths for proportional attribution …", flush=True)
    # Rebuild a lookup: water_row → original (un-buffered) geometry
    water_geom_map = dict(zip(water_work["water_row"], water_work["geometry"]))
    arable_geom_map = dict(zip(arable_work["arable_row"], arable_work["geometry"]))

    def shared_length(row):
        a_geom = arable_geom_map.get(row["arable_row"])
        w_geom = water_geom_map.get(row["water_row"])
        if a_geom is None or w_geom is None:
            return 0.0
        try:
            inter = a_geom.boundary.intersection(w_geom.buffer(SPATIAL_BUFFER_M))
            return inter.length if not inter.is_empty else 0.0
        except Exception:
            return 0.0

    joined = joined.copy()  # avoid SettingWithCopyWarning
    joined["shared_length"] = joined.apply(shared_length, axis=1)

    # Compute per-reach total shared length (sum over all adjacent arable fields)
    reach_shared_total = joined.groupby("water_row")["shared_length"].sum().rename("reach_total_shared_length")
    joined = joined.join(reach_shared_total, on="water_row")

    # Weight = field fraction of total shared boundary with this reach
    # Fall back to equal split when all intersection lengths are zero
    reach_pair_counts = joined.groupby("water_row")["arable_row"].transform("count")
    joined["weight"] = np.where(
        joined["reach_total_shared_length"] > 0,
        joined["shared_length"] / joined["reach_total_shared_length"].clip(lower=1e-12),
        1.0 / reach_pair_counts,
    )

    # Field contribution from this reach = total_reach_exposure × weight
    joined["field_reach_contribution"] = joined["total_reach_exposure"] * joined["weight"]

    # -----------------------------------------------------------------------
    # Step 7 – Aggregate per arable field
    # -----------------------------------------------------------------------
    field_contrib = (
        joined.groupby("arable_row")["field_reach_contribution"]
        .sum()
        .reset_index()
        .rename(columns={"field_reach_contribution": "total_contribution"})
    )

    # Join back ALVID via arable_row  (arable_work still uses the original column name)
    alvid_map = (
        arable_work[["arable_row", LULC_FEATURE_ID_COL]]
        .rename(columns={LULC_FEATURE_ID_COL: "field_id"})
        .drop_duplicates()
    )
    field_contrib = field_contrib.merge(alvid_map, on="arable_row", how="left")

    # -----------------------------------------------------------------------
    # Step 8 – Add all required columns
    # -----------------------------------------------------------------------
    generated_at = datetime.datetime.utcnow().strftime("%Y-%m-%dT%H:%M:%SZ")
    scen_id = scenario_id_from_path(scenario_path)
    run_id = run_id_from_path(run_path)

    field_contrib["field_id"] = field_contrib["field_id"].astype(str)
    field_contrib["scenario_id"] = scen_id
    field_contrib["run_id"] = run_id
    field_contrib["time_start"] = time_start
    field_contrib["time_end"] = time_end
    field_contrib["contribution_unit"] = unit
    field_contrib["dataset_version"] = DATASET_VERSION
    field_contrib["generated_at"] = generated_at
    field_contrib["source_run_path"] = run_path
    field_contrib["source_scenario_path"] = scenario_path
    field_contrib["field_type"] = "arable"
    field_contrib["applied"] = field_contrib["field_id"].astype(int).isin(ppm_applied)

    # Reorder to match data contract column order
    output_cols = [
        "field_id", "scenario_id", "run_id",
        "time_start", "time_end",
        "total_contribution", "contribution_unit",
        "dataset_version", "generated_at",
        "source_run_path", "source_scenario_path",
        "field_type", "applied",
    ]
    field_contrib = field_contrib[output_cols].copy()
    field_contrib.sort_values(
        ["total_contribution", "field_id"],
        ascending=[False, True],
        inplace=True
    )
    field_contrib.reset_index(drop=True, inplace=True)

    # -----------------------------------------------------------------------
    # Step 9 – Write output
    # -----------------------------------------------------------------------
    os.makedirs(os.path.dirname(os.path.abspath(output_table_path)), exist_ok=True)
    field_contrib.to_csv(output_table_path, index=False)
    print(f"\nRanking table written → {output_table_path}")
    print(f"  Rows: {len(field_contrib):,}  |  Applied fields with contribution > 0: "
          f"{(field_contrib['total_contribution'] > 0).sum():,}")

    return field_contrib


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--run_path", required=True, help="Path to the simulation run folder.")
    parser.add_argument("--scenario_path", required=True, help="Path to the scenario folder.")
    parser.add_argument("--output_table_path", default=None,
                        help="Destination CSV (default: <run_path>/field_ranking.csv).")
    parser.add_argument("--top_n", type=int, default=20,
                        help="Number of top fields to display (default 20).")
    args = parser.parse_args()

    try:
        table = generate(
            run_path=args.run_path,
            scenario_path=args.scenario_path,
            output_table_path=args.output_table_path,
            top_n=args.top_n,
        )
    except (ValueError, FileNotFoundError) as exc:
        print(str(exc), file=sys.stderr)
        sys.exit(1)

    top = table.head(args.top_n)
    print(f"\n=== Top {args.top_n} Arable Fields by Spray-Drift Contribution ===")
    print(top[["field_id", "total_contribution", "contribution_unit", "applied"]].to_string(index=False))


if __name__ == "__main__":
    main()
