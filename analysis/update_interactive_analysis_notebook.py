#!/usr/bin/env python
"""Regenerate the standalone interactive analysis notebook."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from textwrap import dedent
import sys

SCRIPT_DIR = Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIR))

from basic_analysis_common import VALID_EXPOSURE_MODELS


NOTEBOOK_FILENAME = "PEC_GUTS_InteractiveAnalysis.ipynb"


def markdown_cell(cell_id: str, source: str):
    return {
        "cell_type": "markdown",
        "id": cell_id,
        "metadata": {},
        "source": dedent(source).strip("\n").splitlines(True),
    }


def code_cell(cell_id: str, source: str):
    return {
        "cell_type": "code",
        "execution_count": None,
        "id": cell_id,
        "metadata": {},
        "outputs": [],
        "source": dedent(source).strip("\n").splitlines(True),
    }


def build_notebook():
    notebook = {
        "cells": [],
        "metadata": {
        "kernelspec": {
            "display_name": "Python 3",
            "language": "python",
            "name": "python3",
        },
        "language_info": {
            "name": "python",
            "version": "3.9",
        },
        },
        "nbformat": 4,
        "nbformat_minor": 5,
    }
    notebook["cells"] = [
        markdown_cell(
            "interactive-analysis-intro",
            """
            # xAquaticRisk Interactive Exposure and Effect Analysis

            This notebook is a generated interactive companion to `analysis/run_basic_analysis.py`.
            Regenerate it with `update_interactive_analysis_notebook.bat` whenever the shared analysis logic changes.

            The notebook is intended for exploratory work. It keeps the PEC and GUTS analysis flow notebook-friendly while reusing the same shared defaults and reach-ID handling as the controlpanel backend.
            """
        ),
        markdown_cell(
            "interactive-analysis-workflow",
            """
            ## Workflow

            1. Edit the input cell below.
            2. Run the setup cell to resolve scenario defaults and prepare the output folder.
            3. Run the PEC cells and then the GUTS cells.
            4. Inspect tables directly in the notebook and find exported Excel and PNG outputs in the generated analysis folder.
            """
        ),
        code_cell(
            "interactive-analysis-imports",
            """
            %matplotlib inline

            from datetime import datetime
            from pathlib import Path

            import h5py
            import matplotlib.pyplot as plt
            import matplotlib.ticker as ticker
            import numpy as np
            import pandas as pd
            import seaborn as sns

            try:
                import geopandas as gpd
            except ImportError:
                gpd = None

            from basic_analysis_common import (
                DEFAULT_BOXPLOT_TEMPORAL_PERCENTILES,
                DEFAULT_GUTS_SPATIAL_PERCENTILES,
                DEFAULT_GUTS_TEMPORAL_PERCENTILES,
                DEFAULT_LP50_THRESHOLD_RANGE,
                DEFAULT_PEC_SPATIAL_PERCENTILES,
                DEFAULT_PEC_TEMPORAL_PERCENTILES,
                EXPOSURE_MODEL_CONFIG,
                SCENARIO_DEFAULTS,
                VALID_EXPOSURE_MODELS,
                coerce_reach_ids,
                extract_run_identifiers,
                find_reach_shapefile,
                normalize_reach_id,
                normalize_reach_ids,
                parse_reach_id_csv,
                read_source_scenario_name,
                resolve_scenario_defaults,
                row_percentiles,
                select_reach_id_column,
            )

            sns.set_theme(style="whitegrid")
            pd.set_option("display.max_columns", 20)
            pd.set_option("display.width", 140)
            """
        ),
        markdown_cell(
            "interactive-analysis-user-input-title",
            """
            ## User Input

            Set the MC run, scenario and optional overrides here. `reach_ids_*_override` accept comma-separated integer IDs.
            """
        ),
        code_cell(
            "interactive-analysis-user-input",
            f"""
            workspace_root = Path.cwd().resolve().parent if Path.cwd().name.lower() == "analysis" else Path.cwd().resolve()

            mc_path = workspace_root / "run" / "Test_Run_aqRisk_14042026-084216" / "mcs" / "X3TESTPLACEHOLDER"
            scenario_path = workspace_root / "scenario" / "muenster-T-Di-02.5-20220429-postproceccing-toxwa"
            scenario_name = "Muenster"
            exposure_model = "{VALID_EXPOSURE_MODELS[0]}"

            base_processing_folder = workspace_root / "analysis_output"
            run_pec_analysis = True
            run_guts_analysis = True
            exposed_streams_only_analysis = False

            reach_ids_single_override = ""
            reach_ids_group_override = ""
            plotzoom_from_override = ""
            plotzoom_to_override = ""
            """
        ),
        code_cell(
            "interactive-analysis-setup",
            """
            mc_path = Path(mc_path)
            scenario_path = Path(scenario_path)
            base_processing_folder = Path(base_processing_folder)

            if exposure_model not in VALID_EXPOSURE_MODELS:
                raise ValueError(f"Unsupported exposure model: {exposure_model}. Expected one of: {VALID_EXPOSURE_MODELS}")

            defaults = resolve_scenario_defaults(scenario_name, scenario_path)
            inherited_from_parent = bool(defaults and not SCENARIO_DEFAULTS.get(scenario_name))

            reach_list_single = coerce_reach_ids(
                parse_reach_id_csv(reach_ids_single_override, defaults.get("reach_list_single", []))
            )
            reach_list_group = coerce_reach_ids(
                parse_reach_id_csv(reach_ids_group_override, defaults.get("reach_list_group", []))
            )
            plotzoom_from = plotzoom_from_override or defaults.get("plotzoom_from", "")
            plotzoom_to = plotzoom_to_override or defaults.get("plotzoom_to", "")

            pec_temporal_percentiles = DEFAULT_PEC_TEMPORAL_PERCENTILES
            pec_spatial_percentiles = DEFAULT_PEC_SPATIAL_PERCENTILES
            guts_temporal_percentiles = DEFAULT_GUTS_TEMPORAL_PERCENTILES
            guts_spatial_percentiles = DEFAULT_GUTS_SPATIAL_PERCENTILES
            boxplot_temporal_percentile = DEFAULT_BOXPLOT_TEMPORAL_PERCENTILES
            lower_LP50_threshold, upper_LP50_threshold = DEFAULT_LP50_THRESHOLD_RANGE

            exp_id, mc_run = extract_run_identifiers(mc_path)
            h5_data = mc_path / "store" / "arr.dat"
            if not h5_data.exists():
                raise FileNotFoundError(f"HDF5 store not found: {h5_data}")

            output_dir = base_processing_folder / f"{exp_id}_{mc_run}__interactive"
            output_dir.mkdir(parents=True, exist_ok=True)
            excel_filename = output_dir / f"{exp_id}_{mc_run}_interactive.xlsx"
            if not excel_filename.exists():
                pd.DataFrame().to_excel(excel_filename, index=False)

            pd.Series(
                {
                    "workspace_root": str(workspace_root),
                    "mc_path": str(mc_path),
                    "scenario_path": str(scenario_path),
                    "scenario_name": scenario_name,
                    "scenario_parent": read_source_scenario_name(scenario_path),
                    "defaults_inherited": inherited_from_parent,
                    "exposure_model": exposure_model,
                    "reach_list_single": ", ".join(reach_list_single) or "<none>",
                    "reach_list_group": ", ".join(reach_list_group) or "<none>",
                    "output_dir": str(output_dir),
                    "excel_filename": str(excel_filename),
                }
            )
            """
        ),
        markdown_cell(
            "interactive-analysis-geometry-title",
            """
            ## Optional Scenario Geometry

            Geo plots require `geopandas` and a reach shapefile in the scenario folder. The notebook falls back cleanly when those are unavailable.
            """
        ),
        code_cell(
            "interactive-analysis-geometry",
            """
            df_geo_reaches = None
            df_scenario_geo_attributes = None
            reach_id_col = None
            shp_path = find_reach_shapefile(scenario_path)

            if gpd is None:
                print("geopandas is not available in the current notebook runtime. Geo plots will be skipped.")
            elif shp_path and shp_path.exists():
                df_geo_reaches = gpd.read_file(str(shp_path))
                if df_geo_reaches.empty:
                    print(f"Shapefile is empty: {shp_path}")
                    df_geo_reaches = None
                else:
                    reach_id_col = select_reach_id_column(df_geo_reaches, reach_list_single + reach_list_group)
                    if reach_id_col is None:
                        print("No usable reach-ID column found in the shapefile.")
                        df_geo_reaches = None
                    else:
                        df_geo_reaches["__reach_id__"] = df_geo_reaches[reach_id_col].map(normalize_reach_id)
                        df_geo_reaches = df_geo_reaches[df_geo_reaches["__reach_id__"].notna()].copy()
                        if df_geo_reaches.empty:
                            print("No mappable reach IDs found in the shapefile.")
                            df_geo_reaches = None
                        else:
                            df_scenario_geo_attributes = (
                                df_geo_reaches.drop(columns="geometry").set_index("__reach_id__")
                            )
                            print(f"Loaded {len(df_geo_reaches)} reach features from {shp_path.name} using '{reach_id_col}'.")
            else:
                print(f"Reach shapefile not found for scenario: {scenario_path}")
            """
        ),
        markdown_cell("interactive-analysis-pec-title", "## Exposure Analysis"),
        code_cell(
            "interactive-analysis-pec-load",
            """
            cfg = EXPOSURE_MODEL_CONFIG[exposure_model]

            df_pecsw = None
            df_pecsed = None
            pecsw_perc_by_reach = None
            pecsed_perc_by_reach = None

            if run_pec_analysis:
                with h5py.File(h5_data, "r") as f:
                    if cfg["pecsw_key"] not in f:
                        raise KeyError(f"{cfg['pecsw_key']} not found in HDF5 store")
                    reach_ids_pecsw = f[f[cfg["pecsw_key"]].attrs["dim1_element_names"]][:]
                    starttime_pecsw = f[cfg["pecsw_key"]].attrs["dim0_offset"]
                    df_pecsw = pd.DataFrame(f[cfg["pecsw_key"]][:]) * cfg["pecsw_scale"]
                    df_pecsed = pd.DataFrame(f[cfg["pecsed_key"]][:]) * cfg["pecsed_scale"]

                time_index = pd.date_range(starttime_pecsw, periods=len(df_pecsw), freq="h")
                for frame in (df_pecsw, df_pecsed):
                    frame.insert(0, "time", time_index)
                    frame.set_index("time", inplace=True)

                normalized_reach_cols = normalize_reach_ids(reach_ids_pecsw)
                df_pecsw.columns = normalized_reach_cols
                df_pecsed.columns = normalized_reach_cols

                if exposed_streams_only_analysis:
                    valid_pecsw_reaches = df_pecsw.gt(0).any(axis=0)
                    valid_pecsw_times = df_pecsw.gt(0).any(axis=1)
                    df_pecsw = df_pecsw.loc[valid_pecsw_times, valid_pecsw_reaches]

                    valid_pecsed_reaches = df_pecsed.gt(0).any(axis=0)
                    valid_pecsed_times = df_pecsed.gt(0).any(axis=1)
                    df_pecsed = df_pecsed.loc[valid_pecsed_times, valid_pecsed_reaches]

                print(f"PECsw shape: {df_pecsw.shape}; PECsed shape: {df_pecsed.shape}")
                display(df_pecsw.head())
            else:
                print("PEC analysis is disabled in the input cell.")
            """
        ),
        code_cell(
            "interactive-analysis-pec-tables",
            """
            if run_pec_analysis and df_pecsw is not None:
                pecsw_perc_by_reach = df_pecsw.quantile(pec_temporal_percentiles)
                pecsed_perc_by_reach = df_pecsed.quantile(pec_temporal_percentiles)

                PECsw_x_t = pecsw_perc_by_reach.apply(
                    lambda row: row_percentiles(row, pec_spatial_percentiles), axis=1
                )
                PECsed_x_t = pecsed_perc_by_reach.apply(
                    lambda row: row_percentiles(row, pec_spatial_percentiles), axis=1
                )

                with pd.ExcelWriter(excel_filename, engine="openpyxl", mode="a", if_sheet_exists="replace") as writer:
                    PECsw_x_t.to_excel(writer, sheet_name="PECsw_percentiles_x_t")
                    PECsed_x_t.to_excel(writer, sheet_name="PECsed_percentiles_x_t")

                display(PECsw_x_t)
                display(PECsed_x_t)
                print(f"PEC tables exported to {excel_filename}")
            """
        ),
        code_cell(
            "interactive-analysis-pec-plots",
            """
            if run_pec_analysis and df_pecsw is not None:
                if reach_list_single:
                    rid = reach_list_single[0]
                    if rid in df_pecsw.columns:
                        fig, axes = plt.subplots(1, 2, figsize=(14, 4))
                        axes[0].plot(df_pecsw.index, df_pecsw[rid], linewidth=0.8)
                        axes[0].set_title(f"PECsw Time Series - Reach {rid}")
                        axes[0].set_ylabel("PECsw [ng/L]")
                        axes[0].grid(True)

                        if rid in df_pecsed.columns:
                            axes[1].plot(df_pecsed.index, df_pecsed[rid], linewidth=0.8, color="sienna")
                            axes[1].set_title(f"PECsed Time Series - Reach {rid}")
                            axes[1].set_ylabel("PECsed [µg/kg]")
                            axes[1].grid(True)
                        plt.tight_layout()
                        plt.show()

                        if plotzoom_from and plotzoom_to:
                            subset = df_pecsw[plotzoom_from:plotzoom_to]
                            if len(subset):
                                plt.figure(figsize=(12, 4))
                                plt.plot(subset.index, subset[rid], linewidth=1.0)
                                plt.title(f"PECsw Time Series (zoomed) - Reach {rid}")
                                plt.ylabel("PECsw [ng/L]")
                                plt.grid(True)
                                plt.tight_layout()
                                plt.show()

                valid_group = [rid for rid in reach_list_group if rid in df_pecsw.columns]
                if valid_group:
                    plt.figure(figsize=(12, 4))
                    for rid in valid_group:
                        plt.plot(df_pecsw.index, df_pecsw[rid], linewidth=0.8, label=str(rid))
                    plt.legend(title="Reach-ID")
                    plt.title("PECsw Time Series - Multiple Reaches")
                    plt.ylabel("PECsw [ng/L]")
                    plt.grid(True)
                    plt.tight_layout()
                    plt.show()

                if df_scenario_geo_attributes is not None and "strahler" in df_scenario_geo_attributes.columns:
                    pecsw_T = pecsw_perc_by_reach.transpose()
                    pecsw_T.index.name = "ReachID"
                    pecsw_T = pecsw_T.join(df_scenario_geo_attributes["strahler"])
                    for bp in boxplot_temporal_percentile:
                        if bp in pecsw_T.columns:
                            plt.figure(figsize=(10, 4))
                            sns.boxplot(x="strahler", y=bp, hue="strahler", data=pecsw_T, palette="viridis", legend=False)
                            plt.title(f"PECsw {int(bp * 100)}th Percentile over Time by Strahler Order")
                            plt.ylabel("PECsw [ng/L]")
                            plt.tight_layout()
                            plt.show()

                if df_geo_reaches is not None and pecsw_perc_by_reach is not None:
                    pecsw_T = pecsw_perc_by_reach.transpose()
                    pecsw_T.index.name = "ReachID"
                    col_99 = 0.99 if 0.99 in pecsw_T.columns else pecsw_T.columns[-1]
                    gdf = df_geo_reaches.merge(pecsw_T[[col_99]], left_on="__reach_id__", right_on="ReachID", how="left")
                    ax = gdf.plot(column=col_99, cmap="viridis", legend=True, figsize=(10, 8), missing_kwds={"color": "lightgrey"})
                    ax.set_title(f"PECsw - {int(col_99 * 100)}th Percentile over Time")
                    ax.axis("off")
            """
        ),
        markdown_cell("interactive-analysis-guts-title", "## Effect Analysis"),
        code_cell(
            "interactive-analysis-guts-load",
            """
            dfs = {}

            if run_guts_analysis:
                effect_prefix = cfg["effect_prefix"]
                guts_keys = [
                    ("surv_sd_sp1", f"IndEffect_{effect_prefix}_SD_Species1/GutsSurvivalReaches", True),
                    ("surv_sd_sp2", f"IndEffect_{effect_prefix}_SD_Species2/GutsSurvivalReaches", True),
                    ("surv_sd_sp3", f"IndEffect_{effect_prefix}_SD_Species3/GutsSurvivalReaches", True),
                    ("surv_it_sp1", f"IndEffect_{effect_prefix}_IT_Species1/GutsSurvivalReaches", True),
                    ("surv_it_sp2", f"IndEffect_{effect_prefix}_IT_Species2/GutsSurvivalReaches", True),
                    ("surv_it_sp3", f"IndEffect_{effect_prefix}_IT_Species3/GutsSurvivalReaches", True),
                    ("lp50_sd_sp1", f"IndEffect_LP50_{effect_prefix}_SD_Species1/LP50", False),
                    ("lp50_sd_sp2", f"IndEffect_LP50_{effect_prefix}_SD_Species2/LP50", False),
                    ("lp50_sd_sp3", f"IndEffect_LP50_{effect_prefix}_SD_Species3/LP50", False),
                    ("lp50_it_sp1", f"IndEffect_LP50_{effect_prefix}_IT_Species1/LP50", False),
                    ("lp50_it_sp2", f"IndEffect_LP50_{effect_prefix}_IT_Species2/LP50", False),
                    ("lp50_it_sp3", f"IndEffect_LP50_{effect_prefix}_IT_Species3/LP50", False),
                ]

                with h5py.File(h5_data, "r") as f:
                    ref_surv_key = f"IndEffect_{effect_prefix}_SD_Species1/GutsSurvivalReaches"
                    ref_lp50_key = f"IndEffect_LP50_{effect_prefix}_SD_Species1/LP50"
                    if ref_surv_key not in f:
                        raise KeyError(f"{ref_surv_key} not found in HDF5 store")

                    starttime_guts = f[ref_surv_key].attrs["dim0_offset"]
                    reach_ids_guts = f[f[ref_surv_key].attrs["dim1_element_names"]][:]
                    reach_ids_lp50 = f[f[ref_lp50_key].attrs["dim1_element_names"]][:] if ref_lp50_key in f else reach_ids_guts

                    for key, hdf_path, is_survival_cube in guts_keys:
                        if hdf_path not in f:
                            continue
                        if is_survival_cube:
                            dfs[key] = pd.DataFrame(f[hdf_path][:, :, 10])
                        else:
                            dfs[key] = pd.DataFrame(f[hdf_path][:])

                n_years_survival = len(dfs.get("surv_sd_sp1", pd.DataFrame()))
                n_years_lp50 = len(dfs.get("lp50_sd_sp1", pd.DataFrame()))
                time_index_survival = pd.date_range(datetime(starttime_guts, 1, 1), periods=n_years_survival, freq="YE")
                time_index_lp50 = pd.date_range(datetime(starttime_guts, 1, 1), periods=n_years_lp50, freq="YE")

                for key, _, is_survival_cube in guts_keys:
                    if key not in dfs:
                        continue
                    time_index = time_index_survival if is_survival_cube else time_index_lp50
                    reach_ids = reach_ids_guts if is_survival_cube else reach_ids_lp50
                    dfs[key].insert(0, "time", time_index[: len(dfs[key])])
                    dfs[key].set_index("time", inplace=True)
                    dfs[key].columns = normalize_reach_ids(reach_ids)

                for key in [name for name in dfs if name.startswith("lp50")]:
                    dfs[key][dfs[key] < 0] = np.nan
                    dfs[key][dfs[key] < lower_LP50_threshold] = lower_LP50_threshold
                    dfs[key][dfs[key] > upper_LP50_threshold] = upper_LP50_threshold

                if exposed_streams_only_analysis and df_pecsw is not None:
                    exposed_reaches = df_pecsw.columns[(df_pecsw > 0).any(axis=0)]
                    for key in dfs:
                        common = [rid for rid in exposed_reaches if rid in dfs[key].columns]
                        dfs[key] = dfs[key][common]

                print(f"Loaded {len(dfs)} GUTS datasets")
                display(dfs.get("surv_sd_sp1", pd.DataFrame()).head())
            else:
                print("GUTS analysis is disabled in the input cell.")
            """
        ),
        code_cell(
            "interactive-analysis-guts-tables",
            """
            if run_guts_analysis and dfs:
                guts_sheet_map = {
                    "surv_sd_sp1": "GUTSperc_SurvSdSp1",
                    "surv_sd_sp2": "GUTSperc_SurvSdSp2",
                    "surv_sd_sp3": "GUTSperc_SurvSdSp3",
                    "surv_it_sp1": "GUTSperc_SurvItSp1",
                    "surv_it_sp2": "GUTSperc_SurvItSp2",
                    "surv_it_sp3": "GUTSperc_SurvItSp3",
                    "lp50_sd_sp1": "GUTSperc_LP50SdSp1",
                    "lp50_sd_sp2": "GUTSperc_LP50SdSp2",
                    "lp50_sd_sp3": "GUTSperc_LP50SdSp3",
                    "lp50_it_sp1": "GUTSperc_LP50ItSp1",
                    "lp50_it_sp2": "GUTSperc_LP50ItSp2",
                    "lp50_it_sp3": "GUTSperc_LP50ItSp3",
                }

                percentile_tables = {}
                with pd.ExcelWriter(excel_filename, engine="openpyxl", mode="a", if_sheet_exists="replace") as writer:
                    for key, sheet_name in guts_sheet_map.items():
                        if key not in dfs:
                            continue
                        perc_by_reach = dfs[key].quantile(guts_temporal_percentiles)
                        perc_x_t = perc_by_reach.apply(
                            lambda row: row_percentiles(row, guts_spatial_percentiles), axis=1
                        )
                        percentile_tables[key] = perc_x_t
                        perc_x_t.to_excel(writer, sheet_name=sheet_name)

                display(percentile_tables.get("surv_sd_sp1"))
                display(percentile_tables.get("lp50_sd_sp1"))
                print(f"GUTS tables exported to {excel_filename}")
            """
        ),
        code_cell(
            "interactive-analysis-guts-plots",
            """
            if run_guts_analysis and dfs:
                if "lp50_sd_sp1" in dfs:
                    lp50_T = dfs["lp50_sd_sp1"].transpose()
                    lp50_T.index.name = "ReachID"
                    lp50_T.columns = [str(col.year) if isinstance(col, pd.Timestamp) else str(col) for col in lp50_T.columns]

                    if df_scenario_geo_attributes is not None and "strahler" in df_scenario_geo_attributes.columns:
                        lp50_T = lp50_T.join(df_scenario_geo_attributes["strahler"], how="left")

                    year_cols = [col for col in lp50_T.columns if col != "strahler"]
                    if year_cols:
                        first_year_col = year_cols[0]
                        plt.figure(figsize=(12, 4))
                        if "strahler" in lp50_T.columns:
                            sns.histplot(data=lp50_T, x=first_year_col, hue="strahler", multiple="stack", bins=50)
                        else:
                            sns.histplot(data=lp50_T, x=first_year_col, bins=50)
                        plt.title(f"LP50 Distribution - SD Species 1 ({first_year_col})")
                        plt.gca().xaxis.set_major_locator(ticker.MultipleLocator(100))
                        plt.tight_layout()
                        plt.show()

                        if "strahler" in lp50_T.columns:
                            bins = [-float("inf"), 0.1, 1, 10, 100, 200, 500, 1000]
                            bin_labels = ["<0.1", "[0.1,1)", "[1,10)", "[10,100)", "[100,200)", "[200,500)", "[500,1000]"]
                            grouped = {}
                            for col in year_cols:
                                grouped[col] = lp50_T.groupby("strahler")[col].apply(
                                    lambda x: pd.cut(x, bins=bins, labels=bin_labels, include_lowest=True).value_counts()
                                ).unstack().fillna(0)
                            combined = pd.concat(grouped, axis=1)
                            features = list(combined.columns.get_level_values(0).unique())
                            cols = min(3, len(features))
                            rows = int(np.ceil(len(features) / cols))
                            fig, axes = plt.subplots(rows, cols, figsize=(cols * 5, rows * 4), squeeze=False)
                            axes_flat = axes.flatten()
                            for index, feature in enumerate(features):
                                sns.heatmap(combined[feature], annot=True, fmt="d", cmap="YlGnBu", ax=axes_flat[index], cbar=False)
                                axes_flat[index].set_title(f"Year {feature}")
                                axes_flat[index].set_xlabel("LP50 Bin")
                                axes_flat[index].set_ylabel("Strahler Order")
                            for index in range(len(features), len(axes_flat)):
                                fig.delaxes(axes_flat[index])
                            fig.suptitle(f"LP50 by Strahler - SD Species 1 - {exp_id}")
                            fig.tight_layout()

                if df_geo_reaches is not None and "surv_sd_sp1" in dfs:
                    surv_T = dfs["surv_sd_sp1"].transpose()
                    surv_T.index.name = "ReachID"
                    surv_T.columns = [str(col.year) if isinstance(col, pd.Timestamp) else str(col) for col in surv_T.columns]
                    last_year = surv_T.columns[-1]
                    gdf = df_geo_reaches.merge(surv_T[[last_year]], left_on="__reach_id__", right_on="ReachID", how="left")
                    ax = gdf.plot(column=last_year, cmap="RdYlGn", legend=True, figsize=(10, 8), missing_kwds={"color": "lightgrey"}, vmin=0, vmax=1)
                    ax.set_title(f"GUTS Survival (SD Species 1) - Year {last_year}")
                    ax.axis("off")
            """
        ),
        markdown_cell(
            "interactive-analysis-output-folder",
            """
            ## Output Folder

            The notebook writes Excel and image outputs to the generated `analysis_output/...__interactive` folder for the selected run. Re-running the setup cell with the same inputs reuses that folder.
            """
        ),
    ]
    return notebook


def main():
    parser = argparse.ArgumentParser(description="Regenerate the interactive analysis notebook")
    parser.add_argument(
        "--output",
        default=str(Path(__file__).with_name(NOTEBOOK_FILENAME)),
        help="Path to the generated .ipynb file",
    )
    args = parser.parse_args()

    output_path = Path(args.output).resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    notebook = build_notebook()
    output_path.write_text(json.dumps(notebook, indent=1) + "\n", encoding="utf-8")
    print(f"Notebook written to {output_path}")


if __name__ == "__main__":
    main()