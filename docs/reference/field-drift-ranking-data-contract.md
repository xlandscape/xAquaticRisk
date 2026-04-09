# Field Spray-Drift Ranking Data Contract

This contract defines the generated ranking table produced by the Copilot agent from a simulation run path and a related scenario path.

## Purpose

The table provides field-level metrics keyed by stable LULC field IDs. The agent generates this table first, then uses it to answer ranking queries.

## Required Generation Inputs

1. `run_path`
2. `scenario_path`

Optional:

1. `output_table_path` (defaults to `<run_path>/field_ranking.csv`)
2. `time_window`

## Required Schema

| Column | Type | Description |
| --- | --- | --- |
| `field_id` | string | Stable identifier from scenario LULC geodata (for example ALVID or mapped equivalent). |
| `scenario_id` | string | Scenario identifier matching scenario folder naming. |
| `run_id` | string | Run identifier matching the simulation run naming. |
| `time_start` | datetime | Inclusive start of period used to compute metrics. |
| `time_end` | datetime | Inclusive end of period used to compute metrics. |
| `total_contribution` | number | Total spray-drift contribution for the field in the selected scope. |
| `contribution_unit` | string | Unit for contribution metric, for example `g/ha`. |
| `dataset_version` | string | Version of the extracted dataset (for example `1.0.0`). |
| `generated_at` | datetime | Timestamp when the table was produced. |
| `source_run_path` | string | Absolute or workspace path used for generation. |
| `source_scenario_path` | string | Absolute or workspace path used for generation. |

## Optional Columns

| Column | Type | Description |
| --- | --- | --- |
| `peak_daily_contribution` | number | Highest daily field contribution in scope. |
| `peak_reach_contribution` | number | Highest contribution to any single reach in scope. |
| `reach_count_affected` | integer | Number of reaches receiving contribution from this field. |
| `geometry_ref` | string | Optional pointer to geometry source (for example shapefile row reference). |
| `field_type` | string | Optional class, expected values include `arable`. |

## Ranking Rules

1. Default ranking metric is `total_contribution`.
2. Sort descending by metric.
3. Resolve ties by ascending `field_id`.
4. Default query returns top 20 rows unless a different `top_n` is requested.

## Arable Field Scope

1. `field_id` represents the unique LULC feature identity.
2. If `field_type` exists, default filtering uses `field_type == arable`.
3. If `field_type` does not exist, producer-side table generation must already be scoped to arable fields.

## Quality and Freshness Checks

1. Mandatory columns must exist and contain non-null values for key fields.
2. Numeric metrics must be parseable and finite.
3. `generated_at` must be valid datetime.
4. Results should be treated as stale when producer policy says so.

## Minimal CSV Example

```csv
field_id,scenario_id,run_id,time_start,time_end,total_contribution,contribution_unit,dataset_version,generated_at,field_type
ALV-10021,oudebeek-beek7-tdi,bruchenbruecken_toxswa_10yrs_noMitig,2010-01-01,2019-12-31,1.842,g/ha,1.0.0,2026-04-01T09:15:00Z,arable
ALV-09311,oudebeek-beek7-tdi,bruchenbruecken_toxswa_10yrs_noMitig,2010-01-01,2019-12-31,1.553,g/ha,1.0.0,2026-04-01T09:15:00Z,arable
```

## Example Query Mapping

Question: "Top 10 arable fields for scenario oudebeek-beek7-tdi and run bruchenbruecken_toxswa_10yrs_noMitig"

1. Filter rows where `scenario_id` and `run_id` match.
2. Keep arable rows if `field_type` exists.
3. Rank by `total_contribution` descending.
4. Return first 10 rows with provenance metadata.

## Ownership Boundary

1. Copilot agent generates and writes the table from `run_path` and `scenario_path`.
2. Copilot agent validates and queries the generated table.
3. If generation cannot be completed due to missing artifacts, the agent returns `generation_failed` with remediation steps.
