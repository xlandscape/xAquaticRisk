---
description: "Use when: working on spray-drift field ranking, arable field identification via LULC IDs, scenario and run filtering, generating field-level contribution tables from run/scenario paths, or validating generated field-level tables."
---

# Spray Drift Field Ranking Instructions

These instructions define deterministic behavior for field-ranking questions in xAquaticRisk.

## Required User Inputs

Treat these as required unless already provided in context:

1. `run_path`
2. `scenario_path`

Optional inputs:

1. `top_n` (default 20)
2. `time_window`
3. `output_table_path` (default `<run_path>/field_ranking.csv`)

## Scope

Apply this guidance when user intent includes any of:

1. Ranking arable fields by spray-drift contribution.
2. Identifying high-priority fields for local mitigation near streams.
3. Comparing field contributions across scenarios or runs.
4. Generating a field-ranking table from a run and scenario path.

## Generated Table Columns

After generation, treat these columns as mandatory in the ranking table:

1. `field_id` (stable field identifier from LULC geodata)
2. `scenario_id`
3. `run_id`
4. `time_start`
5. `time_end`
6. `total_contribution`
7. `contribution_unit`
8. `dataset_version`
9. `generated_at`
10. `source_run_path`
11. `source_scenario_path`

Optional columns:

1. `peak_daily_contribution`
2. `peak_reach_contribution`
3. `reach_count_affected`
4. `geometry_ref`
5. `field_type`

## Validation Rules

1. Validate `run_path` and `scenario_path` exist and are readable.
2. Generate a ranking table before answering ranking questions.
3. Validate all mandatory generated columns exist before answering.
4. Require non-null `field_id`, `scenario_id`, `run_id`, and `total_contribution`.
5. Validate that `generated_at` is parseable datetime.
6. Require numeric `total_contribution` values.
7. If table has `field_type`, only include arable rows unless user requests all field types.

## Ranking Semantics

1. Default metric: `total_contribution`.
2. Order by metric descending, then `field_id` ascending.
3. Default result size: top 20.
4. If user requests top N, honor N.
5. If user requests threshold mode, return all rows with metric >= threshold.

## Filtering Semantics

1. Derive `scenario_id` and `run_id` from generated metadata or explicit generated columns.
2. Filter by exact `scenario_id` and exact `run_id` unless user requests a comparison.
3. If a time window is provided, include rows whose interval intersects the requested window.
4. For comparisons, present one ranking per run and a delta summary.

## Response Contract

Always return:

1. Ranked rows keyed by `field_id`.
2. Metric name and unit.
3. Applied filters.
4. Provenance: `table_path`, `dataset_version`, `generated_at`, `source_run_path`, `source_scenario_path`.
5. Explicit caveats when assumptions were required.

## Failure Contract

If validation fails, do not produce rankings. Return:

1. `status: invalid_input`
2. List of failed checks.
3. Concrete remediation steps.
4. Minimal example of the required schema.

If generation fails, return:

1. `status: generation_failed`
2. Failed generation step and missing artifact detail.
3. Concrete remediation steps.

Canonical reference: `docs/reference/field-drift-ranking-data-contract.md`.
