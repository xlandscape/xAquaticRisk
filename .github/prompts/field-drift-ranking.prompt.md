---
mode: ask
description: "Generate and rank arable fields by spray-drift contribution from run and scenario paths"
---

Generate a field-level spray-drift ranking table, then return ranked arable fields.

Inputs:
- run_path: ${input:run_path}
- scenario_path: ${input:scenario_path}
- top_n: ${input:top_n}
- optional_time_start: ${input:time_start}
- optional_time_end: ${input:time_end}
- optional_output_table_path: ${input:output_table_path}

Instructions:
1. Validate run_path and scenario_path are accessible.
2. Generate a ranking table using the contract in docs/reference/field-drift-ranking-data-contract.md.
3. Persist table to output_table_path, or default to <run_path>/field_ranking.csv.
4. Validate required generated columns and metadata.
5. Derive scenario_id and run_id from generated metadata.
6. If optional time window is provided, apply interval overlap filtering.
7. Rank by total_contribution descending, tie-break by field_id ascending.
8. Return top_n rows with field_id, total_contribution, contribution_unit.
9. Include provenance: table_path, dataset_version, generated_at, source_run_path, source_scenario_path.
10. If validation fails, return status invalid_input.
11. If generation fails, return status generation_failed with remediation steps.
