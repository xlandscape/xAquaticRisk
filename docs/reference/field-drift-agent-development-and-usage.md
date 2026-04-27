# Custom Agent: Development and Usage

This document describes how the field-ranking Copilot custom agent was designed, what constraints it enforces, and how to use it for risk-management questions on pesticide spray-drift to streams.

## Quickstart

1. Open Copilot Chat and select the field-ranking custom agent.
2. Provide only `run_path` and `scenario_path`.
3. Paste and run the prompt below.

```text
Identify the top 10 arable fields by spray-drift contribution.
Use:
- run_path: E:/xAquaticRisk_DLT2025/run/bruchenbruecken_toxswa_10yrs_Mitig1
- scenario_path: E:/xAquaticRisk_DLT2025/scenario/Bruchenbruecken

Generate the ranking table first, then return field_id, total_contribution, contribution_unit, and provenance (table_path, dataset_version, generated_at).
If validation fails, return status invalid_input with missing checks and remediation steps.
If generation fails, return status generation_failed with remediation steps.
```

## Goal

Support landscape-scale mitigation planning by identifying arable fields with the highest potential spray-drift contribution into nearby streams.

Typical question:

- Which arable fields should be prioritized for local mitigation (for example removing arable use near streams) because they contribute most spray-drift?

## Why a Custom Agent

xAquaticRisk model outputs are typically reach-centric. The custom agent therefore standardizes a reproducible generation step that creates a field-level ranking table from run outputs plus scenario geodata, and then answers ranking questions from that table.

This keeps results:

1. Deterministic
2. Auditable
3. Consistent with producer-side preprocessing

## Implemented Artifacts

The implementation consists of four files:

1. Agent definition: `.github/agents/aquatic-drift-field-ranking.agent.md`
2. Behavior and validation rules: `.github/instructions/aquatic-drift-data-contract.instructions.md`
3. Reusable prompt template: `.github/prompts/field-drift-ranking.prompt.md`
4. Canonical data contract: `docs/reference/field-drift-ranking-data-contract.md`

## Design Decisions

1. Scope is workspace-shared so all project collaborators can use the same behavior.
2. Default ranking metric is `total_contribution`.
3. Ranking key is stable LULC field identity (`field_id`).
4. Primary user inputs are only `run_path` and `scenario_path`.
5. Agent generates the ranking table and then validates and queries it.
6. External data paths are supported (for example `E:/xAquaticRisk_DLT2025/...`).

## Data Contract Summary

The agent generates a ranking table with mandatory columns:

1. `field_id`
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

See full definitions and example records in `docs/reference/field-drift-ranking-data-contract.md`.

## Ranking Semantics

Default behavior:

1. Filter by exact `scenario_id` and `run_id`.
2. Restrict to arable rows when `field_type` is available.
3. Rank descending by `total_contribution`.
4. Apply tie-breaker ascending by `field_id`.
5. Return top 20 unless user requests a different N.

## Validation and Failure Behavior

Before ranking, the agent validates schema and core metadata.

If validation fails, it returns a structured remediation response and does not return inferred rankings.

Expected failure payload elements:

1. `status: invalid_input`
2. Failed checks
3. Concrete remediation steps

## Usage Workflow

1. Start a Copilot chat and select the custom agent for field ranking.
2. Provide `run_path` and `scenario_path` (plus optional `top_n` or `time_window`).
3. Let the agent generate the ranking table.
4. Ask a ranking or threshold question.
5. Review output plus provenance (`table_path`, `dataset_version`, `generated_at`, source paths).

## Example Questions

1. Top-N ranking:
   - "Using run path E:/.../bruchenbruecken_toxswa_10yrs_Mitig1 and scenario path E:/.../scenario/Bruchenbruecken, show top 10 arable fields by total contribution"
2. Threshold query:
   - "From run path R and scenario path S, list all arable fields with total_contribution >= 1.0 g/ha"
3. Comparison query:
   - "Generate rankings for run path A and run path B with the same scenario path, then compare top 20 fields"

## Interpreting Results for Mitigation

The ranking is a prioritization signal, not an automatic management decision. Combine it with local constraints such as:

1. Feasibility of land-use changes
2. Reach vulnerability
3. Regulatory constraints and stakeholder objectives

## Maintenance

When producer pipelines change:

1. Keep `field_id` mapping stable across scenario updates.
2. Increment `dataset_version` when extraction logic changes.
3. Refresh `generated_at` on each table rebuild.
4. Update the data contract first, then agent/instruction files if semantics changed.

## Acceptance Checklist

1. Valid table returns deterministic top-N ranking.
2. Missing required columns trigger `invalid_input`.
3. Tie cases resolve by ascending `field_id`.
4. Response always includes provenance metadata.
5. External Windows path handling works or returns actionable path errors.

## Related Documents

1. `docs/reference/field-drift-ranking-data-contract.md`
2. `docs/create-scenario.md`
3. `README.md`
