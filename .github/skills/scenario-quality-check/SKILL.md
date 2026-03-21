---
name: scenario-quality-check
description: 'Validate a newly generated landscape scenario against a reference scenario (for example Rummen), with deep structural checks and detailed hydrology HDF5 quality analysis for hydro/hydro_reaches.h5.'
argument-hint: 'Provide candidate and reference scenarios, e.g. /scenario-quality-check --candidate muenster-slice2 --reference Rummen'
---

# Scenario Quality Check

This skill performs a deep quality audit of a generated landscape scenario against a reference scenario.

Primary focus:
- Structural integrity of the scenario folder
- Internal consistency between `hydro_reaches.h5`, reach geometry, and time-series CSV files
- Detailed diagnostics for the hydrology HDF5 file (`hydro/hydro_reaches.h5`)

## When to Use

- After creating a temporal or spatial subset scenario
- Before launching heavy MC runs
- When generated scenarios fail in components like CascadeToxswa or StepsRiverNetwork
- When validating a new slicing workflow against a known-good reference (for example `Rummen`)

## What Gets Checked

### 1. Scenario Structure

- Required folders and files exist
- Candidate can be resolved from a scenario name or path
- Reach shapefile can be found in `geo/`

### 2. Hydrology HDF5 (`hydro_reaches.h5`)

- Required datasets exist: `flow`, `depth`, `volume`, `area`, `reaches`, `time_from`, `time_to`
- Dimensional consistency:
  - all hydro arrays are 2D and same shape
  - `len(reaches)` equals number of hydro columns
- Time contract consistency:
  - `time_from` and `time_to` parse correctly
  - row count equals expected hourly steps from metadata
- Reach ID quality:
  - IDs are normalized and unique
  - overlap and subset relation against reference scenario
- Data quality:
  - finite ratio scan (NaN/Inf detection)
  - min/max and basic dataset sanity checks

### 3. Cross-File Consistency

- Reach shapefile IDs vs HDF5 reach IDs:
  - reaches in HDF but missing in shapefile
  - features in shapefile but missing in HDF
- Time-series CSV files in `hydro/TimeSeries`:
  - datetime parse success
  - timeframe aligned with hydrology metadata
  - reach IDs aligned with HDF5 reach IDs

### 4. Candidate vs Reference Comparison

- Required datasets and dtypes parity
- Candidate coverage interval compared to reference interval
- Reach overlap metrics and suspicious deviations
- Severity-ranked findings (`critical`, `high`, `medium`, `low`)

## Usage

Run from repository root:

```powershell
python .github/skills/scenario-quality-check/check_scenario_quality.py --candidate muenster-slice2 --reference Rummen
```

With explicit scenario paths:

```powershell
python .github/skills/scenario-quality-check/check_scenario_quality.py --candidate scenario/muenster-slice2 --reference scenario/Rummen
```

Save machine-readable report:

```powershell
python .github/skills/scenario-quality-check/check_scenario_quality.py --candidate muenster-slice2 --reference Rummen --json-out analysis_output/scenario_quality_muenster-slice2.json
```

## Interpreting Results

- `PASS`: no critical or high findings
- `WARN`: medium/low findings only
- `FAIL`: at least one critical/high finding; scenario should be repaired before production runs

## Output

The checker prints:
- Candidate and reference metadata summary
- HDF5 detail summary (shape, time interval, reach counts, finite stats)
- Cross-file consistency findings
- Severity-ranked diagnostics with fix hints

Optional JSON output includes the same findings in structured form for pipelines.
