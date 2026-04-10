# Step 2 Plan: Fast Inflow TimeSeries Trimming

## Goal
Improve performance of inflow CSV trimming during scenario subset creation by introducing a vectorized, chunked backend while preserving exact output semantics and fallback portability.

## Scope
- Target function: `_slice_timeseries_csv(...)` in `controlpanel/server.py`
- Called from scenario subset flow in `create_scenario_subset(...)`
- Input files: `scenario/*/hydro/TimeSeries/*.csv`

## Baseline Observation
A benchmark on a real file (`r1018.csv`, ~8.21 MB) showed Step 1 did not improve runtime on that dataset:
- baseline ~2.169 s vs optimized ~2.239 s (all reaches)
- baseline ~2.223 s vs optimized ~2.271 s (selected reaches sample)

Conclusion: Step 2 should use chunked, vectorized filtering to reduce Python row-loop overhead.

## Design

### 1) Add a pandas backend
Create a new helper in `controlpanel/server.py`:
- `_slice_timeseries_csv_pandas(source_csv, target_csv, start_dt, end_dt, cancel_cb=None, selected_reach_ids=None, chunksize=...)`

Behavior:
- Read CSV in chunks (`pd.read_csv(..., chunksize=...)`)
- Keep header and write filtered chunks incrementally
- Timestamp filtering:
  - preferred: lexical compare on canonical timestamp string column
  - safety: parse invalid rows via strict coercion only when required
- Reach filtering:
  - if `selected_reach_ids` is provided, normalize reach IDs once per chunk and filter via vectorized membership (`isin`)
- Cancellation:
  - check `cancel_cb` between chunks
- Return kept row count

### 2) Keep robust fallback
Update `_slice_timeseries_csv(...)` dispatcher:
- Use pandas backend when `pd is not None`
- Fallback to current csv backend when pandas is unavailable
- Preserve existing signature and return type

### 3) Preserve semantics
Must match current behavior:
- Skip malformed rows (too few columns)
- Skip invalid timestamps
- Respect selected reach filtering
- Keep output format and header compatible with existing pipeline

### 4) Add backend telemetry (optional but recommended)
For diagnostics and future tuning:
- backend used (`pandas` or `csv`)
- processed rows
- kept rows
- elapsed time (ms)

Can be logged in subset progress messages or debug logs.

## Proposed Implementation Steps
1. Add pandas backend helper for chunked filtering.
2. Refactor `_slice_timeseries_csv(...)` into dispatcher + current csv backend helper.
3. Wire cancellation checks per chunk.
4. Add lightweight counters for rows processed/kept.
5. Keep exact function contract for callers.

## Validation Plan

### Functional
- Compare output row count and line count versus current backend on multiple files.
- Compare first/last N lines for deterministic consistency.
- Validate behavior with and without selected reach IDs.

### Performance
Run timing benchmarks on at least 3 representative files:
- small (<10 MB)
- medium (10-100 MB)
- large (>100 MB if available)

Measure:
- elapsed time per file
- total subset phase time in `create_scenario_subset(...)`

Acceptance target:
- >=1.5x speedup on medium/large files
- no regressions in output correctness

## Risks
- CSV schema variability across scenarios
- Timestamp formatting irregularities
- Extra memory usage if chunksize is too large

Mitigations:
- conservative chunksize default
- strict row guards and fallback to csv backend
- benchmark and tune chunksize

## Suggested Initial Defaults
- `chunksize`: 200_000 rows
- `dtype`: read reach/timestamp columns as string for stable filtering
- cancellation check frequency: once per chunk

## Exit Criteria
- Step 2 merged with fallback intact
- Benchmarks documented in this folder
- No behavioral differences from baseline outputs
