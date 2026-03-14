# Plan: Log Severity Reclassification

## Objective

Reclassify log messages in xAquaticRisk so that:
- All WARN (level 2) informational "does not check" and ScalesChecker messages → NOTE (level 3)
- "Failed to compute min/max, no valid pixels found in sampling. (GDAL error 1)" ERROR → WARN
- All other fatal ERRORs stay as ERROR

---

## Background: Observer severity system

Integer levels: `1=ERROR`, `2=WARN`, `3=NOTE`, `4=OK`, `5=INFO`.

Two parallel code paths carry messages:

1. **`write_message(level, message, detail)`** — structured messages (Python components). Sets `MultiObserver._error / _warning / _note` depending on level.
2. **`write(text)`** — unstructured raw text (R subprocess stdout/stderr merged). `MultiObserver.write()` scans each line with regexes to classify it.

At `mc_run_finished()` and `experiment_finished()`, the `MultiObserver` replays the highest-severity state as a run-end summary header + last message at that level.

---

## Change Group A — "currently does not check" WARN → NOTE (5 call sites)

These messages are emitted once during component `prepare()` and declare a known limitation. The simulation proceeds correctly regardless — purely informational.

| File | Line | Change |
|---|---|---|
| `model/core/components/MarsWeather.py` | 141 | `write_message(2, "MarsWeather currently does not check..."` → level `3` |
| `model/core/components/DepositionToReach.py` | 159 | `write_message(2, "DepositionToReach currently does not check..."` → level `3` |
| `model/variant/CascadeToxswa/CascadeToxswa.py` | 428 | `write_message(2, "CascadeToxswa currently does not check..."` → level `3` |
| `model/variant/XSprayDrift/SprayDrift.py` | 542 | `write_message(2, "XSprayDrift currently does not check..."` → level `3` |
| `model/variant/StepsRiverNetwork/StepsRiverNetwork.py` | 424 | `write_message(2, "StepsRiverNetwork currently does not check..."` → level `3` |

---

## Change Group B — ScalesChecker default severity WARN → NOTE

File: `model/core/attrib/Scales.py` — `__init__` default parameter.

```python
# Before
def __init__(self, expected_scales: str, severity: int = 2) -> None:

# After
def __init__(self, expected_scales: str, severity: int = 3) -> None:
```

The `CascadeToxswa` inputs (HydrographyGeometries, DownstreamReach, BottomWidth, BankSlope, OrganicContent, BulkDensity, Porosity) declare `attrib.Scales("space/reach")` but receive data shaped to `space/base_geometry` — a known mismatch that the component handles internally. Changing the global default to `3` demotes all such checks. Any component needing WARN-level enforcement can still pass `severity=2` explicitly.

---

## Change Group C — "Failed to compute min/max" ERROR → WARN

### Root cause

The R subprocess runner in `model/core/base/functions.py` (line 254-265) runs R with `stderr=subprocess.STDOUT` and reads each output line via `observer.write(text)`.

`MultiObserver.write()` scans raw lines:

```python
# model/core/base/Observer.py — MultiObserver.write()
if not self._error and re.search("(?<!std. )error", text, re.IGNORECASE):
    self._error = text, ""        # ← "GDAL error 1" matches here → sets _error
if not self._error and re.search("warn", text, re.IGNORECASE):
    self._warning = text, ""
```

The text `"Failed to compute min/max, no valid pixels found in sampling. (GDAL error 1)"` contains `"error"` not preceded by `"std. "` → sets `_error`. At `mc_run_finished()` this triggers: `ERROR MC run completed with errors / Please report to the developers`.

### Fix

Add a GDAL-specific branch before the general error check:

```python
# model/core/base/Observer.py — MultiObserver.write()
if not self._error and re.search("(?<!std. )error", text, re.IGNORECASE):
    if re.search(r"gdal error", text, re.IGNORECASE):
        self._warning = text, ""   # GDAL errors are benign; demote to warning
    else:
        self._error = text, ""
if not self._error and re.search("warn", text, re.IGNORECASE):
    self._warning = text, ""
```

---

## Run-end summary transformation (Muenster scenario)

| Before | After |
|---|---|
| `ERROR MC run completed with errors` | `WARN  MC run completed with warnings` |
| `      Please report to the developers:` | `      Please check results and consider reporting to the developers:` |
| `ERROR Failed to compute min/max, no valid pixels found in sampling. (GDAL error 1)` | `WARN  Failed to compute min/max, no valid pixels found in sampling. (GDAL error 1)` |
| `WARN  MC run completed with warnings` | `NOTE  MC run completed with notes` |
| `WARN  Warning message:` | `NOTE  Simulation results are not affected:` |

---

## Implications

- **`MultiObserver._error`**: No longer set by GDAL errors, "does not check" messages, or ScalesChecker mismatches.
- **`MultiObserver._warning`**: Set by "Failed to compute min/max" (via GDAL demotion) and raw R "Warning message:" lines. Run ends with WARN summary.
- **`MultiObserver._note`**: Set by "does not check" and ScalesChecker messages.
- **R subprocess "Warning message:\nNAs introduced by coercion"**: Remains `_warning` via the `"warn"` keyword in `write()` — no change needed there.
- **Zero impact on simulation results** — logging-only changes.

---

## What stays as WARN (out of scope)

| File | Message | Reason to keep |
|---|---|---|
| `functions.py` | "R_LIBS_USER not set" | Configuration problem |
| `MarsWeather.py` | "Weather file does not contain field" | Data integrity concern |
| `HydrologyFromTimeSeries.py` | "Temporal inconsistency in hydrological scenario" | Data integrity concern |
| `LandCoverToVegetation.py` | "No vegetation defined for land cover class" | Data completeness concern |
| `X3dfStore.py` (5 messages) | Scale/dimension/geometry mismatches | Store-level anomalies |
| `Experiment.py` (2 messages) | No metadata file / unknown creation method | Configuration concerns |

---

## Files to modify

| File | Location | Type |
|---|---|---|
| `model/core/components/MarsWeather.py` | Line 141 | `2` → `3` |
| `model/core/components/DepositionToReach.py` | Line 159 | `2` → `3` |
| `model/variant/CascadeToxswa/CascadeToxswa.py` | Line 428 | `2` → `3` |
| `model/variant/XSprayDrift/SprayDrift.py` | Line 542 | `2` → `3` |
| `model/variant/StepsRiverNetwork/StepsRiverNetwork.py` | Line 424 | `2` → `3` |
| `model/core/attrib/Scales.py` | Line ~22 | default `severity: int = 2` → `3` |
| `model/core/base/Observer.py` | Lines ~303-307 | Add GDAL branch in `write()` |

---

## Open questions

1. Should the `StepsRiverNetwork` change be included now, or deferred until that component is active in a scenario?
2. R subprocess raw "NAs introduced by coercion" remains in `_warning` (no structured `write_message`, comes through `write()`). Is that acceptable, or should R raw warnings also be excluded from the run-end summary?
3. Should GDAL error demotion be broadened to all GDAL errors (pattern `"gdal error"`) or narrowed further to only `"gdal error 1"` (informational, non-fatal)?
