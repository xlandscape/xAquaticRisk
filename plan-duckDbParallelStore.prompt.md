# Plan: DuckDB Parallel Store — xAquaticRisk

## Background & Motivation

xAquaticRisk uses HDF5 (`arr.dat`) as its single simulation data store — one file per Monte Carlo run, written by the Python `X3dfStore` wrapper and by external R/C++ components (XSprayDrift, CmfContinuous) that access it directly. All analysis scripts (Python `h5py`, R `hdf5r`) read from this file. The goal is to evaluate and incrementally introduce DuckDB as a parallel output store — running alongside HDF5, not replacing it — with a configurable flag in the `.xrun`/`.yaml` parameterisation files.

## Architecture Context (current)

- `model/core/stores/X3dfStore.py` — primary store; wraps `arr.dat` (HDF5) via `h5py`
- `model/core/stores/SqlLiteStore.py` — existing SQLite store; template for DuckDBStore
- `model/core/stores/InMemoryStore.py` — in-memory store for testing
- `model/core/base/Store.py` — abstract base: `set_values`, `get_values`, `describe`, `has_dataset`, `close`
- `model/core/base/MCRun.py:53` — single line that instantiates the store from `mc.xml`: `self._store = getattr(store_module, store_config.attrib["class"])(**store_params)`
- `model/variant/mc.xml` — `<Store module="stores" class="X3dfStore">` block is the only wiring point
- Each MC run: `run/<ExperimentID>/mcs/<MC-ID>/store/arr.dat`
- Key large datasets: `CascadeToxswa/ConLiqWatTgtAvg` (`time/hour × space/reach`), spray drift 3D (`day × x × y`)
- Scales: `time/day`, `time/hour`, `space/reach`, `space/base_geometry`, `other/species`, etc.
- Dataset metadata: `scales`, `unit`, `dim_offset`, `dim_element_names`, `dim_geometries` — stored as HDF5 dataset attributes
- R components (XSprayDrift, CmfContinuous) write directly to `arr.dat` — **not** via Python store API
- Analysis: Python `h5py` (`run_basic_analysis.py`, notebooks) + R `hdf5r` (`ifem_functions.R`, `analysis.R`)

## Pros of DuckDB

1. SQL interface — analysis queries trivially expressed vs. manual index arithmetic in `h5py`
2. Cross-MC aggregation — `ATTACH` or multi-file queries enable ensemble statistics naturally
3. Python integration — `duckdb.connect().sql("...").df()` cleaner than `h5py` slicing
4. R integration — `duckdb` R package + DBI, direct substitute for `hdf5r` reads in analysis
5. Spatial extension — WKT/WKB support for `space/reach` and `space/base_geometry`
6. Parquet interoperability — DuckDB reads/writes Parquet natively
7. Single runtime binary — no HDF5 library installation complexity, no HDFView needed
8. ACID compliance — HDF5 files can silently corrupt on crash/process kill
9. Inspectable by any SQL client
10. `SqlLiteStore.py` as implementation template

## Cons of DuckDB

1. **ND-array mismatch** — HDF5 is purpose-built for dense ND arrays; DuckDB has no native ND array type. Long-format: `87,600 h × 500 reaches = 43.8M rows` per dataset per MC run; 10 datasets × 100 MC runs → ~44 billion rows. Mitigated in first iteration by BLOB storage (no row-per-element).
2. Simulation write performance — needs buffered batch writes
3. External R/C++ components write HDF5 directly — XSprayDrift, CmfContinuous not captured by store API (out of scope for this iteration)
4. Hyperslab slice access — HDF5 zero-copy kernel slicing vs. DuckDB indexed WHERE + materialisation
5. Metadata handling — HDF5 dataset attributes → separate catalog table required
6. MC run initialisation — currently `shutil.copyfile(arr.dat)`; needs redesign for full replacement (out of scope here)

## Three Long-Term Options

| Option | Description | Risk |
|--------|-------------|------|
| A | Full DuckDB replacement of X3dfStore | High |
| **B** | **DualStore: HDF5 primary + DuckDB shadow, configurable per run** ★ | **Low** |
| C | DuckDB + Parquet hybrid (large arrays → Parquet, DuckDB as catalog/query layer) | Medium |

**This plan implements Option B as the first iteration.**

---

## Implementation Plan — Option B: DualStore Parallel Write

### Core design

- `DualStore` wraps `X3dfStore` (primary/read source) and an optional `DuckDBStore` (shadow write)
- All `set_values` calls go to both stores; `get_values`/`describe`/`has_dataset` always delegate to HDF5 only
- Errors in DuckDB shadow writes are **logged but non-fatal** — simulation must never abort due to the parallel store
- Activated by a single `EnableDuckDB` flag in `.xrun`/`.yaml`; default `false`
- No changes to `MCRun.py`, no changes to any component, no changes to R/C++ components

### Phase 1 — DuckDBStore backend

**File: `model/core/stores/DuckDBStore.py`** (new)

Implement `base.Store`. One `.duckdb` file per MC run at `$(_MCS_BASE_DIR_)\$(_MC_NAME_)\store\store.duckdb`.

Schema:

```sql
-- Dataset catalog (mirrors HDF5 dataset attributes)
CREATE TABLE datasets (
    name        TEXT PRIMARY KEY,
    scales      TEXT,           -- comma-separated scale list, e.g. "time/hour, space/reach"
    unit        TEXT,
    dtype       TEXT,           -- numpy dtype string, e.g. "float32"
    shape       TEXT,           -- JSON array, e.g. "[87600, 500]"
    chunks      TEXT,           -- JSON array or NULL
    value_type  TEXT            -- "numpy.ndarray" | "str" | "float" | "int" | "bool" | "datetime.date" | etc.
);

-- Scalar primitives
CREATE TABLE scalars (
    name        TEXT PRIMARY KEY,
    value_text  TEXT
);

-- ND arrays as compressed numpy bytes (gzip via numpy.save / io.BytesIO)
CREATE TABLE arrays (
    name        TEXT PRIMARY KEY,
    data        BLOB
);

-- Named element labels per dimension (space/reach names, species names, etc.)
CREATE TABLE element_names (
    name        TEXT,
    dim         INTEGER,
    idx         INTEGER,
    label       TEXT,
    PRIMARY KEY (name, dim, idx)
);

-- Offsets per dimension (time/day, time/hour integer offsets; datetime offsets stored as ISO string)
CREATE TABLE offsets (
    name        TEXT,
    dim         INTEGER,
    offset_val  TEXT,
    PRIMARY KEY (name, dim)
);

-- Geometry WKB per named element (space/reach, space/base_geometry)
CREATE TABLE geometries (
    name        TEXT,
    dim         INTEGER,
    idx         INTEGER,
    wkb         BLOB,
    PRIMARY KEY (name, dim, idx)
);
```

Key implementation notes:
- `set_values(name, values, ...)`: serialise numpy arrays as `io.BytesIO` + `numpy.save` (keeps dtype/shape), compress with `gzip`, store as BLOB. Write metadata to `datasets`, element names to `element_names`, offsets to `offsets`.
- `get_values(name, ...)`: decompress BLOB, `numpy.load(io.BytesIO(...))` to reconstruct array. Support `slices` keyword via post-load slicing.
- `close()`: commit + close DuckDB connection.
- `has_dataset(name, partial)`: `SELECT COUNT(*) FROM datasets WHERE name = ?` (or `LIKE` for partial).

### Phase 2 — DualStore wrapper

**File: `model/core/stores/DualStore.py`** (new)

```python
class DualStore(base.Store):
    def __init__(self, file_path, observer, mode="a", initialization=None,
                 identifier=0, enable_duckdb=False, duckdb_path=None):
        self._primary = X3dfStore(file_path, observer, mode, initialization, identifier)
        self._secondary = None
        if enable_duckdb and duckdb_path:
            os.makedirs(os.path.dirname(duckdb_path), exist_ok=True)
            self._secondary = DuckDBStore(duckdb_path, observer)

    def set_values(self, name, values, **kw):
        self._primary.set_values(name, values, **kw)
        if self._secondary:
            try:
                self._secondary.set_values(name, values, **kw)
            except Exception as e:
                self._primary._observer.write_message(2, "DualStore DuckDB shadow write failed", str(e))

    def get_values(self, name, **kw):
        return self._primary.get_values(name, **kw)

    def describe(self, name):
        return self._primary.describe(name)

    def has_dataset(self, name, partial=False):
        return self._primary.has_dataset(name, partial)

    def close(self):
        self._primary.close()
        if self._secondary:
            self._secondary.close()
```

Register in `model/core/stores/__init__.py`.

### Phase 3 — mc.xml wiring

Replace the `<Store>` block in `model/variant/mc.xml`:

```xml
<Store module="stores" class="DualStore">
    <File_Path>$(_MCS_BASE_DIR_)\$(_MC_NAME_)\store</File_Path>
    <Observer>
        <ObserverReference/>
    </Observer>
    <Initialization>$(ParentRun)</Initialization>
    <Identifier>$(_MC_ID_)</Identifier>
    <Enable_DuckDB>$(EnableDuckDB)</Enable_DuckDB>
    <DuckDB_Path>$(_MCS_BASE_DIR_)\$(_MC_NAME_)\store\store.duckdb</DuckDB_Path>
</Store>
```

### Phase 4 — Parameterisation files

Add to `Control` section in both `parameterisation/template.yaml` and `parameterisation/template.xrun`:

```yaml
# Enables a DuckDB shadow store written in parallel to the HDF5 store (arr.dat).
# Values: true or false
# Remark: When enabled, a store.duckdb file is written alongside arr.dat in each MC run's store folder.
#         The HDF5 store remains the primary store; DuckDB is written for analysis and future migration.
EnableDuckDB: false
```

Add `"EnableDuckDB"` to `PARAM_ORDER["Control"]` in `convert_xrun.py`.

Update `parameterisation/template - muenster.yaml` as a reference example with `EnableDuckDB: true`.

### Phase 5 — Validation

Write `analysis/validate_duckdb_store.py`:

1. Open `arr.dat` (h5py) and `store.duckdb` (duckdb) for the same MC run
2. Assert dataset count matches
3. Element-wise comparison for `CascadeToxswa/ConLiqWatTgtAvg` and `IndEffect_LP50_*/LP50`
4. Verify metadata: scales, unit, element names, offsets match between stores
5. Report wall-clock overhead: `EnableDuckDB: false` vs. `true` for a standard run

---

## Relevant Files

| File | Role | Change |
|------|------|--------|
| `model/core/stores/DuckDBStore.py` | New DuckDB backend | **New** |
| `model/core/stores/DualStore.py` | Parallel store wrapper | **New** |
| `model/core/stores/__init__.py` | Store exports | Add `DualStore` |
| `model/core/base/Store.py` | Abstract base | Read-only reference |
| `model/core/base/MCRun.py` | Store instantiation | **No changes** |
| `model/variant/mc.xml` | Store wiring | Change `<Store>` block |
| `parameterisation/template.yaml` | User params | Add `EnableDuckDB` |
| `parameterisation/template.xrun` | User params | Add `EnableDuckDB` |
| `parameterisation/template - muenster.yaml` | Reference example | Set `EnableDuckDB: true` |
| `convert_xrun.py` | xrun round-trip converter | Add to `PARAM_ORDER` |
| `analysis/validate_duckdb_store.py` | Validation script | **New** |

## Out of Scope (this iteration)

- External R/C++ components (XSprayDrift, CmfContinuous) — still write HDF5 directly; not captured by Python store API
- Scenario-building HDF5 input files (e.g. Muenster scenario) — input files, not simulation store
- Long-format normalisation of arrays — arrays stored as BLOBs in Phase 1; per-dataset normalisation is a follow-on
- Full replacement of X3dfStore — only after Phase 5 validation passes
- Analysis script migration (h5py → DuckDB) — follow-on after DualStore is proven

## Open Questions

1. Does `base.convert()` in `MCRun.py` parse `$(EnableDuckDB)` correctly as Python `bool` from YAML `true`/`false`? If not, add `str → bool` coercion in `DualStore.__init__`.
2. DuckDB version: confirm latest stable v1.x wheels are available for the bundled Python 3.9.7 environment at `model/core/bin/python-3.9.7-amd64`.
3. Should `store.duckdb` path be configurable per-parameterisation file (for non-standard store locations), or is the convention `store/store.duckdb` always sufficient?
