# document.py

`document.py` is a **developer maintenance utility** located in the model root. It regenerates the repository's metadata and documentation files after a version bump or component update. It is not part of the simulation itself and does not need to be run by end users.

---

## When to run

Run `document.py` manually (or in CI) whenever:

- The model version in [model.json](../../../model.json) is bumped.
- A component is added, removed, or updated and the variant metadata needs to be refreshed.
- `CONTRIBUTING.md` or `latest_versions.json` are out of date.

```powershell
# From the repository root, using the model-core Python
.\model\core\bin\python-3.9.7-amd64\python.exe document.py
```

---

## What it does — step by step

### 1. Resolve the model-core root

```python
root_folder = os.path.abspath(os.path.join(os.path.dirname(base.__file__)))
```

Resolves the absolute path to the LandscapeModel-Core package folder inside `model/core/`. All subsequent paths are anchored here.

### 2. Read the model version

```python
with open(os.path.join(root_folder, "..", "..", "..", "model.json")) as f:
    version = distutils.version.StrictVersion(json.load(f)["version"])
```

Opens `model.json` in the repository root (currently `{ "name": "xAquaticRisk", "version": "2.86" }`) and parses the version as a strict semantic version. The parsed value is available to the `base.documentation` helpers called below.

### 3. Validate variant parts

```python
base.documentation.check_variant_parts(root_folder)
```

Checks that all components declared in the variant folder (`model/variant/`) are actually present and internally consistent. Stops with an error if any component definition is missing or malformed.

### 4. Regenerate CONTRIBUTING.md

```python
base.documentation.write_contribution_notes(
    os.path.join(root_folder, "..", "..", "..", "CONTRIBUTING.md")
)
```

Overwrites `CONTRIBUTING.md` with content generated from component metadata templates managed by the LandscapeModel-Core documentation engine.

### 5. Update the shared version registry

```python
base.documentation.write_repository_info(
    os.path.join(root_folder, "..", "..", ".."),        # repo root
    os.path.join(root_folder, "..", "..", "..", "repository.json"),
    os.path.join(root_folder, "..", "..", "..", "..", "versions.json"),
    "model"
)
```

Writes metadata from `repository.json` (visibility, license, default branch, etc.) into a shared `versions.json` registry one directory **above** the repository. This file is used across the xlandscape organisation to track all component and model versions in one place.

### 6. Update latest_versions.json

```python
base.documentation.write_latest_version_info(
    os.path.join(root_folder, "..", "..", "..", "latest_versions.json")
)
```

Regenerates `latest_versions.json` in the repository root, recording the latest known versions of all subcomponents and dependencies.

---

## Files written

| File | Location | Purpose |
|------|----------|---------|
| `CONTRIBUTING.md` | repo root | Contribution guidelines, auto-generated from component metadata |
| `latest_versions.json` | repo root | Latest versions of all subcomponents |
| `versions.json` | one level above repo root | Shared organisation-wide version registry |

---

## Notes

- `distutils.version.StrictVersion` is deprecated since Python 3.10 and removed in Python 3.12. It still works with the bundled Python 3.9.7 runtime. If the runtime is ever upgraded, this import should be replaced with `packaging.version.Version`.
- The script requires `base` (LandscapeModel-Core) to be importable, so it must be run through the model-core Python runtime or with `model/core` on `PYTHONPATH`.
