# Setup Scripts

xAquaticRisk bundles three setup scripts in the model root that provision the component-local Python runtimes needed to run the Control Panel and the analysis pipeline.
These runtimes are kept completely inside the repository folder so the model stays [xcopy-ready](xcopy-readiness.md) — no system-wide Python installation is required on the target machine.

---

## Why separate runtimes?

The model uses three distinct Python environments:

| Purpose | Runtime location | Provisioned by |
|---------|-----------------|---------------|
| Model core (`__start__.bat`, `notebook.bat`) | `model/core/bin/python-3.9.7-amd64/` | Bundled with the model core submodule — **no setup needed** |
| Control Panel (`controlpanel.bat`) | `controlpanel/python/` | `setup_controlpanel_python.bat` |
| Analysis (`run_basic_analysis.py`) | `analysis/python/` | `setup_analysis_python.bat` |

The model-core runtime ships pre-bundled inside the submodule, so it is always available after cloning or extracting the archive. The Control Panel and analysis runtimes must be provisioned once via the setup scripts because they carry larger package dependencies that would bloat the repository.

---

## `setup_controlpanel_python.bat`

Creates (or repairs) the embedded Python 3.9.7 runtime in `controlpanel\python\` and installs the packages listed in `controlpanel\requirements.txt`.

`controlpanel.bat` checks for `controlpanel\python\python.exe` at startup and prints a clear error message if it is missing:

```
ERROR: Control Panel runtime not found.
Expected bundled runtime: ...\controlpanel\python\python.exe

Run setup_controlpanel_python.bat to provision the bundled runtime.
```

**When to run:**

- Once after cloning the repository.
- After deleting `controlpanel\python\` (for example, to force a clean reinstall).
- To upgrade or repair packages — running the script again on an existing runtime updates packages in place.

---

## `setup_analysis_python.bat`

Creates (or repairs) the embedded Python 3.9.7 runtime in `analysis\python\` and installs the packages listed in `analysis\requirements.txt` (h5py, pandas, geopandas, matplotlib, seaborn, openpyxl, pyogrio).

The Control Panel uses this runtime to run `analysis\run_basic_analysis.py`. If the runtime folder is missing or incomplete, the Control Panel returns a setup error before launching the job rather than a raw Python traceback.

**When to run:**

- Once after cloning the repository.
- After deleting `analysis\python\` to force a clean reinstall.
- To upgrade or repair packages.

---

## `setup_all_runtimes.bat`

Convenience wrapper that runs `setup_controlpanel_python.bat` and `setup_analysis_python.bat` in sequence.

**This is the recommended one-time onboarding step** after cloning:

```bat
setup_all_runtimes.bat
```

If either sub-script fails, the wrapper stops immediately and prints the step that failed.

---

## xcopy deployment

When packaging xAquaticRisk for distribution to a machine without internet access:

1. Run `setup_all_runtimes.bat` on a machine **with** internet access first.
2. Include the runtime folders in the copied or zipped model directory:
   - `controlpanel\python\`
   - `analysis\python\`
3. On the target machine the launchers will find their runtimes without any additional setup.

See [XCopy Readiness](xcopy-readiness.md) for the full portability checklist.
