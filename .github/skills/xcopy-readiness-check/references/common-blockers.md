# Common Xcopy-Readiness Blockers

These are the most common issues that break xcopy-readiness in landscape models. Knowing them helps you spot problems early.

---

## 1. Hard-Coded Absolute Paths (Critical)

### The Problem

```python
# model/base.py
SCENARIO_ROOT = "C:\\Users\\maria\\xAquaticRisk\\scenario"
OUTPUT_DIR = "D:\\runs\\output"
```

When copied to another system, these paths don't exist → model breaks.

### Detection

- Grepping for drive letters (`C:\`, `D:\`)
- UNC paths (`\\server\share`)
- Home directory paths (`/home/user`, `/Users/alice`)

### Typical Locations

- Python imports and module initialization
- Configuration files (YAML, JSON, XML)
- Hard-coded in class constructors
- Batch script working directories

### Repair

✓ Use `os.path` functions:
```python
import os
SCENARIO_ROOT = os.path.join(os.path.dirname(__file__), "..", "scenario")
```

✓ Use environment variables set at runtime:
```python
SCENARIO_ROOT = os.getenv("SCENARIO_ROOT", default_relative_path)
```

✓ Pass paths as parameters:
```python
def initialize(scenario_root, output_dir):
    # Use parameters instead of globals
    pass
```

---

## 2. Missing or Incomplete Setup Script (High Priority)

### The Problem

User copies repo and tries to run it without setup. Model fails because:
- Python venv doesn't exist
- Dependencies not installed
- Binaries not downloaded
- Directories not created

### Detection

- No `setup_*.bat` or `setup.sh` files
- Setup script doesn't handle all dependencies
- README doesn't mention setup steps
- Setup script is incomplete/manual

### Typical Symptoms

```
"ModuleNotFoundError: No module named 'numpy'"
"python-3.9.7-amd64 not found in bin/"
"run directory doesn't exist"
```

### Repair

✓ Create `setup_all_runtimes.bat`:
```batch
@echo off
call .venv\Scripts\activate.bat || (
    python -m venv .venv && call .venv\Scripts\activate.bat
)
pip install -r requirements.txt
mkdir run scenario parameterisation 2>nul
python -m py_compile model\core\base.py
echo Setup complete!
```

✓ Update README with prominent "Setup" section

---

## 3. Unpinned / Flexible Dependencies (Medium Priority)

### The Problem

```text
# ❌ requirements.txt
numpy
pandas>=1.0
scikit-learn
```

Different machines install different versions → Model behaves differently or breaks.

### Detection

- `requirements.txt` missing version pins
- `~=` or `>=` operators (floating version bounds)
- No `requirements.txt` at all

### Repair

✓ Pin all versions:
```text
numpy==1.23.5
pandas==1.5.3
scikit-learn==1.2.1
matplotlib==3.6.2
```

✓ Generate from working environment:
```bash
pip freeze > requirements.txt
```

---

## 4. Missing or Distributed Model Binaries (High Priority)

### The Problem

Model expects pre-installed executables that aren't part of the repo:
- TOXSWA simulator
- R runtime
- Custom DLLs
- Pre-compiled Fortran modules

User gets "file not found" errors.

### Detection

- References to `C:\Program Files\*` paths
- Calls to external commands without confirmation they exist
- Setup script doesn't download/extract binaries

### Typical Errors

```
"toxswa: command not found"
"FileNotFoundError: model\bin\simulator.dll"
"R executable not in Windows PATH"
```

### Repair Strategy

**Option A: Vendor binaries** (if license allows)
```
model/
  bin/
    python-3.9.7-amd64/
    toxswa-4.2/
    r-4.1.0/
```
- Cons: Large file size, may need .gitignore tuning or LFS

**Option B: Auto-download in setup**
```batch
REM In setup_all_runtimes.bat
if not exist "model\bin\toxswa.exe" (
    echo Downloading TOXSWA...
    python download_binaries.py
)
```

**Option C: Document external setup** (for GPL/proprietary)
```markdown
## External Dependencies

Install separately:
1. TOXSWA 4.2: https://wur.nl/toxswa → `C:\Program Files\TOXSWA`
2. R 4.1+: https://cran.r-project.org
3. Add to Windows PATH before running setup

Then: setup_all_runtimes.bat
```

---

## 5. Absolute Paths in Configuration Files (Critical)

### The Problem

Config files lock paths to one system:

```yaml
# ❌ parameterisation/muenster.yaml
LandscapeScenario: "C:\\Users\\team\\projects\\xAquaticRisk\\scenario\\muenster"
Project: "\\\\fileserver\\shared\\runs"
RunDir: "D:\\data\\runs"
```

### Detection

- Searching `.yaml`, `.json`, `.xrun` files for `C:\`, `D:\`, `\\server`
- Searching for `/home/`, `/Users/`

### Repair

Use relative paths:
```yaml
# ✓ GOOD
LandscapeScenario: "../scenario/muenster"
Project: "../run"
RunDir: "../run"
```

Or environment variable placeholders:
```yaml
LandscapeScenario: "${PROJECT_ROOT}/scenario/muenster"
```

And resolve in Python:
```python
import os
config['LandscapeScenario'] = os.path.expandvars(config['LandscapeScenario'])
```

---

## 6. Network/UNC Path Dependencies (Medium Priority)

### The Problem

Model expects data on a network share that won't exist on fresh systems:

```python
SHARED_DATA = "\\\\dataserver\\experiments\\archived_runs"
```

Or configuration reads from remote database without fallback.

### Detection

- UNC paths (`\\server\share`) in code/config
- Hard-coded database connection strings without env var override
- File access to `//mnt/nas` or similar

### Repair

✓ Make paths configurable:
```python
SHARED_DATA = os.getenv("SHARED_DATA_ROOT", os.path.join(PROJECT_ROOT, "data"))
```

✓ Provide fallback:
```python
def get_data_dir():
    if os.path.exists("\\\\dataserver\\experiments"):
        return "\\\\dataserver\\experiments"
    else:
        return os.path.join(PROJECT_ROOT, "sample_data")
```

✓ Document network setup separately from xcopy paradigm

---

## 7. Python sys.path Manipulation (Medium Priority)

### The Problem

Model modifies `sys.path` with absolute paths:

```python
import sys
sys.path.insert(0, "C:\\project\\lib")
sys.path.insert(0, "C:\\project\\analysis")
```

Breaks when paths differ.

### Detection

- Searching for `sys.path.insert` with absolute paths
- Searching for `sys.path.append` in any module

### Repair

✓ Use relative paths:
```python
import os
import sys

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(PROJECT_ROOT, "lib"))
sys.path.insert(0, os.path.join(PROJECT_ROOT, "analysis"))
```

✓ Or convert lib to a proper package:
```
lib/
  __init__.py
  utilities.py
```

Then import as: `from lib import utilities`

---

## 8. Generated Files in Repo (Medium Priority)

### The Problem

Generated files are committed, creating large diffs and sync issues:
- `__pycache__/` directories
- `.pyc` compiled Python
- Jupyter notebook outputs (`.ipynb` with embedded results)
- Generated reports or logs

Users get merge conflicts.

### Detection

- Check for `__pycache__`, `.egg-info`, `.pytest_cache`
- Jupyter notebooks with large output cells
- Logs committed to repo

### Repair

✓ Add to `.gitignore`:
```gitignore
__pycache__/
*.pyc
*.egg-info/
.pytest_cache/
.venv/
run/
output/
*.log
.ipynb_checkpoints/
```

✓ Clean existing:
```bash
find . -type d -name __pycache__ -exec rm -r {} +
git rm -r --cached __pycache__/
```

---

## 9. OS-Specific Path Issues (Low-Medium Priority)

### The Problem

Paths work on Linux/Mac but not Windows or vice versa:

```python
# ❌ BAD: Only works on Linux/Mac
path = "data/scenarios/muenster"  # Works
path = "data\\scenarios\\muenster"  # Fails on Linux

# ✓ GOOD: Works everywhere
path = os.path.join("data", "scenarios", "muenster")
```

### Detection

- Mixed use of `/` and `\` in path strings
- Hard-coded separator assumptions

### Repair

✓ Always use `os.path.join()`:
```python
config_dir = os.path.join(PROJECT_ROOT, "config")
```

✓ For URLs or file:// URIs, use `/`:
```python
file_url = f"file:///{os.path.abspath(path).replace(chr(92), '/')}"
```

---

## 10. Assumptions About Working Directory (Low Priority)

### The Problem

Model assumes it's run from project root, but it's not:

```python
# ❌ BAD: Assumes cwd == project root
config = open("config.yaml")  # Fails if run from subdirectory

# ✓ GOOD: Derives from script location
config_path = os.path.join(os.path.dirname(__file__), "config.yaml")
```

### Detection

- Code opens files without full paths
- Batch scripts don't use `cd /d "%~dp0"`

### Repair

✓ Always derive paths from script/module location:
```python
import os
THIS_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(THIS_DIR)
```

✓ In batch scripts, change to script directory:
```batch
cd /d "%~dp0"
REM Now all relative paths work
```

---

## Repair Priority Matrix

| Issue | Severity | Effort | Priority |
|-------|----------|--------|----------|
| Hard-coded absolute paths | 🔴 Critical | Medium | **1** |
| Missing setup script | 🔴 Critical | Medium | **2** |
| Unpinned dependencies | 🟠 High | Low | **3** |
| Model binaries not available | 🟠 High | High | **4** |
| sys.path manipulation | 🟡 Medium | Low | **5** |
| Generated files in repo | 🟡 Medium | Low | **6** |
| Network path dependencies | 🟡 Medium | Medium | **7** |
| OS-specific path issues | 🟢 Low | Low | **8** |
| Working directory assumptions | 🟢 Low | Low | **9** |

Start with priority 1-2 for maximum impact.

