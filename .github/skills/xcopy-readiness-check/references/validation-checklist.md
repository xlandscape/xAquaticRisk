# Xcopy-Readiness Validation Checklist

Use this checklist to manually verify xcopy-readiness after running automated checks or before releasing a model version.

## Pre-Release Checklist

### Setup & Installation

- [ ] `setup_all_runtimes.bat` (or `.sh`) exists and runs without errors
- [ ] Script creates `.venv` virtual environment automatically
- [ ] Script installs all Python dependencies from `requirements.txt`
- [ ] First-time users can run setup without external guidance
- [ ] No prompts asking for system paths or configuration
- [ ] Setup completes in < 10 minutes on a typical developer machine
- [ ] Setup works on a **fresh VM or container** with no prior setup

### File Structure

- [ ] No absolute paths (`C:\`, `/home/user/`, UNC paths) in committed files
- [ ] All config files use relative paths or environment variables
- [ ] No auto-generated files committed (`.venv`, `__pycache__`, `.pyc`, outputs)
- [ ] `.gitignore` properly excludes runtime/generated files
- [ ] README includes "Fresh Install" section with step-by-step setup

### Python & Dependencies

- [ ] `requirements.txt` exists with pinned versions (e.g., `numpy==1.23.5`)
- [ ] All imports in Python code use relative paths or installed packages
- [ ] No hard-coded paths like `sys.path.append("C:\\model\\lib")`
- [ ] No assumption of system-level Python packages
- [ ] Virtual environment can be deleted and recreated without issues

### Model Binaries & Runtime

- [ ] All model binaries are either:
  - [ ] Vendored (committed in `model/bin/` or similar), OR
  - [ ] Auto-downloaded via setup script, OR
  - [ ] Available as pip-installable packages
- [ ] No assumption of pre-installed executables (e.g., TOXSWA, R)
- [ ] Binary paths are resolved relative to project root
- [ ] Setup script verifies critical binaries are present

### Scenario & Parameterization

- [ ] Scenario files don't contain hard-coded paths
- [ ] Default scenario can be found from project root
- [ ] Parameterization templates use relative paths
- [ ] No UNC share references (`\\server\share\scenario`)
- [ ] Example project configurations included for testing

### Configuration Files

**YAML (`.yaml`, `.xrun` if YAML-based):**
- [ ] No absolute paths in `LandscapeScenario`, `RunDir`, `Project` fields
- [ ] Use format: `"../scenario/muenster"` or `"${PROJECT_ROOT}/scenario/muenster"`
- [ ] Python loader expands environment variables

**JSON:**
- [ ] No absolute paths in any string values
- [ ] Paths use forward slashes or properly escaped backslashes
- [ ] Boolean/numeric types correctly typed (not strings)

**XML:**
- [ ] No absolute paths in element text
- [ ] Relative paths properly resolved in code

### Database & External Services

- [ ] Database paths configurable (not hard-coded)
- [ ] Connection strings use environment variables or config
- [ ] No expectation of remote server availability (unless documented)
- [ ] Fallback to local SQLite or in-memory data if external service unavailable

### Documentation

- [ ] `README.md` has "Quick Start" section
  - [ ] Step 1: Clone/copy repo
  - [ ] Step 2: Run setup script
  - [ ] Step 3: Run example model
- [ ] README includes troubleshooting for common path issues
- [ ] `CONTRIBUTING.md` mentions xcopy-ready paradigm
- [ ] Setup instructions mention minimum requirements (Python 3.9+, etc.)

### Testing on Fresh System

| Test | Expected Result |
|------|-----------------|
| Copy repo to new folder | No symbolic links or missing files |
| Delete `.venv/` and re-run setup | Setup completes successfully |
| Run model simulation | Model runs without "path not found" errors |
| Change `PROJECT_ROOT` env var | Model still works (if using env vars) |
| Move entire repo to different drive | Model still works |
| Run from different working directory | Model still finds resources |

---

## Common Failure Points

### ❌ Hard-Coded Paths
**Found in:** Config files, Python imports, batch scripts
```python
# BAD
MODEL_ROOT = "C:\\Users\\alice\\projects\\xAquaticRisk"
```
**Fix:**
```python
# GOOD
MODEL_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
```

### ❌ Missing Setup Script
**Symptom:** Repo works locally after manual setup, but fails for others
**Fix:** Create comprehensive `setup_all_runtimes.bat`

### ❌ Unpinned Requirements
**Found in:** `requirements.txt`
```text
# BAD
numpy
pandas
scikit-learn>=1.0
```
**Fix:**
```text
# GOOD
numpy==1.23.5
pandas==1.5.3
scikit-learn==1.2.1
```

### ❌ Python Path Manipulation
**Found in:** Model initialization code
```python
# BAD
sys.path.insert(0, "C:\\project\\lib")
```
**Fix:**
```python
# GOOD
lib_path = os.path.join(PROJECT_ROOT, "lib")
sys.path.insert(0, lib_path)
```

### ❌ Environment Variables Assumed But Not Set
**Problem:** Setup doesn't set them; code fails
**Fix:** Setup script must export all required vars:
```batch
set MODEL_DATA=%PROJECT_ROOT%\data
set OUTPUT_DIR=%PROJECT_ROOT%\output
python script.py
```

### ❌ Binary Paths Hard-Coded
**Found in:** Model config or C# code
```xml
<!-- BAD -->
<ExecutablePath>C:\TOXSWA\toxswa.exe</ExecutablePath>
```
**Fix:**
```xml
<!-- GOOD -->
<ExecutablePath>${MODEL_ROOT}\bin\toxswa\toxswa.exe</ExecutablePath>
```

---

## Quick Validation Script

Run this to catch common issues:

```python
import os
import re

def check_xcopy_ready(repo_root):
    """Quick xcopy-readiness scan."""
    issues = []
    
    # Check for absolute paths in key files
    for root, dirs, files in os.walk(repo_root):
        # Skip .git, .venv, etc.
        dirs[:] = [d for d in dirs if not d.startswith('.')]
        
        for file in files:
            if file.endswith(('.yaml', '.json', '.xml', '.py')):
                filepath = os.path.join(root, file)
                with open(filepath, 'r', errors='ignore') as f:
                    for i, line in enumerate(f, 1):
                        # Detect absolute paths
                        if re.search(r'[C-Z]:\\|\\\\[a-zA-Z]|/home/|/Users/', line):
                            if not any(x in line for x in ['example', 'placeholder', '#']):
                                issues.append(f"{filepath}:{i} - Absolute path: {line.strip()}")
    
    if issues:
        print("❌ Xcopy-readiness issues found:")
        for issue in issues:
            print(f"  {issue}")
    else:
        print("✓ No obvious xcopy-readiness issues detected")
    
    return len(issues) == 0

check_xcopy_ready(".")
```

---

## Sign-Off

Before releasing a model version:

- [ ] **Developer**: Runs full validation checklist
- [ ] **QA/Reviewer**: Tests on fresh VM
- [ ] **Documentation**: Updated README and troubleshooting
- [ ] **Release**: Mark as xcopy-ready in release notes

