---
name: xcopy-readiness-check
description: 'Evaluate whether a landscape model repo (e.g., xAquaticRisk) is xcopy-ready by checking for hard-coded paths, dependency issues, setup automation, and portability blockers. Use when preparing a model repo for distribution or migration to new systems. Proposes repair actions for identified issues.'
argument-hint: 'Provide the model repo path (e.g., c:\LocalWork\xAquaticRisk or /path/to/model)'
---

# Xcopy-Readiness Check

The xcopy-ready paradigm: **A user should be able to copy the root folder to any fresh system and start a model simulation without manual configuration or environment setup.**

## When to Use

- **Preparing for distribution**: Before releasing a model version
- **Testing portability**: Validating a repo on a new system
- **Migration**: Moving a model to a different machine or environment
- **CI/CD setup**: Before automating model runs
- **Troubleshooting setup failures**: Diagnosing why a fresh copy doesn't work

## What Gets Checked

### 1. **Setup Automation** (Critical)
   - ✓ Setup scripts present and functional (`setup_*.bat`, `setup_*.sh`)
   - ✓ One-command initialization (`__start__.bat` or equivalent)
   - ✓ Scripts handle missing dependencies gracefully

### 2. **Hard-Coded Paths** (Critical Blocker)
   - ✓ No absolute paths in config files (`.xrun`, `.yaml`, `.json`)
   - ✓ No drive letters (e.g., `C:\`) in configuration files
   - ✓ Python imports use relative paths or package discovery
   - ✓ No UNC paths (`\\server\share`) in configs

### 3. **Python & Runtime Portability** (High Priority)
   - ✓ Virtual environment auto-creation (`setup_*_python.bat`)
   - ✓ `requirements.txt` with pinned versions
   - ✓ No system-level Python dependency assumptions
   - ✓ Model binaries/executables are vendored or auto-downloaded

### 4. **External Dependencies** (Medium Priority)
   - ✓ Database/service paths are configurable
   - ✓ No hardcoded URLs (except as fallback with override)
   - ✓ API keys/credentials can be injected, not stored

### 5. **File Structure** (Documentation)
   - ✓ `README.md` contains setup instructions for fresh install
   - ✓ `CONTRIBUTING.md` or setup guide mentions xcopy-ready paradigm
   - ✓ `.gitignore` includes generated/runtime files (not committed)

---

## Procedure

### Step 1: Run the Automated Checker

Invoke with your model repo path:

```
/xcopy-readiness-check c:\LocalWork\xAquaticRisk
```

This scans:
- Setup scripts for functionality and coverage
- Config files (`.yaml`, `.xrun`, `.json`) for absolute paths
- `requirements.txt` and dependency declarations
- Python code for hard-coded paths
- README and documentation

### Step 2: Review the Diagnostic Report

The report lists:

| Status | Meaning |
|--------|---------|
| ✓ PASS | Feature detects no issues |
| ⚠ WARN | Feature may have issues; manual review recommended |
| ✗ FAIL | Critical blocker for xcopy-readiness |

### Step 3: Assess Proposed Repairs

For each identified issue, the skill proposes:

1. **Issue description** (what breaks xcopy-readiness)
2. **Root cause** (why it's a problem)
3. **Repair action** (how to fix)
4. **Effort estimate** (quick / moderate / complex)
5. **Priority** (critical / high / medium / low)

### Step 4: Approve Repairs (Optional)

Review proposed fixes and accept/reject each one:

```
> Accept repair "Replace hard-coded path in model.json"
> Skip repair "Add setup_gpu.bat" (can do later)
```

### Step 5: Apply Fixes (If Approved)

The skill implements accepted repairs:
- Edits config files to use relative paths
- Creates/updates setup scripts
- Adds environment variable bindings
- Updates documentation

---

## Common Issues & Fixes

### Issue: Hard-Coded Absolute Paths

**Example:**
```json
{
  "scenario_path": "C:\\Users\\researcher\\xAquaticRisk\\scenario\\muenster",
  "run_dir": "\\\\server\\runs"
}
```

**Fix:**
Replace with:
```json
{
  "scenario_path": "./scenario/muenster",
  "run_dir": "./run"
}
```

Or use environment variables:
```json
{
  "scenario_path": "${PROJECT_ROOT}/scenario/muenster"
}
```

### Issue: Missing Setup Script

**Symptom:** No `setup_*.bat` or README doesn't mention setup steps.

**Fix:** Create `setup_all_runtimes.bat` that:
1. Creates Python venv
2. Installs from `requirements.txt`
3. Downloads/extracts model binaries
4. Validates installation

### Issue: Absolute Paths in Python Code

**Example:**
```python
MODEL_ROOT = "C:\\xAquaticRisk"
```

**Fix:**
```python
import os
MODEL_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
```

### Issue: Requirements.txt Missing or Unpinned

**Bad:**
```
numpy
pandas
```

**Good:**
```
numpy==1.23.5
pandas==1.5.3
scikit-learn==1.2.1
```

---

## References

- **Setup Script Guidelines**: [setup-script-template.md](./references/setup-script-template.md)
- **Path Resolution Strategies**: [path-resolution.md](./references/path-resolution.md)
- **Validation Checklist**: [validation-checklist.md](./references/validation-checklist.md)
- **Common Blockers**: [common-blockers.md](./references/common-blockers.md)

---

## Implementation Strategy

1. **Quick scan** (2–5 min): Automated directory and file analysis
2. **Deep analysis** (5–10 min): Parse configs, trace imports, check scripts
3. **Report generation** (1–2 min): Compile findings with repair proposals
4. **Interactive review** (5–15 min): User decides on each repair
5. **Auto-fix** (5–30 min): Apply approved repairs and generate summary

---

## Output

After completion, you receive:

- **Xcopy-Readiness Score**: 0–100% (higher = better portability)
- **Issues Found**: Severity-prioritized list with repair proposals
- **Applied Fixes**: Summary of changes made
- **Next Steps**: Recommendations for manual verification

---

## Tips

- Run this skill **before committing** major changes to your repo
- Validate portability on a **fresh VM or container** after repairs
- Update this annually as new dependencies are added
- Document **any system-level setup** separate from the xcopy-ready repo (e.g., database server installation)
