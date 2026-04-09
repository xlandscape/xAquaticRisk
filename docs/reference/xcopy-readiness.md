# XCopy Readiness

xAquaticRisk is designed to be xcopy-ready on Windows: a user should be able to clone or copy the repository and run the model without relying on host-level Python installations.

This page documents the repository check workflow for that requirement.

## Goal

The xcopy-readiness check verifies that:

1. Launchers use repository-local runtimes.
2. Required embedded runtimes are present.
3. Submodules are available and pinned to the intended release commit pointers.
4. The repository state is suitable for pre-release and post-release portability validation.

## What Is Checked

The checker script validates the following:

1. Required launcher and setup files exist (for example `__start__.bat`, `controlpanel.bat`, setup scripts).
2. `__start__.bat` starts model runtime from repository-local path (no bare `python` call).
3. Runtime executables exist:
   - `model/core/bin/python-*-amd64/python.exe`
   - `controlpanel/python/python.exe`
   - `webui/python/python.exe`
   - `analysis/python/python.exe`
4. `.gitmodules` exists and is parseable.
5. Submodule commit pointers pinned in the target ref are present.
6. Checked-out submodule SHAs match pinned SHAs from the target ref.
7. Submodule status problems are flagged (`not-initialized`, `diverged`, `conflict`, `mismatch`).

## Script Location

Run the checker from:

- `.github/skills/xcopy-readiness/assets/check-xcopy-readiness.ps1`

## Usage

From repository root:

```powershell
git fetch --all --tags
git checkout <branch-or-tag>
powershell -ExecutionPolicy Bypass -File .github/skills/xcopy-readiness/assets/check-xcopy-readiness.ps1 -TargetRef <branch-or-tag>
```

Optional runtime strictness control:

```powershell
powershell -ExecutionPolicy Bypass -File .github/skills/xcopy-readiness/assets/check-xcopy-readiness.ps1 -TargetRef <branch-or-tag> -StrictRuntime:$false
```

## Interpreting Results

The script prints a summary with `PASS`, `WARN`, and `FAIL` counts.

- Any `FAIL` means the checked state is not xcopy-ready.
- `WARN` highlights non-critical gaps that still deserve review.
- A submodule pointer matrix is printed so release reviewers can compare pinned versus checked-out SHAs.

## Typical Remediation

1. Missing runtimes:
   - run `setup_controlpanel_python.bat`
   - run `setup_webui_python.bat`
   - run `setup_analysis_python.bat`
2. Submodule mismatch or not initialized:
   - `git submodule sync --recursive`
   - `git submodule update --init --recursive`
3. If mismatches remain, intentionally re-pin submodule pointers and commit them on purpose as part of release preparation.

## Recommended Release Workflow

1. Pre-release check on the release branch/tag candidate.
2. Create distribution (zip or folder copy) including runtime folders.
3. Post-release check on a clean Windows machine after clone/copy.
4. Store checker output with release notes or QA artifacts.

## Related

- [Getting Started](../getstarted/getstarted.md)
- [Control Panel](controlpanel.md)
- [Troubleshooting](troubleshooting.md)
