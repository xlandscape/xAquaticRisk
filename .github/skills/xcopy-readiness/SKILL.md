---
name: xcopy-readiness
description: "Use when: evaluating xAquaticRisk xcopy-readiness on a specific branch before or after release, verifying portable runtimes, checking that launchers avoid system Python dependencies, and validating submodule commit pointers for the release state."
---

# xAquaticRisk XCopy Readiness

Evaluate whether xAquaticRisk can be cloned or copied to a Windows machine and executed without relying on host-level Python installations or environment state.

## When To Use

Use this skill for:

1. Pre-release readiness checks.
2. Post-release verification checks.
3. Branch-specific portability checks.
4. Validating that submodule pointers match the intended release state.

## Inputs

Collect these inputs before running the check:

1. `branch_or_ref` (required): branch/tag/commit to evaluate (for example `master`, `develop`, `release/2026-04`).
2. `mode` (optional): `pre-release` or `post-release`.
3. `strict_runtime` (optional): `true` by default. If true, missing component-local runtimes fail the check.

## Procedure

1. Ensure you are on the requested reference:
   - `git fetch --all --tags`
   - `git checkout <branch_or_ref>`
2. Run the repository checker:
   - `powershell -ExecutionPolicy Bypass -File .github/skills/xcopy-readiness/assets/check-xcopy-readiness.ps1 -TargetRef <branch_or_ref>`
3. Interpret results:
   - Any `FAIL` means not xcopy-ready.
   - `WARN` means potentially acceptable but should be reviewed before release.
4. For release sign-off, require all critical checks to be `PASS`.

## Required Checks

The checker must validate all of the following:

1. Local launcher files exist and use repo-local execution paths.
2. The model starter (`__start__.bat`) does not rely on global Python.
3. Component-local runtimes are present:
   - `model/core/bin/python-*-amd64/python.exe`
   - `controlpanel/python/python.exe`
   - `analysis/python/python.exe`
4. Git submodule definitions exist and are parseable from `.gitmodules`.
5. For each declared submodule, the currently checked-out commit equals the commit pinned by `TargetRef`.
6. Submodules are initialized (no missing checkout state).

## Output Contract

Always return:

1. `status`: `pass` or `fail`.
2. Branch/ref evaluated.
3. Count of `PASS`, `WARN`, `FAIL` checks.
4. Submodule pointer summary per submodule path:
   - pinned SHA (from `TargetRef` tree)
   - checked-out SHA
   - state (`ok`, `not-initialized`, `diverged`, `conflict`)
5. Concrete remediation actions for each `FAIL`.

## Remediation Guidance

Use these remediations when checks fail:

1. Missing runtimes:
   - Run `setup_controlpanel_python.bat`, `setup_analysis_python.bat`.
   - Ensure runtime folders are included before packaging/copying.
2. Global Python usage in launchers:
   - Replace with repo-local executable paths using `%~dp0` rooted paths.
3. Submodule mismatch:
   - `git submodule sync --recursive`
   - `git submodule update --init --recursive`
   - If still mismatched, update pinned submodule commit intentionally and review.

## Notes

1. This skill is repository-scoped and intended for team-shared release checks.
2. Run it both before release creation and after cloning/copying to a clean Windows environment.
