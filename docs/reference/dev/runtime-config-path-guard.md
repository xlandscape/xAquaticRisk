# Runtime Config Path Guard

The runtime config path guard is a small CI check that prevents machine-specific absolute local paths from entering runtime configuration files.

This protects xcopy portability: copied or zipped repositories should run on another machine without references to developer-local folders.

## Purpose

The check fails CI when runtime config files contain patterns such as:

1. `C:\LocalWork\xAquaticRisk`
2. `C:\Users\...`

## Files

1. Guard script: `scripts/check_runtime_config_paths.py`
2. GitHub workflow: `.github/workflows/runtime-config-path-guard.yml`

## Scope

The scanner currently checks only runtime-relevant config surfaces:

1. `model/**/*.xml`
2. `model/**/*.xrun`
3. `model/**/*.yaml`
4. `parameterisation/**/*.xrun`
5. `parameterisation/**/*.yaml`

This keeps the signal high and avoids false positives in generated output artifacts.

## CI Behavior

The workflow runs on:

1. Pull requests that change any scanned runtime config files.
2. Pushes to `master` and `feature/**` that change scanned files.
3. Manual dispatch.

If a forbidden path is found, the script emits GitHub Actions error annotations and exits with code `1`, causing the job to fail.

## Local Usage

Run from repository root:

```powershell
python scripts/check_runtime_config_paths.py
```

Success output:

```text
No forbidden absolute local paths detected in runtime config files.
```

Failure output includes file and line diagnostics, for example:

```text
::error file=model/variant/experiment.xml,line=135::developer-root path (C:\LocalWork\xAquaticRisk). Offending text: <Script>C:\LocalWork\xAquaticRisk\analysis\profiling.R</Script>
```

## Remediation

Replace machine-specific absolute paths with repository-relative placeholders already used by the model configuration templates.

Example fix:

1. Replace `C:\LocalWork\xAquaticRisk\analysis\profiling.R`
2. With `$(_X3DIR_)/../../analysis/profiling.R`

## Related

1. [XCopy Readiness](../xcopy-readiness.md)
2. [Setup Scripts](../setup-scripts.md)
3. [Troubleshooting](../troubleshooting.md)
