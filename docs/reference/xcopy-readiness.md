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

## Isolated Windows Testing For Training Preparation

When preparing training sessions, it is useful to test xAquaticRisk on the same host as if a trainee starts from a fresh machine.

### Option A: Windows Sandbox (recommended)

Windows Sandbox gives a disposable Windows session: close it and all changes are removed.

1. Enable `Windows Sandbox` in Windows Features (requires admin rights and reboot).
2. Create a `.wsb` file that maps your local xAquaticRisk folder.
3. Launch the `.wsb` file and execute the same steps trainees will execute (clone or unzip, then start).

Example configuration:

```xml
<Configuration>
   <MappedFolders>
      <MappedFolder>
         <HostFolder>C:\Path\To\xAquaticRisk</HostFolder>
         <SandboxFolder>C:\Training\xAquaticRisk</SandboxFolder>
         <ReadOnly>false</ReadOnly>
      </MappedFolder>
   </MappedFolders>
   <LogonCommand>
      <Command>cmd.exe /c "cd /d C:\Training\xAquaticRisk && __start__.bat"</Command>
   </LogonCommand>
</Configuration>
```

Replace `HostFolder` with your actual local clone or unzip location before launching the sandbox profile.

Where to store the `.wsb` file:

1. Personal QA use: store it in any local folder (for example `C:\SandboxProfiles\`) outside the repository.
2. Team-shared reproducible workflow: keep a template in the repository so others can run the same check.

### Option B: Hyper-V VM with Checkpoints

Use a dedicated VM when you want a persistent baseline and richer environment simulation.

1. Create a clean Windows VM image.
2. Install only baseline tooling expected for trainees.
3. Take a checkpoint.
4. For each rehearsal, restore checkpoint, run the training install/start flow, record findings.

### Option C: Separate Local Windows User

This is the lightest approach, but only partially isolated because system-wide software is still shared. Use it for quick profile-level checks, not as full fresh-machine proof.

## Practical Repeatable Checklist

1. Start from a clean state (Sandbox session or restored VM checkpoint).
2. Obtain xAquaticRisk exactly as trainees will (clone from GitHub or unzip distribution package).
3. Run the same setup/start sequence shown in the course material.
4. Verify model start, control panel start, and a minimal test run.
5. Capture failures and remediation notes immediately after each rehearsal.
6. Re-run `.github/skills/xcopy-readiness/assets/check-xcopy-readiness.ps1` on the release ref before the training date.

## One-Click Reset Script (PowerShell)

To speed up regular rehearsals, use the reset helper script:

- `scripts/training/Reset-TrainingRehearsal.ps1`

Default behavior:

1. Uses GitHub source `https://github.com/xlandscape/xAquaticRisk.git`.
2. Uses clean-source branch `feature/usability`.
3. Maintains a staging layout under `C:\TrainingPrep`.
4. Refreshes or creates a clean clone in `C:\TrainingPrep\01-clean-clone\xAquaticRisk`.
5. Rebuilds run input in `C:\TrainingPrep\02-run-input\xAquaticRisk`.
6. Optionally creates a zip in `C:\TrainingPrep\03-packages`.

Run with defaults:

```powershell
.\Reset-TrainingRehearsal.ps1
```

If execution policy blocks local scripts, use:

```powershell
powershell -ExecutionPolicy Bypass -File .\Reset-TrainingRehearsal.ps1
```

Run and launch your sandbox profile automatically:

```powershell
.\Reset-TrainingRehearsal.ps1 -LaunchSandbox -SandboxProfile "C:\TrainingPrep\04-profiles\xAquaticRisk-sandbox.wsb"
```

Run without creating a zip artifact:

```powershell
.\Reset-TrainingRehearsal.ps1 -SkipZip
```

## Related

- [Getting Started](../getstarted/getstarted.md)
- [Control Panel](controlpanel.md)
- [Troubleshooting](troubleshooting.md)
