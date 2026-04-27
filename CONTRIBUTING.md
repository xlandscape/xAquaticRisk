# Contributing

Contributions to the project are welcome. Please contact the authors. These contribution notes refer to the general
Landscape Model contribution guidelines and were written on 2026-02-04.

## Issues

Whenever you identify a bug or like to suggest an enhancement to the code, please file an issue. Stick to the following
checklist when submitting an issue.

1. Check whether a similar issue already exists. If so, please comment on the existing issue instead of creating a new
   one.
2. Identify the most appropriate repository for the issue, that is the repository nearest to the presumed code changes.
   For instance, if you found a bug in the base classes, please submit it in the Landscape Model core repository or an
   enhancement of a specific component in the component's repository. If you are not sure about the most appropriate
   repository, please use the top level repository of the according model variant, e.g., xAquaticRisk or
   xOffFieldSoilRisk.
3. Give the issue a strong, self-explanatory title.
4. Provide any information in the issue's description that is needed to understand the purpose of the issue and allows
   working on it. This includes a rationale, excerpts of log files, screenshots etc. Be concise.
5. If you are going to work on the issue yourself, assign the issue to you. In any other case, assign the issue to the
   repository owner.
6. Assign one of the following labels to the issue: `bug` if the issue is related to exceptions or erroneous runtime
   behavior, `documentation` for issues related with the documentation of functionality and code, `enhancement` for
   code improvements that add to the usability or performance, or `suggestion` for ideas that should be considered part
   of the backlog or require further discussion.
7. In the case of a `bug` issue, you may additionally assign the `highPriority` label if the bug is breaking the normal
   usage of the application.
8. Whenever you start working on an issue, you should assign the label `Work in progress` to it to communicate that the
   issue is actively addressed. Likewise, if you finish work on an issue, the label `work in progress` should be
   removed before closing the issue.
9. If an issue requires additional input or is delayed for future work actions, you can mark it with the label
   `Waiting`.
10. In case another issue is blocking the work on an issue or if two issues are otherwise related, please actively link
    the issues using the according GitLab options.

## Submodule Development Strategy

xAquaticRisk is composed of multiple submodules (components, core, and scenarios) that are maintained in separate
repositories. All these repositories maintain a *master* branch representing release-ready code. To prevent
developmental changes in xAquaticRisk from affecting the release branches of these submodules (which other xLandscape
models depend on), feature development follows a specific branching pattern:

### When to Create Feature Branches in Submodules

Create a feature branch in a submodule when your xAquaticRisk development affects it. This includes:

- **Parameterization changes:** Updates to `template.xrun` or `template.yaml` in the `parameterisation/` folder
- **Code changes:** Modifications to submodule logic triggered by xAquaticRisk requirements
- **Configuration changes:** Updates affecting component initialization or behavior
- **Scenario updates:** Data or configuration changes in scenario submodules

### Feature Branch Naming Convention

Use the naming pattern: **`feature/xAquaticRisk-{descriptive-name}`** or **`dev/xAquaticRisk-{issue-id}`**

Examples:
- `feature/xAquaticRisk-parameter-updates-2.87`
- `dev/xAquaticRisk-issue-456`
- `feature/xAquaticRisk-spray-drift-calibration`

This naming makes it clear that the branch is for xAquaticRisk integration and facilitates cleanup after release.

### Workflow

1. **During development:** xAquaticRisk's `.gitmodules` points to feature branches in affected submodules
2. **Creating branches:** Use the helper script (see Tools section below) to create feature branches efficiently:
   ```powershell
   .\scripts\submodule-workflow-helper.ps1 -Action NewFeature -SubmodulePath "model/variant/CmfContinuous" -FeatureName "parameter-updates-2.87"
   ```
3. **At release time:** Feature branches are merged to submodule master branches (manual, coordinated process)
4. **After integration:** xAquaticRisk `.gitmodules` is updated to point to master branches (now release-ready)

### Tools for Submodule Management

A helper script (`scripts/submodule-workflow-helper.ps1`) is available to simplify submodule workflows:

- **NewFeature:** Create and check out a feature branch in a submodule, auto-update xAquaticRisk `.gitmodules`
- **Validate:** Scan all submodules to verify they're on development branches (not master during development)
- **GetStatus:** Display current status of all submodules
- **ResetToRelease:** Pin all submodules back to master (used during release preparation)

**Usage examples:**
```powershell
# Create a feature branch in a component
.\scripts\submodule-workflow-helper.ps1 -Action NewFeature -SubmodulePath "model/variant/CmfContinuous" -FeatureName "parameter-updates"

# Create a feature branch in a scenario
.\scripts\submodule-workflow-helper.ps1 -Action NewFeature -SubmodulePath "scenario/oudebeek-beek7-tdi" -FeatureName "data-update"

# Validate all submodules are on correct branches
.\scripts\submodule-workflow-helper.ps1 -Action Validate

# View status of all submodules
.\scripts\submodule-workflow-helper.ps1 -Action GetStatus

# Prepare for release (pin all submodules to master)
.\scripts\submodule-workflow-helper.ps1 -Action ResetToRelease
```

---

## Merge requests

The Landscape Model repositories adapt the GitFlow approach for versioning (see
[A successful Git branching model](https://nvie.com/posts/a-successful-git-branching-model/) for a detailed
explanation). Briefly, there is a *master* branch that always contains the latest tested stable version (tagged with a
version number). This branch is protected and only the repository owner can push to it. To contribute to the repository,
please adhere to the following steps:

1. Locally, create a new branch starting at the newest commit of the master branch. You should do all your coding and
   modifications in this feature branch.
2. Develop in your local feature branch until you reach a state that you like to submit. This may encompass multiple
   local commits.
3. Use concise and meaningful commit messages that help to track changes.
4. If the repository gets updated during your development, please merge the new master commit into your feature branch
   and resolve merge conflicts, if any occur.
5. Make sure that all your changes are reflected in the repositories documentation and modify the documentation if
   needed.
6. Do not assign new version numbers. This will be done during the next release of the repository.
7. Test your code extensively!
8. If your code works satisfyingly, push your local feature branch to the GitLab repository.
9. Create then a merge request for your pushed branch (= source branch) into the master branch.
10. Assign the owner of the repository to the merge request.
11. Your changes will be reviewed by the owner of the repository and the merge will be performed, or you may be asked
    for additional modifications of your code.

## Components

If you are requesting a merge containing component code, please make sure that the following applies:

- [ ] The commit that is requested to branch is based on the most recent commit on the master branch.
- [ ] The repository can be cloned from GitLab.
- [ ] The component runs successfully without any errors using the most recent model version.
- [ ] You haven't reverted any changes made by other contributors unless there is a good reason to do so.
- [ ] You haven't introduced inputs to the component that are not needed for calculations.

## Model variant

If you are requesting a merge relating to a model variant, please make sure that the following applies:

- [ ] The commit that is requested to branch is based on the most recent commit on the master branch.
- [ ] The entire model, including all submodules, can be cloned from GitLab.
- [ ] The model runs successfully without any errors using the most recent model version.
- [ ] You haven't reverted any changes made by other contributors unless there is a good reason to do so.

### Pre-Release Checklist for Model Variants

When preparing a release, coordinate submodule updates carefully to prevent breaking downstream xLandscape models.
Use this checklist **before** creating a release merge request:

- [ ] **Validate submodule branches:** Run `.\scripts\submodule-workflow-helper.ps1 -Action Validate` to ensure all
  submodules are on development branches (not master during dev)
- [ ] **Parameterization review:** All parameterization changes in `parameterisation/` are tested and approved
- [ ] **Version update:** Update version in `model.json` and add a corresponding entry to `CHANGELOG.md` 
- [ ] **Submodule coordination:** For each submodule with a feature branch:
  - Ensure the feature branch is approved and ready
  - Plan the merge to submodule master *before* updating xAquaticRisk `.gitmodules`
  - Coordinate with component maintainers if needed
- [ ] **Reset to release:** Run `.\scripts\submodule-workflow-helper.ps1 -Action ResetToRelease` to pin all
  submodules to master (release-ready commits)
- [ ] **Verify `.gitmodules`:** Manually verify that `.gitmodules` points to master commits in all submodules
  (or to release-pinned versions if specified)
- [ ] **Create release PR:** Merge request should include version bump and `.gitmodules` updates
- [ ] **CI validation passes:** Automated validation confirms branch correctness and version consistency
- [ ] **Communicate:** After release merge, notify other xLandscape model maintainers that component dependencies
  have been updated if submodules were affected

## Scenario

If you are requesting a merge relating to a scenario, please make sure that the following applies:

- [ ] The commit that is requested to branch is based on the most recent commit on the master branch.
- [ ] The repository can be cloned from GitLab.
- [ ] The component runs successfully without any errors using the most recent model version.
- [ ] You haven't reverted any changes made by other contributors unless there is a good reason to do so.
- [ ] Added data is required, cannot be retrieved by a component and is not redundant.

