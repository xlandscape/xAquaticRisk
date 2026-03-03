# Troubleshooting

This page covers common error messages, what they mean, and how to resolve them.

## FileExistsError

``` { .yaml .no-copy }
FileExistsError: [WinError 183] Cannot create a file when that file already exists:
'C:\\xAquaticRisk\\run\\Test_Run_aqRisk'
```

**Explanation**:

A folder with the same name as `ExperimentID` already exists in the `\run\` folder.

**Possible solutions**:

- Delete or move the existing folder, then re-run the simulation.
- Change the `ExperimentID` value in your `.xrun` file to one that does not already exist in the `\run\` folder.

## FileNotFoundError

``` { .yaml .no-copy}
FileNotFoundError: [Errno 2] No such file or directory: 'C:\\xAquaticRisk\\run\\...'
```

**Explanation**:

No `run` folder exists in the xAquaticRisk directory. This usually occurs the first time the repository is cloned from GitHub.

**Possible solutions**:

- Create a new folder named `run` in the root directory of xAquaticRisk. It should be at the same level as the `model` and `scenario` folders.

## Scenario Not Found

``` { .yaml .no-copy }
FileNotFoundError: [Errno 2] No such file or directory: '...\\scenario\\<project name>\\scenario.xproject'
```

**Explanation**:

The scenario specified in the `<LandscapeScenario>` parameter of the `.xrun` file does not exist or the scenario folder is incomplete.

**Possible solutions**:

- Check that the `<LandscapeScenario>` value in the `.xrun` file matches one of the sub-folders in `xAquaticRisk\scenario\`.
- Verify that the scenario folder contains a `scenario.xproject` file and all required data sub-folders (`geo/`, `hydro/`, `weather/`, `stream_com/`).

## Simulation Date Out of Range

**Explanation**:

The `<SimulationStart>` or `<SimulationEnd>` dates in the `.xrun` file are outside the range supported by the chosen scenario's hydrological and weather data.

**Possible solutions**:

- Consult the scenario README for the valid date range of the available time series data.
- Adjust `<SimulationStart>` and `<SimulationEnd>` to fall within the valid range.

## CascadeToxswa Errors

``` { .yaml .no-copy }
Error in CascadeToxswa: ...
```

**Explanation**:

CascadeToxswa may fail if substance properties are outside physically meaningful ranges, if hydrological data is incomplete, or if the number of worker processes is too high for available system resources.

**Possible solutions**:

- Check that all substance properties in the `<EnvironmentalFate>` section of the `.xrun` file are within reasonable ranges (e.g., DT50 > 0, KOC > 0, molar mass > 0).
- Reduce the `<CascadeToxswaWorkers>` parameter if the system runs out of memory.
- Ensure that `<RunCascadeToxswa>` is set to `true` only if CascadeToxswa is needed.

## Out of Memory / System Resources

**Explanation**:

Long simulation runs with many Monte Carlo iterations and/or many parallel processes can exhaust system memory.

**Possible solutions**:

- Reduce `<NumberParallelProcesses>` and/or `<CascadeToxswaWorkers>`.
- Apply the rule of thumb: `min(NumberMC, NumberParallelProcesses) * CascadeToxswaWorkers <= available CPU cores`.
- Set `<DeleteComponentProcessingFolders>` to `true` to free disk space during the run.
- Run fewer Monte Carlo iterations (`<NumberMC>`) for initial testing.

## HDFView Error

In rare cases, opening large datasets from the HDF5 output file (`arr.dat`) may cause HDFView to crash. Use the Jupyter notebooks in the `analysis` folder to extract and analyse specific data instead.

## GUTS Parameters

**Explanation**:

If GUTS-SD or GUTS-IT parameters are incorrectly specified (e.g., negative values, zero killing rate), the LEffectModel component may produce unexpected results or errors.

**Possible solutions**:

- Verify that all GUTS parameters are positive and physically meaningful.
- Ensure that the dominant rate constant, threshold concentration/distribution, and killing rate/width are consistent with published calibration results for the species.

## No Effects Calculated

**Explanation**:

If `<RunLGuts>` is set to `true` but no effects appear in the output, it may be because:

- No environmental fate component is enabled (`<RunStepsRiverNetwork>` and `<RunCascadeToxswa>` are both `false`).
- The application rate or exposure is too low to trigger effects given the species' GUTS parameters.

**Possible solutions**:

- Enable at least one environmental fate component.
- Check the PEC output to confirm that exposure concentrations are above the GUTS threshold values.

## General Tips

- **Check log files**: After a simulation, log files are written to `\run\<ExperimentID>\log\`. These contain detailed information about each component's execution .
- **Start small**: For initial testing, use short simulation periods (a few days) and a single Monte Carlo run (`<NumberMC>1</NumberMC>`).
- **Validate incrementally**: Enable components one at a time (e.g., first run with only StepsRiverNetwork, then add CascadeToxswa, then add LGuts) to isolate issues.
