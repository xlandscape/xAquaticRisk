# xAquaticRisk Parameterisation

This page provides a comprehensive reference for parameterising the xAquaticRisk landscape model. It describes the two supported parameterisation formats (`.xrun` and `.yaml`), all available parameters organised by section, and practical guidance for setting up simulation runs.

For a quick-start walkthrough, see [Getting Started](../getstarted/getstarted.md). For the regulatory context that motivates these parameters, see [Regulatory Framework](regulatory-framework.md).

---

## Parameterisation vs Configuration

xAquaticRisk distinguishes two levels of customisation:

| Level | Purpose | Files | Target user |
|---|---|---|---|
| **Parameterisation** | Simulation-specific settings (scenario, substance, species, etc.) | `.xrun` or `.yaml` at the model root | Standard model user |
| **Configuration** | Fundamental model behaviour (active components, data flow) | `model/variant/mc.xml`, `model/variant/experiment.xml` | Model developer |

This page covers **parameterisation** only. Configuration changes are typically not required for standard use.

---

## File Formats

xAquaticRisk accepts two equivalent parameterisation formats. Both contain the same parameters and produce identical simulation results.

### .xrun (XML)

The `.xrun` format is an XML document with a namespace `urn:xAquaticRisk` and inline documentation comments for every parameter. It is validated against the XSD schema at `model/variant/parameters.xsd`.

```xml
<?xml version="1.0" encoding="utf-8"?>
<Parameters
        xmlns="urn:xAquaticRisk"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="urn:xAquaticRisk model/variant/parameters.xsd"
>
  <Control>
    <!--
    Parameter     :  ExperimentID
    Description   :  A unique identifier of a simulation run
    -->
    <ExperimentID>Test_Run_aqRisk</ExperimentID>
    <!-- ... -->
  </Control>
  <!-- ... further sections ... -->
</Parameters>
```

The main template is **`template.xrun`** at the model root.

### .yaml

The `.yaml` format provides the same parameters in a more human-readable structure with inline comments. Sections are top-level YAML keys; parameters are nested sub-keys.

```yaml
Control:
  # A unique identifier of a simulation run
  ExperimentID: Test_Run_aqRisk
  # ...

Scenario:
  LandscapeScenario: scenario/oudebeek-beek7-tdi
  SimulationStart: "2015-05-01"
  SimulationEnd: "2015-05-07"
# ...
```

The main template is **`template.yaml`** at the model root.

!!! note "YAML quoting"
    Dates (e.g., `"2015-05-01"`), time windows (e.g., `"05-02 to 05-02"`), and scientific notation (e.g., `"1.10E-06"`) must be quoted in YAML to prevent misinterpretation by YAML parsers. Booleans are written lowercase (`true`, `false`).

### Converting between formats

A conversion utility `convert_xrun.py` is provided at the model root. It converts legacy `.xrun` files to the current format and simultaneously generates an equivalent `.yaml` file. See the [xrun Converter](../xrun-converter.md) documentation for details.

---

## Section Overview

Parameters are grouped into nine sections, listed here in canonical order:

| # | Section | Purpose |
|---|---------|---------|
| 1 | [Control](#control) | Run identification, parallelisation, logging and profiling |
| 2 | [Scenario](#scenario) | Landscape scenario selection and simulation period |
| 3 | [PppUse](#pppuse) | Application rate and time windows |
| 4 | [Mitigation](#mitigation) | In-crop buffer and drift-reducing technology |
| 5 | [Exposure](#exposure) | Rautmann drift class or predefined deposition file |
| 6 | [EnvironmentalFate](#environmentalfate) | Toggle fate models; substance physico-chemical properties |
| 7 | [Effects](#effects) | Toggle GUTS effects; warm-up/recovery; species GUTS parameters |
| 8 | [Settings](#settings) | Export options |
| 9 | [Analysis](#analysis) | Reporting reaches and PEC display settings |

---

## Control

Parameters that identify the simulation run, control parallelisation, and manage logging.

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `ExperimentID` | Unique identifier of the simulation run. Results are written to `run/<ExperimentID>/`. | Any valid file-system characters | `Test_Run_aqRisk` |
| `ParentRun` | File pattern for parent runs (experimental feature). | Empty or a glob yielding X3df files | *(empty)* |
| `NumberMC` | Number of Monte Carlo runs. | Positive integer | `1` |
| `NumberParallelProcesses` | Number of Monte Carlo runs conducted simultaneously. | Positive integer | `2` |
| `CascadeToxswaWorkers` | Number of processes spawned by CascadeToxswa. Only in effect if `RunCascadeToxswa` is `true`. | Positive integer | `12` |
| `DeleteComponentProcessingFolders` | Delete component processing folders after the run finishes. | `true` / `false` | `false` |
| `VerboseLogging` | Enable verbose logging output. | `true` / `false` | `false` |
| `EnableProfiling` | Enable performance profiling of the simulation. | `true` / `false` | `false` |
| `ProfilingWaitingTime` | Interval in seconds between profiling rounds. Only in effect if profiling is enabled. | Positive integer | `5` |

!!! tip "Parallelisation"
    For optimal hardware use, apply: `min(NumberMC, NumberParallelProcesses) × CascadeToxswaWorkers ≤ available CPU cores`.

---

## Scenario

Selects the landscape scenario and defines the simulation period.

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `LandscapeScenario` | Path to the landscape scenario directory. | `scenario/<name>` — must match a sub-folder in `scenario/` | `scenario/oudebeek-beek7-tdi` |
| `SimulationStart` | First simulated date. | `YYYY-MM-DD` within the scenario's valid range | `2015-05-01` |
| `SimulationEnd` | Last simulated date. Must be ≥ `SimulationStart`. | `YYYY-MM-DD` within the scenario's valid range | `2015-05-07` |

Available scenarios are described in [Scenarios](../scenarios/scenario-intro.md). The valid date range depends on the input data of each scenario (see the scenario's own documentation).

---

## PppUse

Defines how the plant protection product is applied.

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `ApplicationRate` | Application rate in g/ha. Applies to all applications in a sequence. | Zero or positive number | `12.5` |
| `ApplicationTimeWindow` | Time window(s) within which applications take place. | `MM-DD to MM-DD[, MM-DD to MM-DD[, ...]]` | `05-02 to 05-02` |

!!! example "Application time window examples"
    - `04-15 to 04-15` — All fields applied on 15 April every year (deterministic).
    - `04-07 to 04-21` — Random date between 7 and 21 April per field per year.
    - `04-07 to 04-21, 05-02 to 05-16` — Two applications per year at random dates within each window.

---

## Mitigation

Risk mitigation measures applied during simulation.

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `InCropBuffer` | In-crop buffer width in meters — the strip at the field edge that is not applied. | Zero or positive number | `0` |
| `TechnologyDriftReduction` | Fraction of spray-drift deposition filtered out by drift-reducing technology. | `0` (no reduction) to `1` (full filtering) | `0` |

!!! info
    The in-crop buffer is applied geometrically to the field polygon and may result in very small fields not being treated at all.

---

## Exposure

Controls how spray-drift deposition is determined.

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `RautmannClass` | Rautmann drift class for the simulation. | `orchards.early`, `orchards.late`, or `arable` | `orchards.early` |
| `DepositionInputFile` | Path to a CSV file with predefined spray-drift depositions per reach and day (g/ha). If set, XSprayDrift is bypassed. | Valid file path or empty | *(empty)* |

!!! tip
    Select the Rautmann class that matches the crop type and application timing. See the [Glossary — Rautmann drift values](glossary.md#rautmann-drift-values) for background.

---

## EnvironmentalFate

Toggles environmental fate model components and defines substance physico-chemical properties.

### Fate model toggles

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `RunStepsRiverNetwork` | Run the StepsRiverNetwork environmental fate component. | `true` / `false` | `true` |
| `RunCascadeToxswa` | Run the CascadeToxswa (TOXSWA) environmental fate component. | `true` / `false` | `true` |

### StepsRiverNetwork precision

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `ThresholdSW` | Surface water value precision for StepsRiverNetwork. | Positive number | `0.001` |
| `ThresholdSediment` | Sediment value precision for StepsRiverNetwork. | Positive number | `0.1` |

### Substance properties

| Parameter | Description | Unit | Default |
|-----------|-------------|------|---------|
| `MolarMass` | Molar mass of the applied substance. | g/mol | `505.2` |
| `SaturatedVapourPressure` | Saturated vapour pressure at 20 °C. | Pa | `1.10E-06` |
| `MolarEnthalpyOfVaporization` | Molar enthalpy of vaporization. | kJ/mol | `95` |
| `SolubilityInWater` | Solubility in water at 20 °C. | mg/l | `0.001` |
| `MolarEnthalpyOfDissolution` | Molar enthalpy of dissolution. | kJ/mol | `27` |
| `DiffusionCoefficient` | Diffusion coefficient at 20 °C. | m²/d | `4.30E-05` |
| `DT50sw` | Half-life transformation time in water at 20 °C. | d | `1000` |
| `MolarActivationEnthalpyOfTransformationInWater` | Molar activation enthalpy for transformation in water. | kJ/mol | `65.4` |
| `DT50sed` | Half-life transformation time in sediment at 20 °C. | d | `43.9` |
| `MolarActivationEnthalpyOfTransformationInSediment` | Molar activation enthalpy for transformation in sediment. | kJ/mol | `65.4` |
| `KOC` | Organic carbon–water partition coefficient. | l/kg | `460000` |
| `ReferenceConcentrationForKOC` | Reference concentration for the KOC. | mg/l | `1` |
| `FreundlichExponentInSedimentAndSuspendedParticles` | Freundlich exponent in sediment and suspended particles. | — | `0.93` |
| `CoefficientForLinearAdsorptionOnMacrophytes` | Coefficient for linear adsorption on macrophytes. | l/kg | `0` |

!!! note
    The KOM used by some modules is automatically derived from the KOC by dividing it by 1.742.

---

## Effects

Controls GUTS effect modelling and species parameterisation for up to three species. For background on GUTS models, see [GUTS Models](../index.md#guts-models) on the main page and the [EFSA TKTD Scientific Opinion](https://doi.org/10.2903/j.efsa.2018.5377).

### General effect parameters

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `RunLGuts` | Simulate individual-level effects with GUTS for enabled environmental fate components. | `true` / `false` | `true` |
| `NumberOfWarmUpYears` | Number of years to run LPop before the first application year for stable population cycles. | Non-negative integer | `1` |
| `RecoveryPeriodYears` | Number of years to run LPop after the last application year to allow for population recovery. | Non-negative integer | `1` |

### Species parameters

For each of up to three species, the following GUTS-SD and GUTS-IT parameters are specified. The table below uses `SpeciesN` as a placeholder (replace `N` with `1`, `2`, or `3`).

| Parameter | Description | Unit | GUTS type |
|-----------|-------------|------|-----------|
| `SpeciesN` | Species name (must be valid file-system characters). | — | — |
| `SpeciesNDominantRateConstantSD` | Dominant rate constant. | 1/h | SD |
| `SpeciesNThresholdConcentrationSD` | Threshold for lethal effects. | ng/l | SD |
| `SpeciesNKillingRateSD` | Killing rate. | l/(ng·h) | SD |
| `SpeciesNDominantRateConstantIT` | Dominant rate constant. | 1/h | IT |
| `SpeciesNThresholdDistributionIT` | Median threshold for lethal effects. | ng/l | IT |
| `SpeciesNWidthOfThresholdDistributionIT` | Width of the threshold distribution. | — | IT |

### Default species parameterisation

The template provides parameters for three macroinvertebrate species:

| # | Species | SD rate const. (1/h) | SD threshold (ng/l) | SD killing rate (l/(ng·h)) | IT rate const. (1/h) | IT threshold (ng/l) | IT width |
|---|---------|---------------------|---------------------|---------------------------|---------------------|---------------------|----------|
| 1 | *Asellus aquaticus* | 0.01188 | 0.05507 | 0.002615 | 0.0005541 | 0.4429 | 1.582 |
| 2 | *Cloeon dipterum* | 0.03719 | 4.702 | 0.0009361 | 0.01416 | 36.27 | 1.648 |
| 3 | *Gammarus pulex* | 0.008858 | 0.6292 | 0.01989 | 0.002324 | 0.8206 | 3.049 |

!!! info "Species 1 and the population model"
    Species 1 should be parameterised as *Asellus aquaticus*, because the LEffectModel population implementation is designed for *Asellus aquaticus*. Parameterising a different species as Species 1 is possible but makes population model results harder to interpret: the population dynamics still simulate *Asellus aquaticus* behaviour, but with sensitivity governed by the GUTS parameters specified here.

---

## Settings

General simulation settings.

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `ExportToSqlite` | Export PECs and individual effects to a SQLite database. | `true` / `false` | `true` |

!!! tip "Monte Carlo runs"
    Always conduct multiple MC runs except for quick technical tests. Recommended: 3 for long-running simulations, 10–30 for smaller ones. MC runs are parallelised according to `NumberParallelProcesses`.

---

## Analysis

Controls the analysis and reporting output.

| Parameter | Description | Values | Default |
|-----------|-------------|--------|---------|
| `ReportingReaches` | Reaches for which detailed reports are generated. | Comma-separated reach identifiers, e.g. `r577` (no spaces) | `r577` |
| `PecDisplayedTime` | The date/time for which PEC values are plotted per environmental fate component. | `YYYY-MM-DD HH:MM` | `2015-05-02 12:00` |

!!! note
    Reporting is only in effect if at least one environmental fate module is enabled and `RunLGuts` is `true`.

---

## Complete Example — .xrun

The following is the complete content of `template.tmp.xrun` (the compact template without inline comments):

```xml
<?xml version='1.0' encoding='utf-8'?>
<Parameters xmlns="urn:xAquaticRisk">
  <Control>
    <ExperimentID>Test_Run_aqRisk</ExperimentID>
    <ParentRun />
    <NumberMC>1</NumberMC>
    <NumberParallelProcesses>2</NumberParallelProcesses>
    <CascadeToxswaWorkers>12</CascadeToxswaWorkers>
    <DeleteComponentProcessingFolders>false</DeleteComponentProcessingFolders>
    <VerboseLogging>false</VerboseLogging>
    <EnableProfiling>false</EnableProfiling>
    <ProfilingWaitingTime>5</ProfilingWaitingTime>
  </Control>
  <Scenario>
    <LandscapeScenario>scenario/oudebeek-beek7-tdi</LandscapeScenario>
    <SimulationStart>2015-05-01</SimulationStart>
    <SimulationEnd>2015-05-07</SimulationEnd>
  </Scenario>
  <PppUse>
    <ApplicationRate>12.5</ApplicationRate>
    <ApplicationTimeWindow>05-02 to 05-02</ApplicationTimeWindow>
  </PppUse>
  <Mitigation>
    <InCropBuffer>0</InCropBuffer>
    <TechnologyDriftReduction>0</TechnologyDriftReduction>
  </Mitigation>
  <Exposure>
    <RautmannClass>orchards.early</RautmannClass>
    <DepositionInputFile />
  </Exposure>
  <EnvironmentalFate>
    <RunStepsRiverNetwork>true</RunStepsRiverNetwork>
    <RunCascadeToxswa>true</RunCascadeToxswa>
    <ThresholdSW>0.001</ThresholdSW>
    <ThresholdSediment>0.1</ThresholdSediment>
    <MolarMass>505.2</MolarMass>
    <SaturatedVapourPressure>1.10E-06</SaturatedVapourPressure>
    <MolarEnthalpyOfVaporization>95</MolarEnthalpyOfVaporization>
    <SolubilityInWater>0.001</SolubilityInWater>
    <MolarEnthalpyOfDissolution>27</MolarEnthalpyOfDissolution>
    <DiffusionCoefficient>4.30E-05</DiffusionCoefficient>
    <DT50sw>1000</DT50sw>
    <MolarActivationEnthalpyOfTransformationInWater>65.4</MolarActivationEnthalpyOfTransformationInWater>
    <DT50sed>43.9</DT50sed>
    <MolarActivationEnthalpyOfTransformationInSediment>65.4</MolarActivationEnthalpyOfTransformationInSediment>
    <KOC>460000</KOC>
    <ReferenceConcentrationForKOC>1</ReferenceConcentrationForKOC>
    <FreundlichExponentInSedimentAndSuspendedParticles>0.93</FreundlichExponentInSedimentAndSuspendedParticles>
    <CoefficientForLinearAdsorptionOnMacrophytes>0</CoefficientForLinearAdsorptionOnMacrophytes>
  </EnvironmentalFate>
  <Effects>
    <RunLGuts>true</RunLGuts>
    <NumberOfWarmUpYears>1</NumberOfWarmUpYears>
    <RecoveryPeriodYears>1</RecoveryPeriodYears>
    <Species1>Asellus aquaticus</Species1>
    <Species1DominantRateConstantSD>0.01188</Species1DominantRateConstantSD>
    <Species1ThresholdConcentrationSD>0.05507</Species1ThresholdConcentrationSD>
    <Species1KillingRateSD>0.002615</Species1KillingRateSD>
    <Species1DominantRateConstantIT>0.0005541</Species1DominantRateConstantIT>
    <Species1ThresholdDistributionIT>0.4429</Species1ThresholdDistributionIT>
    <Species1WidthOfThresholdDistributionIT>1.582</Species1WidthOfThresholdDistributionIT>
    <Species2>Cloeon dipterum</Species2>
    <Species2DominantRateConstantSD>0.03719</Species2DominantRateConstantSD>
    <Species2ThresholdConcentrationSD>4.702</Species2ThresholdConcentrationSD>
    <Species2KillingRateSD>0.0009361</Species2KillingRateSD>
    <Species2DominantRateConstantIT>0.01416</Species2DominantRateConstantIT>
    <Species2ThresholdDistributionIT>36.27</Species2ThresholdDistributionIT>
    <Species2WidthOfThresholdDistributionIT>1.648</Species2WidthOfThresholdDistributionIT>
    <Species3>Gammarus pulex</Species3>
    <Species3DominantRateConstantSD>0.008858</Species3DominantRateConstantSD>
    <Species3ThresholdConcentrationSD>0.6292</Species3ThresholdConcentrationSD>
    <Species3KillingRateSD>0.01989</Species3KillingRateSD>
    <Species3DominantRateConstantIT>0.002324</Species3DominantRateConstantIT>
    <Species3ThresholdDistributionIT>0.8206</Species3ThresholdDistributionIT>
    <Species3WidthOfThresholdDistributionIT>3.049</Species3WidthOfThresholdDistributionIT>
  </Effects>
  <Settings>
    <ExportToSqlite>true</ExportToSqlite>
  </Settings>
  <Analysis>
    <ReportingReaches>r577</ReportingReaches>
    <PecDisplayedTime>2015-05-02 12:00</PecDisplayedTime>
  </Analysis>
</Parameters>
```

---

## Complete Example — .yaml

The following is the complete content of `template.yaml`:

```yaml
Control:
  ExperimentID: Test_Run_aqRisk
  ParentRun:
  NumberMC: 1
  NumberParallelProcesses: 2
  CascadeToxswaWorkers: 12
  DeleteComponentProcessingFolders: false
  VerboseLogging: false
  EnableProfiling: false
  ProfilingWaitingTime: 5

Scenario:
  LandscapeScenario: scenario/oudebeek-beek7-tdi
  SimulationStart: "2015-05-01"
  SimulationEnd: "2015-05-07"

PppUse:
  ApplicationRate: 12.5
  ApplicationTimeWindow: "05-02 to 05-02"

Mitigation:
  InCropBuffer: 0
  TechnologyDriftReduction: 0

Exposure:
  RautmannClass: orchards.early
  DepositionInputFile:

EnvironmentalFate:
  RunStepsRiverNetwork: true
  RunCascadeToxswa: true
  ThresholdSW: 0.001
  ThresholdSediment: 0.1
  MolarMass: 505.2
  SaturatedVapourPressure: "1.10E-06"
  MolarEnthalpyOfVaporization: 95
  SolubilityInWater: 0.001
  MolarEnthalpyOfDissolution: 27
  DiffusionCoefficient: "4.30E-05"
  DT50sw: 1000
  MolarActivationEnthalpyOfTransformationInWater: 65.4
  DT50sed: 43.9
  MolarActivationEnthalpyOfTransformationInSediment: 65.4
  KOC: 460000
  ReferenceConcentrationForKOC: 1
  FreundlichExponentInSedimentAndSuspendedParticles: 0.93
  CoefficientForLinearAdsorptionOnMacrophytes: 0

Effects:
  RunLGuts: true
  NumberOfWarmUpYears: 1
  RecoveryPeriodYears: 1
  Species1: Asellus aquaticus
  Species1DominantRateConstantSD: 0.01188
  Species1ThresholdConcentrationSD: 0.05507
  Species1KillingRateSD: 0.002615
  Species1DominantRateConstantIT: 0.0005541
  Species1ThresholdDistributionIT: 0.4429
  Species1WidthOfThresholdDistributionIT: 1.582
  Species2: Cloeon dipterum
  Species2DominantRateConstantSD: 0.03719
  Species2ThresholdConcentrationSD: 4.702
  Species2KillingRateSD: 0.0009361
  Species2DominantRateConstantIT: 0.01416
  Species2ThresholdDistributionIT: 36.27
  Species2WidthOfThresholdDistributionIT: 1.648
  Species3: Gammarus pulex
  Species3DominantRateConstantSD: 0.008858
  Species3ThresholdConcentrationSD: 0.6292
  Species3KillingRateSD: 0.01989
  Species3DominantRateConstantIT: 0.002324
  Species3ThresholdDistributionIT: 0.8206
  Species3WidthOfThresholdDistributionIT: 3.049

Settings:
  ExportToSqlite: true

Analysis:
  ReportingReaches: r577
  PecDisplayedTime: "2015-05-02 12:00"
```

---

## Migrating from Older Versions

If you have parameterisation files from older versions of xAquaticRisk, use `convert_xrun.py` to bring them up to date. The converter handles the following legacy changes automatically:

| Old format | New format |
|---|---|
| `<SimulationInfo>` section | `<Control>` section |
| `<SimID>` parameter | `<ExperimentID>` |
| `<DeleteFoldersAtFinish>` | `<DeleteComponentProcessingFolders>` |
| `<Project>` | `<LandscapeScenario>` |
| `<CropStage>` with values `early`/`late` | `<RautmannClass>` with `orchards.early`/`orchards.late` |
| `<ReachSelection>` parameter | Removed (effects now always run for all reaches) |
| `<Control>` at the bottom of the file | `<Control>` as the first section |

See the [xrun Converter](../xrun-converter.md) page for full usage instructions and batch conversion examples.

---

## WebUI

As an alternative to editing XML or YAML files manually, xAquaticRisk provides a **web-based user interface**. Start it via `webui.bat` at the model root. The WebUI exposes all parameters from this page in a form-based interface, generates `.xrun` files, and can launch simulation runs directly. See [Getting Started — WebUI](../getstarted/getstarted.md#running-xaquaticrisk) for details.
