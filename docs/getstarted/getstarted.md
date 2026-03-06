# Intro

## Get xAquaticRisk

### Option 1: Clone from GitHub

If you just want to inspect or try xAquaticRisk, the easiest way is to get the code directly from GitHub. Below are short, practical steps for a scientific modeller who knows basic IT but is not a Git expert. Choose either the GUI approach (Download ZIP or GitHub Desktop) or the PowerShell/command-line route.

- Prerequisites:
  - Install Git for Windows (<https://git-scm.com/download/win>) if you plan to use PowerShell/command-line.
  - Optionally create a free GitHub account if you want to use GitHub Desktop or contribute changes.

1) Quick — download a ZIP (no git required)
    - Open the repository page in your browser: <https://github.com/xlandscape/xAquaticRisk>
    - Click the green **Code** button and choose **Download ZIP**.
    - Unzip the downloaded file to a folder of your choice, then open it with your editor (e.g., Visual Studio Code).

2) GUI — use GitHub Desktop (recommended if you prefer a graphical client)
    - Install GitHub Desktop (<https://desktop.github.com/>) and sign in with your GitHub account.
    - In the app choose File → Clone repository → URL and paste: <https://github.com/xlandscape/xAquaticRisk.git>
    - Choose a local folder and click **Clone**.

3) Windows Command Prompt / Command-line (quick & reproducible)
    - Open the Windows Command Prompt (cmd.exe) and run these commands:

```bat
REM clone the main repo (HTTPS)
git clone https://github.com/xlandscape/xAquaticRisk.git

REM go into the project folder
cd xAquaticRisk

REM open the project folder in Explorer (Windows)
start .

REM (optional) open the project in Visual Studio Code (if installed and in PATH)
code .
```

Notes on authentication and HTTPS vs SSH:

- Public repositories like xAquaticRisk can be cloned via HTTPS without a GitHub account.
- If you prefer SSH and have an SSH key set up, you can clone with the SSH URL instead (`git@github.com:xlandscape/xAquaticRisk.git`).

After cloning: the repository contains documentation, model components, example scenarios and parameterisation files. The default scenario (oudebeek-beek7-tdi) is included and ready to run.

Cloning steps vary based on the application being used, eg. [Sourcetree](https://support.atlassian.com/bitbucket-cloud/docs/clone-a-git-repository/) or [Visual Studio Code](https://learn.microsoft.com/en-us/azure/developer/javascript/how-to/with-visual-studio-code/clone-github-repository?tabs=activity-bar). Contact Sascha Bub or Thorsten Schad in case of problems.  

After cloning the repository, a user will have everything necessary to start using xAquaticRisk including sample scenarios and parametrisation files. For background on the regulatory context in which xAquaticRisk operates, see the [Regulatory Framework](../reference/regulatory-framework.md).  

### Option 2: Download xAquaticRisk.zip

To avoid the technical steps of cloning a GitHub repository, the most recent version(s) of xAquaticRisk are offered as a zip-file for direct download.  

### Prerequisites

xAquaticRisk requires a **64-bit Windows** operating system. No additional software installation is required — the model is fully portable.

## xAR Parameterisation and Configuration

As every landscape model derived from the xLandscape framework, xAR distinguishes a *parameterisation* from a *configuration* level.  

- ***Parameterisation*** is offered to the (standard) model user as `.xrun` files located at the model root directory. The `.xrun` file is an XML document that defines all user-accessible simulation parameters including scenario selection, simulation period, substance properties, effect model parameters and analysis settings. An equivalent `.yaml` format is also supported.  
- ***Configuration*** defines the fundamental model behaviour, i.e., which components are active and how they interact. The configuration is defined in `model/variant/mc.xml` and `model/variant/experiment.xml`. Configuration changes are typically not required for standard use.  

The file `template.xrun` is the main parameterisation template shipped with xAquaticRisk. It contains in-line documentation for all parameters. For a complete reference of all parameters, formats and examples, see **[xAquaticRisk Parameterisation](../reference/parameterisation.md)**.

Example ***.xrun*** file (simplified):

``` xml
<?xml version="1.0" encoding="utf-8"?>
<Parameters xmlns="urn:xAquaticRisk">
  <Control>
    <ExperimentID>Test_Run_aqRisk</ExperimentID>
    <NumberMC>1</NumberMC>
    <NumberParallelProcesses>2</NumberParallelProcesses>
    <CascadeToxswaWorkers>12</CascadeToxswaWorkers>
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
  </Exposure>
  <EnvironmentalFate>
    <RunStepsRiverNetwork>true</RunStepsRiverNetwork>
    <RunCascadeToxswa>true</RunCascadeToxswa>
    <MolarMass>505.2</MolarMass>
    <DT50sw>1000</DT50sw>
    <DT50sed>43.9</DT50sed>
    <KOC>460000</KOC>
    <!-- ... further substance properties ... -->
  </EnvironmentalFate>
  <Effects>
    <RunLGuts>true</RunLGuts>
    <Species1>Asellus aquaticus</Species1>
    <!-- ... GUTS parameters for species ... -->
  </Effects>
  <Settings>
    <ExportToSqlite>true</ExportToSqlite>
  </Settings>
</Parameters>
```

### Key Parameter Sections

The table below gives a brief overview. See the **[Parameterisation Reference](../reference/parameterisation.md)** for detailed descriptions, allowed values and defaults for every parameter.

| Section | Purpose |
|---|---|
| **Control** | Unique run ID, parallelisation settings, logging and profiling |
| **Scenario** | Landscape scenario selection and simulation period |
| **PppUse** | Application rate (g/ha) and application time window(s) |
| **Mitigation** | In-crop buffer (m) and drift-reducing technology (fraction) |
| **Exposure** | Rautmann drift class (arable, orchards.early, orchards.late) or predefined deposition file |
| **EnvironmentalFate** | Toggle fate models (StepsRiverNetwork, CascadeToxswa); substance physico-chemical properties (molar mass, DT50, KOC, etc.) |
| **Effects** | Toggle GUTS (LGuts), species names and GUTS-SD/IT parameters for up to 3 species |
| **Settings** | Number of Monte Carlo runs, SQLite export |
| **Analysis** | Reporting reaches, PEC display time |

## Running xAquaticRisk

To start xAquaticRisk using the sample scenario, **drag `template.xrun` onto `__start__.bat`**. This will start an xAquaticRisk run using the default scenario. Output of the model run can be found in the newly created `\run\<ExperimentID>\` folder.

> [!IMPORTANT]  
> **ExperimentIDs need to be unique**. xAquaticRisk will create a folder for each run using the ExperimentID defined in the `.xrun` file. The ExperimentID cannot be the same as a folder already contained in the run folder. If you want to run a simulation with the same ExperimentID you need to delete the existing folder first.

A typical simulation workflow:  

1. Copy `template.xrun` and rename it (e.g., `my_experiment.xrun`).  
2. Open the copy in a text editor and adjust parameters (scenario, substance properties, species, etc.).  
3. Drag the `.xrun` file onto `__start__.bat`.  
4. Monitor progress in the console window or the Control Panel. Log files are written to `\run\<ExperimentID>\log`.  
5. When the run completes, find results in `\run\<ExperimentID>\reporting` and analysis outputs.  

We are fully aware that XML is not the ideal **user interface**. The **[Control Panel](../reference/controlpanel.md)** combines a web-based parameterisation editor, real-time run monitor, and analysis tools in a single browser tab — start it via `controlpanel.bat`. The standalone WebUI (`webui.bat`) remains available as well.  

## Viewing and Analysing the Output

### HDFView  

[xLandscape](xLandscape/xLandscape-intro.md) makes use of multidimensional data stores. At present, [HDF](xLandscape/xLandscape-intro.md#multidimensional-data-store) is being used.  

To view the raw output of xAquaticRisk, open `\run\<ExperimentID>\mcs\[mc run ID]\store\arr.dat` with a HDF5 file viewer such as [HDFView](https://portal.hdfgroup.org/downloads/index.html). Expand the folder tree to inspect individual data stores (e.g., PEC values, spray-drift depositions, effect results).

Right click on an item and click "Open" to view its attributes and data.

### Reporting

The **ReportingObserver** automatically generates a set of plots and tables in the `\run\<ExperimentID>\reporting` folder. These include:

- Hydrological summaries (flow, water depth)
- PEC time series for selected reaches
- Spray-drift deposition maps
- Effect summaries (GUTS survival, LP50 values)
- StreamCom population dynamics

### Jupyter Notebooks

The **`analysis` folder** contains Jupyter notebooks which can analyse and visualise the output of xAquaticRisk. Start `notebook.bat` to access the notebooks. Available notebooks include:

- **PECsw_toExcel.ipynb**: exports predicted environmental concentrations (PECs) in surface water to Excel format  
- **LP50 Distribution.ipynb**: analyses and visualises LP50 value distributions  
- **LP50 by Strahler Order.ipynb**: analyses LP50 values grouped by Strahler stream order  
- **ARE_LP50.ipynb**: aquatic risk envelope analysis based on LP50 values  
- **Example Figure StreamCom.ipynb**: generates figures from StreamCom population model output  
- **Hydrographic Maps.ipynb**: generates maps of the hydrographic network with results  
- **Default Reporting Elements.ipynb**: produces standard reporting elements for the simulation
