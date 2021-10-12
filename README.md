## Table of Contents
* [About the project](#about-the-project)
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Usage](#usage)
* [Roadmap](#roadmap)
* [Contributing](#contributing)
* [License](#license)
* [Contact](#contact)
* [Acknowledgements](#acknowledgements)


## About the project
xAquaticRisk is a modular model that simulates processes at landscape-scale. It helps to assess potential effects of 
pesticide applications on aquatic non-target invertebrate populations. Its main features are:
* A modular structure that couples existing expert models like XDrift (spray-drift model based on AgDrift values),
  CascadeToxswa (Toxswa-based implementation of environmental fate simulation in a hydrological network), CmfContinuous
  (cmf-based environmental fate simulation), LEffectModel (GUTS implementation in landscape-contexts) or StreamCom
  (population model for Asellus aquaticus). Modules can be flexibly replaced or added as needed. 
* A unified and consistent view on the environmental state of the simulated landscape that is shared among modules and
  available to risk-assessment. 
* Data semantics that explicitly express values and their physical units at different spatial and temporal scales plus 
  the ability of transient data transformations according to the requirements of used models.
* Computational scaling that allows to conduct Monte Carlo runs in different sizes of landscapes, ranging from small
  schematic setups to real-world landscapes. 

### Built with
xAquaticRisk is composed of the following building blocks: 
* The Landscape Model core that provides the base functionality for managing experiments and exchanging data in a 
  landscape-scale context. See `\model\core\README` for further details.  
* The AnalysisObserver that implements default assessments of simulations, including the output of maps, plots and 
  tables. See `\model\variant\AnalysisObserver\README` for further details.
* CascadeToxswa, a wrapping that enables Toxswa to be applied within a hydrological network. See 
  `\model\variant\CascadeToxswa\README` for further details.
* CmfContinuous, a cmf-based environmental fate model. See `\model\variant\CmfContinuous\README` for further details.
* LEffectModel, an implementation of individual- and population-based internal-threshold´and sudden death GUTS models 
  within a landscape-context. See `\model\variant\LEffectModel\README` for further details.
* The LP50 module that derives LP50 values within the spatio-temporal setting of the Landscape Model for multiple Monte
  Carlo runs and using specified multiplication factors with which the LEffectModel simulated effects.
* The ReportingObserver that reports various plots regarding hydrology, exposure, environmental fate and effects. See
  `\model\variant\ReportingObserver\README` for further details.
* StepsRiverNetwork, an environmental fate module based on Steps1234, wrapped for usage within a landscape-context. See
  `\model\variant\StepsRiverNetwork\README` for further details.
* XDrift, a spray-drift model based on Rautmann drift values (
  [https://doi.org/10.1016/j.softx.2020.100610](https://doi.org/10.1016/j.softx.2020.100610)). See 
  `\model\variant\XSprayDrift\README` for further details.
* An exemplary landscape scenario consisting of approximately 1 km of stream with adjacent fields. See 
  `\scenario\oudebeek-beek7-tdi\README` for further details.
* A set of Jupyter notebooks for various analysis and visualizations of the simulation data. Start `notebook.bat` for 
  access to the notebooks.  


## Getting Started
xAquaticRisk is fully portable and is tested to run on a range of hardware.

### Prerequisites
xAquaticRisk requires a 64-bit Windows to run.

### Installation
1. Download the most recent xAquaticRisk zip-archive.
2. Extract the archive into a folder on your hard drive. Simulation data and temporary files will be written to a 
   sub-folder of this folder, so a fast hard-drive with lots of available space is preferable.  


## Usage
1. Open the `template.xrun` in any text editor and modify the parameters for the simulation to your needs. The 
   `template.xrun` contains an in-line documentation to assist you in deciding for valid parameter values.
2. Save the modified `template.xrun` under a different name in the same directory, using a `.xrun` extension.
3. Drag and drop the saved parameterization file onto the `__start__.bat`.
4. Check the console window occasionally for successful conclusion of the simulation or for errors. Logfiles can be
   found under `\run\<name-of-your-experiment>\log`.
5. If the run was successful, you can find analysis results over the entire experiment under 
   `\run\<name-of-your-experiment>\reporting`.


## Roadmap
xAquaticRisk is under continuous development. Future versions will include further and updated models. Usage of 
ontologies for semantic description of data will be added in a future version.


## Contributing
Contributions are welcome. Please contact the authors (see [Contact](#contact)).


## License
xAquaticRisk is distributed under the CC0 License. See the according `LICENSE` files for more information. See also the
`README` files of individual model parts for further information.


## Contact
Thorsten Schad - thorsten.schad@bayer.com  
Sascha Bub - sascha.bub@gmx.de


## Acknowledgements
See `README`s of individual building blocks for acknowledgements of these parts.
