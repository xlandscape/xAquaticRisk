# WetterfeldSlice

WetterfeldSlice is a subset scenario derived from the Muenster landscape scenario for focused xAquaticRisk analyses.
This document describes the shipped scenario package currently bundled with xAquaticRisk. The current version of this
document is from 2026-04-16.

Public repository: [Scenario-WetterfeldSlice](https://github.com/xlandscape/Scenario-WetterfeldSlice)

## Compatibility

The scenario can be used with xAquaticRisk version 2.86 and higher.

## Getting Started

Make sure you use a compatible version of xAquaticRisk.

This scenario is currently bundled in the `scenario/WetterfeldSlice` folder of xAquaticRisk. Keep the complete
scenario folder unaltered inside the `scenario` subdirectory of the model and reference it from the model
parameterisation.

## Usage

Reference the scenario from a `.xrun` or `.yaml` parameterisation using:

```xml
<LandscapeScenario>scenario/WetterfeldSlice</LandscapeScenario>
```

The scenario time series cover the period from 1991-01-01 to 2000-12-30.

The package adds the following macros to the Landscape Model:

* `:LandscapeScenario` (version 1.0)
* `:Temperature` (version 1.0)
* `:Hydrology` (version 1.0)
* `:Catchment` (version 1.0)
* `:TimeSeries` (version 1.0)
* `:Map` (version 2.0.1)
* `:SiteInformation` (version 2.0.10)
* `:SpeciesParameters` (version 2.0.19)
* `:WaterTemperature` (version 2.0.10)
* `:Biomass` (version 0.1.20201127)
* `:FocusMacroData` (version 1.0.0)

## Roadmap

The scenario now has a dedicated public repository for versioning and release tracking. The remaining integration step
is to populate that repository with the full scenario package and switch xAquaticRisk to consume it as a git submodule,
matching the shipping model used for the other bundled scenarios.

## Contributing

Contributions are welcome. Please contact the authors and see `CONTRIBUTING.md`.

## License

Distributed under the CC0 License. See `LICENSE` for more information.

## Contact

* Thorsten Schad - [thorsten.schad@bayer.com](mailto:thorsten.schad@bayer.com)
* Sascha Bub - [sascha.bub@gmx.de](mailto:sascha.bub@gmx.de)

## Acknowledgements

* Sebastian Multsch for hydrological, hydrographic and geospatial parts of the scenario
* Tido Strauß for the StreamCom parameterization
