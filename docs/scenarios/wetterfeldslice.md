# WetterfeldSlice (Germany)

WetterfeldSlice is a subset scenario derived from the Muenster landscape scenario for focused xAquaticRisk analyses.
It is shipped with xAquaticRisk as an additional bundled scenario.

Public repository: [Scenario-WetterfeldSlice](https://github.com/xlandscape/Scenario-WetterfeldSlice)

## Compatibility

The scenario can be used with xAquaticRisk version **2.86** and higher.

## Location in xAquaticRisk

The shipped scenario package is located at:

- `scenario/WetterfeldSlice`

Keep the complete scenario folder unaltered inside the `scenario` subdirectory of the model and reference it from the model parameterisation.

## Usage

Reference the scenario from a `.xrun` or `.yaml` parameterisation using:

```xml
<LandscapeScenario>scenario/WetterfeldSlice</LandscapeScenario>
```

Canonical example parameterisations are provided in:

- `parameterisation/template_WetterfeldSlice_10d.xrun`
- `parameterisation/template_WetterfeldSlice_10d.yaml`

## Time Series Range

The scenario time series cover the period from **1991-01-01** to **2000-12-30**.

## Included Scenario Macros

The package adds the following macros to the Landscape Model:

- `:LandscapeScenario` (version 1.0)
- `:Temperature` (version 1.0)
- `:Hydrology` (version 1.0)
- `:Catchment` (version 1.0)
- `:TimeSeries` (version 1.0)
- `:Map` (version 2.0.1)
- `:SiteInformation` (version 2.0.10)
- `:SpeciesParameters` (version 2.0.19)
- `:WaterTemperature` (version 2.0.10)
- `:Biomass` (version 0.1.20201127)
- `:FocusMacroData` (version 1.0.0)

## License

Distributed under the CC0 License. See `scenario/WetterfeldSlice/LICENSE` for more information.

## Contact

- Thorsten Schad - [thorsten.schad@bayer.com](mailto:thorsten.schad@bayer.com)
- Sascha Bub - [sascha.bub@gmx.de](mailto:sascha.bub@gmx.de)
