# Example Scenarios

xAquaticRisk ships with several landscape scenarios representing different catchments and agricultural contexts. These scenarios are located in the `xAquaticRisk\scenario\` folder and can be selected via the `<Project>` parameter in the `.xrun` parameterisation file.

# Oudebeek-Beek7 (Belgium)

## Overview

This landscape scenario represents a 2 km river branch (oudebeek_beek7) in the East of the Rummen catchment, Belgium. It is the default scenario shipped with xAquaticRisk and is referenced by `template.xrun`.

[GitHub repository for scenario](https://github.com/xlandscape/Scenario-OudebeekBeek7TDI)

**Compatibility**: xAquaticRisk version **2.86** and higher.

## Geo Information

The scenario comprises a stream network section with adjacent agricultural fields. The geospatial data includes:

- Stream reach geometries defining the hydrological network
- Agricultural field geometries with land use/cover types
- Field-to-reach distances for spray-drift deposition calculation
- Catchment delineation for hydrological routing

## Hydrological Data

Hydrological time series (flow, water depth, volume) are provided for each reach in the stream network. The hydrology was derived using CMF-based hydrological simulation.

## Species Data

The scenario includes StreamCom species parameter databases and biomass data for population-level modelling of *Asellus aquaticus*.

## Usage

To use this scenario, set the following in your `.xrun` file:

```xml
<Project>scenario/oudebeek-beek7-tdi</Project>
```

The scenario supports simulation periods within the range of the provided hydrological time series. Consult the scenario README for the exact valid date range.

---

# Oudebeek (Belgium)

## Overview

This landscape scenario represents the broader Oudebeek catchment in Belgium. It provides a larger spatial extent than the Oudebeek-Beek7 scenario.

[GitHub repository for scenario](https://github.com/xlandscape/Scenario-OudebeekTDI)

**Compatibility**: xAquaticRisk version **2.85** and higher.

## Geo Information

The scenario covers the Oudebeek catchment with a more extensive stream network and a larger number of adjacent agricultural fields compared to the Oudebeek-Beek7 subset.

## Usage

To use this scenario, set the following in your `.xrun` file:

```xml
<Project>scenario/Scenario-OudebeekTDI</Project>
```

---

# Grote Kemmelbeek (Belgium)

## Overview

This landscape scenario represents the Grote Kemmelbeek catchment in Belgium. The catchment features a v-shaped valley with stream flow modelled using kinematic wave routing.

[GitHub repository for scenario](https://github.com/xlandscape/Scenario-GroteKemmelbeekTDI)

**Compatibility**: xAquaticRisk version **2.67** and higher.

## Geo Information

The scenario includes stream reach geometries, field geometries, hydrological time series and weather data for the Grote Kemmelbeek catchment.

## Usage

To use this scenario, set the following in your `.xrun` file:

```xml
<Project>scenario/GroteKemmelbeekTDI</Project>
```

---

# Münster (Germany)

## Overview

This landscape scenario represents an agricultural catchment in the Münster region, Germany. It provides a different agricultural and hydrological context compared to the Belgian scenarios.

**Compatibility**: xAquaticRisk version **2.70** and higher.

## Hydrological Data

Time series cover the period from 1991-01-01 to 2018-12-31. Water depth was preprocessed in the hydrological simulation. Flow data was preprocessed: negative flow values were set to positive, and zero flow values were set to 0.05 m³/day.

## Usage

To use this scenario, set the following in your `.xrun` file:

```xml
<Project>scenario/muenster-T-Di-02.5-20220429-postproceccing-toxwa</Project>
```

Example parameterisation files for the Münster scenario are provided at the xAquaticRisk root directory:

- `muenster_arable1x7.5_1_23feb_early_20mBuffer.xrun` — Arable, 1 application at 7.5 g/ha, orchards.early Rautmann class, 20 m in-crop buffer
- `muenster_arable1x7.5_1_23feb_late_20mBuffer.xrun` — Same as above but with orchards.late Rautmann class

---

# Scenario Comparison

| Scenario | Location | Stream Length | Time Series Range | Compatibility |
|---|---|---|---|---|
| Oudebeek-Beek7 | Belgium (Rummen) | ~2 km branch | See scenario README | v2.86+ |
| Oudebeek | Belgium (Rummen) | Full catchment | See scenario README | v2.85+ |
| Grote Kemmelbeek | Belgium | V-shaped catchment | See scenario README | v2.67+ |
| Münster | Germany | Agricultural catchment | 1991–2018 | v2.70+ |
