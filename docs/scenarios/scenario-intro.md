# Scenarios

First of all, scenarios have a **technical meaning** as a structured integration of all necessary environmental, hydrological, land use/cover, agricultural, ecological data, information and assumptions to run a model (here, xAquaticRisk). However, in environmental risk assessment, scenarios have a **regulatory meaning** as a testing system, that has to assure a certain protection level in case the test gets passed. In this role, scenarios have to address a certain **level of conservatism**, hence, need to be **representative** for a range of conditions of a typically large geographic area (e.g., a country).  
Whereas detailed scenarios are applied to derive risk estimates, simple ones are used, e.g., for screening a level of conservatism for large areas.  
Scenarios play a key role in the analysis and definition of effective **risk mitigation** measures. Scenarios can incorporate buffer zones, the use of drift-reducing technology, reduced application rates, or alternative practices to evaluate risk reduction. Beyond the focus on actual pesticide uses, scenarios serve **risk management** approaches targeting generic landscape characteristics which affect species populations, communities and biodiversity: e.g., the composition, management and structure of cultivated landscapes, and their relationship to their surrounding regions.  
Realistic landscape scenarios can play an important role in **policy alignment**: The definition of Specific Protection Goals (SPGs, e.g., [EFSA 2010](https://doi.org/10.2903/j.efsa.2010.1821), [EFSA 2025](https://doi.org/10.2903/j.efsa.2025.9501)) is based on the ecosystem services concepts. Modern regulatory developments highlight the important role of landscape scenarios and approaches as a way to contextualize SPGs, e.g., for aquatic organisms. Future risk and risk assessment will develop beyond single-species laboratory tests and incorporate realistic agricultural landscapes, including spatial and temporal heterogeneity, e.g., to capture both direct and indirect effects of pesticides on aquatic ecosystems. Risk management decisions should be informed by how pesticides affect organisms in actual European agroecosystems, not abstract worst-cases or averages. Landscape scenarios can ensure that SPGs align with broader environmental protection mandates to EU biodiversity and sustainability goals. So, landscape scenarios serve as a **bridge between scientific modelling and regulatory decision-making**.

## xAR Scenarios

### General

According to their technical meaning, scenario definition and requirements depend on the given composition of the xAR landscape model (i.e., integrated components and their version). Thus, scenario development has to be adaptive and in line with xAR model development.  
[Example scenarios](Example-scenarios.md) shipped with xAR fit to the given model version and reside in the scenario folder after model download (`xAquaticRisk\scenario`).  
[Example scenarios](Example-scenarios.md) also provide a good starting point for the development of user-defined (new) scenarios.  

Outlook:  
For scenario creation web-based services have been experimentally tested. Such services provide a (simple) GUI via web browser to enable the user to select a geographic location (e.g., via mouse click or by providing coordinates), to set parameters and to get a ready-to-use scenario back. This significantly reduces effort and cost for the user and supports consistency and harmonisation.  
Furthermore, such services can support the study site identification, i.e., the selection of representative scenario locations (and numbers).  

### Scenario Structure

An xAR scenario is stored in a sub-folder of `xAquaticRisk\scenario\` and typically contains the following elements:

| Folder / File | Content |
|---|---|
| `geo/` | Geospatial data: field geometries (shapefiles), stream reach geometries, field-to-reach distances, catchment delineation |
| `hydro/` | Hydrological data: flow time series, water depth, volume for each reach in the stream network |
| `weather/` | Meteorological data: temperature, precipitation, wind, solar radiation time series |
| `stream_com/` | StreamCom species parameters: life-history traits, biomass data for population modelling |
| `scenario.xproject` | Scenario configuration file linking all data layers and defining macros |
| `README.md` | Scenario documentation (auto-generated) |

### Scenario Development Aspects

#### Scenario Site Selection

The selection of scenario sites is a crucial part of the scenario development process and depends on the study context, purpose and goals.  
For aquatic risk assessment, site selection typically considers:

1. **Catchment characteristics**: The hydrological network structure (stream order, connectivity), catchment size, and the density of agricultural fields near water bodies. Catchments with a high proportion of treated fields draining into streams represent higher exposure scenarios.  
2. **Agricultural intensity**: The density and proximity of treated crop fields to the stream network, crop types and application regimes. Regions with intensive agriculture bordering streams are of particular interest.  
3. **Hydrological conditions**: Base flow, flow variability, and dilution capacity of the stream network. Smaller streams with lower dilution may represent more conservative scenarios.  
4. **Climatic variability**: Weather conditions affect hydrology, substance degradation, and species phenology. Scenarios covering different climatic zones increase representativeness.  
5. **Ecological relevance**: Presence of sensitive aquatic communities, protected water bodies, or areas of ecological importance.  

The current example scenarios shipped with xAR are located in Belgium (Oudebeek/Rummen catchment, Grote Kemmelbeek) and Germany (Münster region), representing different catchment types and agricultural contexts.

#### Levels of Detail

The ideal data of local hydrology, water quality, and aquatic habitat conditions, together with the necessary data on environmental conditions, is hardly fully available. Typically, **scenario conditions are modelled**, as a composite of data, models and expert judgement. Scale, precision and accuracy of this modelling approach should be adapted to the problem at hand. Transparency in data preparation, processing and modelling steps is key to acceptability.  
To support transparency and structure in scenario development, we propose to consider explicit levels of detail:

| Level | Data Acquisition | Description |
| --- | --- | --- |
| 1 | Off-the-shelf data | Basic, wide-coverage datasets (satellite, national/regional hydrological products) suitable for large-scale or screening analyses. |
| 2 | Best-available data | Curated and locally verified datasets with additional manual processing or expert review for improved accuracy. |
| 3 | Contemporary data generation | High-resolution, targeted data collected for the study (e.g., local hydrological measurements, field surveys). |
| 4 | Field study | Detailed field measurements providing high-resolution information about hydrology, water quality and local environmental conditions. |

#### Base Data

This section introduces some base data commonly used in scenario development. The information is intended to give a general overview. We highly recommend consulting web pages, search engines and AI agents for recent information before you start your development or study.  

##### Land Use Land Cover (LULC)

A typical base geodata layer in scenario development is composed from pan-European and national/topographic sources. The table below summarises common products and their typical availability across Europe and individual countries.

| Dataset / Product | Geographic coverage | Typical spatial resolution / unit | Access / Notes |
| --- | --- | --- | --- |
| CORINE Land Cover (CLC) / Copernicus Land Monitoring | Pan-Europe (EEA) | Vector polygons, minimum mapping unit ≈25 ha (derived raster products ~100 m) | Open via EEA/Copernicus. Good for consistent, multi-year land-cover mapping but coarse for parcel-scale studies. |
| Copernicus High Resolution Layers (HRL) | Pan-Europe (selected layers) | Thematic layers (e.g., forests, grasslands) often at 10–20 m equivalent detail | Open via Copernicus; complements CLC with thematic detail. |
| EU-DEM / SRTM / national DEMs | Pan-Europe (EU-DEM) / global (SRTM) / national high-res | DEMs at 25 m (EU-DEM) to 90 m (SRTM); national LiDAR-derived DEMs much finer | EU-DEM open; national DEMs/LiDAR availability varies by country and region. |
| National crop cover / thematic crop maps | Country (examples: Germany, Netherlands, UK, France) | Varies — from 10 m to parcel level | Produced by national agencies or research groups. Check national portals for access and licensing. |
| IACS / LPIS (administrative parcel data) | National / regional (EU) | Parcel-level declarations (farmers' subsidy claims) | Highly detailed for crop/use per parcel but often restricted. Excellent for parcel-scale scenario building when available. |
| OpenStreetMap (OSM) & volunteered data | Global (incl. Europe) | Vector features (roads, buildings, landuse polygons) | Open and continuously updated; useful for topographic features and small-structure mapping. |

##### Hydrological Data

Hydrological data forms the backbone of aquatic risk scenarios. Key data requirements include:

| Data Type | Description | Typical Sources |
| --- | --- | --- |
| Stream network geometry | Reach geometries, connectivity, stream order | National topographic databases, EU-Hydro, OpenStreetMap |
| Flow time series | Discharge (m³/d) for each reach | Hydrological simulations (e.g., CMF), gauging station records, national hydrological services |
| Water depth and volume | Water depth (m) and volume (m³) per reach over time | Derived from flow using hydraulic geometry or cross-section data |
| Field-to-reach distances | Minimum distance from each agricultural field to the nearest stream reach | Computed from field and reach geometries (GIS) |
| Catchment delineation | Sub-catchment boundaries for hydrological routing | Derived from DEM analysis |

##### Environmental Fate Data

For the environmental fate modelling with CascadeToxswa and StepsRiverNetwork, additional reach-level data is needed:

| Data Type | Description |
| --- | --- |
| Sediment properties | Organic matter content, bulk density of sediment |
| Water body dimensions | Width, depth, cross-sectional profile of reaches |
| Macrophyte coverage | Percentage coverage by aquatic macrophytes (affects sorption) |
| Suspended solids | Concentration of suspended particulate matter |

##### Weather Data

Meteorological data is required for environmental fate modelling (temperature-dependent degradation, volatilisation). Common sources include:

| Source | Coverage | Resolution | Notes |
| --- | --- | --- | --- |
| Agri4Cast (JRC) | Pan-Europe | 25 km grid, daily | Open access; includes temperature, precipitation, wind, solar radiation |
| National meteorological services | Country-level | Station-based or gridded | Often higher resolution than European products |
| ERA5 (ECMWF) | Global | ~31 km, hourly | Reanalysis product; consistent and long time series |

#### Crop Protection

The user defines PPP uses through the parameterisation (`.xrun` file), specifying:

- **Application rate** (g/ha)
- **Application time window** (date range within which applications occur)
- **Rautmann drift class** (arable, orchards.early, orchards.late) — determines the spray-drift curve used for deposition calculation
- **In-crop buffer** (m) — distance from field edge that is not sprayed
- **Drift-reducing technology** (fraction 0–1) — reduction factor applied to spray-drift deposition

These parameters directly control the exposure pathway from field application to surface water deposition and are key drivers of the aquatic risk assessment.

#### Substance Properties

Environmental fate modelling requires substance-specific physico-chemical properties. These are defined in the `.xrun` parameterisation file and include:

| Property | Unit | Description |
| --- | --- | --- |
| Molar mass | g/mol | Molecular weight of the active substance |
| Saturated vapour pressure | Pa | Vapour pressure at 20 °C |
| Solubility in water | mg/l | Water solubility at 20 °C |
| DT50 in water | d | Half-life in water at 20 °C |
| DT50 in sediment | d | Half-life in sediment at 20 °C |
| KOC | l/kg | Organic carbon–water partition coefficient |
| Diffusion coefficient | m²/d | Diffusion coefficient at 20 °C |
| Freundlich exponent | – | For sorption to sediment and suspended particles |

#### Species and Effect Parameters

Effect modelling using GUTS requires species-specific toxicological parameters. xAR supports up to three species simultaneously. For each species, GUTS-SD and GUTS-IT parameters are defined:

| Parameter | GUTS-SD | GUTS-IT |
| --- | --- | --- |
| Dominant rate constant | kd (1/h) | kd (1/h) |
| Threshold | zw (ng/l) — threshold concentration | mw (ng/l) — median threshold |
| Effect parameter | bw (l/(ng·h)) — killing rate | β — width of threshold distribution |

The default parameterisation includes three representative macroinvertebrate species:

- **Asellus aquaticus** — isopod; also used by the StreamCom population model
- **Cloeon dipterum** — mayfly
- **Gammarus pulex** — amphipod

## References

EFSA PPR Panel (2013). Guidance on tiered risk assessment for plant protection products for aquatic organisms in edge-of-field surface waters. EFSA Journal 11(7): 3290. [https://doi.org/10.2903/j.efsa.2013.3290](https://doi.org/10.2903/j.efsa.2013.3290)  
EFSA PPR Panel (2018). Scientific Opinion on the state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect models for regulatory risk assessment of pesticides for aquatic organisms. EFSA Journal 16(8): 5377. [https://doi.org/10.2903/j.efsa.2018.5377](https://doi.org/10.2903/j.efsa.2018.5377)

