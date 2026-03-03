# Glossary

This glossary defines key terms used in the xAquaticRisk documentation and the broader context of landscape-scale aquatic risk assessment.

## Active Substance

An active substance (a.s.) in a plant protection product is any chemical, plant extract, pheromone, or micro-organism (including viruses) that acts against pests or affects plants, parts of plants, or plant products (<https://food.ec.europa.eu/plants/pesticides_en>). Essentially, it's the component that provides the intended protective or pesticidal effect.  
*Active Substances* have physical and chemical properties as well as ecotoxicological endpoints. These data are kept in corresponding databases and are required as input for the environmental fate and effect modelling in xAquaticRisk.

## Application

When a PPP is applied to a certain crop at a certain field we call this an *Application*. Typically, a farmer puts a PPP (or a set of PPPs) into a spraying tank, fills the tank up with water, drives to the field and sprays the solution onto the field (into the crop).  
An *Application* is defined by:

- the PPP to be applied  
- an *Application Rate* (e.g., in [g a.s./ha])  
- a time when the application is intended (e.g., a deterministic day or an application time window)  
- the technology with which the application will be done (e.g., boom sprayer)  
- risk mitigation measures (e.g., spray buffer, in-crop buffer, drift-reducing nozzles)  

In xAquaticRisk, applications are defined in the `.xrun` parameterisation file.

## Application Rate

The amount of active substance applied per unit area, typically expressed in g/ha. The application rate is a key driver of exposure: it directly determines the mass of substance available for spray-drift deposition into surface water.

## Buffer

A distance ([m]) that is kept (has to be kept) to a certain entity during an agricultural activity. Buffers are typically used to implement risk mitigation means, e.g., to reduce spray-drift depositions into aquatic habitats. To protect aquatic organisms, no-spray buffers are defined as a distance to water bodies (e.g., a PPP may not be sprayed closer than 10 m to streams). In-crop buffers define a distance from the cropped area boundary which must not be sprayed.

## CascadeToxswa

A component of xAquaticRisk that wraps the [TOXSWA](https://www.wur.nl/en/show/toxswa.htm) model for application within a hydrological network. CascadeToxswa simulates the environmental fate of pesticides in surface water and sediment, accounting for degradation, sorption, volatilisation and transport within the stream network. It operates on individual reaches and routes substance mass downstream.

## CmfContinuous

A component of xAquaticRisk based on the Catchment Modelling Framework (CMF). It simulates hydrology and substance transport in the catchment.

## Components

In xAquaticRisk and [xLandscape](../xLandscape/xLandscape-intro.md#xlandscape), a *component* is a modular, self-contained unit that encapsulates a single area of functionality (for example: spray-drift modelling, environmental fate simulation, or effect modelling). Components can be composed or replaced to build and customise landscape models. They typically expose configuration and parameterisation settings to control behaviour.

## Configuration

As for each [xLandscape](../xLandscape/xLandscape-intro.md#xlandscape) component and landscape model, user simulation inputs and control settings are separated in two different levels, a *Configuration* and a [*Parameterisation*](#parameterisation) level:  

- the **configuration** of a component allows to change fundamental model behaviour (e.g., which environmental fate model is active).  
- the [*Parameterisation*](#parameterisation) level represents the actual user interface, i.e., the model parameters exposed to the user.  

In xAquaticRisk, configuration files are located in `model/variant/` (e.g., `mc.xml`, `experiment.xml`). Configuration changes are typically not required for standard use.

## DT50

The half-life degradation time of a substance. In xAquaticRisk, DT50 values are specified separately for water (DT50sw) and sediment (DT50sed) at a reference temperature of 20 °C. Temperature correction is applied during simulation using the molar activation enthalpy.

## Experiment

The term *Experiment* has been introduced to [xLandscape](../xLandscape/xLandscape-intro.md#xlandscape) model simulations as an analogue to experimental setups.  
An *Experiment* has a single model parameterisation and consists of a number of *Monte Carlo* runs. The latter are independent from each other, i.e., in each *Monte Carlo Run* the defined variabilities (Probability Density Functions, PDFs) are independently sampled.  
As compared to experimental setups, the 'control' setup is basically assumed to be the no-PPP-use, i.e., non-exposure situation. However, alternative baseline scenarios can be defined as separate *Experiments*.

## GUTS

General Unified Threshold models of Survival — a toxicokinetic–toxicodynamic (TKTD) framework for modelling the effects of time-variable exposure on survival. Two variants are used in xAquaticRisk:

- **GUTS-SD** (Stochastic Death): assumes a common threshold for all individuals and a stochastic killing process once the threshold is exceeded. Parameters: dominant rate constant (kd), threshold concentration (zw), killing rate (bw).  
- **GUTS-IT** (Individual Tolerance): assumes individual variation in sensitivity thresholds across a population. Parameters: dominant rate constant (kd), median threshold (mw), width of threshold distribution (β).

GUTS models are scientifically validated and accepted in regulatory risk assessment (see [EFSA Scientific Opinion on TKTD models](https://doi.org/10.2903/j.efsa.2018.5377) and [Regulatory Framework — TKTD](regulatory-framework.md#tktd-and-mechanistic-effect-modelling)).

## KOC

The organic carbon–water partition coefficient (l/kg). KOC describes the tendency of a substance to sorb to organic carbon in sediment and suspended particles. It is a key parameter for environmental fate modelling in surface water systems. The KOM used by some modules is automatically derived from the KOC by dividing by 1.742.

## LEffectModel

A component of xAquaticRisk that implements individual-based and population-based GUTS models within a landscape context. It calculates lethal effects of pesticide exposure on aquatic organisms based on predicted environmental concentrations in surface water.

## LP50

The multiplication factor applied to the exposure profile (or application rate) at which 50% lethal effects occur. LP50 is a key risk metric in xAquaticRisk, derived by the LP50 component across multiple Monte Carlo runs and reaches. An LP50 value greater than 1 indicates that effects only occur at exposure levels above the simulated application rate. LP50 values support the [tiered risk assessment](regulatory-framework.md#tiered-risk-assessment-under-the-uniform-principles) approach defined by the EU regulatory framework.

## Monte Carlo Run

Monte Carlo simulations rely on random sampling drawn from probability distributions to obtain numerical results. They are used to model phenomena with significant variability in inputs, such as calculating the range of possible exposure profiles and effects in a catchment.  
In xAquaticRisk, Monte Carlo runs sample variability in application timing (within specified time windows). The number of Monte Carlo runs is defined by the `<NumberMC>` parameter.

## Parameterisation

As for each [xLandscape](../xLandscape/xLandscape-intro.md#xlandscape) component and landscape model, user simulation inputs and control settings are separated in two different levels, a [*Configuration*](#configuration) and a *Parameterisation* level:  

- the [*Configuration*](#configuration) allows to change fundamental model behaviour.
- the **parameterisation** level represents the actual user interface, i.e., the model parameters exposed to the user. Inputs made by the user in the `.xrun` parameterisation file define a landscape modelling [Experiment](#experiment).  

## PEC

Predicted Environmental Concentration — the concentration of a substance in a specific environmental compartment (surface water or sediment) at a given location and time. PECs are calculated by the environmental fate components (CascadeToxswa, StepsRiverNetwork) and serve as input for the effect models.

## Plant Protection Product (PPP)

Plant protection products (PPPs) are chemical or biological products which are used to protect plants or plant products from harm caused by pests, diseases or weeds. The term "pesticides" is often used instead of plant protection product.  
PPPs contain one or more *Active Substances* and other co-formulants.

## Rautmann Drift Values

Standardised spray-drift deposition curves used to calculate the fraction of applied substance deposited at a given distance from the field edge. xAquaticRisk uses Rautmann drift values through the XSprayDrift component. Three drift classes are available: `arable`, `orchards.early` and `orchards.late`.

## Reach

A single segment of the stream network. In xAquaticRisk, each reach has associated geospatial, hydrological and environmental fate properties. Reaches are identified by numeric IDs (e.g., r567, r577) and are the basic spatial units for surface water modelling, effect modelling and reporting.

## Risk Mitigation / Risk Mitigation Measures

Regarding the use of PPPs, *Risk Mitigation* basically refers to reducing exposure caused by PPP application (see [Regulatory Framework — Mitigation](regulatory-framework.md#mitigation-measures) for the regulatory requirements). In xAquaticRisk, the following *Risk Mitigation* measures are implemented:

- **In-crop buffer**: a distance (m) from the crop boundary that must not be sprayed, reducing spray-drift deposition into nearby water bodies.
- **Drift-reducing technology**: the use of drift-reducing nozzles or other sprayer technology to reduce the fraction of applied substance that becomes spray-drift (specified as a fraction 0–1).

## Scenario

In the context of aquatic risk assessment, a *Scenario* refers to a set of conditions and assumptions used to model and predict the environmental fate and effects of pesticides in surface water systems. Key elements include:

1. **Hydrological conditions**: Stream network geometry, flow regime, water depth and volume.  
2. **Landscape configuration**: Field geometries, land use/cover, field-to-stream distances.  
3. **Meteorological conditions**: Temperature, precipitation, wind and solar radiation.  
4. **Agricultural practices**: Crop types, application regimes, mitigation measures.  
5. **Ecological conditions**: Species parameters, population dynamics data.

In xAquaticRisk, scenarios are stored in sub-folders of `xAquaticRisk\scenario\` and selected via the `<LandscapeScenario>` parameter.

## Simulation

The term *Simulation* is used in various contexts:

- a model simulation can mean to parameterise and run a model in general
- an xLandscape model simulation can mean to conduct an *Experiment*, i.e., the terms are used synonymously
- a *Simulation* can also address an individual *Monte Carlo* run of an xLandscape model [*Experiment*](#experiment)

## StepsRiverNetwork

A component of xAquaticRisk that implements the Steps1234 environmental fate model within a hydrological network. It provides an alternative (or complement) to CascadeToxswa for simulating pesticide fate in surface water and sediment.

## StreamCom

A population model component for *Asellus aquaticus* in xAquaticRisk. StreamCom simulates population dynamics including reproduction, growth, mortality and dispersal within the stream network. It is coupled with the GUTS effect model to assess population-level effects and recovery following pesticide exposure.

## XSprayDrift

A component of xAquaticRisk that calculates spray-drift deposition into stream reaches based on Rautmann drift values. It uses field-to-reach distances, the Rautmann drift class, application rates and drift-reducing technology settings to determine deposition into each reach of the stream network.
