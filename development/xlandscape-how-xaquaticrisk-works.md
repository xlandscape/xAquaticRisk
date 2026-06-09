# How xLandscape Works in Practice (Using xAquaticRisk)

## Big picture

xLandscape, as used here, is a Python orchestration framework for scientific model pipelines.

It provides:

- experiment and Monte Carlo orchestration
- component lifecycle and wiring
- a shared data store with semantic metadata
- configuration-driven composition (XML plus macros)
- observers for logging and post-processing

xAquaticRisk is built as:

- core framework in model/core
- variant-specific components and composition in model/variant
- scenario packages in scenario
- bundled runtimes and wrapped external modules

So it is not a single monolith. It is a composed runtime model stack.

## Startup and run assembly

The launch path is:

1. __start__.bat invokes model/core/init.py.
2. init.py loads user parameters from xrun or yaml.
3. init.py creates a base.Experiment instance.
4. Experiment materializes experiment.xml and one mc.xml per MC run by replacing macros.
5. Each generated MC config is run by base.MCRun.

This is the key architectural point: the pipeline is declared in XML and assembled at runtime, not hard-coded as one Python script.

## Core responsibilities

The core in model/core handles orchestration and contracts, not all domain science.

Main classes:

- base.Experiment: global run setup, token replacement, scenario resolution, MC scheduling
- base.MCRun: instantiate store, build composition, wire inputs and outputs, execute components in order
- base.Component: component contract
- base.Input and base.Output: typed, scale-aware, unit-aware data access
- stores.X3dfStore: HDF5-backed shared store with metadata

The MC execution model is deterministic and sequential by component order in mc.xml.

## Data model and flow contract

A major strength is that values are not passed as raw arrays only. They carry semantic metadata:

- scales, for example time/hour, time/day, space/reach, space/base_geometry
- units
- offsets (for example simulation start time)
- element names (for example reach ids)
- geometries

This enables safe coupling across heterogeneous modules and runtimes.

## Scenario integration

Scenario metadata is defined in scenario.xproject.

The Project loader reads entries from scenario content and exposes them as scenario macros, for example:

- $(:LandscapeScenario)
- $(:Temperature)
- $(:Hydrology)
- $(:Catchment)

This decouples model wiring from hard-coded scenario file paths.

## xAquaticRisk pipeline (main chain)

In model/variant/mc.xml, the core chain is:

1. LandscapeScenario (load static scenario data)
2. Weather and Hydrology
3. PPM calendar (application timing and field-level applications)
4. XSprayDrift (spray drift simulation)
5. DepositionToReach (map drift deposition to reaches)
6. Environmental fate modules (StepsRiverNetwork and or CascadeToxswa, optionally CmfContinuous)
7. Effects modules (LEffectModel variants)
8. LP50 and optional SQL export components

So conceptually:

scenario plus weather plus hydrology
-> application schedule
-> spray drift
-> deposition to aquatic network
-> fate concentrations
-> biological effects
-> summary products

## How core and external modules interact

The core talks to wrapper components, not directly to every scientific engine.

Each wrapper component does this pattern:

1. read standardized inputs from the store
2. prepare module-specific files and runtime environment
3. call external process (Rscript, bundled Python, executable, etc.)
4. parse module outputs
5. write standardized outputs back to the shared store

Examples:

- XSprayDrift wrapper calls bundled R runtime
- StepsRiverNetwork wrapper writes CSV inputs, runs module, reads HDF outputs
- CascadeToxswa wrapper writes INI and CSV inputs, runs bundled Python workflow
- LEffectModel wrapper runs module-specific effect simulations

This keeps scientific engines replaceable while preserving one shared integration contract.

## Observers and post-processing

Observers are configured in experiment.xml and MC XML.

Typical observers include:

- console and logfile observers from core
- AnalysisObserver (runs R scripts after MC or experiment completion)
- ReportingObserver (generates standard reporting outputs)

So reporting is part of the orchestrated lifecycle, not a disconnected post-hoc step.

## Why this architecture is useful

Strengths:

- modular composition
- explicit data contracts with scale and unit semantics
- integration of heterogeneous tools without rewriting all science code
- reproducible MC run directories and outputs
- scenario portability via metadata and macros

Trade-offs compared to modern app stacks:

- XML and macro-based configuration instead of code-first DAG frameworks
- sequential pipeline execution by configured order
- substantial adapter code for file-based interop

Given the domain constraints, this design is coherent and practical.

## One-line summary

xLandscape in xAquaticRisk is a configuration-driven Python orchestration layer that wires scenario data and user parameters into a semantically rich shared store, executes wrapped scientific components in sequence, and feeds their outputs into fate, effects, and reporting workflows.