#!/usr/bin/env python
"""
convert_xrun.py  –  Convert old-format xAquaticRisk .xrun files to new format.

Reads a legacy .xrun file (with CropStage and/or Control at the bottom)
and produces:
  1.  A new-format .xrun file  (Control first, RautmannClass)
  2.  An equivalent .yaml parameterisation file

Usage
-----
    python convert_xrun.py  <old_file.xrun>  [--outdir <folder>]  [--suffix _new]

By default the outputs are written next to the input file with the same base
name (the .xrun is overwritten and a .yaml is added).  Use --suffix to append
a tag, e.g.  ``--suffix _new`` → ``basename_new.xrun`` + ``basename_new.yaml``.
Use --outdir to redirect output to a different directory.

Requires
--------
*  Python ≥ 3.6  (uses f-strings)
*  PyYAML (``import yaml``) – only for YAML writing; the script falls back to a
   simple manual emitter if PyYAML is not available.
"""
from __future__ import annotations

import argparse
import copy
import os
import re
import sys
import xml.etree.ElementTree as ET
from collections import OrderedDict
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

# ---------------------------------------------------------------------------
#  Namespace handling
# ---------------------------------------------------------------------------
NS = "urn:xAquaticRisk"
NS_MAP = {"": NS}

# Make ElementTree emit the namespace properly
ET.register_namespace("", NS)
ET.register_namespace("xsi", "http://www.w3.org/2001/XMLSchema-instance")


def _tag(local: str) -> str:
    """Return a namespace-qualified tag name."""
    return f"{{{NS}}}{local}"


# ---------------------------------------------------------------------------
#  Canonical section order (new format) and parameter order within sections
# ---------------------------------------------------------------------------
SECTION_ORDER: List[str] = [
    "Control",
    "Scenario",
    "PppUse",
    "Mitigation",
    "Exposure",
    "EnvironmentalFate",
    "Effects",
    "Settings",
    "Analysis",
]

# Canonical parameter order inside each section (new format).
# A parameter that appears in the old file but is *not* listed here will be
# appended at the end of its section, preserving the original relative order.
PARAM_ORDER: Dict[str, List[str]] = {
    "Control": [
        "ExperimentID", "ParentRun", "NumberMC", "NumberParallelProcesses",
        "CascadeToxswaWorkers", "DeleteComponentProcessingFolders",
        "VerboseLogging", "EnableProfiling", "ProfilingWaitingTime",
    ],
    "Scenario": ["LandscapeScenario", "SimulationStart", "SimulationEnd"],
    "PppUse": ["ApplicationRate", "ApplicationTimeWindow"],
    "Mitigation": ["InCropBuffer", "TechnologyDriftReduction"],
    "Exposure": ["RautmannClass", "DepositionInputFile"],
    "EnvironmentalFate": [
        "RunStepsRiverNetwork", "RunCascadeToxswa",
        "ThresholdSW", "ThresholdSediment",
        "MolarMass", "SaturatedVapourPressure",
        "MolarEnthalpyOfVaporization", "SolubilityInWater",
        "MolarEnthalpyOfDissolution", "DiffusionCoefficient",
        "DT50sw",
        "MolarActivationEnthalpyOfTransformationInWater",
        "DT50sed",
        "MolarActivationEnthalpyOfTransformationInSediment",
        "KOC", "ReferenceConcentrationForKOC",
        "FreundlichExponentInSedimentAndSuspendedParticles",
        "CoefficientForLinearAdsorptionOnMacrophytes",
    ],
    "Effects": [
        "RunLGuts",
        "NumberOfWarmUpYears", "RecoveryPeriodYears",
        "Species1",
        "Species1DominantRateConstantSD",
        "Species1ThresholdConcentrationSD",
        "Species1KillingRateSD",
        "Species1DominantRateConstantIT",
        "Species1ThresholdDistributionIT",
        "Species1WidthOfThresholdDistributionIT",
        "Species2",
        "Species2DominantRateConstantSD",
        "Species2ThresholdConcentrationSD",
        "Species2KillingRateSD",
        "Species2DominantRateConstantIT",
        "Species2ThresholdDistributionIT",
        "Species2WidthOfThresholdDistributionIT",
        "Species3",
        "Species3DominantRateConstantSD",
        "Species3ThresholdConcentrationSD",
        "Species3KillingRateSD",
        "Species3DominantRateConstantIT",
        "Species3ThresholdDistributionIT",
        "Species3WidthOfThresholdDistributionIT",
    ],
    "Settings": ["ExportToSqlite"],
    "Analysis": ["ReportingReaches", "PecDisplayedTime"],
}

# ---------------------------------------------------------------------------
#  CropStage → RautmannClass mapping
# ---------------------------------------------------------------------------
CROP_STAGE_MAP = {
    "early": "orchards.early",
    "late": "orchards.late",
    "arable": "arable",
    # pass-through for values that are already in new format
    "orchards.early": "orchards.early",
    "orchards.late": "orchards.late",
}

# ---------------------------------------------------------------------------
#  Legacy parameter / section renames
# ---------------------------------------------------------------------------
# Section renames: old name → new name
SECTION_RENAMES = {
    "SimulationInfo": "Control",
}

# Parameter renames: old name → new name
PARAM_RENAMES = {
    "SimID": "ExperimentID",
    "DeleteFoldersAtFinish": "DeleteComponentProcessingFolders",
    "Project": "LandscapeScenario",
}

# Parameters to drop entirely
DROPPED_PARAMS = {"ReachSelection"}

# Parameters that moved between sections: param_name → new_section_name
PARAM_SECTION_MOVES = {
    "NumberMC": "Control",
}

# ---------------------------------------------------------------------------
#  Comment blocks for the new xrun output  (keyed by parameter name)
# ---------------------------------------------------------------------------
# fmt: off
COMMENTS: Dict[str, str] = {
    "ExperimentID": (
        "    <!--\n"
        "    Parameter     :  ExperimentID\n"
        "    Description   :  A unique identifier of a simulation run\n"
        "    Values        :  Any characters that are valid in file system identifiers\n"
        "    Remark        :  Running a simulation with the same name as an existing simulation run results in an error.\n"
        "    Best practice :  Change this parameter with every simulation run. When doing test runs, indicate the test character\n"
        "                     of clearly in the identifier. If conducting a run from the experiment table, register it there and\n"
        "                     use the naming scheme <experiment id>-<n> where n is the number of the run.\n"
        "    -->\n"
    ),
    "ParentRun": (
        "    <!--\n"
        "    Parameter     :  ParentRun\n"
        "    Description   :  A file pattern of potential parents for this run\n"
        "    Values        :  Empty (=do not use this feature) or a file pattern yielding at least one X3df data store file.\n"
        "    Remark        :  The feature is currently experimental.\n"
        "                     If the pattern yields more than one file, a single file will be sampled per Monte Carlo run.\n"
        "    Best practice :  Only fill out the parent run if you like to test the feature.\n"
        "    -->\n"
    ),
    "NumberParallelProcesses": (
        "    <!--\n"
        "    Parameter     :  NumberParallelProcesses\n"
        "    Description   :  The number of Monte Carlo runs that are conducted simultaneously\n"
        "    Values        :  Any positive integer\n"
        "    Remark        :  This parameter should be modified according to the available hardware resources.\n"
        "    Best practice :  To best use the available hardware, apply this rule of thumb:\n"
        "                     min(NumberMC, NumberParallelProcesses) <= available processors\n"
        "                     if RunCascadeToxswa is false, else\n"
        "                     min(NumberMC, NumberParallelProcesses) * CascadeToxswaWorker <= available processors\n"
        "    -->\n"
    ),
    "CascadeToxswaWorkers": (
        "    <!--\n"
        "    Parameter     :  CascadeToxswaWorkers\n"
        "    Description   :  The number of processes spawned by CascadeToxswa\n"
        "    Values        :  Any positive integer\n"
        "    Remark        :  This parameter should be modified according to the available hardware resources. Only in effect, if\n"
        "                     RunCascadeToxswa is true.\n"
        "    Best practice :  To best use the available hardware, apply this rule of thumb:\n"
        "                     min(NumberMC, NumberParallelProcesses) * CascadeToxswaWorker <= available processors\n"
        "    -->\n"
    ),
    "DeleteComponentProcessingFolders": (
        "    <!--\n"
        "      Parameter     :   DeleteComponentProcessingFolders\n"
        "      Description   :   Specifies whether processing folders should be deleted after the model run is finished\n"
        "      Values        :   true or false\n"
        "      Best practice :   Should be set to true for a complete 26-year run to save memory, but can be set to false when\n"
        "                        testing\n"
        "    -->\n"
    ),
    "VerboseLogging": (
        "    <!--\n"
        "      Parameter     :   VerboseLogging\n"
        "      Description   :   Specifies whether verbose logging is used\n"
        "      Values        :   true or false\n"
        "      Best practice :   Should be set to false when using the model in production environments and only to false if\n"
        "                        there is need for debugging or during development\n"
        "    -->\n"
    ),
    "EnableProfiling": (
        "    <!--\n"
        "      Parameter     :   EnableProfiling\n"
        "      Description   :   Specifies whether the performance of the simulation run is profiled\n"
        "      Values        :   true or false\n"
        "      Remark        :   Profiling comes with a little overhead and makes observer outputs less verbose\n"
        "    -->\n"
    ),
    "ProfilingWaitingTime": (
        "    <!--\n"
        "      Parameter     :   ProfilingWaitingTime\n"
        "      Description   :   The time interval in seconds between to profiling rounds\n"
        "      Values        :   Any positive integer\n"
        "      Remark        :   Only in effect if profiling is enabled\n"
        "      Best practice :   Choose an interval that does not produce an overly large amount of output but that still results\n"
        "                         in a sufficient temporal resolution\n"
        "    -->\n"
    ),
    "LandscapeScenario": (
        "    <!--\n"
        "    Parameter     :  LandscapeScenario\n"
        "    Description   :  The scenario used by the simulation\n"
        "    Values        :  scenario/<xyz> where <xyz> is one of the sub-folders in the scenario folder.\n"
        "    Remark        :  Make sure that the scenario is present in the scenario sub-folder.\n"
        "    -->\n"
    ),
    "SimulationStart": (
        "    <!--\n"
        "    Parameter     :  SimulationStart\n"
        "    Description   :  The first date that is simulated\n"
        "    Values        :  A date of format YYYY-MM-DD within the valid range of dates\n"
        "    Remark        :  The valid range of start dates is mainly determined by the input data for the selected scenario.\n"
        "                     The available ranges can be obtained from the scenario documentation.\n"
        "    -->\n"
    ),
    "SimulationEnd": (
        "    <!--\n"
        "    Parameter     :  SimulationEnd\n"
        "    Description   :  The last date that is simulated\n"
        "    Values        :  A date of format YYYY-MM-DD within the valid range of dates\n"
        "    Remark        :  Must be later than the simulation start (or the same date). For limitations of allowed dates, see\n"
        "                     remark for SimulationStart.\n"
        "    -->\n"
    ),
    "ApplicationRate": (
        "    <!--\n"
        "    Parameter     :  ApplicationRate\n"
        "    Description   :  The application rate in g/ha\n"
        "    Values        :  Any positive number (or zero)\n"
        "    Remark        :  The application rate applies to all applications within application sequences.\n"
        "    -->\n"
    ),
    "ApplicationTimeWindow": (
        "    <!--\n"
        "    Parameter     :  ApplicationTimeWindow\n"
        "    Description   :  The time windows within which applications take place\n"
        "    Values        :  format: MM-DD to MM-DD[, MM-DD to MM-DD[, ...]]\n"
        "    Remark        :  For each target field, year and application within an application sequence, a random date within\n"
        "                     the specified time window is selected.\n"
        "    -->\n"
    ),
    "InCropBuffer": (
        "    <!--\n"
        "    Parameter     :  InCropBuffer\n"
        "    Description   :  A in-crop buffer not applied in meters\n"
        "    Values        :  Zero or a positive number.\n"
        "    -->\n"
    ),
    "TechnologyDriftReduction": (
        "    <!--\n"
        "    Parameter     :  TechnologyDriftReduction\n"
        "    Description   :  The effect of drift-reducing nozzles to spray-drift deposition; the fraction filtered out\n"
        "    Values        :  A number between 0 and 1\n"
        "    Remark        :  Zero means no effect = full spray-drift deposition, 1 means full filtering = no deposition at all\n"
        "    -->\n"
    ),
    "RautmannClass": (
        "    <!--\n"
        "    Parameter     :  RautmannClass\n"
        "    Description   :  The Rautmann drift class used in the simulation\n"
        "    Values        :  orchards.early, orchards.late or arable\n"
        "    Remark        :  The Rautmann class is applied to all applications of application sequences.\n"
        "    Best practice :  Select the Rautmann class fitting to the crop type and application dates.\n"
        "    -->\n"
    ),
    "DepositionInputFile": (
        "    <!--\n"
        "    Parameter     :  DepositionInputFile\n"
        "    Description   :  File path to a CSV file containing predefined spray-drift depositions per reach and day in g/ha\n"
        "    Values        :  A valid file path or left empty\n"
        "    Remark        :  Setting this parameter to a file path will use depositions as specified in the file. The\n"
        "                     SprayDrift component is not run in this case.\n"
        "    -->\n"
    ),
    "RunStepsRiverNetwork": (
        "    <!--\n"
        "    Parameter     :  RunStepsRiverNetwork\n"
        "    Description   :  Controls whether environmental fate is calculated by the StepsRiverNetwork component\n"
        "    Values        :  true or false\n"
        "      -->\n"
    ),
    "RunCascadeToxswa": (
        "    <!--\n"
        "    Parameter     :  RunCascadeToxswa\n"
        "    Description   :  Controls whether environmental fate is calculated by the CascadeToxswa component\n"
        "    Values        :  true or false\n"
        "      -->\n"
    ),
    "ThresholdSW": (
        "    <!--\n"
        "    Parameter     :  ThresholdSW\n"
        "    Description   :  The surface water value precision used by StepsRiverNetwork\n"
        "    Values        :  A positive number\n"
        "    Remark        :  Only applies if RunStepsRiverNetwork is true.\n"
        "    -->\n"
    ),
    "ThresholdSediment": (
        "    <!--\n"
        "    Parameter     :  ThresholdSediment\n"
        "    Description   :  The sediment value precision used by StepsRiverNetwork\n"
        "    Values        :  A positive number\n"
        "    Remark        :  Only applies if RunStepsRiverNetwork is true.\n"
        "    -->\n"
    ),
    "MolarMass": (
        "    <!--\n"
        "    Parameter     :  MolarMass\n"
        "    Description   :  The molar mass of the applied substance\n"
        "    Values        :  A value in g/mol\n"
        "    -->\n"
    ),
    "SaturatedVapourPressure": (
        "    <!--\n"
        "    Parameter     :  SaturatedVapourPressure\n"
        "    Description   :  The saturated vapour pressure of the applied substance at 20\u00b0C\n"
        "    Values        :  A value in Pa\n"
        "    -->\n"
    ),
    "MolarEnthalpyOfVaporization": (
        "    <!--\n"
        "    Parameter     :  MolarEnthalpyOfVaporization\n"
        "    Description   :  The molar enthalpy of vaporization of the applied substance\n"
        "    Values        :  A value in kJ/mol\n"
        "    -->\n"
    ),
    "SolubilityInWater": (
        "    <!--\n"
        "    Parameter     :  SolubilityInWater\n"
        "    Description   :  The solubility in water of the applied substance at 20 \u00b0C\n"
        "    Values        :  A concentration (mg/l).\n"
        "    -->\n"
    ),
    "MolarEnthalpyOfDissolution": (
        "    <!--\n"
        "    Parameter     :  MolarEnthalpyOfDissolution\n"
        "    Description   :  The molar enthalpy of dissolution of the applied substance\n"
        "    Values        :  A value in kJ/mol\n"
        "    -->\n"
    ),
    "DiffusionCoefficient": (
        "    <!--\n"
        "    Parameter     :  DiffusionCoefficient\n"
        "    Description   :  The diffusion coefficient of the applied substance at 20 \u00b0C\n"
        "    Values        :  A value in m\u00b2/d\n"
        "    -->\n"
    ),
    "DT50sw": (
        "    <!--\n"
        "    Parameter     :  DT50sw\n"
        "    Description   :  The half-life transformation time in water of the applied substance at 20 \u00b0C\n"
        "    Values        :  A value in d\n"
        "    -->\n"
    ),
    "MolarActivationEnthalpyOfTransformationInWater": (
        "    <!--\n"
        "    Parameter     :  MolarActivationEnthalpyOfTransformationInWater\n"
        "    Description   :  The molar activation enthalpy for transformation in water of the applied substance\n"
        "    Values        :  A value in kJ/mol\n"
        "    -->\n"
    ),
    "DT50sed": (
        "    <!--\n"
        "    Parameter     :  DT50sed\n"
        "    Description   :  The half-life transformation time in sediment of the applied substance at 20 \u00b0C\n"
        "    Values        :  A value in d\n"
        "    -->\n"
    ),
    "MolarActivationEnthalpyOfTransformationInSediment": (
        "    <!--\n"
        "    Parameter     :  MolarActivationEnthalpyOfTransformationInSediment\n"
        "    Description   :  The molar activation enthalpy for transformation in sediment of the applied substance\n"
        "    Values        :  A value in kJ/mol\n"
        "    -->\n"
    ),
    "KOC": (
        "    <!--\n"
        "    Parameter     :  KOC\n"
        "    Description   :  The organic carbon-water partition coefficient of the applied substance\n"
        "    Values        :  A value in l/kg\n"
        "    Remark        :  The KOM used by some modules is automatically derived from the KOC by dividing it by 1.742\n"
        "    -->\n"
    ),
    "ReferenceConcentrationForKOC": (
        "    <!--\n"
        "    Parameter     :  ReferenceConcentrationForKOC\n"
        "    Description   :  The reference concentration for the KOC of the applied substance\n"
        "    Values        :  A concentration (mg/l)\n"
        "    -->\n"
    ),
    "FreundlichExponentInSedimentAndSuspendedParticles": (
        "    <!--\n"
        "    Parameter     :  FreundlichExponentInSedimentAndSuspendedParticles\n"
        "    Description   :  The Freundlich exponent in sediment and suspended particles of the applied substance\n"
        "    Values        :  A value without unit\n"
        "    -->\n"
    ),
    "CoefficientForLinearAdsorptionOnMacrophytes": (
        "    <!--\n"
        "    Parameter     :  CoefficientForLinearAdsorptionOnMacrophytes\n"
        "    Description   :  The coefficient for linear adsorption on macrophytes of the applied substance\n"
        "    Values        :  A value in l/kg\n"
        "    -->\n"
    ),
    "RunLGuts": (
        "    <!--\n"
        "    Parameter     :  RunLGuts\n"
        "    Description   :  Specifies whether effects are simulated by LGuts for the switched-on environmental fate components\n"
        "    Values        :  true or false\n"
        "    -->\n"
    ),
    "NumberOfWarmUpYears": (
        "    <!--\n"
        "    Parameter     : NumberOfWarmupYears\n"
        "    Description   : Number of years to run LPop in order to get stable population cycles\n"
        "    Values        : Integer value\n"
        "    -->\n"
    ),
    "RecoveryPeriodYears": (
        "    <!--\n"
        "    Parameter     : RecoveryPeriodYears\n"
        "    Description   : Number of years to run LPop after last year of application to allow for recovery\n"
        "    Values        : Integer value\n"
        "    -->\n"
    ),
    "NumberMC": (
        "    <!--\n"
        "    Parameter     :  NumberMC\n"
        "    Description   :  The number of Monte Carlo runs\n"
        "    Values        :  Any positive integer\n"
        "    -->\n"
    ),
    "ExportToSqlite": (
        "    <!--\n"
        "    Parameter     :  ExportToSqlite\n"
        "    Description   :  Specifies whether PECs and individual effects are exported into a SQLite database\n"
        "    Values        :  true or false\n"
        "    -->\n"
    ),
    "ReportingReaches": (
        "      <!--\n"
        "      Parameter     :  ReportingReaches\n"
        "      Description   :  Pre-selection of reaches for which detailed reports are generated\n"
        "      Values        :  Comma-separated reach identifiers in the format rXYZ (no spaces)\n"
        "      -->\n"
    ),
    "PecDisplayedTime": (
        "      <!--\n"
        "      Parameter     :  PecDisplayedTime\n"
        "      Description   :  The PEC value that is plotted for each enabled environmental fate component\n"
        "      Values        :  A date and time of format YYYY-MM-DD HH:MM within the valid range of dates\n"
        "      -->\n"
    ),
}
# fmt: on


# ---------------------------------------------------------------------------
#  YAML quoting helpers
# ---------------------------------------------------------------------------
# Values that should be quoted in YAML to preserve formatting / avoid
# mis-interpretation by yaml parsers
_NEEDS_YAML_QUOTE_RE = re.compile(
    r"^\d{4}-\d{2}-\d{2}"  # date or datetime
    r"|^\d{2}-\d{2}\s+to\s+"  # application time windows
    r"|[eE][+-]?\d+$"       # scientific notation (trailing)
    r"|^\s*$"                # blank / empty
    r"|^\d[\d ]+\d$"        # space-separated numbers (reach lists)
)


def _yaml_value(value: str) -> str:
    """Return *value* formatted for YAML output (with quoting where needed)."""
    if value == "":
        return ""
    # Booleans – emit lowercase unquoted
    if value.lower() in ("true", "false"):
        return value.lower()
    # Scientific notation with uppercase E – quote to preserve casing
    if re.search(r"[eE][+-]?\d+", value):
        return f'"{value}"'
    # Dates, time windows, reach lists containing spaces
    if _NEEDS_YAML_QUOTE_RE.search(value):
        return f'"{value}"'
    return value


# ---------------------------------------------------------------------------
#  Parse old xrun
# ---------------------------------------------------------------------------
def parse_old_xrun(filepath: str) -> Dict[str, OrderedDict]:
    """
    Parse an old (or partially converted) xrun file.

    Returns a dict  section_name → OrderedDict(param_name → text_value).
    ``CropStage`` is automatically renamed to ``RautmannClass`` with the
    appropriate value mapping applied.
    """
    tree = ET.parse(filepath)
    root = tree.getroot()

    sections: Dict[str, OrderedDict] = OrderedDict()

    for section_elem in root:
        section_local = section_elem.tag.replace(f"{{{NS}}}", "")

        # ---- Section rename (e.g. SimulationInfo → Control) ----
        section_local = SECTION_RENAMES.get(section_local, section_local)

        params: OrderedDict = OrderedDict()

        for param_elem in section_elem:
            param_local = param_elem.tag.replace(f"{{{NS}}}", "")
            text = (param_elem.text or "").strip()

            # ---- Drop removed parameters ----
            if param_local in DROPPED_PARAMS:
                continue

            # ---- CropStage → RautmannClass conversion ----
            if param_local == "CropStage":
                mapped = CROP_STAGE_MAP.get(text, text)
                if mapped == text and text not in CROP_STAGE_MAP.values():
                    print(f"  WARNING: Unknown CropStage value '{text}'; "
                          f"passing through as RautmannClass={text}")
                params["RautmannClass"] = mapped
            else:
                # ---- Parameter renames ----
                param_local = PARAM_RENAMES.get(param_local, param_local)
                params[param_local] = text

        sections[section_local] = params

    # ---- Move parameters between sections (e.g. NumberMC: Settings → Control) ----
    for param_name, target_section in PARAM_SECTION_MOVES.items():
        for sec_name, sec_params in list(sections.items()):
            if sec_name != target_section and param_name in sec_params:
                value = sec_params.pop(param_name)
                if target_section not in sections:
                    sections[target_section] = OrderedDict()
                sections[target_section][param_name] = value

    return sections


# ---------------------------------------------------------------------------
#  Emit new-format xrun
# ---------------------------------------------------------------------------
def _xrun_param_line(name: str, value: str, indent: str = "    ") -> str:
    """Return a single XML element line for a parameter."""
    if value == "":
        return f"{indent}<{name}/>\n"
    return f"{indent}<{name}>{value}</{name}>\n"


def write_new_xrun(sections: Dict[str, OrderedDict], outpath: str) -> None:
    """Write the new-format .xrun file."""
    lines: List[str] = []
    lines.append('<?xml version="1.0" encoding="utf-8"?>\n')
    lines.append("<Parameters\n")
    lines.append('        xmlns="urn:xAquaticRisk"\n')
    lines.append('        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n')
    lines.append('        xsi:schemaLocation="urn:xAquaticRisk model/variant/parameters.xsd"\n')
    lines.append(">\n")

    for sec_name in SECTION_ORDER:
        sec_params = sections.get(sec_name)
        if sec_params is None:
            continue

        lines.append(f"  <{sec_name}>\n")

        # Determine parameter order: canonical first, then any extras
        canonical = PARAM_ORDER.get(sec_name, [])
        written = set()
        ordered_names: List[str] = []
        for p in canonical:
            if p in sec_params:
                ordered_names.append(p)
                written.add(p)
        for p in sec_params:
            if p not in written:
                ordered_names.append(p)

        for param_name in ordered_names:
            value = sec_params[param_name]
            # Comment block (if we have one)
            comment = COMMENTS.get(param_name)
            if comment:
                lines.append("\n")
                lines.append(comment)
            # Parameter element
            lines.append(_xrun_param_line(param_name, value))

        lines.append(f"  </{sec_name}>\n")

    lines.append("</Parameters>\n")

    with open(outpath, "w", encoding="utf-8") as fh:
        fh.writelines(lines)


# ---------------------------------------------------------------------------
#  Emit YAML
# ---------------------------------------------------------------------------
# YAML comment templates (short form – one line per param)
YAML_COMMENTS: Dict[str, str] = {
    "ExperimentID":                          "# A unique identifier of a simulation run",
    "ParentRun":                      "# A file pattern of potential parents for this run (leave empty to disable)",
    "NumberParallelProcesses":        "# The number of Monte Carlo runs that are conducted simultaneously",
    "CascadeToxswaWorkers":           "# The number of processes spawned by CascadeToxswa",
    "DeleteComponentProcessingFolders": "# Delete processing folders after the model run is finished (true/false)",
    "VerboseLogging":                 "# Enable verbose logging (true/false)",
    "EnableProfiling":                "# Enable performance profiling (true/false)",
    "ProfilingWaitingTime":           "# Time interval in seconds between profiling rounds",
    "LandscapeScenario":              "# The scenario used by the simulation (scenario/<sub-folder>)",
    "SimulationStart":                "# The first date that is simulated (YYYY-MM-DD)",
    "SimulationEnd":                  "# The last date that is simulated (YYYY-MM-DD)",
    "ApplicationRate":                "# The application rate in g/ha",
    "ApplicationTimeWindow":          "# Time windows: MM-DD to MM-DD[, MM-DD to MM-DD[, ...]]",
    "InCropBuffer":                   "# In-crop buffer not applied in meters",
    "TechnologyDriftReduction":       "# Drift-reducing nozzle effect (0 = none, 1 = full filtering)",
    "RautmannClass":                  "# Rautmann drift class: orchards.early, orchards.late or arable",
    "DepositionInputFile":            "# CSV file with predefined spray-drift depositions (leave empty to disable)",
    "RunStepsRiverNetwork":           "# Run StepsRiverNetwork environmental fate (true/false)",
    "RunCascadeToxswa":               "# Run CascadeToxswa environmental fate (true/false)",
    "ThresholdSW":                    "# Surface water value precision (StepsRiverNetwork)",
    "ThresholdSediment":              "# Sediment value precision (StepsRiverNetwork)",
    "MolarMass":                      "# Molar mass of the applied substance (g/mol)",
    "SaturatedVapourPressure":        "# Saturated vapour pressure at 20 deg C (Pa)",
    "MolarEnthalpyOfVaporization":    "# Molar enthalpy of vaporization (kJ/mol)",
    "SolubilityInWater":              "# Solubility in water at 20 deg C (mg/l)",
    "MolarEnthalpyOfDissolution":     "# Molar enthalpy of dissolution (kJ/mol)",
    "DiffusionCoefficient":           "# Diffusion coefficient at 20 deg C (m2/d)",
    "DT50sw":                         "# Half-life transformation time in water at 20 deg C (d)",
    "MolarActivationEnthalpyOfTransformationInWater":    "# Molar activation enthalpy for transformation in water (kJ/mol)",
    "DT50sed":                        "# Half-life transformation time in sediment at 20 deg C (d)",
    "MolarActivationEnthalpyOfTransformationInSediment": "# Molar activation enthalpy for transformation in sediment (kJ/mol)",
    "KOC":                            "# Organic carbon-water partition coefficient (l/kg)",
    "ReferenceConcentrationForKOC":   "# Reference concentration for KOC (mg/l)",
    "FreundlichExponentInSedimentAndSuspendedParticles": "# Freundlich exponent in sediment and suspended particles",
    "CoefficientForLinearAdsorptionOnMacrophytes":       "# Coefficient for linear adsorption on macrophytes (l/kg)",
    "RunLGuts":                       "# Simulate effects with LGuts (true/false)",
    "NumberOfWarmUpYears":            "# Number of LPop warm-up years for stable population cycles",
    "RecoveryPeriodYears":            "# Number of years after last application to allow recovery",
    "NumberMC":                       "# Number of Monte Carlo runs",
    "ExportToSqlite":                 "# Export PECs and effects to SQLite database (true/false)",
    "ReportingReaches":               "# Reaches for detailed reports (comma-separated, e.g. r577)",
    "PecDisplayedTime":               "# PEC time plotted per environmental fate component (YYYY-MM-DD HH:MM)",
}

# Section header comments
YAML_SECTION_HEADERS: Dict[str, str] = {
    "Control":           "# --- Control ---",
    "Scenario":          "# --- Scenario ---",
    "PppUse":            "# --- PPP Use ---",
    "Mitigation":        "# --- Mitigation ---",
    "Exposure":          "# --- Exposure ---",
    "EnvironmentalFate": "# --- Environmental Fate ---",
    "Effects":           "# --- Effects ---",
    "Settings":          "# --- Settings ---",
    "Analysis":          "# --- Analysis ---",
}

# Species parameters that get a sub-heading
_SPECIES_HEADING = {
    "Species1": "# --- Species 1 ---",
    "Species2": "# --- Species 2 ---",
    "Species3": "# --- Species 3 ---",
}


def write_yaml(sections: Dict[str, OrderedDict], outpath: str) -> None:
    """Write the .yaml parameterisation file."""
    lines: List[str] = []
    lines.append("# =============================================================================\n")
    lines.append("# xAquaticRisk Simulation Parameters\n")
    lines.append("# =============================================================================\n")
    lines.append(f"# Converted from .xrun by convert_xrun.py\n")
    lines.append("# =============================================================================\n")
    lines.append("\n")

    for sec_name in SECTION_ORDER:
        sec_params = sections.get(sec_name)
        if sec_params is None:
            continue

        header = YAML_SECTION_HEADERS.get(sec_name, f"# --- {sec_name} ---")
        lines.append(f"{header}\n")
        lines.append(f"{sec_name}:\n")

        # Determine parameter order
        canonical = PARAM_ORDER.get(sec_name, [])
        written = set()
        ordered_names: List[str] = []
        for p in canonical:
            if p in sec_params:
                ordered_names.append(p)
                written.add(p)
        for p in sec_params:
            if p not in written:
                ordered_names.append(p)

        for param_name in ordered_names:
            value = sec_params[param_name]
            yaml_val = _yaml_value(value)

            # Species sub-headings
            if param_name in _SPECIES_HEADING:
                lines.append(f"\n  {_SPECIES_HEADING[param_name]}\n")

            # Comment (for non-species detail params)
            comment = YAML_COMMENTS.get(param_name)
            if comment and param_name not in _SPECIES_HEADING:
                lines.append(f"  {comment}\n")

            if yaml_val == "":
                lines.append(f"  {param_name}:\n")
            else:
                lines.append(f"  {param_name}: {yaml_val}\n")

        lines.append("\n")

    with open(outpath, "w", encoding="utf-8") as fh:
        fh.writelines(lines)


# ---------------------------------------------------------------------------
#  CLI
# ---------------------------------------------------------------------------
def main() -> None:
    parser = argparse.ArgumentParser(
        description="Convert old-format xAquaticRisk .xrun files to the new "
                    "format (Control first, RautmannClass) and emit a "
                    "parallel .yaml parameterisation file.",
    )
    parser.add_argument(
        "input",
        help="Path to the old .xrun file to convert",
    )
    parser.add_argument(
        "--outdir",
        default=None,
        help="Directory for the output files (default: parameterisation/ subfolder next to this script)",
    )
    parser.add_argument(
        "--suffix",
        default="",
        help="Optional suffix appended to the output base name, e.g. '_new'",
    )
    parser.add_argument(
        "--no-xrun",
        action="store_true",
        help="Skip generating the .xrun output (only produce .yaml)",
    )
    parser.add_argument(
        "--no-yaml",
        action="store_true",
        help="Skip generating the .yaml output (only produce .xrun)",
    )

    args = parser.parse_args()

    inpath = Path(args.input).resolve()
    if not inpath.exists():
        print(f"ERROR: Input file not found: {inpath}", file=sys.stderr)
        sys.exit(1)

    _default_outdir = Path(__file__).resolve().parent / "parameterisation"
    outdir = Path(args.outdir) if args.outdir else _default_outdir
    outdir.mkdir(parents=True, exist_ok=True)

    stem = inpath.stem + args.suffix
    xrun_out = outdir / f"{stem}.xrun"
    yaml_out = outdir / f"{stem}.yaml"

    # ---- Parse ----
    print(f"Parsing  : {inpath}")
    sections = parse_old_xrun(str(inpath))

    # Quick summary
    n_params = sum(len(v) for v in sections.values())
    sec_names = ", ".join(sections.keys())
    print(f"  Found {n_params} parameters in sections: {sec_names}")

    # Check for conversions applied
    for sec_params in sections.values():
        if "RautmannClass" in sec_params:
            print(f"  RautmannClass = {sec_params['RautmannClass']}")
            break

    # ---- Write outputs ----
    if not args.no_xrun:
        write_new_xrun(sections, str(xrun_out))
        print(f"Written  : {xrun_out}")

    if not args.no_yaml:
        write_yaml(sections, str(yaml_out))
        print(f"Written  : {yaml_out}")

    print("Done.")


if __name__ == "__main__":
    main()
