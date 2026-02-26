# xrun Converter

## Overview

`convert_xrun.py` is a Python utility that converts legacy xAquaticRisk parameterisation files (`.xrun`) from the old format to the new format, and simultaneously generates an equivalent YAML parameterisation file.

**Key conversions:**
- `<CropStage>` element → `<RautmannClass>` (with value mapping: `early` → `orchards.early`, `late` → `orchards.late`, `arable` passes through)
- `<SimulationInfo>` section moved from bottom to top (now the first section after `<Parameters>`, per XSD schema order)
- All comment blocks refreshed per current template style
- All 64 parameters preserved with original values

## Usage

### Basic invocation
```bash
python convert_xrun.py  <old_file.xrun>  [--outdir DIR]  [--suffix SUFFIX]  [--no-xrun]  [--no-yaml]
```

### Examples

**Default (overwrites .xrun, creates .yaml with same base name):**
```bash
python convert_xrun.py  muenster_old.xrun
# Output: muenster_old.xrun (new format) + muenster_old.yaml
```

**Append suffix to avoid overwriting:**
```bash
python convert_xrun.py  muenster_old.xrun  --suffix _new
# Output: muenster_old_new.xrun + muenster_old_new.yaml
```

**Redirect to a different folder:**
```bash
python convert_xrun.py  muenster_old.xrun  --outdir converted/
# Output: converted/muenster_old.xrun + converted/muenster_old.yaml
```

**Generate only .xrun (skip YAML):**
```bash
python convert_xrun.py  muenster_old.xrun  --no-yaml
```

**Generate only .yaml (skip .xrun):**
```bash
python convert_xrun.py  muenster_old.xrun  --no-xrun
```

## Command-line options

| Option | Description |
|--------|-------------|
| `--outdir DIR` | Directory for output files (default: same as input) |
| `--suffix SUFFIX` | Append suffix to output base name, e.g., `_new` |
| `--no-xrun` | Skip generating .xrun output (YAML only) |
| `--no-yaml` | Skip generating .yaml output (.xrun only) |

## Input format (old)

Old xrun files have:
- `<CropStage>` element (with values `early` or `late`) in the `<Exposure>` section
- `<SimulationInfo>` section at the **bottom** (after `<Analysis>`)
- Parameters spread across multiple sections in any order

Example old structure:
```xml
<?xml version="1.0"?>
<Parameters xmlns="urn:xAquaticRisk" ...>
  <Scenario>...</Scenario>
  <PppUse>...</PppUse>
  <!-- ... other sections ... -->
  <Analysis>...</Analysis>
  <SimulationInfo>
    <!-- Simulation settings at the end -->
  </SimulationInfo>
</Parameters>
```

## Output format (new)

Converted xrun files have:
- `<RautmannClass>` element (with values `orchards.early`, `orchards.late`, or `arable`) in the `<Exposure>` section
- `<SimulationInfo>` section at the **top** (first section, per XSD schema)
- All sections in canonical order: `SimulationInfo`, `Scenario`, `PppUse`, `Mitigation`, `Exposure`, `EnvironmentalFate`, `Effects`, `Settings`, `Analysis`
- Comment blocks for all parameters per current template style

Example new xrun structure:
```xml
<?xml version="1.0"?>
<Parameters xmlns="urn:xAquaticRisk" ...>
  <SimulationInfo>
    <!-- Simulation settings at the top -->
  </SimulationInfo>
  <Scenario>...</Scenario>
  <PppUse>...</PppUse>
  <!-- ... other sections in order ... -->
  <Analysis>...</Analysis>
</Parameters>
```

### YAML output

The generated `.yaml` file is an exact equivalent to the converted `.xrun`:
- Sections as top-level YAML keys
- Parameters as sub-keys within each section
- Inline comments per parameter
- Scientific notation (e.g., `1.10E-06`) and dates quoted to preserve formatting
- Booleans lowercase (`true`, `false`)

Example YAML excerpt:
```yaml
SimulationInfo:
  SimID: muenster_test
  NumberParallelProcesses: 3
  CascadeToxswaWorkers: 20
  DeleteFoldersAtFinish: true

Scenario:
  Project: scenario/muenster-T-Di-02.5-20220429-postproceccing-toxwa
  SimulationStart: "2015-05-01"
  SimulationEnd: "2015-05-07"

Exposure:
  RautmannClass: orchards.early
  DepositionInputFile:
```

## Technical details

### CropStage → RautmannClass mapping

| Old value | New value |
|-----------|-----------|
| `early` | `orchards.early` |
| `late` | `orchards.late` |
| `arable` | `arable` |

If an unknown value is encountered, it is passed through with a warning.

### Section ordering (XSD-compliant)

The converter respects the canonical section order defined in `parameters.xsd`:
1. `SimulationInfo`
2. `Scenario`
3. `PppUse`
4. `Mitigation`
5. `Exposure`
6. `EnvironmentalFate`
7. `Effects`
8. `Settings`
9. `Analysis`

### Parameter ordering within sections

Parameters within each section follow the order defined in the template files (`template.xrun`, `template.yaml`). Any unlisted parameters are appended at the end of their section in the order they appeared in the input.

## Requirements

- **Python** ≥ 3.6 (uses f-strings)
- **Built-in libraries only** (no external dependencies for basic functionality)
- PyYAML is optional; if not available, the script emits plain YAML via manual formatting

## Execution with bundled Python

xAquaticRisk includes a bundled Python 3.9.7 at `model\core\bin\python-3.9.7-amd64\python.exe`:

```powershell
# Windows PowerShell
model\core\bin\python-3.9.7-amd64\python.exe convert_xrun.py  muenster_old.xrun
```

## Example: batch conversion

To convert all old-format xrun files in a directory:

```powershell
# PowerShell
foreach ($file in Get-ChildItem *.xrun) {
    python convert_xrun.py $file --suffix _new
}
```

Or in bash:
```bash
for file in *.xrun; do
    python convert_xrun.py "$file" --suffix _new
done
```

## Validation

After conversion, the new `.xrun` file can be validated against the XSD schema via:
```bash
xmllint --schema model/variant/parameters.xsd  <converted_file.xrun>
```

Both the new `.xrun` and the original `.yaml` can be used interchangeably to start a simulation (see `model/core/init.py` for parameterisation loading logic).
