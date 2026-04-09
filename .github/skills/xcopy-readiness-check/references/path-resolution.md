# Path Resolution Strategies

Landscape models must resolve paths dynamically to work on any system. This guide covers patterns for Python, batch scripts, configuration files, and XML.

## Python Path Resolution

### Pattern 1: Relative to Script Location (Recommended)

```python
import os

# Get the directory of the current script
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# Resolve paths relative to project root
PROJECT_ROOT = os.path.dirname(os.path.dirname(SCRIPT_DIR))
SCENARIO_DIR = os.path.join(PROJECT_ROOT, "scenario")
RUN_DIR = os.path.join(PROJECT_ROOT, "run")

# Example usage
def load_scenario(scenario_name):
    scenario_path = os.path.join(SCENARIO_DIR, scenario_name)
    if not os.path.exists(scenario_path):
        raise FileNotFoundError(f"Scenario not found: {scenario_path}")
    return scenario_path
```

### Pattern 2: Environment Variables

```python
import os

# Allow override via environment variable
PROJECT_ROOT = os.environ.get("PROJECT_ROOT", os.path.dirname(os.path.abspath(__file__)))
SCENARIO_DIR = os.path.join(PROJECT_ROOT, "scenario")

# Usage in setup_*.bat:
# set PROJECT_ROOT=%cd%
# python script.py
```

### Pattern 3: Package Discovery

```python
import pkg_resources

# For installed packages (if your model is pip-installable)
model_data = pkg_resources.resource_filename("xaquaticrisk", "data")
```

## Batch Script Path Resolution

### Pattern 1: Using %~dp0 (Directory of Current Script)

```batch
REM Get the directory where this batch file is located
set SCRIPT_DIR=%~dp0

REM Navigate to project root (if script is in a subfolder)
cd /d "%SCRIPT_DIR%"

REM Define relative paths from here
set PROJECT_ROOT=%cd%
set SCENARIO_DIR=%PROJECT_ROOT%\scenario
set RUN_DIR=%PROJECT_ROOT%\run

REM Pass to Python
cd /d "%PROJECT_ROOT%"
python -c "import os; print(os.environ.get('PROJECT_ROOT'))"
```

### Pattern 2: Flexible Working Directory

```batch
@echo off
REM Don't assume the script is run from project root

if "%PROJECT_ROOT%"=="" (
    REM If not set, derive from script location
    cd /d "%~dp0"
    set PROJECT_ROOT=%cd%
)

echo Project Root: %PROJECT_ROOT%
```

## YAML/JSON Configuration Files

### Anti-Pattern (Don't Do This):

```yaml
# ❌ BAD: Hard-coded paths
ExperimentID: "test_run_1"
LandscapeScenario: "C:\\Users\\john\\xAquaticRisk\\scenario\\muenster"
RunDir: "\\\\server\\shared\\runs"
```

### Pattern 1: Relative Paths (Recommended)

```yaml
# ✓ GOOD: Relative to config file location
ExperimentID: "test_run_1"
LandscapeScenario: "../scenario/muenster"
RunDir: "../run"
```

### Pattern 2: Environment Variable Substitution

```yaml
# ✓ GOOD: Use environment variables
ExperimentID: "test_run_1"
LandscapeScenario: "${PROJECT_ROOT}/scenario/muenster"
RunDir: "${PROJECT_ROOT}/run"
NumberMC: "100"
```

**In Python:**
```python
import yaml
import os

def load_config(config_file):
    with open(config_file) as f:
        config = yaml.safe_load(f)
    
    # Resolve environment variables
    config = {
        k: os.path.expandvars(v) if isinstance(v, str) else v
        for k, v in config.items()
    }
    return config
```

### Pattern 3: Resolve Relative to Config File Location

```python
import yaml
import os

def load_config(config_file):
    config_dir = os.path.dirname(os.path.abspath(config_file))
    
    with open(config_file) as f:
        config = yaml.safe_load(f)
    
    # Resolve relative paths to config directory
    for key in ['LandscapeScenario', 'RunDir']:
        if key in config:
            path = config[key]
            if not os.path.isabs(path):
                config[key] = os.path.join(config_dir, path)
                config[key] = os.path.normpath(config[key])
    
    return config
```

## XML Configuration Files

### Pattern 1: Relative Paths Using XPath

```xml
<?xml version="1.0"?>
<Configuration>
    <!-- ✓ GOOD: Relative paths -->
    <ScenarioPath>../scenario/muenster</ScenarioPath>
    <RunPath>../run</RunPath>
    
    <!-- ✓ GOOD: Use placeholders for env vars -->
    <OutputDir>${OUTPUT_ROOT}/results</OutputDir>
</Configuration>
```

### Pattern 2: Python XML Processing

```python
import xml.etree.ElementTree as ET
import os

def load_xml_config(xml_file):
    tree = ET.parse(xml_file)
    root = tree.getroot()
    
    config_dir = os.path.dirname(os.path.abspath(xml_file))
    
    # Extract paths and resolve relative ones
    scenario = root.find("ScenarioPath").text
    if not os.path.isabs(scenario):
        scenario = os.path.join(config_dir, scenario)
    
    return {
        "scenario": os.path.normpath(scenario),
        "output": os.path.join(config_dir, root.find("OutputDir").text)
    }
```

## C# / .NET Path Resolution

```csharp
// Get assembly directory
string assemblyDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
string projectRoot = Path.GetFullPath(Path.Combine(assemblyDir, ".."));

// Resolve relative paths
string scenarioPath = Path.Combine(projectRoot, "scenario", "muenster");
string runDir = Path.Combine(projectRoot, "run");
```

## Validation: Testing Path Resolution

Add a test to your setup that validates paths work:

```python
def validate_paths():
    """Verify all critical paths are accessible."""
    paths = {
        "PROJECT_ROOT": PROJECT_ROOT,
        "SCENARIO_DIR": SCENARIO_DIR,
        "RUN_DIR": RUN_DIR,
        "MODEL_DIR": MODEL_DIR,
    }
    
    for name, path in paths.items():
        if not os.path.exists(path):
            raise RuntimeError(f"{name} not found: {path}")
    
    print("✓ All paths validated")

# Call after setup
validate_paths()
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "ModuleNotFoundError" when using relative paths | Ensure script is run from correct working directory; use `sys.path.insert(0, ...)` |
| Environment variables not expanded in YAML | Use `os.path.expandvars()` or `string.Template` |
| Relative paths work locally but not in a scheduled task | Task may run from different working dir; use absolute derived from script location |
| UNC paths break on different network | Use file:// URLs or pass network shares as parameters |
