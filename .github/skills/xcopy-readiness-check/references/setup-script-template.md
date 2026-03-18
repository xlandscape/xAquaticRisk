# Setup Script Template

A proper xcopy-ready setup script ensures fresh installations work automatically.

## Master Setup Script

**File:** `setup_all_runtimes.bat` (or `.sh` for Linux)

```batch
@echo off
REM Master setup for xcopy-ready model deployment

setlocal enabledelayedexpansion
cd /d "%~dp0"
set PROJECT_ROOT=%cd%

echo.
echo ========== xAquaticRisk Setup ==========
echo Project Root: %PROJECT_ROOT%
echo.

REM Step 1: Create Python virtual environment
echo [1/4] Setting up Python environment...
if not exist .venv (
    python -m venv .venv
    if errorlevel 1 (
        echo Failed to create virtual environment
        exit /b 1
    )
)

call .venv\Scripts\activate.bat
python -m pip install --upgrade pip setuptools wheel
python -m pip install -r requirements.txt
if errorlevel 1 (
    echo Failed to install Python dependencies
    exit /b 1
)

REM Step 2: Download/extract binaries (if needed)
echo [2/4] Checking model binaries...
if not exist "model\core\bin\python-3.9.7-amd64" (
    echo Model binaries not found. Download from: https://releases.example.com/model-bins
    echo.
)

REM Step 3: Initialize data directories
echo [3/4] Initializing directories...
if not exist "run" mkdir run
if not exist "scenario" mkdir scenario
if not exist "parameterisation" mkdir parameterisation

REM Step 4: Validate setup
echo [4/4] Validating setup...
python -m py_compile controlpanel\server.py
if errorlevel 1 (
    echo Setup validation failed
    exit /b 1
)

echo.
echo ========== Setup Complete ==========
echo.
echo Next: Run model with:
echo   webui.bat           (web interface)
echo   - or -
echo   notebook.bat        (Jupyter notebooks)
echo.
pause
```

## Environment-Specific Setup Scripts

Create modular scripts that can be called independently:

**`setup_analysis_python.bat`:**
```batch
@echo off
cd /d "%~dp0"
call .venv\Scripts\activate.bat
pip install -r analysis\requirements.txt
echo Analysis environment ready.
```

**`setup_webui_python.bat`:**
```batch
@echo off
cd /d "%~dp0"
call .venv\Scripts\activate.bat
pip install -r webui\requirements.txt
echo Web UI environment ready.
```

**`setup_controlpanel_python.bat`:**
```batch
@echo off
cd /d "%~dp0"
call .venv\Scripts\activate.bat
pip install -r controlpanel\requirements.txt
echo Control Panel environment ready.
```

## Key Principles

1. **Relative Paths**: Use `%~dp0` (current script dir) or `cd` to project root
2. **Error Handling**: Check `errorlevel` after critical operations
3. **Idempotent**: Safe to run multiple times
4. **Clear Output**: Echo progress messages
5. **No Interaction Assumed**: Don't prompt for paths; auto-detect
6. **Dependency Order**: Create venv → install packages → download binaries → validate
7. **Documentation**: Include usage instructions at the end

## Validation Pattern

```batch
REM Validate critical components exist
if not exist ".venv\Scripts\python.exe" (
    echo ERROR: Python venv not found. Run setup_all_runtimes.bat first.
    exit /b 1
)
```

## Linux/macOS Equivalent

**`setup_all_runtimes.sh`:**
```bash
#!/bin/bash
set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_ROOT"

echo "========== xAquaticRisk Setup =========="
echo "Project Root: $PROJECT_ROOT"

# Step 1: Python venv
if [ ! -d ".venv" ]; then
    python3 -m venv .venv
fi

source .venv/bin/activate
pip install --upgrade pip setuptools
pip install -r requirements.txt

# Step 2: Initialize directories
mkdir -p run scenario parameterisation

# Step 3: Validate
python -m py_compile controlpanel/server.py

echo "========== Setup Complete =========="
```

## Testing the Setup Script

After creating, test on a fresh machine/container:

```batch
# Copy just the repo root, no hidden files or venv
xcopy /E /I C:\xAquaticRisk D:\TestFresh
cd D:\TestFresh
setup_all_runtimes.bat
webui.bat  # Or whatever starts the app
```

If it works without intervention, it's xcopy-ready.
