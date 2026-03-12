@echo off
setlocal

REM ─────────────────────────────────────────────────────────────────────────────
REM  setup_analysis_python.bat
REM
REM  Downloads a portable Python and installs the packages required by
REM  run_basic_analysis.py into  analysis\python\
REM
REM  After this script completes, the analysis step is fully self-contained:
REM  no system Python or installed software is needed on the target machine.
REM
REM  To reinstall or upgrade, delete  analysis\python\  and re-run this script.
REM ─────────────────────────────────────────────────────────────────────────────

set PYTHON_VERSION=3.12.8
set PYTHON_URL=https://www.python.org/ftp/python/%PYTHON_VERSION%/python-%PYTHON_VERSION%-embed-amd64.zip
set GET_PIP_URL=https://bootstrap.pypa.io/get-pip.py

set PYTHON_DIR=%~dp0analysis\python
set REQUIREMENTS=%~dp0analysis\requirements.txt

echo ============================================================
echo  xAquaticRisk – Analysis Python Setup
echo  Target  : %PYTHON_DIR%
echo  Python  : %PYTHON_VERSION%
echo ============================================================
echo.

if exist "%PYTHON_DIR%\python.exe" (
    echo Analysis Python already installed.
    echo Delete  analysis\python\  and re-run to reinstall.
    echo.
    pause
    exit /b 0
)

REM ── Step 1: Download embeddable Python ───────────────────────────────────────
echo [1/4] Downloading Python %PYTHON_VERSION% embeddable package...
powershell -NoProfile -Command ^
    "Invoke-WebRequest -Uri '%PYTHON_URL%' -OutFile '%TEMP%\python-embed.zip' -UseBasicParsing"
if errorlevel 1 (
    echo.
    echo ERROR: Download failed. Check your internet connection.
    pause
    exit /b 1
)

REM ── Step 2: Extract ──────────────────────────────────────────────────────────
echo [2/4] Extracting to %PYTHON_DIR% ...
mkdir "%PYTHON_DIR%" 2>nul
powershell -NoProfile -Command ^
    "Expand-Archive -Path '%TEMP%\python-embed.zip' -DestinationPath '%PYTHON_DIR%' -Force"
del "%TEMP%\python-embed.zip"

REM ── Step 3: Enable site-packages (required for pip to work) ──────────────────
echo [3/4] Enabling site-packages...
for %%f in ("%PYTHON_DIR%\python3*._pth") do (
    powershell -NoProfile -Command ^
        "(Get-Content '%%f') -replace '#import site', 'import site' | Set-Content '%%f'"
)

REM Install pip via get-pip.py
powershell -NoProfile -Command ^
    "Invoke-WebRequest -Uri '%GET_PIP_URL%' -OutFile '%PYTHON_DIR%\get-pip.py' -UseBasicParsing"
"%PYTHON_DIR%\python.exe" "%PYTHON_DIR%\get-pip.py" --no-warn-script-location --quiet
del "%PYTHON_DIR%\get-pip.py"

REM ── Step 4: Install analysis packages ────────────────────────────────────────
echo [4/4] Installing analysis packages (this may take a few minutes)...
"%PYTHON_DIR%\python.exe" -m pip install -r "%REQUIREMENTS%" --no-warn-script-location
if errorlevel 1 (
    echo.
    echo ERROR: Package installation failed.
    echo Check the error messages above and re-run after resolving them.
    pause
    exit /b 1
)

echo.
echo ============================================================
echo  Setup complete!
echo  Analysis Python is ready at:
echo    %PYTHON_DIR%
echo ============================================================
pause
