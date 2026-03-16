@echo off
setlocal

REM ─────────────────────────────────────────────────────────────────────────────
REM  setup_analysis_python.bat
REM
REM  Creates a dedicated embedded Python runtime for analysis in
REM  analysis\python\ and installs the packages required by
REM  run_basic_analysis.py into that runtime.
REM
REM  After this script completes, the analysis step is fully self-contained:
REM  no system Python or installed software is needed on the target machine.
REM
REM  To reinstall or upgrade, delete  analysis\python\  and re-run this script.
REM ─────────────────────────────────────────────────────────────────────────────

set PYTHON_VERSION=3.9.7
set PYTHON_URL=https://www.python.org/ftp/python/%PYTHON_VERSION%/python-%PYTHON_VERSION%-embed-amd64.zip
set GET_PIP_URL=https://bootstrap.pypa.io/get-pip.py

set PYTHON_DIR=%~dp0analysis\python
set REQUIREMENTS=%~dp0analysis\requirements.txt

echo ============================================================
echo  xAquaticRisk – Analysis Python Setup
echo  Python  : %PYTHON_VERSION% embeddable package
echo  Target  : %PYTHON_DIR%
echo ============================================================
echo.

if not exist "%REQUIREMENTS%" (
    echo ERROR: Requirements file not found.
    echo Expected: %REQUIREMENTS%
    pause
    exit /b 1
)

if exist "%PYTHON_DIR%\python.exe" (
    echo Analysis Python already installed at:
    echo   %PYTHON_DIR%\python.exe
    echo Delete analysis\python\ and re-run this script to reinstall.
    echo.
    pause
    exit /b 0
)

REM ── Step 1: Download embeddable Python ───────────────────────────────────────
echo [1/4] Downloading Python %PYTHON_VERSION% embeddable package...
powershell -NoProfile -Command ^
    "Invoke-WebRequest -Uri '%PYTHON_URL%' -OutFile '%TEMP%\xaq-analysis-python.zip' -UseBasicParsing"
if errorlevel 1 (
    echo.
    echo ERROR: Download failed. Check your internet connection.
    pause
    exit /b 1
)

REM ── Step 2: Extract runtime ──────────────────────────────────────────────────
echo [2/4] Extracting to %PYTHON_DIR% ...
if exist "%PYTHON_DIR%" rmdir /s /q "%PYTHON_DIR%"
mkdir "%PYTHON_DIR%" 2>nul
powershell -NoProfile -Command ^
    "Expand-Archive -Path '%TEMP%\xaq-analysis-python.zip' -DestinationPath '%PYTHON_DIR%' -Force"
del "%TEMP%\xaq-analysis-python.zip"

REM ── Step 3: Enable site-packages and install pip ─────────────────────────────
echo [3/4] Enabling site-packages and installing pip...
mkdir "%PYTHON_DIR%\Lib\site-packages" 2>nul
for %%f in ("%PYTHON_DIR%\python3*._pth") do (
    powershell -NoProfile -Command ^
        "$content = Get-Content '%%f';" ^
        "$content = $content | Where-Object { $_ -ne 'Lib\\site-packages' };" ^
        "$content += 'Lib\\site-packages';" ^
        "$content = $content -replace '#import site', 'import site';" ^
        "Set-Content '%%f' $content"
)
powershell -NoProfile -Command ^
    "Invoke-WebRequest -Uri '%GET_PIP_URL%' -OutFile '%PYTHON_DIR%\get-pip.py' -UseBasicParsing"
if errorlevel 1 (
    echo.
    echo ERROR: Failed to download get-pip.py.
    pause
    exit /b 1
)
"%PYTHON_DIR%\python.exe" "%PYTHON_DIR%\get-pip.py" --no-warn-script-location --quiet
if errorlevel 1 (
    echo.
    echo ERROR: Failed to install pip into the embedded analysis runtime.
    pause
    exit /b 1
)
del "%PYTHON_DIR%\get-pip.py"

REM ── Step 4: Install analysis packages ────────────────────────────────────────
echo [4/4] Installing analysis packages (this may take a few minutes)...
"%PYTHON_DIR%\python.exe" -m pip install ^
    --upgrade ^
    --only-binary=:all: ^
    -r "%REQUIREMENTS%" ^
    --no-warn-script-location
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
echo  Analysis Python runtime is ready at:
echo    %PYTHON_DIR%
echo.
echo  Important: include the entire analysis\python\ folder when copying
echo  or packaging xAquaticRisk for xcopy deployment.
echo ============================================================
pause
