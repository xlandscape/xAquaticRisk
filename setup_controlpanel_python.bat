@echo off
setlocal

REM -----------------------------------------------------------------------------
REM  setup_controlpanel_python.bat
REM
REM  Creates a dedicated embedded Python runtime for controlpanel\server.py in
REM  controlpanel\python\ so the Control Panel has no dependency on model-core
REM  or system Python.
REM
REM  Maintainer utility: rebuild or repair the vendored runtime that is checked
REM  into the working tree for offline xcopy use. End users should receive the
REM  controlpanel\python\ folder already populated.
REM -----------------------------------------------------------------------------

set PYTHON_VERSION=3.9.7
set PYTHON_URL=https://www.python.org/ftp/python/%PYTHON_VERSION%/python-%PYTHON_VERSION%-embed-amd64.zip
set GET_PIP_URL=https://bootstrap.pypa.io/get-pip.py

set PYTHON_DIR=%~dp0controlpanel\python
set REQUIREMENTS=%~dp0controlpanel\requirements.txt

echo ============================================================
echo  xAquaticRisk - Control Panel Python Setup
echo  Python  : %PYTHON_VERSION% embeddable package
echo  Target  : %PYTHON_DIR%
echo  Purpose : maintainer rebuild of bundled runtime
echo ============================================================
echo.

if not exist "%REQUIREMENTS%" (
    echo ERROR: Requirements file not found.
    echo Expected: %REQUIREMENTS%
    pause
    exit /b 1
)

set NEED_DOWNLOAD=1
if exist "%PYTHON_DIR%\python.exe" (
    set NEED_DOWNLOAD=0
    echo Existing controlpanel runtime detected at:
    echo   %PYTHON_DIR%\python.exe
    echo Runtime will be validated and required packages will be repaired or upgraded.
    echo.
)

if "%NEED_DOWNLOAD%"=="1" (
    echo [1/4] Downloading Python %PYTHON_VERSION% embeddable package...
    powershell -NoProfile -Command ^
        "Invoke-WebRequest -Uri '%PYTHON_URL%' -OutFile '%TEMP%\xaq-controlpanel-python.zip' -UseBasicParsing"
    if errorlevel 1 (
        echo.
        echo ERROR: Download failed. Check your internet connection.
        pause
        exit /b 1
    )
) else (
    echo [1/4] Reusing existing embedded Python runtime...
)

if "%NEED_DOWNLOAD%"=="1" (
    echo [2/4] Extracting to %PYTHON_DIR% ...
    if exist "%PYTHON_DIR%" rmdir /s /q "%PYTHON_DIR%"
    mkdir "%PYTHON_DIR%" 2>nul
    powershell -NoProfile -Command ^
        "Expand-Archive -Path '%TEMP%\xaq-controlpanel-python.zip' -DestinationPath '%PYTHON_DIR%' -Force"
    del "%TEMP%\xaq-controlpanel-python.zip"
) else (
    echo [2/4] Keeping existing runtime files...
)

echo [3/4] Enabling standard site initialization and pip...
mkdir "%PYTHON_DIR%\Lib\site-packages" 2>nul
for %%f in ("%PYTHON_DIR%\python3*._pth") do (
    powershell -NoProfile -Command ^
        "$content = Get-Content '%%f';" ^
        "$content = $content | Where-Object { $_ -ne 'Lib\\site-packages' };" ^
        "$content += 'Lib\\site-packages';" ^
        "$content = $content -replace '#import site', 'import site';" ^
        "Set-Content '%%f' $content"
)
"%PYTHON_DIR%\python.exe" -m pip --version >nul 2>nul
if errorlevel 1 (
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
        echo ERROR: Failed to install pip into the embedded controlpanel runtime.
        pause
        exit /b 1
    )
    del "%PYTHON_DIR%\get-pip.py"
)
"%PYTHON_DIR%\python.exe" -m pip install --upgrade pip setuptools wheel --no-warn-script-location
if errorlevel 1 (
    echo.
    echo ERROR: Failed to upgrade pip tooling in the embedded controlpanel runtime.
    pause
    exit /b 1
)

echo [4/4] Installing controlpanel packages (this may take a few minutes)...
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
echo  Control Panel runtime is ready at:
echo    %PYTHON_DIR%
echo.
echo  Commit or package the entire controlpanel\python\ folder together
echo  with xAquaticRisk so copied working trees stay offline-ready.
echo ============================================================
pause
