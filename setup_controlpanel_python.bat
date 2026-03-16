@echo off
setlocal

REM -----------------------------------------------------------------------------
REM  setup_controlpanel_python.bat
REM
REM  Creates a dedicated embedded Python runtime for controlpanel\server.py in
REM  controlpanel\python\ so the Control Panel has no dependency on model-core
REM  or system Python.
REM -----------------------------------------------------------------------------

set PYTHON_VERSION=3.9.7
set PYTHON_URL=https://www.python.org/ftp/python/%PYTHON_VERSION%/python-%PYTHON_VERSION%-embed-amd64.zip

set PYTHON_DIR=%~dp0controlpanel\python

echo ============================================================
echo  xAquaticRisk - Control Panel Python Setup
echo  Python  : %PYTHON_VERSION% embeddable package
echo  Target  : %PYTHON_DIR%
echo ============================================================
echo.

if exist "%PYTHON_DIR%\python.exe" (
    echo Control Panel Python already installed at:
    echo   %PYTHON_DIR%\python.exe
    echo Delete controlpanel\python\ and re-run this script to reinstall.
    echo.
    pause
    exit /b 0
)

echo [1/3] Downloading Python %PYTHON_VERSION% embeddable package...
powershell -NoProfile -Command ^
    "Invoke-WebRequest -Uri '%PYTHON_URL%' -OutFile '%TEMP%\xaq-controlpanel-python.zip' -UseBasicParsing"
if errorlevel 1 (
    echo.
    echo ERROR: Download failed. Check your internet connection.
    pause
    exit /b 1
)

echo [2/3] Extracting to %PYTHON_DIR% ...
if exist "%PYTHON_DIR%" rmdir /s /q "%PYTHON_DIR%"
mkdir "%PYTHON_DIR%" 2>nul
powershell -NoProfile -Command ^
    "Expand-Archive -Path '%TEMP%\xaq-controlpanel-python.zip' -DestinationPath '%PYTHON_DIR%' -Force"
del "%TEMP%\xaq-controlpanel-python.zip"

echo [3/3] Enabling standard site initialization...
mkdir "%PYTHON_DIR%\Lib\site-packages" 2>nul
for %%f in ("%PYTHON_DIR%\python3*._pth") do (
    powershell -NoProfile -Command ^
        "$content = Get-Content '%%f';" ^
        "$content = $content | Where-Object { $_ -ne 'Lib\\site-packages' };" ^
        "$content += 'Lib\\site-packages';" ^
        "$content = $content -replace '#import site', 'import site';" ^
        "Set-Content '%%f' $content"
)

echo.
echo ============================================================
echo  Setup complete!
echo  Control Panel runtime is ready at:
echo    %PYTHON_DIR%
echo.
echo  Important: include the entire controlpanel\python\ folder when
echo  copying or packaging xAquaticRisk for xcopy deployment.
echo ============================================================
pause
