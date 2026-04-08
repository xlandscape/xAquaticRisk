@echo off
setlocal

echo ============================================================
echo Starting xAquatic Risk Control Panel...
echo ============================================================
echo.

set "CONTROLPANEL_PY=%~dp0controlpanel\python\python.exe"
if not exist "%CONTROLPANEL_PY%" (
    echo ERROR: Control Panel runtime not found.
    echo Expected: %CONTROLPANEL_PY%
    echo.
    echo Run setup_controlpanel_python.bat once and keep controlpanel\python\ in the copied model folder.
    pause
    exit /b 1
)

"%CONTROLPANEL_PY%" "%~dp0controlpanel\server.py" %*
pause
