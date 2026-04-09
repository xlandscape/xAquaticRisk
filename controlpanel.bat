@echo off
setlocal

echo ============================================================
echo Starting xAquatic Risk Control Panel...
echo ============================================================
echo.

set "CONTROLPANEL_PY=%~dp0controlpanel\python\python.exe"
if not exist "%CONTROLPANEL_PY%" (
    set "CONTROLPANEL_PY=%~dp0.venv\Scripts\python.exe"
    if exist "%CONTROLPANEL_PY%" (
        echo INFO: Bundled controlpanel runtime not found.
        echo Using development venv runtime: %CONTROLPANEL_PY%
        echo.
    ) else (
        echo ERROR: Control Panel runtime not found.
        echo Expected bundled runtime: %~dp0controlpanel\python\python.exe
        echo Also checked dev fallback: %~dp0.venv\Scripts\python.exe
        echo.
        echo Run setup_controlpanel_python.bat or create a local .venv.
        pause
        exit /b 1
    )
)

"%CONTROLPANEL_PY%" "%~dp0controlpanel\server.py" %*
pause
