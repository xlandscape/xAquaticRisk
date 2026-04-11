@echo off
setlocal

set "ALLOW_DEV_PYTHON=%XAQ_ALLOW_DEV_PYTHON%"

echo ============================================================
echo Starting xAquatic Risk Control Panel...
echo ============================================================
echo.

set "CONTROLPANEL_PY=%~dp0controlpanel\python\python.exe"
if not exist "%CONTROLPANEL_PY%" (
    if /I "%ALLOW_DEV_PYTHON%"=="1" (
        set "CONTROLPANEL_PY=%~dp0.venv\Scripts\python.exe"
        if exist "%CONTROLPANEL_PY%" (
            echo INFO: Bundled controlpanel runtime not found.
            echo Using development venv runtime because XAQ_ALLOW_DEV_PYTHON=1.
            echo.
        ) else (
            echo ERROR: Control Panel runtime not found.
            echo Expected bundled runtime: %~dp0controlpanel\python\python.exe
            echo Dev fallback was enabled, but .venv\Scripts\python.exe was not found either.
            echo.
            echo Run setup_controlpanel_python.bat to provision the bundled runtime.
            pause
            exit /b 1
        )
    ) else (
        echo ERROR: Control Panel runtime not found.
        echo Expected bundled runtime: %~dp0controlpanel\python\python.exe
        echo.
        echo Run setup_controlpanel_python.bat to provision the bundled runtime.
        echo For development-only fallback to .venv, set XAQ_ALLOW_DEV_PYTHON=1 explicitly.
        pause
        exit /b 1
    )
)

"%CONTROLPANEL_PY%" "%~dp0controlpanel\server.py" %*
pause
