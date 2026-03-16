@echo off
setlocal

echo ============================================================
echo Starting xAquatic Model Web Interface...
echo ============================================================
echo.

set "WEBUI_PY=%~dp0webui\python\python.exe"
if not exist "%WEBUI_PY%" (
    echo ERROR: WebUI runtime not found.
    echo Expected: %WEBUI_PY%
    echo.
    echo Run setup_webui_python.bat once and keep webui\python\ in the copied model folder.
    pause
    exit /b 1
)

"%WEBUI_PY%" "%~dp0webui\server.py" %*

pause
