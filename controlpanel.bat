@echo off
echo ============================================================
echo Starting xAquaticRisk Control Panel...
echo ============================================================
echo.

REM Try to use the bundled Python first, then fall back to system Python
if exist "%~dp0model\core\bin\python-3.9.7-amd64\python.exe" (
    "%~dp0model\core\bin\python-3.9.7-amd64\python.exe" "%~dp0controlpanel\server.py" %*
) else (
    python "%~dp0controlpanel\server.py" %*
)

pause
