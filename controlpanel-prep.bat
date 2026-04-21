@echo off
setlocal

echo ============================================================
echo Starting xAquaticRisk Preparation and Execution UI...
echo ============================================================
echo.

if not defined XAQ_PORT set "XAQ_PORT=8090"
call "%~dp0controlpanel.bat" --ui-profile prep %*
