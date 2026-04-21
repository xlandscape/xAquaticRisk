@echo off
setlocal

echo ============================================================
echo Starting xAquaticRisk Analysis and Exploration UI...
echo ============================================================
echo.

if not defined XAQ_PORT set "XAQ_PORT=8091"
call "%~dp0controlpanel.bat" --ui-profile analysis %*
