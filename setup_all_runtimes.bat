@echo off
setlocal

echo ============================================================
echo  xAquaticRisk - Rebuild Bundled Component Runtimes
echo ============================================================
echo  Maintainer utility. This refreshes the vendored runtimes that are
echo  expected to already exist in a copied working tree.
echo.
echo  This runs:
echo    1) setup_controlpanel_python.bat
echo    2) setup_analysis_python.bat
echo.

call :run_setup "setup_controlpanel_python.bat" "1/2"
if errorlevel 1 goto :failed

call :run_setup "setup_analysis_python.bat" "2/2"
if errorlevel 1 goto :failed

echo.
echo ============================================================
echo  All component runtimes are ready.
echo  Keep controlpanel\python\ and analysis\python\ inside the
echo  committed or packaged xAquaticRisk working tree.
echo ============================================================
pause
exit /b 0

:run_setup
set "SCRIPT=%~1"
set "STEP=%~2"
set "SCRIPT_PATH=%~dp0%SCRIPT%"

if not exist "%SCRIPT_PATH%" (
    echo.
    echo ERROR: Missing setup script: %SCRIPT_PATH%
    exit /b 1
)

echo.
echo [%STEP%] Running %SCRIPT% ...
call "%SCRIPT_PATH%" <nul
if errorlevel 1 (
    echo.
    echo ERROR: %SCRIPT% failed.
    exit /b 1
)
exit /b 0

:failed
echo.
echo ============================================================
echo  Bootstrap failed. Review errors above.
echo ============================================================
pause
exit /b 1
