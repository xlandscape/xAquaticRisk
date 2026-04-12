@echo off
setlocal

echo ============================================================
echo  xAquaticRisk - Bootstrap All Component Runtimes
echo ============================================================
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
echo  Keep controlpanel\python\ and analysis\python\
echo  inside the copied model folder for xcopy deployment.
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
