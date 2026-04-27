@ echo off
setlocal
set "CHECK_SCRIPT=%~dp0check_bundled_runtime_blocks.ps1"
set "PREFLIGHT_MARKER=%~dp0.runtime_preflight.ok"
set "PREFLIGHT_MODE=%XAQR_PREFLIGHT_MODE%"

if not defined PREFLIGHT_MODE set "PREFLIGHT_MODE=auto"

set "RUN_PREFLIGHT=0"
if /I "%PREFLIGHT_MODE%"=="always" (
	set "RUN_PREFLIGHT=1"
) else if /I "%PREFLIGHT_MODE%"=="auto" (
	if not exist "%PREFLIGHT_MARKER%" set "RUN_PREFLIGHT=1"
) else if /I "%PREFLIGHT_MODE%"=="never" (
	set "RUN_PREFLIGHT=0"
) else (
	echo WARNING: Invalid XAQR_PREFLIGHT_MODE="%PREFLIGHT_MODE%". Falling back to "auto".
	set "PREFLIGHT_MODE=auto"
	if not exist "%PREFLIGHT_MARKER%" set "RUN_PREFLIGHT=1"
)

if "%RUN_PREFLIGHT%"=="1" (
	if exist "%CHECK_SCRIPT%" (
		echo Running bundled runtime preflight ^(mode: %PREFLIGHT_MODE%^)...
		powershell -NoProfile -ExecutionPolicy Bypass -File "%CHECK_SCRIPT%" -ProjectRoot "%~dp0."
		if errorlevel 1 (
			echo.
			echo Startup aborted due to bundled runtime preflight failure.
			pause
			exit /b 1
		)
		(
			echo preflight_ok=1
			echo mode=%PREFLIGHT_MODE%
			echo marker_created=%DATE% %TIME%
		) > "%PREFLIGHT_MARKER%"
	) else (
		echo WARNING: Preflight script not found. Continuing without runtime preflight.
	)
)

"%~dp0\model\core\bin\python-3.9.7-amd64\python.exe" -u "%~dp0\model\core\init.py" %*
pause