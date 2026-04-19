@ echo off
setlocal
set "CHECK_SCRIPT=%~dp0check_bundled_runtime_blocks.ps1"

if exist "%CHECK_SCRIPT%" (
	powershell -NoProfile -ExecutionPolicy Bypass -File "%CHECK_SCRIPT%" -ProjectRoot "%~dp0."
	if errorlevel 1 (
		echo.
		echo Startup aborted due to bundled runtime preflight failure.
		pause
		exit /b 1
	)
)

"%~dp0\model\core\bin\python-3.9.7-amd64\python.exe" -u "%~dp0\model\core\init.py" %*
pause