@echo off
set "SHAFT_CONTROLLER_HOME=%~dp0"
set "SHAFT_MAINTENANCE_HOME=%~dp0..\.."
set "SHAFT_ROOT=%SHAFT_MAINTENANCE_HOME%\SHAFT_ENGINE-main"
set "SHAFT_SENTINEL=%SHAFT_MAINTENANCE_HOME%\.shaft-nightly-maintenance.json"
set "SHAFT_LOG=%SHAFT_MAINTENANCE_HOME%\Logs\shaft-knowledge-refresh.log"
py -3 "%~dp0shaft_knowledge_refresh.py" --root "%SHAFT_ROOT%" --sentinel "%SHAFT_SENTINEL%" --validate-only >nul 2>&1
if errorlevel 1 exit /b 1
if exist "%SHAFT_LOG%" move /y "%SHAFT_LOG%" "%SHAFT_MAINTENANCE_HOME%\Logs\shaft-knowledge-refresh.previous.log" >nul
py -3 "%~dp0shaft_knowledge_refresh.py" --root "%SHAFT_ROOT%" --sentinel "%SHAFT_SENTINEL%" > "%SHAFT_LOG%" 2>&1
exit /b %errorlevel%
