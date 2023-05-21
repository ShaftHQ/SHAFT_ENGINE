@echo off
where node > nul 2>&1
if %errorlevel% neq 0 (
echo Node.js is not installed,please donwnload Node.JS
exit /b 1 
)
call npm list -g --depth 0 lighthouse > nul 2>&1
if %errorlevel% neq 0 (
call npm install -g lighthouse  
)
call npm list -g --depth 0 puppeteer > nul 2>&1
if %errorlevel% neq 0 (
call npm install -g puppeteer 
)
call npm list -g --depth 0 optimist > nul 2>&1
if %errorlevel% neq 0 (
call npm install -g optimist 
)
call npm list -g --depth 0 fs > nul 2>&1
if %errorlevel% neq 0 (
call npm install -g fs 
)
call npm list -g --depth 0 open > nul 2>&1
if %errorlevel% neq 0 (
call npm install -g open 
)
exit /b 0