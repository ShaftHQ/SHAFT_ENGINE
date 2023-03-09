@echo OFF

@set PATH=C:\Users\AbbasM3\node-v16.19.1-win-x86

@cd %PATH%

set "status=NodeJS not installed"

if exist "./node.exe" (
    set "status=NodeJS installed"
)


echo %status%

if "%status%" equ "NodeJS installed" (

@cd node_modules
set "LhStatus=Lighthouse Module isn't installed"
set "PuppeteerStatus=Puppeteer Module isn't installed"
set "FSStatus=FS Module isn't installed"
set "OpenStatus=Open Module isn't installed"
set "OptimistStatus=Optimist Module isn't installed"

if exist "./lighthouse" (
    set "LhStatus=Lighthouse Module is installed"
	
)

if exist "./puppeteer" (
    set "PuppeteerStatus=Puppeteer Module is installed"

)
if exist "./fs" (
    set "FSStatus=FS Module is installed"
	
	
)
if exist "./open" (
    set "OpenStatus=Open Module is installed"
	
)
if exist "./optimist" (
    set "OptimistStatus=Optimist Module is installed"
	
)

echo %LhStatus%
echo %PuppeteerStatus%
echo %FSStatus%
echo %OpenStatus%
echo %OptimistStatus%

@cd..

if "%LhStatus%" equ "Lighthouse Module isn't installed" (
call npm install lighthouse
)
if "%PuppeteerStatus%" equ "Puppeteer Module isn't installed" (
call npm install puppeteer

)
if "%FSStatus%" equ "FS Module isn't installed" (
call npm install fs

)
if "%OpenStatus%" equ "Open Module isn't installed" (
call npm install open

)
if "%OptimistStatus%" equ "Optimist Module isn't installed" (
call npm install optimist

)

)
@cmd.exe /k