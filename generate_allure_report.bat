@echo off
set path=src\main\resources\allure\bin;%path%
allure serve allure-results
pause
exit