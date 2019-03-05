@echo off
set path=C:\Program Files\Java\jdk1.8.0_131\bin\;%path%
java -version
java -jar selenium-server-standalone.jar -port 4444 -role hub
pause
exit