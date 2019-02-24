@echo off
set path=C:\Program Files\Java\jdk1.8.0_131\bin\;%path%
java -version
java -jar selenium-server-standalone.jar -role node -nodeConfig nodeconfig.json
pause
exit