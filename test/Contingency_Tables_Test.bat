@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Contingency_Tables_Test.adb
del /q *.ali
del /q *.o
echo -----------
Contingency_Tables_Test.exe
echo -----------
del Contingency_Tables_Test.exe
pause
