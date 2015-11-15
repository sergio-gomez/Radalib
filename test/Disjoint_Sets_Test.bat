@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Disjoint_Sets_Test.adb
del /q *.ali
del /q *.o
echo -----------
Disjoint_Sets_Test.exe
echo -----------
del Disjoint_Sets_Test.exe
pause
