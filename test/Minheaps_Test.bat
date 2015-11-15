@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Minheaps_Test.adb
del /q *.ali
del /q *.o
echo -----------
Minheaps_Test.exe
echo -----------
del Minheaps_Test.exe
pause
