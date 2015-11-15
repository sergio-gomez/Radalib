@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Histogram_Test.adb
del /q *.ali
del /q *.o
echo -----------
Histogram_Test.exe
echo -----------
del Histogram_Test.exe
pause
