@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graphs_Test.adb
del /q *.ali
del /q *.o
echo -----------
Graphs_Test.exe
echo -----------
del Graphs_Test.exe
pause
