@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Queues_Test.adb
del /q *.ali
del /q *.o
echo -----------
Queues_Test.exe
echo -----------
del Queues_Test.exe
pause
