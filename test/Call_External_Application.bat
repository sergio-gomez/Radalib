@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Call_External_Application.adb
del /q *.ali
del /q *.o
echo -----------
Call_External_Application.exe
echo -----------
del Call_External_Application.exe
pause
