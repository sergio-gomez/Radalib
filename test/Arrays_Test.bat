@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Arrays_Test.adb
del /q *.ali
del /q *.o
echo -----------
Arrays_Test.exe
echo -----------
del Arrays_Test.exe
pause
