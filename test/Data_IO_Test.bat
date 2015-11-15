@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Data_IO_Test.adb
del /q *.ali
del /q *.o
echo -----------
Data_IO_Test.exe
echo -----------
del Data_IO_Test.exe
pause
