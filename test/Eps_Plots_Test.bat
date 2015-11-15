@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Eps_Plots_Test.adb
del /q *.ali
del /q *.o
echo -----------
Eps_Plots_Test.exe
echo -----------
del Eps_Plots_Test.exe
pause
