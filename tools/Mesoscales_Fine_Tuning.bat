@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Mesoscales_Fine_Tuning.adb
del /q *.ali
del /q *.o
echo -----------
Mesoscales_Fine_Tuning.exe  test-zachary_unwh  WN
Mesoscales_Fine_Tuning.exe  test-dolphins      WS
echo -----------
del Mesoscales_Fine_Tuning.exe
pause
