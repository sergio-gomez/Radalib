@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Net_To_Matrix.adb
del /q *.ali
del /q *.o
echo -----------
Net_To_Matrix.exe  test-net_toy_01.net  test-net_toy_01-matrix.txt
Net_To_Matrix.exe  test-net_toy_02.net  test-net_toy_02-matrix.txt  0.0
echo -----------
del Net_To_Matrix.exe
pause
