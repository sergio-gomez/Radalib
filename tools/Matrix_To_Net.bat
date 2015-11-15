@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Matrix_To_Net.adb
del /q *.ali
del /q *.o
echo -----------
Matrix_To_Net.exe  test-matrix_toy_01.txt  test-matrix_toy_01.net  0
Matrix_To_Net.exe  test-matrix_toy_02.txt  test-matrix_toy_02.net  0.00
Matrix_To_Net.exe  test-matrix_toy_03.txt  test-matrix_toy_03.net  N/A
echo -----------
del Matrix_To_Net.exe
pause
