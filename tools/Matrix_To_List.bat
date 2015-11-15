@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Matrix_To_List.adb
del /q *.ali
del /q *.o
echo -----------
Matrix_To_List.exe  test-matrix_toy_01.txt  test-matrix_toy_01-list.txt  0
Matrix_To_List.exe  test-matrix_toy_02.txt  test-matrix_toy_02-list.txt  0.00
Matrix_To_List.exe  test-matrix_toy_03.txt  test-matrix_toy_03-list.txt  N/A
echo -----------
del Matrix_To_List.exe
pause
