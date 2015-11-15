@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Data_To_Correlations.adb
del /q *.ali
del /q *.o
echo -----------
Data_To_Correlations.exe  test-data_toy.txt  r  s01  test-data_toy-rows.net  6
Data_To_Correlations.exe  test-data_toy.txt  c  ns   test-data_toy-cols.net  6
echo -----------
del Data_To_Correlations.exe
pause
