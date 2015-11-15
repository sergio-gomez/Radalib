@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Data_Statistics.adb
del /q *.ali
del /q *.o
echo -----------
Data_Statistics.exe  test-data_toy.txt  5     r  4
Data_Statistics.exe  test-data_toy.txt  2     c  6
Data_Statistics.exe  test-data_toy.txt  1  3  c  6
Data_Statistics.exe  test-data_toy.txt  test-data_toy-stats.txt  c  6
echo -----------
del Data_Statistics.exe
pause
