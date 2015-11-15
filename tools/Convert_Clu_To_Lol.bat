@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Convert_Clu_To_Lol.adb
del /q *.ali
del /q *.o
echo -----------
Convert_Clu_To_Lol.exe  test-convert_clu_toy.clu  test-convert_clu_toy-lol.txt
Convert_Clu_To_Lol.exe  test-convert_clu_toy.clu  test-convert_clu_toy-sorted-lol.txt  sorted
echo -----------
del Convert_Clu_To_Lol.exe
pause
