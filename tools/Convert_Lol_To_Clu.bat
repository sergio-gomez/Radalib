@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Convert_Lol_To_Clu.adb
del /q *.ali
del /q *.o
echo -----------
Convert_Lol_To_Clu.exe  test-convert_lol_toy-lol.txt  test-convert_lol_toy.clu  4
Convert_Lol_To_Clu.exe  test-zachary-lol1.txt  test-zachary-lol1.clu  4
echo -----------
del Convert_Lol_To_Clu.exe
pause
