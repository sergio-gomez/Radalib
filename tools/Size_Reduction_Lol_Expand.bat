@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Size_Reduction_Lol_Expand.adb
del *.ali
del *.o
echo -----------
Size_Reduction_Lol_Expand.exe  test-size_toy-reduced-lol.txt  test-size_toy-reducer.txt  test-size_toy-expanded-lol.txt  4  CH
echo -----------
del Size_Reduction_Lol_Expand.exe
pause
