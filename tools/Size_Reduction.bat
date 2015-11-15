@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Size_Reduction.adb
del *.ali
del *.o
echo -----------
Size_Reduction.exe  test-size_toy  Float 2
Size_Reduction.exe  test-size_toy  Integer
Size_Reduction.exe  test-size_toy-reduced  Double  6
Size_Reduction.exe  test-zachary_unwh  Integer
Size_Reduction.exe  test-dolphins  Integer
echo -----------
del Size_Reduction.exe
pause
