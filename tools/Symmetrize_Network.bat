@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Symmetrize_Network.adb
del *.ali
del *.o
echo -----------
Symmetrize_Network.exe  test-size_toy.net  test-size_toy-sym.net  Integer
Symmetrize_Network.exe  test-zachary_unwh.net  test-zachary_unwh-sym.net  Float  2
echo -----------
del Symmetrize_Network.exe
pause
