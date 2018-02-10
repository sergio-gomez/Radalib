@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graph_Eigenvalues.adb
del /q *.ali
del /q *.o
echo -----------
Graph_Eigenvalues.exe test-zachary_unwh.net
Graph_Eigenvalues.exe test-zachary_unwh.net test-zachary_unwh-eig.txt
echo -----------
del Graph_Eigenvalues.exe
pause
