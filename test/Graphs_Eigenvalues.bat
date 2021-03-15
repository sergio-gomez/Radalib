@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graphs_Eigenvalues.adb
del /q *.ali
del /q *.o
echo -----------
Graphs_Eigenvalues.exe test-zachary_unwh.net
Graphs_Eigenvalues.exe test-zachary_unwh.net test-zachary_unwh-eig.txt
echo -----------
del Graphs_Eigenvalues.exe
pause
