@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Multiplex_Extract_Layers.adb
del /q *.ali
del /q *.o
echo -----------
Multiplex_Extract_Layers.exe  test-multiplex_toy_01.txt  test-multiplex_toy_01  undirected
Multiplex_Extract_Layers.exe  test-multiplex_toy_02.txt  test-multiplex_toy_02  directed
echo -----------
del Multiplex_Extract_Layers.exe
pause
