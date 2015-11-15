@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graphs_Properties_Test.adb
del /q *.ali
del /q *.o
echo -----------
Graphs_Properties_Test.exe toy_network_DU.net
Graphs_Properties_Test.exe toy_network_DW.net
Graphs_Properties_Test.exe toy_network_UU.net
Graphs_Properties_Test.exe toy_network_UW.net
rem Graphs_Properties_Test.exe zachary_unwh.net
rem Graphs_Properties_Test.exe zachary_wh.net
echo -----------
del Graphs_Properties_Test.exe
pause
