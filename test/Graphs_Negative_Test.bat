@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graphs_Negative_Test.adb
del /q *.ali
del /q *.o
echo -----------
Graphs_Negative_Test.exe toy_network_4+4_UW.net
Graphs_Negative_Test.exe toy_network_4+4_DWpos.net
Graphs_Negative_Test.exe toy_network_4+4_DWneg.net
Graphs_Negative_Test.exe toy_network_4+4_DWmix.net
echo -----------
del Graphs_Negative_Test.exe
pause
