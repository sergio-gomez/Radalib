@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graphs_Modularities_D_Test.adb
del /q *.ali
del /q *.o
echo -----------
Graphs_Modularities_D_Test.exe toy_network_4+4_UW.net    toy_network_4+4.clu
Graphs_Modularities_D_Test.exe toy_network_4+4_DWpos.net toy_network_4+4.clu
echo -----------
del Graphs_Modularities_D_Test.exe
pause
