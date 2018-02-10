@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graphs_Modularities_Test_D.adb
del /q *.ali
del /q *.o
echo -----------
Graphs_Modularities_Test_D.exe test-toy_network_4+4_UW.net    test-toy_network_4+4.clu
Graphs_Modularities_Test_D.exe test-toy_network_4+4_DWpos.net test-toy_network_4+4.clu
echo -----------
del Graphs_Modularities_Test_D.exe
pause
