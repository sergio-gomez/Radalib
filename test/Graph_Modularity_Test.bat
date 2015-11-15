@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graph_Modularity_Test.adb
del /q *.ali
del /q *.o
echo -----------
Graph_Modularity_Test.exe toy_network_4+4_UW.net    toy_network_4+4.clu     WN
Graph_Modularity_Test.exe toy_network_4+4_DWmix.net toy_network_4+4-lol.txt WS
Graph_Modularity_Test.exe toy_network_4+4_DWpos.net toy_network_4+4-lol.txt WLR
echo -----------
del Graph_Modularity_Test.exe
pause
