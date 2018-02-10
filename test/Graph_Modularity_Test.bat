@echo off
set ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;%RADALIB_ROOT%\source
set ADA_OBJECTS_PATH=%ADA_OBJECTS_PATH%;%RADALIB_ROOT%\compiled
gnatmake -O2 Graph_Modularity_Test.adb
del /q *.ali
del /q *.o
echo -----------
Graph_Modularity_Test.exe test-toy_network_4+4_UW.net    test-toy_network_4+4.clu     WN
Graph_Modularity_Test.exe test-toy_network_4+4_DWmix.net test-toy_network_4+4-lol.txt WS
Graph_Modularity_Test.exe test-toy_network_4+4_DWpos.net test-toy_network_4+4-lol.txt WLR
echo -----------
del Graph_Modularity_Test.exe
pause
